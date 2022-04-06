#lang scribble/manual

@(require #;racket
          "../define.rkt"
          "../program.rkt"
          "../primitives.rkt"
          "../testing-util.rkt"
          (rename-in scribble/example [examples s/e:examples])
	  syntax/parse/define
          (for-label racket
                     "../define.rkt"
                     "../program.rkt"
                     "../primitives.rkt"
                     "../testing-util.rkt")
          (for-syntax racket/base))

@(define new-eval (make-eval-factory '(syntax/parse racket/list mutate/define mutate/program mutate/primitives mutate/testing-util)))

@(define-simple-macro (examples {~optional {~seq #:eval user-eval}} more ...)
   #:with eval-e (or (attribute user-eval) #'(new-eval))
   (s/e:examples
     #:escape _examples-escape_
     #:eval eval-e
     more ...))

@title{Mutate}
@author[(author+email "Lukas Lazarek" "lukas.lazarek@eecs.northwestern.edu"
#:obfuscate? #t)]




A library for mutating Racket programs.
That is, systematically injecting possible bugs by making small syntactic modifications to a program.

@section{Prologue}
@subsection{This library's structure}
This library consists of two basic pieces.
First, it provides an embedded language and tools for defining and composing mutators.
Second, it provides a framework for applying mutators to whole programs.

This documentation is structured according to these two pieces.

@subsection[#:tag "concepts"]{Mutator concepts}
This library centers around the concept of a @deftech{mutator}, which describes a small syntactic modification to a piece of syntax.
A mutator can roughly be thought of as a function from syntax to syntax which does the modification.
In typical mutation practice, the mutation is meant to introduce a possible change in program behavior --- usually this means a bug.
For example, a typical standard mutator negates the condition of @racket[if] expressions, which we might write as a kind of pattern transformation like so:

@racket[(if c t e)] ~> @racket[(if (not c) t e)]

By using this mutator on a program, we can create a mutated variant of the program called a @deftech{mutant} program;
in other words, a possibly-buggy variant of the original program.
The purpose of mutators is to create mutants.

Given a whole program, there are typically many possible places that a mutator can be applied.
For example, this program has two @racket[if] expressions, each of which could be mutated by our if-negating mutator.
@codeblock{
#lang racket
(define (abs x)
  (if (< x 0) ; mutation point
      (- x)
      x))
(define (f n)
  (if (< (abs n) 50) ; mutation point
      'ok
      'too-big))
(displayln (f (random)))
}
Each of those places where our mutator could be applied are called @deftech{mutation point}s.
This program illustrates that even our single mutator can have multiple mutation points for some program;
add in that usually a mutation system includes many different mutators, and any given program will have many mutation points.

In typical mutation practice, however, one only wants to use @emph{one} of those mutation points to mutate a program.
This means that @tech{mutant}s result from the application of a mutator at a single mutation point, and that a single program has many mutants that one can select from --- one corresponding to each of the mutation points in the program.
(Check out @secref{literature} for the details of why one usually wants just one mutation per mutant.)
Hence, using mutators effectively requires a way to select one mutation point among the many in a program.

This library provides that selection through a pre-defined traversal of programs, which places all of the mutation points of a given program in a sequence.
For example, if the traversal is roughly top-to-bottom, left-to-right, then the example program from before (with just the if-negating mutator) has a sequence of two mutation points: first the one in @tt{abs} and then the one in @tt{f}.
We can select one of those mutation points with an index into the sequence, which is called a @deftech{mutation index}.
The mutation index is the way to select a particular mutation point with this library.

Summing it all up so far, to mutate a program entails applying a mutator (or several) to a program along with a @tech{mutation index}, which selects which @tech{mutation point} to use.

Internally, this library uses a counter (called a @deftech{mutation counter}) to keep track of @tech{mutation point}s as it traverses a program.
Every mutator consumes a counter and produces an updated counter along with its possibly-mutated result syntax.
The counter (along with the @tech{mutation index}) provides the mechanism to decide whether a particular @tech{mutation point} should be used or not:
if a mutator is applied with a counter that is smaller than the mutation index, then that mutator's mutation point should be skipped and the counter incremented to record that a mutation point has been passed;
when the counter is equal to the mutation index, then the mutation point has been selected and the mutation should be performed as well as incrementing the counter.
Thus the counter is threaded through the program traversal and mutator applications.

Now we have all the pieces to define a mutator concretely.
A mutator is a function of three arguments: a piece of syntax, a @tech{mutation index}, and the current @tech{mutation counter},
and it returns two things: the possibly-mutated syntax, and the updated @tech{mutation counter}.
Technically, the two results are wrapped in a @racket[mutated] struct.
See @racket[mutator/c] for all the technical details to the definition, but this is the core idea.

To alleviate the burden of keeping track of the @tech{mutation index} and @tech{mutation counter}, this library provides different tiers of interface to defining mutators that abstract over that accounting.
@itemlist[
@item{The highest level are the mutator definition forms in the next section, @secref{definition}, which support defining mutators with simple patterns and should suffice for most common mutators.}
@item{For more complex mutators, the next tier (in @secref{procedural-api}) provides an api for performing mutations and threading the @tech{mutation counter}. The api lets mutators be defined in a style where the implementation simply applies all possible mutations to a piece of syntax, and the internals of the api cooperate to select the right one.}
@item{The final tier (in @secref{composition}) provides a small combinator-like language for creating mutators out of other mutators (or plain functions), and for manipulating mutators.}
]


@subsection[#:tag "big-picture-usage"]{Using mutators to mutate programs}
The perspective on mutation described in the previous section (and embodied in this library) lends itself to the following style of creating mutators.

First, one defines @deftech{simple mutator}s, each of which succinctly describes a single mutation by only considering the outermost shape of a piece of syntax.
In other words, simple mutators need only "do the right thing" when given directly a piece of syntax that they can mutate; they do not seek out (ie traverse to find) sub-parts of the syntax that could be mutated.

With one or more simple mutators in hand, one combines them all together (with @racket[compose-mutators]) and uses the resulting mutator to create an @deftech{expression mutator} (with @racket[make-expr-mutator]).
The expression mutator traverses the syntax it receives, searching for @tech{mutation points} for the simple mutator(s) it was created with.
In other words, @racket[make-expr-mutator] lifts a simple mutator from mutating single expressions to mutating expressions including all of their sub-expressions.

Finally, one can use an expression mutator to create a @deftech{program mutator} (with @racket[make-program-mutator]).
The program mutator traverses a whole program / module based on a notion of top level forms, deciding which top level forms to consider for mutation, and communicating to its caller additional information about which top level form is selected for mutation by a given @tech{mutation index} -- or raising an error if the index exceeds the maximum possible for a program.


@subsection[#:tag "literature"]{The mutation literature}
For a detailed overview of the idea of mutation and how it is used in the literature, see this survey paper:
@para{Yue Jia and Mark Harman. 2011. An analysis and survey of the development of mutation testing. IEEE transactions on software engineering 37, 5 (2011), 649-678.}

The core ideas of mutation originate in these papers:
@itemlist[
@item{Richard J Lipton. 1971. Fault diagnosis of computer programs.}
@item{Richard A. DeMillo, Richard J. Lipton, and Frederick G. Sayward. 1978. Hints on test data selection: Help for the practicing programmer. Computer 11, 4 (1978), 34-41.}
@item{Richard A. DeMillo, Dana S. Guindi, Kim King, Mike M. McCracken, and Jefferson A. Offutt. 1988. An extended overview of the Mothra software testing environment. In Proceedings of the Second Workshop on Software Testing, Verification, and Analysis. IEEE, New York, NY, 142-151.}
]


@section{Mutators}
@subsection[#:tag "definition"]{Defining mutators}
@defmodule[mutate/define @; #:multi (mutate )
]

The following forms define @tech{mutator}s (see @secref{concepts}).
In addition to the basic model of a mutator as a function, every mutator has a @deftech{mutator type}: a string that names the mutator for logging purposes (see @secref{logging}).

@defform[
(define-simple-mutator (id syntax-id) maybe-type-spec
  #:pattern syntax-parse-pattern
  maybe-guard
  body ...)
#:grammar [(maybe-type-spec (code:line) (code:line #:type type-name-expr))
           (maybe-guard (code:line)     (code:line #:when guard-expr))]
]{
The simplest generic mutator definition form.

Defines a simple mutator named @racket[id] that mutates syntax bound to @racket[syntax-id].

If provided, @racket[type-name-expr] must produce a string that is the @tech{mutator type} of the mutator.
If not provided, the type defaults to @racket[id] as a string.

@racket[syntax-parse-pattern] is a @racket[syntax/parse] pattern captures what shape of syntax this mutator operates upon.

If provided, @racket[guard-expr] guards the application of the mutator (evaluating to @racket[#f] causes the mutation to be skipped). Pattern variables in @racket[syntax-parse-pattern] are bound in the scope of @racket[guard-expr].

The @racket[body] forms must produce the mutated syntax.

@examples[#:label @para{Example: (The two @tt{0}s are the @tech{mutation index} and @tech{mutation counter} respectively; see @secref{concepts}.)}
(define-simple-mutator (if-swap stx)
  #:pattern ({~literal if} c t e)
  #'(if c e t))

(if-swap #'(if (< x 0) 0 (f x)) 0 0)
(if-swap #'(not-an-if 42 (+ 2 3)) 0 0)]
}

@defform[
(define-id-mutator id maybe-type-spec
  id-swap-spec ...)
#:grammar [(maybe-type-spec (code:line) (code:line #:type type-name-expr))
           (id-swap-spec (code:line [original-id #:->  swapped-id])
                         (code:line [left-id     #:<-> right-id]))]
]{
Defines a mutator that only swaps identifiers.

Each @racket[id-swap-spec] specifies one way to swap identifiers.
The first variant of @racket[id-swap-spec]s, using @racket[#:->], specifies a one-way swap: @racket[original-id] will be swapped with @racket[swapped-id].
In contrast, the second variant, using @racket[#:<->], specifies a two-way swap: @racket[left-id] will be swapped with @racket[right-id], and vice-versa.
The second form is equivalent to two copies of the first form with swapped sides.

@examples[
(define-id-mutator arithmetic-op-swap
  [+ #:<-> -]
  [* #:->  /])

(arithmetic-op-swap #'+ 0 0)
(arithmetic-op-swap #'- 0 0)
(arithmetic-op-swap #'* 0 0)
(code:line (arithmetic-op-swap #'/ 0 0) (code:comment "no mutation"))
]
}

@defform[
(define-constant-mutator (id constant-value-id) maybe-type-spec
  value-swap-spec ...)
#:grammar [(maybe-type-spec (code:line) (code:line #:type type-name-expr))
           (value-swap-spec (code:line [original-value-pat #:-> replacement-expr]))]
]{
Defines a mutator that transforms constants.
Unlike most other mutators, this mutator is not defined in terms of syntax.
Constant mutators automatically extract the value of constants to allow easier transformation, and re-pack the transformed values back into syntax objects.

Each @racket[value-swap-spec] specifies one way to transform constants.
@racket[original-value-pat] is a @racket[match] pattern that matches the value of a constant, and if the pattern matches @racket[replacement-expr] is its replacement value.
Each matching @racket[value-swap-spec] is tried in turn, so they may overlap.

@examples[
(define-constant-mutator (number-constant-swap v)
  [(? number?)                 #:-> (- v)]
  [(? integer?)                #:-> (exact->inexact v)]
  [(and (? number?) (? zero?)) #:-> 1]
  [(? real?)                   #:-> (* 1+0.0i v)])

(number-constant-swap #'5 0 0)
(number-constant-swap #'5 1 0)
(number-constant-swap #'5 2 0)
(number-constant-swap #'0 0 0)
(number-constant-swap #'0 1 0)]
}

@defform[
(define-mutator (id stx-id mutation-index-id counter-id) #:type type-expr
  body ...)
]{
Defines a general-purpose mutator accepting first the syntax to mutate, then the @tech{mutation index}, and finally the current @tech{mutation counter}.

Unlike the simpler mutator definition forms above, the @racket[body] of the mutator should produce a @racket[mutated] value rather than plain syntax.
The result should be a mutated version of the syntax received, produced with the procedural mutator tools in @secref{procedural-api}.

Usually this form is only necessary for complex mutators, and even then most of the time you will be better off building such a mutator out of simpler pieces defined with the above forms and combined together with @racket[compose-mutators].
One common exception is mutators for which the number of possible mutations depends on the shape of the syntax itself;
for example, a mutator that swaps every adjacent pair of expressions (as in the example below) cannot be defined in terms of the simpler forms.

See also @racket[make-stream-mutator], which may provide a more convenient interface for defining such mutators.

@examples[
(define-mutator (rearrange-positional-exprs stx mutation-index counter)
  #:type "position-swap"
  (syntax-parse stx
    [(head e ...)
     (mutated-do-single [rearranged-e-stxs (rearrange-in-sequence (attribute e)
                                                             	  mutation-index
                                                             	  counter)]
                        #:return (quasisyntax/loc stx
                                   (head #,@rearranged-e-stxs)))]
    [else
     (no-mutation stx mutation-index counter)]))

(rearrange-positional-exprs #'(f 1 2 x y) 0 0)
(rearrange-positional-exprs #'(f 1 2 x y) 1 0)]
}

@defform[
(define-dependent-mutator (id formal ...) #:type type-expr
  body ...)
]{
Defines a dependent mutator, which is roughly a function of any arguments that produces a mutator.

This form is useful for defining mutators for which the mutation depends on something other than the immediate syntax of an expression to be mutated.
For example, a mutator might depend on the contents of the whole module; in that case, this form can define a dependent mutator that expects the module syntax, and analyzes that syntax to produce a mutator.
The easiest way to produce a mutator is by using one of the above mutator definition forms and then returning it.
}



@subsection[#:tag "procedural-api"]{Procedural mutator tools}
@defmodule[mutate/primitives]

@defproc[(maybe-mutate [original syntax?] [new syntax?] [mutation-index mutation-index?] [counter counter?]
                       [#:equivalent? equivalent? (syntax? syntax? . -> . boolean?) syntax-equal?])
         mutated?]{
The primitive mutation operation.
All mutators boil down to applications of @racket[maybe-mutate].

Decides whether to swap @racket[original] with @racket[new] based on whether @racket[mutation-index] and @racket[counter] are equal.

Regardless of whether the swap is performed or not, the resulting @racket[mutated?] object accounts for the fact that a @tech{mutation point} has been reached.
Hence, all accounting of @tech{mutation index}es and @tech{mutation counter}s is managed by @racket[maybe-mutate].

The @racket[equivalent?] argument determines if @racket[original] and @racket[new] are equivalent, in which case the swap does not count as a @tech{mutation point}.

@examples[
(maybe-mutate #'x #'y 0 0)
(code:line (maybe-mutate #'x #'x 0 0) (code:comment "no mutation point recorded because original and new are the same"))
(code:line (maybe-mutate #'x #'y 1 0) (code:comment "no mutation because counter != index"))
(code:line (maybe-mutate #'x #'y 0 1) (code:comment "no mutation because counter != index"))
]
}

@defstruct*[mutated ([stx syntax?] [new-counter counter?]) #:transparent]{
The struct that wraps a possibly-mutated piece of syntax with an updated @tech{mutation counter}.
}

@defproc[(mutated/c [inner-ctc contract?]) contract?]{
The contract combinator for making contracts that recognize @racket[mutated] structs.
}

@(define mutated-do-eval (new-eval))

@defform[
(mutated-do #:count-with [counter-id counter-value-expr]
  action-clause ...
  terminating-clause)
#:grammar [(action-clause (code:line [pat mutated-expr])
                          (code:line #:let [pat expr]))
           (terminating-clause (code:line #:return expr)
                               (code:line #:in mutated-expr))]
]{
A syntactic shorthand for sequencing a series of mutator applications, threading the counter through the applications.
@racket[mutated-do] implicitly unpacks the result of each application to extract the new syntax (binding it with @racket[pat] like @racket[match-define]), and the new counter (binding it to @racket[counter-id]).
Hence, the form is analagous to @racket[match-let*], where every clause always has the most-up-to-date counter bound to @racket[counter-id].

The @racket[#:return] clause terminates the sequence with an expression for the resulting piece of syntax; @racket[mutated-do] implicitly wraps that resulting syntax with the current value of the counter.

@examples[#:eval mutated-do-eval
(define-mutator (to-a/b/c! stx mutation-index counter) #:type "to-a/b/c!"
  (mutated-do #:count-with [current-counter counter]
    [after-a     (maybe-mutate stx       #'a! mutation-index current-counter)]
    (code:comment "`current-counter` is now counter + 1")
    [after-a+b   (maybe-mutate after-a   #'b! mutation-index current-counter)]
    (code:comment "`current-counter` is now counter + 2")
    [after-a+b+c (maybe-mutate after-a+b #'c! mutation-index current-counter)]
    (code:comment "`current-counter` is now counter + 3, which will be wrapped with the returned result")
    #:return after-a+b+c))
(to-a/b/c! #'original 0 0)
(to-a/b/c! #'original 1 0)
(to-a/b/c! #'original 2 0)
]

The @racket[#:let] @racket[action-clause] inserts a @racket[let]-like binding of a normal value inside a sequence; i.e. the @racket[expr] of such a clause is not expected to be a @racket[mutated] value and is not implicitly unwrapped.


@examples[#:eval mutated-do-eval
(define-mutator (add-length stx mutation-index counter) #:type "add-length"
  (mutated-do #:count-with [current-counter counter]
    [maybe-a/b/c!    (to-a/b/c! stx mutation-index current-counter)]
    #:let [stx-parts (syntax->list maybe-a/b/c!)]
    #:let [len       (if stx-parts (length stx-parts) -1)]
    [with-len        (maybe-mutate maybe-a/b/c!
                                   #`(#,maybe-a/b/c! #,len)
                                   mutation-index
                                   current-counter)]
    #:return with-len))
(add-length #'(+ 1 2) 0 0)
(code:comment "mutation indices 1 and 2 trigger `to-a/b/c!`...")
(add-length #'(+ 1 2) 3 0)
(add-length #'0 3 0)
]

The @racket[#:in] terminating clause terminates the sequence, but the expected result of the @racket[mutated-expr] should be a @racket[mutated] value, which is the result of the overall sequence as-is (no implicit wrapping).
@examples[#:eval mutated-do-eval
#:label @para{For example, @tt{to-a/b/c!} could have terminated the sequence directly with the final application of @racket[maybe-mutate]:}
(define-mutator (to-a/b/c!-2 stx mutation-index counter) #:type "to-a/b/c!"
  (mutated-do #:count-with [current-counter counter]
    [after-a     (maybe-mutate stx       #'a! mutation-index current-counter)]
    [after-a+b   (maybe-mutate after-a   #'b! mutation-index current-counter)]
    #:in (maybe-mutate after-a+b #'c! mutation-index current-counter)))
(to-a/b/c!-2 #'original 0 0)
(to-a/b/c!-2 #'original 1 0)
(to-a/b/c!-2 #'original 2 0)
]


}

@defform[
(mutated-do-single clause #:return result)
]{
Shorthand for @racket[mutated-do] with a single action clause followed by a return.

This shorthand is provided because it is a common pattern, and in this case there's no point specifying a counter identifier because the counter is only threaded directly from the result of @racket[clause] to the wrapper of @racket[result].
}

@defproc[(mmap [f (syntax? . -> . syntax?)] [m (mutated/c syntax?)]) (mutated/c syntax?)]{
Maps @racket[f] over the syntax in @racket[m], producing a new @racket[mutated] struct with the resulting syntax and the same counter as @racket[m].
}

@defproc[(mbind [f (syntax? counter? . -> . (mutated/c syntax?))] [m (mutated/c syntax?)])
         (mutated/c syntax?)]{
Applies @racket[f] with the contents of @racket[m].
}

@defproc[(mtest [pred (syntax? . -> . boolean?)] [m (mutated/c syntax?)]) boolean?
]{
Applies @racket[pred] to the syntax in @racket[m].
}

@deftogether[(
@defthing[mutation-index? natural?]
@defthing[counter? natural?]
)]{
Predicates for @tech{mutation index}es and @tech{mutation counter}s.
Since they're both just natural numbers, the value of these predicates is purely for documentation.
}


@subsection[#:tag "composition"]{Mutator combinators}
@defmodule[mutate/define]

The following procedures provide an api for creating mutators in a different way from the definition forms above, as well as manipulating mutators.

@defthing[#:kind "contract" mutator/c contract?]{
The contract identifying mutators as produced by the definition forms of @secref{definition} and the mutator creation functions below.

In fact, plain functions can be used as mutators too, if they have the right interface.
Roughly, this contract corresponds to either:
@itemlist[
@item{A mutator produced by a mutator definition form from @secref{definition} or one of the mutator creation functions below, or}
@item{a function satisfying @racket[(syntax? mutation-index? counter? . -> . (mutated/c syntax?))]}
]
}

@defproc[(dependent-mutator/c [dom-ctc contract?] ...) contract?]{
Creates a contract for a dependent mutator with domain specified by @racket[dom-ctc]s and which produces a @racket[mutator/c].
}


@(define simple-mutators-eval (new-eval))
@defproc[(compose-mutators [mutator mutator/c] ...+) mutator/c]{
Composes multiple mutators together into a single one, which applies each of the mutators in the given order.

@examples[#:eval simple-mutators-eval
(define-constant-mutator (increment-integer-consts v)
  [(? integer?) #:-> (add1 v)])
(define-constant-mutator (negate-integer-consts v)
  [(? integer?) #:-> (- v)])
(define inc-or-negate-ints (compose-mutators increment-integer-consts negate-integer-consts))
(inc-or-negate-ints #'5 0 0)
(inc-or-negate-ints #'5 1 0)
]
}

@defproc[(apply-mutators [stx syntax?] [mutators (listof mutator/c)] [mutation-index mutation-index?] [counter counter?]) (mutated/c syntax?)]{
Applies a sequence of mutators in order.

@examples[#:eval simple-mutators-eval
(apply-mutators #'5 (list increment-integer-consts negate-integer-consts) 0 0)
(apply-mutators #'5 (list increment-integer-consts negate-integer-consts) 1 0)
]
}

@defproc[(make-guarded-mutator [guard (syntax? . -> . boolean?)]
			       [transformer (syntax? . -> . syntax?)]
			       [#:type type string?])
	 mutator/c]{
Creates a mutator which performs the transformation @racket[transformer] on syntax only if @racket[guard] produces @racket[#t].

This is a more flexible, if often more verbose, version of @racket[define-simple-mutator].

If not provided, @racket[type] defaults to the type of the mutator in which @racket[make-stream-mutator] is applied.

@examples[
(define if-swap2
  (make-guarded-mutator (syntax-parser [({~literal if} c t e) #t]
  		 		       [else #f])
		        (syntax-parser [({~literal if} c t e) #'(if c e t)])))

(if-swap2 #'(if (< x 0) 0 (f x)) 0 0)
(if-swap2 #'(not-an-if 42 (+ 2 3)) 0 0)
]
}

@defproc[(make-stream-mutator [transformer (syntax counter? . -> . (or/c syntax? #f))]
			      [#:type type string?])
	 mutator/c]{
Creates a mutator that draws syntax transformations from @racket[transformer], treating it like a stream of mutations to a given piece of syntax that can be indexed into with its second argument.
The stream is considered finished when @racket[transfomer] returns false.

The resulting mutator maps the sequence of mutations produced by @racket[transformer] to distinct @tech{mutation point}s on the same piece of syntax.

If not provided, @racket[type] defaults to the type of the mutator in which @racket[make-stream-mutator] is applied.

@examples[
(define (pick-permutation stx i)
  (define l (syntax->list stx))
  (define perms (permutations l))
  (and (< i (length perms))
       (datum->syntax stx (list-ref perms i))))
(define rearrange (make-stream-mutator pick-permutation))

(rearrange #'(a b c) 0 0) @; #'(b a c)
(rearrange #'(a b c) 1 0) @; #'(a c b)
(rearrange #'(a b c) 2 0) @; #'(c a b)
(rearrange #'(a b c) 3 0) @; #'(b c a)
(rearrange #'(a b c) 4 0) @; #'(c b a)
(rearrange #'(a b c) 5 0) @; #'(a b c)
]

}

@defproc[(mutator-type [mutator mutator/c] [default string? "<?>"]) string?]{
Tries to extract the @tech{mutator type} of a mutator.
For mutators defined by the mutator definition forms and created by @racket[make-guarded-mutator] and @racket[make-stream-mutator], this produces the string provided at definition/creation.

If the type can't be extracted (e.g. because the mutator is a plain function), @racket[default] is returned.

@examples[#:eval simple-mutators-eval
(mutator-type increment-integer-consts)
]
}


@defproc[(mutate-in-sequence [stxs (listof syntax?)] [mutation-index mutation-index?] [counter counter?] [mutator mutator/c])
         (mutated/c syntax?)]{
Maps @racket[mutator] over each element the given list in sequence, threading the counter across the applications and wrapping the resulting list with the final counter value.

@examples[#:eval simple-mutators-eval
(mutate-in-sequence (list #'5 #'#f #'7 #'(+ 2 2)) 0 0 increment-integer-consts)
(mutate-in-sequence (list #'5 #'#f #'7 #'(+ 2 2)) 1 0 increment-integer-consts)
]
}

@defproc[(rearrange-in-sequence [stxs (listof syntax?)] [mutation-index mutation-index?] [counter counter?])
         (mutated/c syntax?)]{
A (sort-of) mutation that swaps elements of stxs pairwise.

@examples[
(rearrange-in-sequence (list #'1 #'2 #'3 #'4 #'5) 0 0)
(rearrange-in-sequence (list #'1 #'2 #'3 #'4 #'5) 1 0)
(rearrange-in-sequence (list #'1 #'2 #'3 #'4 #'5) 2 0)
]
}

@defproc[(compound-expr? [stx syntax?]) boolean?]{
Returns true for syntax starting with parentheses.
}

@defthing[#:kind "mutator" no-mutation mutator/c]{
A mutator that does nothing to its argument.

@examples[
(no-mutation #'x 0 0)
]
}



@section[#:tag "traversal"]{Expression and program mutators: syntax traversal}
@defmodule[mutate/program]

@subsection{Expression mutators}

@(define expr-mutator-eval (new-eval))

@defproc[(make-expr-mutator [mutator mutator/c] [#:select select-expr expression-selector/c select-any-expr]) mutator/c]{
Creates an @tech{expression mutator} out of a @tech{simple mutator} by considering not only the top level of a piece of syntax for mutation, but also traversing into the syntax to consider sub-expressions for mutation.
See @secref{big-picture-usage} for details.

@racket[select-expr] selects which expressions to traverse into for discovering @tech{mutation point}s;
see @racket[expression-selector/c].
The default selects everything.

@examples[#:eval expr-mutator-eval
(define-constant-mutator (increment-integer-consts v)
  [(? integer?) #:-> (add1 v)])
(code:comment "Simple mutators only consider the top level shape of the syntax.")
(code:line (increment-integer-consts #'5 0 0) (code:comment "so this mutates"))
(code:line (increment-integer-consts #'(list 5 10) 0 0) (code:comment "but this doesn't"))
(define increment-integer-consts/expr (make-expr-mutator increment-integer-consts))
(code:line (increment-integer-consts/expr #'(list 5 10) 0 0) (code:comment "now the traversal finds the 5 to mutate"))
(code:line (increment-integer-consts/expr #'(list 5 10) 1 0) (code:comment "and then the 10"))
]
}

@defproc[(mutation-guard [stx syntax?]) syntax?]{
Guards the given piece of syntax from being traversed any further by @racket[mutate-expr].
This effectively hides any @tech{mutation point}s in subexpressions of @racket[stx] from @racket[mutate-expr]'s traversal.

This is useful for ensuring that only certain mutations can happen to a form, like in the example below.
@examples[
(define-mutator (negate-if-cond-only stx mutation-index counter) #:type "negate-if-cond-only"
  (syntax-parse stx
    [({~literal if} cond t e)
     (code:comment "Guard below marks conditions so that sub-parts don't get considered for mutation.")
     (code:comment "This avoids some obvious equivalent mutants, but may miss interesting ones (e.g. involving side effects in conditions).")
     (code:comment "E.g. (if (not #t) a b) could be mutated by this mutator to (if (not (not #t)) a b),")
     (code:comment "and by a constant swap mutator to (if (not #f) a b).")
     (code:comment "The mutation guard prevents the second mutation.")
     (mutated-do-single [negated-cond (maybe-mutate #'cond #'(not cond) mutation-index counter)]
                        #:return (mutation-guard #`(if #,negated-cond t e)))]
    [else (no-mutation stx mutation-index counter)]))
(define-constant-mutator (bool-constant-swap v)
  [(? boolean?) #:-> (not v)])
(define expr-mutator
  (make-expr-mutator (compose-mutators negate-if-cond-only bool-constant-swap)))
(expr-mutator #'(if (not #f) 1 2) 0 0)
(expr-mutator #'(if (not #f) 1 2) 1 0)
]
}
@defproc[(mutation-guarded? [stx syntax?]) boolean?]{
A predicate recognizing @racket[mutation-guard]ed syntax.

@examples[
(mutation-guarded? #'hello)
(mutation-guarded? (mutation-guard #'hello))
]
}

@defthing[#:kind "contract" expression-selector/c
		 	    contract?
			    #:value (syntax?
				     . -> .
				     (or/c #f
					   (list/c syntax?
						   (syntax? . -> . syntax?)
						   (listof (cons/c parameter? any/c)))))]{
The contract for expression selectors provided to @racket[make-expr-mutator].

Expression selectors are functions that, provided the syntax of an expression, return either
@itemlist[
@item{false, to indicate that the expression should not be considered for mutation or traversal, or}
@item{a list of three things:
@itemlist[
@item{The syntax to be considered for mutation and traversal. This is typically the expression given as input, but not necessarily so: the selector may extract a just sub-part of the expression to be considered.}
@item{A function to reconstruct the whole expression corresponding to the input, given a mutated version of the first item of the list.}
@item{An association list of parameters and values to set while considering the first item of the list for mutation and traversal.}
]
}
]

@examples[#:eval expr-mutator-eval
(define ignore-begin-effect-exprs
  (syntax-parser
    [({~literal begin} non-result-e ... result-e)
     (list #'result-e
           (λ (new-result-e) #`(begin non-result-e ... #,new-result-e))
	   empty)]
    [other-e
     (list #'other-e
           (λ (x) x)
	   empty)]))
(define iic/except-in-begin-effect-exprs
  (make-expr-mutator increment-integer-consts #:select ignore-begin-effect-exprs))
(code:comment "The 5 in (displayln 5) will not be considered.")
(iic/except-in-begin-effect-exprs #'(if (= 5 x) (begin (displayln 5) (+ y 5)) 0) 0 0)
(iic/except-in-begin-effect-exprs #'(if (= 5 x) (begin (displayln 5) (+ y 5)) 0) 1 0)
(iic/except-in-begin-effect-exprs #'(if (= 5 x) (begin (displayln 5) (+ y 5)) 0) 2 0)
]
}

@defthing[select-any-expr expression-selector/c]{
The expression selector that selects everything.
}

@subsection{Program mutators}
@defproc[(make-program-mutator [mutator mutator/c]
			       [select top-level-selector/c select-all])
	 ({syntax? mutation-index?} {counter?} . ->* . (mutated/c mutated-program?))]{
Creates a full @tech{program mutator} out of a @racket[mutator] (which is usually an @tech{expression mutator} produced by @racket[make-expr-mutator]).

Similar to @racket[make-expr-mutator], @racket[select] selects top level expressions to consider for mutation and traversal.

The resulting program mutator expects a syntax-list of top level forms of the program; i.e. a program with the shape @racket[#'[top-level-form ...]].
It returns a @racket[mutated-program], which contains the syntax of the mutated program and an identifier for the top level form which was mutated (as produced by @racket[select], see @racket[top-level-selector/c] for details).

If the program mutator is called with a mutation index that is larger than the total number of @tech{mutation points}, it raises a @racket[mutation-index-exception?].

@examples[#:eval expr-mutator-eval
(define mutate-program
  (make-program-mutator (make-expr-mutator increment-integer-consts)))
(mutate-program #'[(provide x y)
	           (displayln 0)
		   (define x 5)
		   (define y (+ x 1))]
		0)
(mutate-program #'[(provide x y)
	           (displayln 0)
		   (define x 5)
		   (define y (+ x 1))]
		1)
(mutate-program #'[(provide x y)
	           (displayln 0)
		   (define x 5)
		   (define y (+ x 1))]
		2)
(with-handlers ([mutation-index-exception? (λ _ 'out-of-mutants)])
  (mutate-program #'[(provide x y)
	             (displayln 0)
		     (define x 5)
		     (define y (+ x 1))]
		  3))
]
}

@defproc[(mutation-index-exception? [v any/c]) boolean?]{
Recognizes exceptions thrown by @tech{program mutator}s when called with a mutation index that exceeds the total number of @tech{mutation points} in the program.
}

@defproc[(without-counter [program-mutator ({syntax? mutation-index?}
			  		    {counter?}
					    . ->* .
					    (mutated/c mutated-program?))])
         ({syntax? mutation-index?} {counter?} . ->* . mutated-program?)]{
Transforms a @tech{program mutator} returned by @racket[make-program-mutator] to strip the @racket[mutated] wrapper from its results.
@examples[#:eval expr-mutator-eval
(define mutate-program/no-counter
  (without-counter (make-program-mutator (make-expr-mutator increment-integer-consts))))
(mutate-program/no-counter #'[(provide x y)
		   	      (define x 5)
		   	      (define y (+ x 1))]
			   0)
(mutate-program/no-counter #'[(provide x y)
			      (define x 5)
			      (define y (+ x 1))]
			   1)
]
}
@defproc[(syntax-only [program-mutator ({syntax? mutation-index?}
					{counter?}
					. ->* .
					(mutated/c mutated-program?))])
         ({syntax? mutation-index?} {counter?} . ->* . syntax?)]{
Similar to @racket[without-counter], transforms a @tech{program mutator} returned by @racket[make-program-mutator] to strip both the @racket[mutated] and @racket[mutated-program] wrappers from its results, so that it produces only syntax.
}

@defstruct*[mutated-program ([stx syntax?] [mutated-id any/c]) #:transparent]{
The wrapper of mutated program syntax, returned by @tech{program mutator}s, which carries the identifier of the top level form mutated alongside the mutated program syntax.
}

@defthing[#:kind "contract" top-level-selector/c
          (syntax? . -> . (or/c #f
                          	(list/c (listof syntax?)
                                	any/c
                                	((listof syntax?) . -> . syntax?))))]{
The contract for top level selectors provided to @racket[make-program-mutator].

Top level selectors are functions that, provided the syntax of a top level form, return either
@itemlist[
@item{False, to indicate that the form should not be considered for mutation or traversal, or}
@item{A list of three things:
@itemlist[
@item{A listof of syntaxes to be considered for mutation and traversal. This may just be the whole top level form, or it may be sub-parts of it.}
@item{An identifier for this top level form.}
@item{A function to reconstruct the whole top level form corresponding to the input, given mutated versions of the syntaxes returned in the first value.}
]
}
]

@examples[#:eval expr-mutator-eval
(require syntax/parse/lib/function-header)
(define (select-define-body-only stx)
  (syntax-parse stx
    [({~literal define} {~or* plain-name:id sig:function-header} body ...)
     (list (attribute body)
	   (or (attribute plain-name) (attribute sig.name))
	   (λ (mutated-body-stxs) #`(define {~? plain-name sig} #,@mutated-body-stxs)))]
    [_ #f]))
(define mutate-program
  (make-program-mutator (make-expr-mutator increment-integer-consts)
  			select-define-body-only))

(code:comment "0 will not be considered because it is not in the body of a define")
(mutate-program #'[(provide x y)
	           (displayln 0)
		   (define x 5)
		   (define y (+ x 1))]
		0)
(mutate-program #'[(provide x y)
	           (displayln 0)
		   (define x 5)
		   (define y (+ x 1))]
		1)
(with-handlers ([mutation-index-exception? (λ _ 'out-of-mutants)])
  (mutate-program #'[(provide x y)
	             (displayln 0)
		     (define x 5)
		     (define y (+ x 1))]
		  2))
]
}

@deftogether[(
@defthing[select-all top-level-selector/c]
@defthing[select-define-body top-level-selector/c]
@defthing[select-define/contract-body top-level-selector/c]
@defthing[select-any-define-named-form-body top-level-selector/c]
)]{
Some top level selectors for common cases.
}



@subsection[#:tag "logging"]{Logging: recovering the mutation type that causes a mutation}
@defmodule[mutate/define]

@racket[maybe-mutate] logs a mutation message when it executes a mutation on the @racket[mutate-logger] (topic @tt{mutate}) at level @tt{info}.
The message has a data payload which is a list of three elements:
@itemlist[
@item{the @tech{mutator type} of the mutator that caused the mutation}
@item{the original syntax}
@item{the mutated syntax}
]

Since all mutators boil down to applications of @racket[maybe-mutate], the @racket[mutate-logger] provides a channel to recover which mutator is used to mutate a program.

@defthing[#:kind "logger" mutate-logger logger?]{}

