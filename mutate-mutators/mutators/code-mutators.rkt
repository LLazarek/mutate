#lang at-exp racket/base

(require racket/contract)

(provide (contract-out
          [arithmetic-op-swap             mutator/c]
          [boolean-op-swap                mutator/c]
          [class-method-publicity-swap    mutator/c]
          [delete-super-new               mutator/c]
          [data-accessor-swap             mutator/c]
          [nested-list-construction-swap  mutator/c]

          [replace-constants/type-level   mutator/c]

          [delete-begin-result-expr       mutator/c]
          [negate-conditionals            mutator/c]
          [force-conditionals             mutator/c]
          [wrap-conditionals              mutator/c]
          [replace-class-parent           mutator/c]
          [swap-class-initializers        mutator/c]
          [swap-arguments                 mutator/c]
          [add-extra-class-method         mutator/c]

          [comparison-op-swap             mutator/c]
          [replace-constants/similar      mutator/c]
          [begin-drop                     mutator/c]

          [make-top-level-id-swap-mutator (dependent-mutator/c syntax?)]
          [make-method-id-swap-mutator    (dependent-mutator/c syntax?)]
          [make-field-id-swap-mutator     (dependent-mutator/c syntax?)]))

;; unimplemented mutator ideas
;; - begin-expr-swap
;; - class initialization expr swap, drop

(require racket/class
         racket/function
         racket/list
         racket/match
         racket/format
         racket/set
         racket/string
         racket/stream
         syntax/parse
         syntax/id-set
         mutate/define
         mutate/low-level
         mutate/traversal
         "private/common.rkt")

(define-id-mutator arithmetic-op-swap
  #:type "arithmetic-op-swap"
  [+ #:<-> -]
  [* #:-> /]
  [quotient #:-> /]
  [modulo #:-> /]
  [add1 #:<-> sub1])

(define-id-mutator boolean-op-swap
  #:type "boolean-op-swap"
  [and #:<-> or])

(define-id-mutator class-method-publicity-swap
  #:type "class:publicity"
  [define/public #:<-> define/private])

(define-id-mutator delete-super-new
  #:type "class:super-new"
  [super-new #:-> void])

(define-id-mutator mutate-data-structure
  #:type "data-structure-mutability"
  [make-hash #:<-> make-immutable-hash]
  [vector #:<-> vector-immutable])

(define-id-mutator data-accessor-swap
  #:type "data-accessor-swap"
  [car #:<-> cdr])

(define-id-mutator nested-list-construction-swap
  #:type "nested-list-construction-swap"
  [append #:-> cons])

(define-constant-mutator (replace-constants/type-level value)
  #:type "constant-swap"
  ;; May mess with occurrence typing
  [(? boolean?)              #:-> (not value)]

  ;; Type generalization or subdivision swaps
  [(? number?)               #:-> (- value)]
  [(? integer?)              #:-> (exact->inexact value)]
  [(and (? number?)
        (? zero?))           #:-> 1]
  [(and (? number?)
        (? (negate zero?)))  #:-> 0]
  [(? real?)                 #:-> (* 1+0.0i value)]
  ;; this kind of narrowing probably doesn't help
  ;; [(? inexact?)          (exact-floor value)]

  ;; Blatantly ill-typed swaps
  [(? boolean?)              #:-> (if value 1 0)]
  [(? number?)               #:-> #f]
  [(? string?)               #:-> (string->bytes/utf-8 value)])

(module+ test
  (require ruinit
           racket
           mutate/tests/testing-util)
  (define mutate-datum (compose-mutators arithmetic-op-swap
                                         boolean-op-swap
                                         class-method-publicity-swap
                                         delete-super-new
                                         mutate-data-structure
                                         data-accessor-swap
                                         replace-constants/type-level))


  (define-test (test-datum-mutation-seq orig-v new-vs)
    (define (mutate-value v index)
      (syntax->datum (mutated-stx (mutate-datum (datum->syntax #f v)
                                                index
                                                0))))
    (for/and/test
     ([new-v (in-list new-vs)]
      [i (in-naturals)])
     (extend-test-message
      #:append? #f
      (test-equal? (mutate-value orig-v i)
                   new-v)
      @~a{
          Mutation sequence [@orig-v -> @new-vs] failed on @|new-v|:

          })))
  (define-test-syntax (test-datum-mutations
                       (~or (v {~datum ->} [new-vs ...])
                            (orig {~datum ->} mutated)
                            (left {~datum <->} right)) ...)
    #'(and/test/message
       [(test-programs-equal? (mutated-stx (mutate-datum #'orig 0 0))
                              #'mutated)
        @~a{@'orig -> @'mutated failed:}] ...
       [(test-programs-equal? (mutated-stx (mutate-datum #'left 0 0))
                              #'right)
        @~a{@'left -> @'right failed:}] ...
       [(test-programs-equal? (mutated-stx (mutate-datum #'right 0 0))
                              #'left)
        @~a{@'right -> @'left failed:}] ...

       [(test-datum-mutation-seq v (list new-vs ...))
        ""] ...))
  (test-begin
    #:name mutate-datum
    (test-datum-mutations
     ;; Arithmetic operators
     [+ <-> -]
     [* -> /]
     [quotient -> /]
     [modulo -> /]
     [add1 <-> sub1]

     ;; Boolean operators
     [and <-> or]

     ;; Class operators
     [define/public <-> define/private]
     [super-new -> void]

     ;; Data structures
     [make-hash <-> make-immutable-hash]
     [vector <-> vector-immutable]

     ;; Other builtins
     [car <-> cdr]

     ;; Anything else is left alone
     [x -> x]

     ;; Constants
     [1 -> [-1 1.0 0 1+0.0i #f]]
     [5 -> [-5 5.0 0 5+0.0i #f]]
     [2.5 -> [-2.5 0 2.5+0.0i #f]]
     [0 -> [0.0 1 #f]]
     [0.0 -> [-0.0 1 0.0+0.0i #f]]
     [-42 -> [42 -42.0 0 -42-0.0i #f]]
     [#t -> [#f 1]]
     [#f -> [#t 0]]
     ["a" -> [#"a"]]))

  (test-begin
    #:name mutate-datum/preserve-guards
    (mtest mutation-guarded?
           (mutate-datum (mutation-guard #'(not #t))
                         0
                         1))
    (mtest (negate mutation-guarded?)
           (mutate-datum #'(not #t)
                         0
                         1))
    (mtest (compose1 mutation-guarded?
                     second
                     syntax->list)
           (mutate-datum #`(if #,(mutation-guard #'(not #t)) a b)
                         0
                         1))
    (mtest (compose1 not
                     mutation-guarded?
                     second
                     syntax->list)
           (mutate-datum #'(if (not #t) a b)
                         0
                         1))))


(define-mutator (delete-begin-result-expr stx mutation-index counter)
  #:type "begin-result-deletion"
  (define-mutator (delete-result-expr stx mutation-index counter)
    #:type "begin-result-deletion"
    (syntax-parse stx
      [[e ...+ result]
       (maybe-mutate stx
                     (syntax/loc stx [e ...])
                     mutation-index
                     counter)]
      [else
       (no-mutation stx mutation-index counter)]))
  (syntax-parse stx
    #:datum-literals [λ lambda begin begin0 cond]
    [({~and {~seq head ...}
            {~or* begin
                  {~seq {~or* λ lambda} formals}}}
      es ...+ e-result)
     (mutated-do-single
      [mutated-es (delete-result-expr (syntax/loc stx [es ... e-result])
                                      mutation-index
                                      counter)]
      #:return (quasisyntax/loc stx
                 (head ... #,@mutated-es)))]
    [(begin0 e-result es ...+)
     (mutated-do-single
      [mutated-es (delete-result-expr (quasisyntax/loc stx
                                        [#,@(reverse (attribute es)) e-result])
                                      mutation-index
                                      counter)]
      #:return (quasisyntax/loc stx
                 (begin0 #,@(reverse (syntax->list mutated-es)))))]
    [(cond [test e ...] ...)
     (mutated-do-single
      [mutated-case-bodies (mutate-in-sequence (syntax->list #'[[e ...] ...])
                                               mutation-index
                                               counter
                                               delete-result-expr)]
      #:return (syntax-parse mutated-case-bodies
                 [[[mutated-e ...] ...]
                  (syntax/loc stx
                    (cond [test mutated-e ...] ...))]))]
    [else
     (no-mutation stx mutation-index counter)]))

(module+ test
  (test-begin
    #:name delete-begin-result-expr
    (test-mutator* delete-begin-result-expr
                   #'(begin 1 2 3)
                   (list #'(begin 1 2)
                         #'(begin 1 2 3)))
    (test-mutator* delete-begin-result-expr
                   #'(begin0 1 2 3)
                   (list #'(begin0 2 3)
                         #'(begin0 1 2 3)))
    (test-mutator* delete-begin-result-expr
                   #'(λ _ (displayln 'hi) 42)
                   (list #'(λ _ (displayln 'hi))
                         #'(λ _ (displayln 'hi) 42)))
    (test-mutator* delete-begin-result-expr
                   #'(λ _ 42)
                   (list #'(λ _ 42)))
    (test-mutator* delete-begin-result-expr
                   #'(cond [#t 42] ['true (launch-missiles!) -42] [else (displayln 'bye) 0])
                   (list #'(cond [#t 42] ['true (launch-missiles!)] [else (displayln 'bye) 0])
                         #'(cond [#t 42] ['true (launch-missiles!) -42] [else (displayln 'bye)])
                         #'(cond [#t 42] ['true (launch-missiles!) -42] [else (displayln 'bye) 0])))))


(define (make-conditional-test-mutator mutate-condition)
  (define-mutator (m stx mutation-index counter) #:type (mutator-type mutate-condition)
    (syntax-parse stx
      #:datum-literals [cond if]
      [(cond [test . body] ...)
       (define test-stxs (attribute test))
       (mutated-do-single
        [mutated-test-stxs (mutate-in-sequence test-stxs
                                               mutation-index
                                               counter
                                               mutate-condition)]
        #:return
        (syntax-parse mutated-test-stxs
          [[mutated-test ...]
           (syntax/loc stx
             (cond [mutated-test . body] ...))]))]
      [(if test then-e else-e)
       (define cond-form #'(cond [test then-e] [else else-e]))
       (mutated-do-single
        [mutated-cond-form (m cond-form
                              mutation-index
                              counter)]
        #:return
        (syntax-parse mutated-cond-form
          [(cond [mutated-test _] _)
           (syntax/loc stx
             (if mutated-test then-e else-e))]))]
      [else
       (no-mutation stx mutation-index counter)]))
  m)

(define (mutation-guard-if condition mutated-v)
  (mmap (if condition
            identity
            mutation-guard)
        mutated-v))

(define (make-simple-condition-mutator condition-stx-transform
                                       type)
  (define-mutator (m stx mutation-index counter) #:type type
    (define new-stx
      (syntax-parse stx
        [{~datum else} stx]
        [condition (condition-stx-transform stx)]))
    (define stx-mutated
      (maybe-mutate stx
                    new-stx
                    mutation-index
                    counter))
    ;; Guard conditionals so they don't get considered for mutation beyond these
    ;; condition mutators. This avoids obvious equivalent mutants, but may miss
    ;; interesting ones.
    ;; E.g. (if #t a b) -> (if (not #t) a b) and -> (if #f a b)
    (mutation-guard-if (compound-expr? stx)
                       stx-mutated))
  m)

(define negate-condition
  (make-simple-condition-mutator (λ (condition)
                                   (quasisyntax/loc condition
                                     (not #,condition)))
                                 "negate-conditional"))
(define negate-conditionals (make-conditional-test-mutator negate-condition))

(define force-condition
  (make-simple-condition-mutator (λ (condition)
                                   (syntax/loc condition #t))
                                 "force-conditional"))
(define force-conditionals (make-conditional-test-mutator force-condition))

(define wrap-condition
  (make-simple-condition-mutator (λ (condition)
                                   (quasisyntax/loc condition
                                     (cast #,condition Any)))
                                 "wrap-conditional"))
(define wrap-conditionals (make-conditional-test-mutator wrap-condition))

(module+ test
  (test-begin
    #:name negate-conditionals
    (test-mutator* negate-conditionals
                   #'(if #t (+ 2 2) 42)
                   (list #'(if (not #t) (+ 2 2) 42)
                         #'(if #t (+ 2 2) 42)))
    (test-mutator* negate-conditionals
                   #'(if (some long (thing here)) (+ 2 2) 42)
                   (list #'(if (not (some long (thing here))) (+ 2 2) 42)
                         #'(if (some long (thing here)) (+ 2 2) 42)))
    (test-mutator* negate-conditionals
                   #'(cond [first 42]
                           [(second?) => values]
                           [else 33])
                   (list #'(cond [(not first) 42]
                                 [(second?) => values]
                                 [else 33])
                         #'(cond [first 42]
                                 [(not (second?)) => values]
                                 [else 33])
                         #'(cond [first 42]
                                 [(second?) => values]
                                 [else 33])))
    (test-mutator* force-conditionals
                   #'(cond [first 42]
                           [(second?) => values]
                           [else 33])
                   (list #'(cond [#t 42]
                                 [(second?) => values]
                                 [else 33])
                         #'(cond [first 42]
                                 [#t => values]
                                 [else 33])
                         #'(cond [first 42]
                                 [(second?) => values]
                                 [else 33])))
    (test-mutator* wrap-conditionals
                   #'(cond [first 42]
                           [(second?) => values]
                           [else 33])
                   (list #'(cond [(cast first Any) 42]
                                 [(second?) => values]
                                 [else 33])
                         #'(cond [first 42]
                                 [(cast (second?) Any) => values]
                                 [else 33])
                         #'(cond [first 42]
                                 [(second?) => values]
                                 [else 33])))
    (test-mutator* wrap-conditionals
                   #'(if (natural? x) (* 2 x) #f)
                   (list #'(if (cast (natural? x) Any) (* 2 x) #f)
                         #'(if (natural? x) (* 2 x) #f)))

    (for/and/test
     ([cond-expr (in-list (list #'#t #'(+ 2 2)))]
      [simple-cond? (in-list '(#t #f))]
      #:when #t
      [index (in-range 2)]
      [mutated-cond-expr (in-list (list #`(not #,cond-expr) cond-expr))])
     (define mutated-result (negate-conditionals #`(if #,cond-expr + -)
                                                 index
                                                 0))
     (define stx (mutated-stx mutated-result))
     (define mutated-cond
       (second (syntax->list stx)))
     (and/test/message
      [(test-programs-equal? stx #`(if #,mutated-cond-expr + -))
       @~a{Mutation is different than expected.}]
      [(if simple-cond?
           (mutation-guarded? mutated-cond)
           (not (mutation-guarded? mutated-cond)))
       @~a{@(if simple-cond?
                "simple"
                "complex") @;
           cond expr is @;
           @(if simple-cond?
                "not guarded when it should be"
                "guarded when it shouldn't be")}]))))

(define-mutator (replace-class-parent stx mutation-index counter) #:type "class:parent-swap"
  (syntax-parse stx
    [(~or ({~and {~datum class}  class-form} superclass:expr . body)
          ({~and {~datum class*} class-form} superclass:expr
                                             interfaces:expr . body))
     (define superclass-stx (attribute superclass))
     (mutated-do-single
      [mutated-superclass (maybe-mutate superclass-stx
                                        (datum->syntax superclass-stx
                                                       'object%
                                                       superclass-stx
                                                       superclass-stx)
                                        mutation-index
                                        counter)]
      #:return
      (quasisyntax/loc stx
        (class-form #,mutated-superclass
                    {~? interfaces}
                    . body)))]
    [else
     (no-mutation stx mutation-index counter)]))

(module+ test
  (test-begin
    #:name replace-class-parent
    (test-mutator* replace-class-parent
                   #'(class a-parent
                       (field a)
                       (define/public (foo x) x))
                   (list #'(class object%
                             (field a)
                             (define/public (foo x) x))
                         #'(class a-parent
                             (field a)
                             (define/public (foo x) x))))))

(define-mutator (swap-class-initializers stx mutation-index counter)
  #:type "class:initializer-swap"
  (syntax-parse stx
    [({~and {~or {~datum init-field}
                 {~datum field}}
            field-type}
      {~and no-init-field
            {~or _:id [_:id {~datum :} _]}}
      ...
      [field-id:id other-field-stuff ... initial-value:expr]
      ...)
     (define init-value-stxs (attribute initial-value))
     (mutated-do-single
      [rearranged-init-value-stxs
       (rearrange-in-sequence init-value-stxs
                              mutation-index
                              counter)]
      #:return
      (syntax-parse rearranged-init-value-stxs
        [[new-init-value ...]
         (quasisyntax/loc stx
           (field-type no-init-field ...
                       [field-id other-field-stuff ... new-init-value] ...))]))]
    [({~and {~or {~datum new} {~datum instantiate}} instantiator}
      class-e
      {~and positional-initializer {~not (_ ...)}} ...
      [field-id:id other-field-stuff ... initial-value:expr]
      ...)
     (define init-value-stxs (attribute initial-value))
     (mutated-do-single
      [rearranged-init-value-stxs
       (rearrange-in-sequence init-value-stxs
                              mutation-index
                              counter)]
      #:return
      (syntax-parse rearranged-init-value-stxs
        [[new-init-value ...]
         (quasisyntax/loc stx
           (instantiator class-e
                         positional-initializer ...
                         [field-id other-field-stuff ... new-init-value] ...))]))]
    [else
     (no-mutation stx mutation-index counter)]))

(module+ test
  (test-begin
    #:name swap-class-initializers
    (for/and/test
     ([field-name (in-list (list #'field #'init-field))])
     (extend-test-message
      (test-mutator* swap-class-initializers
                     #`(#,field-name
                        mandatory-1
                        [mandatory-2 : T2]
                        [a 1]
                        [b 2]
                        [c 3]
                        [d : T1 4]
                        [e 5])
                     (list #`(#,field-name
                              mandatory-1
                              [mandatory-2 : T2]
                              [a 2]
                              [b 1]
                              [c 3]
                              [d : T1 4]
                              [e 5])
                           #`(#,field-name
                              mandatory-1
                              [mandatory-2 : T2]
                              [a 1]
                              [b 2]
                              [c 4]
                              [d : T1 3]
                              [e 5])
                           #`(#,field-name
                              mandatory-1
                              [mandatory-2 : T2]
                              [a 1]
                              [b 2]
                              [c 3]
                              [d : T1 4]
                              [e 5])))
      @~a{Field: @field-name}))

    (test-mutator* swap-class-initializers
                   #'(new my-class 42 [a 5] [b "hi"])
                   (list #'(new my-class 42 [a "hi"] [b 5])
                         #'(new my-class 42 [a 5] [b "hi"])))
    (test-mutator* swap-class-initializers
                   #'(new my-class 42 33 [a 5] [b "hi"] [c 'not-this-one])
                   (list #'(new my-class 42 33 [a "hi"] [b 5] [c 'not-this-one])
                         #'(new my-class 42 33 [a 5] [b "hi"] [c 'not-this-one])))
    (test-mutator* swap-class-initializers
                   #'(new my-class 42 33 [a 5])
                   (list #'(new my-class 42 33 [a 5])))))

(define-simple-mutator (swap-arguments stx)
  #:pattern ({~and head {~not _:special-form}} e ...)
  (for/stream ([rearranged-stxs (in-swaps (attribute e))])
    (syntax-parse rearranged-stxs
      [[swapped-e ...]
       (quasisyntax/loc stx
         (head swapped-e ...))])))

(define-syntax-class special-form
  #:description "special form"
  (pattern {~or {~datum define}
                {~datum define-values}
                {~datum define/contract}
                {~datum lambda}
                {~datum λ}
                {~datum struct}
                {~datum class}
                {~datum if}
                {~datum cond}
                {~datum when}
                {~datum unless}
                {~datum match-define}
                {~datum =>}
                {~datum ==}
                {~datum let}
                {~datum let*}
                {~datum let-values}
                {~datum let*-values}
                {~datum set!}
                {~datum define-syntax-rule}
                {~datum for}
                {~datum for*}
                {~datum for/fold}
                {~datum for*/fold}
                {~datum for/list}
                {~datum for*/list}
                {~datum for/vector}
                {~datum for*/vector}
                {~datum define/public}
                {~datum define/private}
                {~datum define/override}}))

(module+ test
  (test-begin
    #:name swap-arguments
    (test-mutator* swap-arguments
                   #'(a-function b c d e)
                   (map (λ (x) (datum->syntax #f x))
                        (remove '(a-function b c d e)
                                (for/list ([args (in-swaps '(b c d e))])
                                  (cons 'a-function args)))))))

(define-mutator (add-extra-class-method stx mutation-index counter) #:type "class:add-extra-method"
  (syntax-parse stx
    [(~or ({~and {~datum class}  class-form} superclass:expr . body)
          ({~and {~datum class*} class-form} superclass:expr
                                             interfaces:expr . body))
     (mutated-do-single
      [extra-method-stx
       (maybe-mutate (syntax/loc stx [])
                     (syntax/loc stx
                       [(define/public (a-nonexistant-method x) x)])
                     mutation-index
                     counter)]
      #:return
      (quasisyntax/loc stx
        (class-form superclass
                    {~? interfaces}
                    #,@extra-method-stx
                    . body)))]
    [else
     (no-mutation stx mutation-index counter)]))

(module+ test
  (test-begin
    #:name add-extra-class-method
    (test-mutator* add-extra-class-method
                   #'(class a-parent
                       (field a))
                   (list #'(class a-parent
                             (define/public (a-nonexistant-method x) x)
                             (field a))
                         #'(class a-parent
                             (field a))))))

(require syntax/parse/lib/function-header)

(define (id-list-swap-mutators ids name)
  (for/list ([top-level-id (in-list ids)])
    (define-mutator (replace-with-top-level-id stx mutation-index counter)
      #:type name
      (syntax-parse stx
        [ref:id
         #:when (member #'ref ids free-identifier=?)
         (maybe-mutate (attribute ref)
                       top-level-id
                       mutation-index
                       counter)]
        [else
         (no-mutation stx mutation-index counter)]))
    replace-with-top-level-id))

(define (combined-id-list-swap-mutator ids name)
  (define mutators (id-list-swap-mutators ids name))
  (match mutators
    ['()  no-mutation]
    [else (apply compose-mutators mutators)]))

(define top-level-id-swap-type "top-level-id-swap")
(define-dependent-mutator (make-top-level-id-swap-mutator mod-stx)
  #:type top-level-id-swap-type
  (define all-top-level-identifiers (top-level-definitions mod-stx))
  (mutator (combined-id-list-swap-mutator all-top-level-identifiers top-level-id-swap-type)
           top-level-id-swap-type))

;; Analagous to `function-header` but:
;; - matches for plain ids as well as function headers
;; - doesn't check the shape of arguments, so that it can recognize headers with
;;   type annotations too
(define-syntax-class simple-function-or-value-header
  #:attributes [name]
  (pattern {(~or header:simple-function-or-value-header name*:id) . _}
           #:attr name   #'{~? header.name name*})
  (pattern name:id))

(define top-level-definitions
  (syntax-parser
    #:datum-literals [define]
    [{{~or (define header:simple-function-or-value-header . _)
           _} ...}
     ;; No need to remove duplicates, can't have duplicate top level id definitions
     (syntax->list #'[header.name ...])]))

(module+ test
  (test-begin
    #:name top-level-definitions
    (test-equal? (map syntax->datum
                      (top-level-definitions
                       #'{(require foo x y)
                          (define v 42)
                          (+ v v)
                          (define (f x) (define y x) y)
                          (f v)}))
                 '(v f))
    (test-equal?
     (map
      syntax->datum
      (top-level-definitions
       #'{(define (word->hyphenation-points word
                                            [min-l  default-min-length]
                                            [min-ll  default-min-left-length]
                                            [min-rl  default-min-right-length])
            42)}))
     '(word->hyphenation-points))
    (test-equal?
     (map
      syntax->datum
      (top-level-definitions
       #'{(: word->hyphenation-points (->* (String) (Index Index Index) (Listof String)))
          (define (word->hyphenation-points word
                                            [min-l : Index default-min-length]
                                            [min-ll : Index default-min-left-length]
                                            [min-rl : Index default-min-right-length])
            (: add-no-hyphen-zone (-> (Listof Index) (Listof Integer)))
            42)}))
     '(word->hyphenation-points)))
  (test-begin
    #:name make-top-level-id-swap-mutator
    (ignore
     (define top-level-id-swap-mutator
       (make-top-level-id-swap-mutator
        #'{(require foobar)
           (define (f x) x)
           (+ 2 2)
           (define (g a b) (/ (f a) b))})))
    (test-mutator* top-level-id-swap-mutator
                   #'x
                   (list #'x))
    (test-mutator* top-level-id-swap-mutator
                   #'a
                   (list #'a))
    (test-mutator* top-level-id-swap-mutator
                   #'f
                   (list #'g
                         #'f))
    (test-mutator* top-level-id-swap-mutator
                   #'g
                   (list #'f
                         #'g))

    (ignore
     (define top-level-id-swap-mutator/no-ids
       (make-top-level-id-swap-mutator
        #'{(require foobar)
           (+ 2 y)})))
    (test-mutator* top-level-id-swap-mutator/no-ids
                   #'x
                   (list #'x))
    (test-mutator* top-level-id-swap-mutator/no-ids
                   #'y
                   (list #'y))))

(define (normalize-name-list l #:< [< symbol<?] #:key [get-key syntax->datum])
  (sort (remove-duplicates l #:key get-key)
        <
        #:key get-key))


(define-dependent-mutator (make-method-id-swap-mutator mod-stx)
  #:type "method-id-swap"
  (define all-methods (method-names-in mod-stx))
  (mutator (combined-id-list-swap-mutator all-methods (current-mutator-type))
           (current-mutator-type)))

(define-syntax-class public-method-def
  #:attributes [name]
  #:datum-literals [define/public define/pubment define/public-final]
  (pattern ({~or define/public define/pubment define/public-final}
            (header:simple-function-or-value-header _ ...)
            _ ...)
           #:with name #'header.name))

(define (method-names-in mod-stx)
  (define collect-method-names
    (syntax-parser
      #:datum-literals [class class*]
      [({~or class class*}
        {~seq {~not _:public-method-def} ...
              def:public-method-def} ...
        {~not _:public-method-def} ...)
       (syntax->list #'[def.name ...])]
      [(inner-es ...)
       (flatten
        (map collect-method-names
             (attribute inner-es)))]
      [else empty]))
  (define methods+duplicates (collect-method-names mod-stx))
  (normalize-name-list methods+duplicates))

(module+ test
  (test-begin
    #:name method-names-in
    (test-equal? (map
                  syntax->datum
                  (method-names-in #'{(define x 5) (+ x x)}))
                 '())
    (test-equal? (map
                  syntax->datum
                  (method-names-in #'{(define x 5) (+ x (let ([c (class object% (super-new))])
                                                          (new c)
                                                          5))}))
                 '())
    (test-equal? (map
                  syntax->datum
                  (method-names-in #'{(define x 5) (+ x (let ([c (class object%
                                                                   (super-new)
                                                                   (define/public y 42))])
                                                          (new c)
                                                          5))}))
                 '())
    (test-equal? (map
                  syntax->datum
                  (method-names-in #'{(define x 5) (+ x (let ([c (class object%
                                                                   (super-new)
                                                                   (define/public (m x) (* 42 x)))])
                                                          (send (new c) m x)))}))
                 '(m))
    (test-equal? (map
                  syntax->datum
                  (method-names-in #'{(define x 5) (+ x (let ([c (class object%
                                                                   (super-new)
                                                                   (define/public (m x) (* 42 x))
                                                                   (define/public (n x) (* 42 x)))])
                                                          (send (new c) m x)))}))
                 '(m n))
    (test-equal? (map
                  syntax->datum
                  (method-names-in #'{(define x 5) (+ x (let ([c (class object%
                                                                   (super-new)
                                                                   (define/public (m x) (* 42 x))
                                                                   (define/private (p) (void))
                                                                   (define/public (n x) (* 42 x)))])
                                                          (send (new c) m x)))}))
                 '(m n))
    (test-equal? (map
                  syntax->datum
                  (method-names-in #'{}))
                 '())
    (test-equal? (map
                  syntax->datum
                  (method-names-in #'{(define x 5)
                                      (+ x (let ([c (class object%
                                                      (super-new)
                                                      (define/public (m x) (* 42 x)))])
                                             (send (new c) m x)))
                                      (+ x (let ([c (class object%
                                                      (super-new)
                                                      (define/public (m x) (* 42 x)))])
                                             (send (new c) m x)))}))
                 '(m)))

  (test-begin
    #:name make-method-id-swap-mutator
    (ignore
     (define method-call-swap-mutator
       (make-method-id-swap-mutator
        #'{(define x 5) (+ x (let ([c (class object%
                                        (super-new)
                                        (define/public (m x) (* 42 x))
                                        (define/private (p) (void))
                                        (define/public (n x) (* 42 x)))])
                               (send (new c) m x)))})))
    (test-mutator* method-call-swap-mutator
                   #'x
                   (list #'x))
    (test-mutator* method-call-swap-mutator
                   #'p
                   (list #'p))
    (test-mutator* method-call-swap-mutator
                   #'m
                   (list #'n
                         #'m))
    (test-mutator* method-call-swap-mutator
                   #'n
                   (list #'m
                         #'n))

    (ignore
     (define method-call-swap-mutator/double
       (make-method-id-swap-mutator
        #'{(define x 5) (+ x (let ([c (class object%
                                        (super-new)
                                        (define/public (m x) (* 42 x))
                                        (define/private (p) (void))
                                        (define/public (n x) (* 42 x)))])
                               (send (class object%
                                        (super-new)
                                       (define/public (n x) (* 42 x))) m x)))})))
    (test-mutator* method-call-swap-mutator/double
                   #'m
                   (list #'n
                         #'m))))

(define field-id-swap-mutator-type "field-id-swap")
(define-dependent-mutator (make-field-id-swap-mutator mod-stx)
  #:type field-id-swap-mutator-type
  (define all-fields (field-names-in mod-stx))
  (mutator (combined-id-list-swap-mutator all-fields (current-mutator-type))
           (current-mutator-type)))

(define-syntax-class class-maybe-renamed
  #:attributes [name internal-name]
  (pattern name:id
           #:with internal-name #'name)
  (pattern [internal-name:id name:id]))

(define-syntax-class field-defs
  #:attributes [(name 1)]
  #:datum-literals [field inherit-field]
  ;; Extra-liberal pattern here: this discovery of field names has to work for
  ;; both untyped and typed, which might have type annotations as well as the
  ;; default value
  (pattern (field [clause:class-maybe-renamed . _] ...)
           #:with [name ...] #'[clause.name ...])
  (pattern (inherit-field clause:class-maybe-renamed ...)
           #:with [name ...] #'[clause.name ...]))

(define (field-names-in mod-stx)
  (define collect-field-names
    (syntax-parser
      #:datum-literals [class class*]
      [({~or class class*}
        {~seq {~not _:field-defs} ...
              def:field-defs} ...
        {~and after {~not _:field-defs}} ...)
       (syntax->list #'[def.name ... ...])]
      [(inner-es ...)
       (flatten
        (map collect-field-names
             (attribute inner-es)))]
      [else empty]))
  (define field-names+duplicates (collect-field-names mod-stx))
  (normalize-name-list field-names+duplicates))

(module+ test
  (test-begin
    #:name field-names-in
    (test-equal? (map
                  syntax->datum
                  (field-names-in #'{(define x 5) (+ x x)}))
                 '())
    (test-equal? (map
                  syntax->datum
                  (field-names-in #'{(define x 5) (+ x (let ([c (class object% (super-new))])
                                                          (new c)
                                                          5))}))
                 '())
    (test-equal? (map
                  syntax->datum
                  (field-names-in #'{(define x 5) (+ x (let ([c (class object%
                                                                  (super-new)
                                                                  (define/public y 42))])
                                                         (new c)
                                                         5))}))
                 '())
    (test-equal? (map
                  syntax->datum
                  (field-names-in #'{(define x 5) (+ x (let ([c (class object%
                                                                  (super-new)
                                                                  (field [m 5])
                                                                  (define/public (l) #f))])
                                                         (send (new c) m x)))}))
                 '(m))
    (test-equal? (map
                  syntax->datum
                  (field-names-in #'{(define x 5) (+ x (let ([c (class object%
                                                                  (super-new)
                                                                  (field [m 5]
                                                                         [n 42])
                                                                  (define/public (l) #f))])
                                                         (send (new c) m x)))}))
                 '(m n))
    (test-equal? (map
                  syntax->datum
                  (field-names-in #'{(define x 5) (+ x (let ([c (class object%
                                                                  (super-new)
                                                                  (inherit-field m)
                                                                  (define/public (l) #f)
                                                                  (field [n 42]))])
                                                         (send (new c) m x)))}))
                 '(m n))
    (test-equal? (map
                  syntax->datum
                  (field-names-in #'{(define x 5) (+ x (let ([c (class object%
                                                                  (super-new)
                                                                  (define/public (l) #f)
                                                                  (inherit-field m
                                                                                 [internal-n n]))])
                                                         (send (new c) m x)))}))
                 '(m n))
    (test-equal? (map
                  syntax->datum
                  (field-names-in #'{}))
                 '())
    (test-equal? (map
                  syntax->datum
                  (field-names-in #'{(define x 5) (+ x (let ([c (class object%
                                                                  (super-new)
                                                                  (field [m : SomeType 5])
                                                                  (define/public (l) #f))])
                                                         (send (new c) m x)))}))
                 '(m))
    (test-equal? (map
                  syntax->datum
                  (field-names-in #'{(define x 5)
                                     (+ x (let ([c (class object%
                                                     (super-new)
                                                     (field [m : SomeType 5])
                                                     (define/public (l) #f))])
                                            (send (new c) m x)))
                                     (+ x (let ([c (class object%
                                                     (super-new)
                                                     (field [m : SomeType 5])
                                                     (define/public (l) #f))])
                                            (send (new c) m x)))}))
                 '(m)))

  (test-begin
    #:name make-field-id-swap-mutator
    (ignore
     (define field-call-swap-mutator
       (make-field-id-swap-mutator
        #'{(define x 5) (+ x
                           (let ([c (class object%
                                      (super-new)
                                      (inherit-field m)
                                      (field [n 42])
                                      (define/public (mtd) m))])
                             (send (new c) mtd 42))
                           (+ x (let ([c (class object%
                                           (super-new)
                                           (field [m : SomeType 5])
                                           (define/public (l) #f))])
                                  (send (new c) m x))))})))
    (test-mutator* field-call-swap-mutator
                   #'x
                   (list #'x))
    (test-mutator* field-call-swap-mutator
                   #'mtd
                   (list #'mtd))
    (test-mutator* field-call-swap-mutator
                   #'m
                   (list #'n
                         #'m))
    (test-mutator* field-call-swap-mutator
                   #'n
                   (list #'m
                         #'n))
    (ignore (local-require racket/logging
                           mutate/logger))
    (test-logged-mutation field-call-swap-mutator
                          #'n
                          field-id-swap-mutator-type)))

;; blutil mutators
; <new> begin-drop any expr inside
; negate-conditionals
; swap class initializers
; rearrange funcall args

(define-id-mutator comparison-op-swap
  [< #:<-> <=]
  [> #:<-> >=]
  [< #:<-> >]
  [= #:<-> <=]
  [= #:<-> >=])

(define-constant-mutator (replace-constants/similar v)
  [1            #:-> 0]
  [(? integer?) #:-> (add1 v)]
  [(? number?)  #:-> (- v)]
  [(? number?)  #:-> (- -1 v)])

(define-simple-mutator (begin-drop stx)
  #:pattern ({~and begin-form {~or {~datum begin} {~datum begin0}}} e ...)
  (let ([es (attribute e)])
    (for/stream ([i (in-range (length (attribute e)))])
      (quasisyntax/loc stx
        (begin-form #,@(list-remove es i))))))
(define (list-remove l i)
  (unless (< i (length l))
    (error 'list-remove "cannot remove index beyond list bounds"))
  (append (take l i)
          (drop l (add1 i))))

