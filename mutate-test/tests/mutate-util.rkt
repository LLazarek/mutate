#lang at-exp racket

(require syntax/parse
         ruinit
         mutate/private/mutate-util
         mutate/private/mutator-lib
         mutate/private/mutated
         "testing-util.rkt")

(test-begin
  #:name mutate-in-sequence
  (test-mutation/in-seq
   (list #'#t
         #''a
         #'1
         #'(+ 1 2))
   mutate-in-sequence
   (λ (stx mutation-index counter)
     (maybe-mutate stx
                   (syntax-parse stx
                     [_:number #'42]
                     [b:boolean #'(not b)]
                     [other stx])
                   mutation-index
                   counter))
   (curry map syntax->datum)
   `(#;[1 (,#'#t
           ,#''a
           ,#'1
           ,#'(+ 1 2))]
     [0 (,#'(not #t)
         ,#''a
         ,#'1
         ,#'(+ 1 2))]
     [1 (,#'#t
         ,#''a
         ,#'42
         ,#'(+ 1 2))]
     [2 (,#'#t
         ,#''a
         ,#'1
         ,#'(+ 1 2))])))

(test-begin
  #:name rearrange-in-sequence
  (test-programs-equal?
   #`(#,@(mutated-stx (rearrange-in-sequence
                       (syntax->list #'(a (+ 1 2)))
                       0 0)))
   #'((+ 1 2) a))
  (test-programs-equal?
   #`(#,@(mutated-stx (rearrange-in-sequence
                       (syntax->list #'(a (+ 1 2) b))
                       0 0)))
   #'((+ 1 2) a b))
  (test-programs-equal?
   #`(#,@(mutated-stx (rearrange-in-sequence
                       (syntax->list #'(a (+ 1 2) b (foo 3)))
                       1 0)))
   #'(a (+ 1 2) (foo 3) b))
  (test-programs-equal?
   #`(#,@(mutated-stx (rearrange-in-sequence
                       (syntax->list #'(a (+ 1 2) b (foo 3) (bar 3 4 5)))
                       2 0)))
   #'(a (+ 1 2) b (foo 3) (bar 3 4 5)))
  #| ... |#)
