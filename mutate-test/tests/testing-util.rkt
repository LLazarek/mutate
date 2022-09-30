#lang at-exp racket/base

(provide programs-equal?
         test-programs-equal?
         test-mutator
         test-mutator*
         test-mutation/in-seq
         test-failure-diff-programs?)

(require racket/format
         racket/list
         racket/pretty

         ruinit
         ruinit/diff/diff

         mutate/private/mutated)

(define (programs-equal? a b)
  (equal? (syntax->datum a)
          (syntax->datum b)))

(define test-failure-diff-programs? (make-parameter #t))

(define (diff-programs/string actual expected)
  (define actual-str (pretty-format (syntax->datum actual)))
  (define expected-str (pretty-format (syntax->datum expected)))
  (if (test-failure-diff-programs?)
      (dumb-diff-lines/string actual-str expected-str)
      @~a{
          Expected:
          @expected-str

          Actual:
          @actual-str

          }))

(define-simple-test (test-programs-equal? actual expected)
  #:fail-message @~a{
                     Programs are not equal. Diff (expected >):
                     @(diff-programs/string actual expected)
                     }
  (programs-equal? actual expected))


(define-test (test-mutator mutator orig-stx expected-stx
                           [counter-index-offset 0])
  (test-programs-equal? (mutated-stx (mutator orig-stx counter-index-offset 0))
                        expected-stx))

(define-test (test-mutator* mutator
                            orig-stx
                            expected-stx-seq)
  (for/and/test ([expected-stx (in-list expected-stx-seq)]
                 [counter (in-naturals)])
                (test-mutator mutator
                              orig-stx
                              expected-stx
                              counter)))

(define-test (test-mutation/in-seq orig-seq
                                   seq-mutator
                                   mutator
                                   pretty-printer
                                   expects)
  (for ([index (in-list (map first expects))]
        [expect (in-list (map second expects))])
    (define ms (mutated-stx (seq-mutator orig-seq
                                         index
                                         0
                                         mutator)))
    (unless (andmap programs-equal?
                    (flatten ms)
                    (flatten expect))
      (fail "Result does not match expected output.
Mutation index: ~v
Expected:
~a

Actual:
~a
"
            index
            (pretty-printer expect)
            (pretty-printer ms)))))
