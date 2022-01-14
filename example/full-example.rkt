#lang racket

(require syntax/parse
         "../main.rkt")

(define-simple-mutator (if-swap stx)
  #:pattern ({~literal if} cond t e)
  #:when (list? (syntax->datum #'cond))
  #'(if cond e t))

(define-constant-mutator (constant-swap v) #:type "constant-swap"
  [(? number?) #:-> (- v)])

(define active-mutators (list if-swap
                              constant-swap))

(define combined-active-mutators (apply compose-mutators active-mutators))
(define mutate-expr (make-expr-mutator combined-active-mutators))
(define mutate-program (make-program-mutator mutate-expr))
(define mutate-program-syntax (syntax-only mutate-program))

(define program-to-mutate #'{(require "a.rkt")
                             (define x (if (yes?) 0 42))
                             (define y (if (negative? x)
                                           "negative!"
                                           (if (zero? x)
                                               "zero!"
                                               "positive!")))
                             (displayln y)})
(with-handlers ([mutation-index-exception? (λ _ (displayln 'done!))])
  (for ([i (in-naturals)])
    (displayln (list i (syntax->datum (mutate-program-syntax program-to-mutate i))))))
; =>
;; (0 ((require a.rkt) (define x (if (yes?) 42 0)) (define y (if (negative? x) negative! (if (zero? x) zero! positive!))) (displayln y)))
;; (1 ((require a.rkt) (define x (if (yes?) 0 -42)) (define y (if (negative? x) negative! (if (zero? x) zero! positive!))) (displayln y)))
;; (2 ((require a.rkt) (define x (if (yes?) 0 42)) (define y (if (negative? x) (if (zero? x) zero! positive!) negative!)) (displayln y)))
;; (3 ((require a.rkt) (define x (if (yes?) 0 42)) (define y (if (negative? x) negative! (if (zero? x) positive! zero!))) (displayln y)))
;; done!

(module test racket)
