#lang racket/base

(require racket/contract)

(define A any/c) ;; Instead of parametric ctcs
(define B any/c)
(provide mutation-index?
         counter?
         mutated/c
         (contract-out
          [mmap ((A . -> . B)
                 (mutated/c A)
                 . -> .
                 (mutated/c B))]
          [mbind ((A counter? . -> . (mutated/c B))
                  (mutated/c A)
                  . -> .
                  (mutated/c B))]
          [mtest ((A . -> . boolean?)
                  (mutated/c A)
                  . -> .
                  boolean?)])
         mutated-do
         mutated-do-single
         (struct-out mutated))

(require (for-syntax syntax/parse
                     racket/base)
         racket/match
         racket/math)

(define mutation-index? natural?)
(define counter? natural?)

;; mutated represents a piece of syntax that has been considered for
;; mutation; it may or may not have actually been mutated.
;; It carries with it the state of the mutation search _after_ its
;; consideration for mutation.
;; Thus, all mutation attempts after some mutated object should
;; use its accompanying counter value.
(struct mutated (stx new-counter) #:transparent)

(define (mutated/c a) (struct/c mutated a counter?))

(define (mmap f m)
  (mutated (f (mutated-stx m))
           (mutated-new-counter m)))

(define (mbind f m)
  (f (mutated-stx m)
     (mutated-new-counter m)))

(define (mtest f m)
  (f (mutated-stx m)))

;; Performs sequential mutations with automatic threading of the
;; counter
(define-syntax (mutated-do stx)
  (syntax-parse stx
    #:datum-literals (count-with def return in def/value)
    [(_ #:count-with [counter-id:id current-value:expr]
        [bound-id:expr m-expr:expr]
        more-clauses ...+)
     #'(match-let* ([counter-id current-value]
                    [(mutated bound-id counter-id) m-expr])
         (mutated-do #:count-with [counter-id counter-id]
                     more-clauses ...))]
    [(_ #:count-with [counter-id:id current-value:expr]
        #:let [bound-id:expr non-m-expr:expr]
        more-clauses ...+)
     #'(match-let* ([bound-id non-m-expr])
         (mutated-do #:count-with [counter-id current-value]
                     more-clauses ...))]
    ;; return <=> mmap
    [(_ #:count-with [counter-id:id current-value:expr]
        #:return m-expr:expr)
     #'(mutated m-expr
                current-value)]
    ;; in <=> mbind
    [(_ #:count-with [counter-id:id current-value:expr]
        #:in m-expr:expr)
     #'m-expr]))

;; bind version that shows more clearly how this is similar to monadic do
;; but it doesn't simply support pattern matching for id like above..
#;(define-syntax (mdo stx)
  (syntax-parse stx
    #:datum-literals (count-with def return in)
    [(_ [count-with (counter-id:id current-value:expr)]
        (def bound-id:id m-expr:expr)
        more-clauses ...+)
     #'(mbind (λ (bound-id counter-id)
                (mdo [count-with (counter-id counter-id)]
                     more-clauses ...))
              (let ([counter-id current-value])
                m-expr))]
    ;; return <=> mmap
    [(_ [count-with (counter-id:id current-value:expr)]
        [return m-expr:expr])
     #'(mutated m-expr
                counter-id)]
    ;; in <=> mbind
    [(_ [count-with (counter-id:id current-value:expr)]
        [in m-expr:expr])
     #'m-expr]))

(define-syntax-rule (mutated-do-single def-clause #:return result-clause)
  (mutated-do #:count-with [unused #f]
              def-clause
              #:return result-clause))
