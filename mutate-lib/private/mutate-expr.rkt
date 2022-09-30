#lang at-exp racket/base

(require racket/contract)

(provide (contract-out
          [make-expr-mutator ({mutator/c}
                              {#:select expression-selector/c}
                              . ->* .
                              mutator/c)]
          [mutation-guard    (syntax? . -> . syntax?)]
          [mutation-guarded? (syntax? . -> . boolean?)]))

(require racket/function
         racket/match
         "mutate-util.rkt"
         "mutated.rkt"
         "mutator-lib.rkt"
         "expression-selectors.rkt")

(define stx-prop:mutation-guarded? 'mutation-guarded?)
(define (mutation-guard stx)
  (syntax-property stx stx-prop:mutation-guarded? #t))
(define (mutation-guarded? stx)
  (syntax-property stx stx-prop:mutation-guarded?))

(define (make-expr-mutator mutator
                           #:select [select-expr select-any-expr])
  (define (select? expr)
    (and (not (mutation-guarded? expr))
         (select-expr expr)))

  (define (mutate-expr stx mutation-index [counter 0])
    (match (and (<= counter mutation-index)
                (select? stx))
      [(list selected-stx reconstruct-original-stx parameters-to-set)
       (define (mutate-it)
         (mutated-do
          #:count-with [__ counter]
          [outer-level-mutated-stx (mutator selected-stx mutation-index __)]
          [result
           (cond
             [(and (compound-expr? outer-level-mutated-stx)
                   (not (mutation-guarded? outer-level-mutated-stx)))
              (mutated-do-single
               [inner-parts-mutated-stx-split
                (mutate-in-sequence (syntax->list outer-level-mutated-stx)
                                    mutation-index
                                    __
                                    mutate-expr)]
               #:return (datum->syntax stx
                                       inner-parts-mutated-stx-split
                                       stx
                                       stx))]
             [else (no-mutation outer-level-mutated-stx
                                mutation-index
                                __)])]
          #:return (reconstruct-original-stx result)))
       (call-with-parameterization* parameters-to-set
                                    mutate-it)]
      [#f
       (no-mutation stx mutation-index counter)]))

  mutate-expr)

(define (call-with-parameterization* parameter-alist thunk)
  (let set-next-param ([remaining-params parameter-alist])
    (match remaining-params
      [(cons (cons param v) remaining)
       (parameterize ([param v])
         (set-next-param remaining))]
      ['() (thunk)])))

