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

  (define (mutate-expr stx mutation-index counter)
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

(module+ test
  (require ruinit
           racket
           syntax/parse
           "mutate-test-common.rkt")
  (test-begin
    #:name make-expr-mutator/traversal
    (ignore
     (define exprs-mutated
       (box empty))
     (define (record-expr! stx)
       (define hist (unbox exprs-mutated))
       (set-box! exprs-mutated (cons stx hist)))
     (define recording-mutator
       (make-guarded-mutator (const #t)
                             (λ (stx)
                               (record-expr! stx)
                               stx)))
     (define recording-mutate-expr
       (make-expr-mutator recording-mutator)))
    (test-mutator recording-mutate-expr
                  #'(class parent
                      (field a)
                      (define/public (f x) x)
                      (define/private (g x) x))
                  #'(class parent
                      (field a)
                      (define/public (f x) x)
                      (define/private (g x) x)))
    (test-equal? (map syntax->datum
                      (reverse (unbox exprs-mutated)))
                 '((class parent
                     (field a)
                     (define/public (f x) x)
                     (define/private (g x) x))
                   class
                   parent
                   (field a)
                   field
                   a
                   (define/public (f x) x)
                   define/public
                   (f x)
                   f
                   x
                   x
                   (define/private (g x) x)
                   define/private
                   (g x)
                   g
                   x
                   x)))

  (define-constant-mutator (replace-any-datum-with-0 value)
    #:type "test"
    [(not (? list?)) #:-> 0])
  (test-begin
    #:name make-expr-mutator
    (ignore
     (define just-replace-any-datum-with-0
       (make-expr-mutator replace-any-datum-with-0)))
    (test-mutator* just-replace-any-datum-with-0
                   #'(begin 1 2 3)
                   (list #'(0 1 2 3)
                         #'(begin 0 2 3)
                         #'(begin 1 0 3)
                         #'(begin 1 2 0)
                         #'(begin 1 2 3)))
    (test-mutator* just-replace-any-datum-with-0
                   #'(#%module-begin
                      (define x 5)
                      (+ x 42))
                   (list #'(0
                            (define x 5)
                            (+ x 42))
                         #'(#%module-begin
                            (0 x 5)
                            (+ x 42))
                         #'(#%module-begin
                            (define 0 5)
                            (+ x 42))
                         #'(#%module-begin
                            (define x 0)
                            (+ x 42))
                         #'(#%module-begin
                            (define x 5)
                            (0 x 42))
                         #'(#%module-begin
                            (define x 5)
                            (+ 0 42))
                         #'(#%module-begin
                            (define x 5)
                            (+ x 0))
                         #'(#%module-begin
                            (define x 5)
                            (+ x 42))))

    (ignore
     (define replace-datums-not-under-define-with-0
       (make-expr-mutator replace-any-datum-with-0
                          #:select (syntax-parser
                                     [({~datum define} . _) #f]
                                     [else (list this-syntax values empty)]))))
    (test-mutator* replace-datums-not-under-define-with-0
                   #'(#%module-begin
                      (define x 5)
                      (+ x 42))
                   (list #'(0
                            (define x 5)
                            (+ x 42))
                         #'(#%module-begin
                            (define x 5)
                            (0 x 42))
                         #'(#%module-begin
                            (define x 5)
                            (+ 0 42))
                         #'(#%module-begin
                            (define x 5)
                            (+ x 0))
                         #'(#%module-begin
                            (define x 5)
                            (+ x 42)))))

  (test-begin
    #:name make-mutate-expr/guarding
    (ignore
     (define replace-head-of-exprs-with-0-and-prevent-recur
       (make-expr-mutator
        (λ (stx mutation-index counter)
          (syntax-parse stx
            [(head . rest)
             (mutated-do-single
              [mutated-head (maybe-mutate #'head
                                          #'0
                                          mutation-index
                                          counter)]
              #:return (mutation-guard #`(#,mutated-head . rest)))]
            [other
             (no-mutation stx mutation-index counter)])))))
    (test-mutator* replace-head-of-exprs-with-0-and-prevent-recur
                   #'(#%module-begin
                      (define x 5)
                      (+ x 42))
                   (list #'(0
                            (define x 5)
                            (+ x 42))
                         #'(#%module-begin
                            (define x 5)
                            (+ x 42)))))

  (test-begin
    #:name make-mutate-expr/params
    (ignore
     (define inside-annotation? (make-parameter #f))
     (define (select-type-annotations stx)
       (syntax-parse stx
         [({~datum :} . Ts)
          (list (attribute Ts)
                (λ (mutated-Ts) #`(: . #,mutated-Ts))
                (list (cons inside-annotation? #t)))]
         [other
          #:when (inside-annotation?)
          (list stx
                identity
                empty)]
         [else #f]))

     (define replace-any-datum-with-0-only-under-:
       (make-expr-mutator
        replace-any-datum-with-0
        #:select select-type-annotations)))
    (test-mutator* replace-any-datum-with-0-only-under-:
                   #'(#%module-begin
                      (define x 5)
                      (+ x 42))
                   (list #'(#%module-begin
                            (define x 5)
                            (+ x 42))))
    (test-mutator* replace-any-datum-with-0-only-under-:
                   #'(: y
                        (define x 5)
                        (+ x 42))
                   (list #'(: 0
                              (define x 5)
                              (+ x 42))
                         #'(: y
                              (0 x 5)
                              (+ x 42))
                         #'(: y
                              (define 0 5)
                              (+ x 42))
                         #'(: y
                              (define x 0)
                              (+ x 42))
                         #'(: y
                              (define x 5)
                              (0 x 42))
                         #'(: y
                              (define x 5)
                              (+ 0 42))
                         #'(: y
                              (define x 5)
                              (+ x 0))
                         #'(: y
                              (define x 5)
                              (+ x 42))))))
