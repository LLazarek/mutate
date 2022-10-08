#lang at-exp racket

(require ruinit
         syntax/parse
         mutate/private/mutate-expr
         mutate/private/mutator-lib
         mutate/private/mutated
         "testing-util.rkt")

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
      (λ (stx mutation-index [counter 0])
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
                            (+ x 42)))))
