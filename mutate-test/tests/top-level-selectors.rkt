#lang at-exp racket

(require ruinit
         mutate/private/top-level-selectors)

(define-test (test-stx=? a b)
  (test-equal? (syntax->datum a) (syntax->datum b)))
(define-test (test-selector selector
                            stx
                            body-stxs/expected
                            id/expected
                            test-reconstruct)
  (match-define (list body-stxs id reconstruct)
    (selector stx))
  (and/test/message
   [(for/and/test ([part (in-list body-stxs)]
                   [part/expected (in-list body-stxs/expected)])
      (test-stx=? part part/expected))
    @~a{Body stxs are different:}]
   [(test-equal? id id/expected)
    @~a{Mutated ids are different:}]
   [(test-reconstruct reconstruct)
    @~a{Reconstruction test failed:}]))

(test-begin
  #:name select-all
  (test-selector select-all
                 #'(define x 5)
                 (list #'(define x 5))
                 'define
                 (λ (reconstruct)
                   (test-stx=? (reconstruct (list #'(void)))
                               #'(void))))
  (test-selector select-all
                 #'(require racket/function)
                 (list #'(require racket/function))
                 'require
                 (λ (reconstruct)
                   (test-stx=? (reconstruct (list #'(begin 42)))
                               #'(begin 42)))))

(test-begin
  #:name select-define/contract-body
  (test-selector select-define/contract-body
                 #'(define/contract x (and/c number? positive?) 5)
                 (list #'5)
                 'x
                 (λ (reconstruct)
                   (test-stx=? (reconstruct (list #'42))
                               #'(define/contract x (and/c number? positive?) 42))))
  (test-selector select-define/contract-body
                 #'(define/contract (f x)
                     (-> (and/c number? positive?) any/c)
                     5)
                 (list #'(begin 5))
                 'f
                 (λ (reconstruct)
                   (test-stx=? (reconstruct (list #'(begin 42)))
                               #'(define/contract (f x)
                                   (-> (and/c number? positive?) any/c)
                                   42)))))

(test-begin
  #:name select-define-body
  (test-selector select-define-body
                 #'(define x 5)
                 (list #'5)
                 'x
                 (λ (reconstruct)
                   (test-stx=? (reconstruct (list #'42))
                               #'(define x 42))))
  (test-selector select-define-body
                 #'(define (f x) 5)
                 (list #'(begin 5))
                 'f
                 (λ (reconstruct)
                   (test-stx=? (reconstruct (list #'(begin 42)))
                               #'(define (f x) 42)))))

(test-begin
  #:name select-any-define-named-form-body
  (test-selector select-any-define-named-form-body
                 #'(my-fancy-define x 5)
                 (list #'5)
                 'x
                 (λ (reconstruct)
                   (test-stx=? (reconstruct (list #'42))
                               #'(my-fancy-define x 42))))
  (test-selector select-any-define-named-form-body
                 #'(my-fancy-define (f x) 5)
                 (list #'5)
                 'f
                 (λ (reconstruct)
                   (test-stx=? (reconstruct (list #'42))
                               #'(my-fancy-define (f x) 42)))))

(test-begin
  #:name select-type-annotations+define-body
  (test-selector select-type-annotations+define-body
                 #'(: foo Any)
                 (list #'(: foo Any))
                 'foo:annotation
                 (λ (reconstruct)
                   (test-stx=? (reconstruct (list #'(: foo Something-Else)))
                               #'(: foo Something-Else))))
  (test-selector select-type-annotations+define-body
                 #'(: foo : Any)
                 (list #'(: foo : Any))
                 'foo:annotation
                 (λ (reconstruct)
                   (test-stx=? (reconstruct (list #'(: foo : Something-Else)))
                               #'(: foo : Something-Else))))
  (test-selector select-type-annotations+define-body
                 #'(define (f x)
                     (: y A)
                     (define y 32)
                     (: z B)
                     (define z 40)
                     (+ y z))
                 (list #'(: y A)
                       #'(define y 32)
                       #'(: z B)
                       #'(define z 40)
                       #'(+ y z))
                 'f:body
                 (λ (reconstruct)
                   (test-stx=? (reconstruct (list #'(: y A)
                                                  #'(define y 32)
                                                  #'(: z Any)
                                                  #'(define z 40)
                                                  #'(+ y z)))
                               #'(define (f x)
                                   (: y A)
                                   (define y 32)
                                   (: z Any)
                                   (define z 40)
                                   (+ y z)))))
  (test-selector select-type-annotations+define-body
                 #'(define (f x) : Number
                     (: y A)
                     (define y 32)
                     (: z B)
                     (define z 40)
                     (+ y z))
                 (list #'(: gensym Number)
                       #'(: y A)
                       #'(define y 32)
                       #'(: z B)
                       #'(define z 40)
                       #'(+ y z))
                 'f:body
                 (λ (reconstruct)
                   (test-stx=? (reconstruct (list #'(: gensym Any)
                                                  #'(: y A)
                                                  #'(define y 32)
                                                  #'(: z B)
                                                  #'(define z 40)
                                                  #'(+ y z)))
                               #'(define (f x) : Any
                                   (: y A)
                                   (define y 32)
                                   (: z B)
                                   (define z 40)
                                   (+ y z)))))
  (test-selector select-type-annotations+define-body
                 #'(define f
                     (λ (x)
                       (: y A)
                       x))
                 (list #'(λ (x)
                           (: y A)
                           x))
                 'f:body
                 (λ (reconstruct)
                   (test-stx=? (reconstruct (list #'(λ (x)
                                                      (: y Foobar)
                                                      x)))
                               #'(define f
                                   (λ (x)
                                     (: y Foobar)
                                     x))))))
