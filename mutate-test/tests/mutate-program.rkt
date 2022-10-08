#lang at-exp racket/base

(require racket/stream
         ruinit
         mutate
         mutate/traversal
         mutate/private/mutate-program
         mutate/private/mutated
         "testing-util.rkt")

(define-id-mutator plus-swap
  [+ #:-> -])
(define em (make-expr-mutator plus-swap))

(test-begin
  #:name make-program-mutator
  (test-mutator* (syntax-only (make-program-mutator em))
                 #'[(require foo/bar)
                    (define (f x) (+ x 42))
                    (define (g x) (f (+ (f (- x)) x)))
                    (+ 1 2)]
                 (list #'[(require foo/bar)
                          (define (f x) (- x 42))
                          (define (g x) (f (+ (f (- x)) x)))
                          (+ 1 2)]
                       #'[(require foo/bar)
                          (define (f x) (+ x 42))
                          (define (g x) (f (- (f (- x)) x)))
                          (+ 1 2)]
                       #'[(require foo/bar)
                          (define (f x) (+ x 42))
                          (define (g x) (f (+ (f (- x)) x)))
                          (- 1 2)])
                 values)
  (test-equal? ((make-program-mutator em)
                #'[(require foo/bar)
                   (define (f x) (+ x 42))
                   (define (g x) (f (+ (f (- x)) x)))
                   (+ 1 2)]
                3)
               no-more-mutations-flag)

  (test-mutator* (syntax-only (make-program-mutator em #:select select-define-body))
                 #'[(require foo/bar)
                    (define (f x) (+ x 42))
                    (define (g x) (f (+ (f (- x)) x)))
                    (+ 1 2)]
                 (list #'[(require foo/bar)
                          (define (f x) (- x 42))
                          (define (g x) (f (+ (f (- x)) x)))
                          (+ 1 2)]
                       #'[(require foo/bar)
                          (define (f x) (+ x 42))
                          (define (g x) (f (- (f (- x)) x)))
                          (+ 1 2)])
                 values)
  (test-equal? ((make-program-mutator em #:select select-define-body)
                #'[(require foo/bar)
                   (define (f x) (+ x 42))
                   (define (g x) (f (+ (f (- x)) x)))
                   (+ 1 2)]
                2)
               no-more-mutations-flag)
  (test-match ((make-program-mutator em #:select select-define-body)
               #'[(require foo/bar)
                  (define (f x) (+ x 42))
                  (define (g x) (f (+ (f (- x)) x)))
                  (+ 1 2)]
               0)
              (mutated (mutated-program _ 'f) _)))

(test-begin
  #:name stream-builder
  (test-equal? (map syntax->datum
                    (stream->list
                     ((program-mutator->stream-builder
                       (syntax-only
                        (make-program-mutator em)))
                      #'[(require foo/bar)
                         (define (f x) (+ x 42))
                         (define (g x) (f (+ (f (- x)) x)))
                         (+ 1 2)])))
               '([(require foo/bar)
                  (define (f x) (- x 42))
                  (define (g x) (f (+ (f (- x)) x)))
                  (+ 1 2)]
                 [(require foo/bar)
                  (define (f x) (+ x 42))
                  (define (g x) (f (- (f (- x)) x)))
                  (+ 1 2)]
                 [(require foo/bar)
                  (define (f x) (+ x 42))
                  (define (g x) (f (+ (f (- x)) x)))
                  (- 1 2)])))

(test-begin
  #:name module-mutator
  (test-equal? (syntax->datum
                ((program-mutator->module-mutator
                  (syntax-only
                   (make-program-mutator em)))
                 #'(module a-test racket
                     (#%module-begin
                      (require foo/bar)
                      (define (f x) (+ x 42))
                      (define (g x) (f (+ (f (- x)) x)))
                      (+ 1 2)))
                 0))
               '(module a-test racket
                  (#%module-begin
                   (require foo/bar)
                   (define (f x) (- x 42))
                   (define (g x) (f (+ (f (- x)) x)))
                   (+ 1 2)))))

