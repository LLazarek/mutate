#lang at-exp racket/base

(require racket/stream
         ruinit
         mutate
         mutate/traversal
         "testing-util.rkt")

(test-begin
  #:do [(define program->mutants
          (build-mutation-engine
           #:mutators
           (define-id-mutator plus-swap
             [+ #:-> -])
           #:syntax-only
           #:streaming))]
  (test-equal? (map syntax->datum
                    (stream->list
                     (program->mutants
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
  #:do [(define program->mutant
          (build-mutation-engine
           #:mutators
           (define-id-mutator plus-swap
             [+ #:-> -])
           #:top-level-selector select-define-body
           #:with-mutated-id
           #:module-mutator))]
  (test-match (program->mutant
               #'(module a mylang
                   (#%module-begin
                    (require foo/bar)
                    (define (f x) (+ x 42))
                    (define (g x) (f (+ (f (- x)) x)))
                    (+ 1 2)))
               0)
              (mutated-program
               (app syntax->datum
                    '(module a mylang
                       (#%module-begin
                        (require foo/bar)
                        (define (f x) (- x 42))
                        (define (g x) (f (+ (f (- x)) x)))
                        (+ 1 2))))
               'f)))

(test-begin
  #:do [(define program->mutant
          (build-mutation-engine
           #:mutators
           (define-id-mutator plus-swap
             [+ #:-> -])
           #:top-level-selector select-define-body
           #:expression-selector select-exprs-as-if-untyped
           #:with-mutated-id
           #:module-mutator))]
  (test-match (program->mutant
               #'(module a typed/racket
                   (#%module-begin
                    (require foo/bar)
                    (define (f x) (: x (a + type + with + +s + in + it)) (+ x 42))
                    (define (g x) (f (+ (f (- x)) x)))
                    (+ 1 2)))
               0)
              (mutated-program
               (app syntax->datum
                    '(module a typed/racket
                       (#%module-begin
                        (require foo/bar)
                        (define (f x) (: x (a + type + with + +s + in + it)) (- x 42))
                        (define (g x) (f (+ (f (- x)) x)))
                        (+ 1 2))))
               'f)))
