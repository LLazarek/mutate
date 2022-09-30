#lang racket

(require syntax/parse
         mutate
         mutate/quick)

(define program-mutations
  (build-mutation-engine
   #:mutators
   (define-simple-mutator (if-swap stx)
     #:pattern ({~literal if} cond t e)
     #:when (list? (syntax->datum #'cond))
     #'(if cond e t))
   (define-constant-mutator (constant-swap v)
     [(? number?) #:-> (- v)])
   #:syntax-only
   #:streaming
   #:module-mutator))

(define program-to-mutate
  #'(module test-program racket
      (#%module-begin
       (require "a.rkt")
       (define x (if (yes?) 0 42))
       (define y (if (negative? x)
                     "negative!"
                     (if (zero? x)
                         "zero!"
                         "positive!")))
       (displayln y))))
(map syntax->datum
     (stream->list (program-mutations program-to-mutate)))
;; =>
#;'((module test-program racket
      (#%module-begin
       (require "a.rkt")
       (define x (if (yes?) 42 0))
       (define y (if (negative? x) "negative!" (if (zero? x) "zero!" "positive!")))
       (displayln y)))
    (module test-program racket
      (#%module-begin
       (require "a.rkt")
       (define x (if (yes?) 0 -42))
       (define y (if (negative? x) "negative!" (if (zero? x) "zero!" "positive!")))
       (displayln y)))
    (module test-program racket
      (#%module-begin
       (require "a.rkt")
       (define x (if (yes?) 0 42))
       (define y (if (negative? x) (if (zero? x) "zero!" "positive!") "negative!"))
       (displayln y)))
    (module test-program racket
      (#%module-begin
       (require "a.rkt")
       (define x (if (yes?) 0 42))
       (define y (if (negative? x) "negative!" (if (zero? x) "positive!" "zero!")))
       (displayln y))))

(module test racket)
