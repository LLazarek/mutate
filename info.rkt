#lang info
(define collection "mutate")
(define deps '("base"
               "ruinit"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "at-exp-lib"
                     "syntax-sloc"))
(define pkg-desc "Program mutation library")
(define pkg-authors '(lukas))
(define scribblings '(("scribblings/mutate.scrbl" ())))
(define compile-omit-paths '("example"))
(define test-omit-paths    '("example"))

