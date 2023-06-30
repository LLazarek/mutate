#lang info
(define collection "mutate")
(define deps '("base"
               "mutate-lib"
               "mutate-mutators"
               "mutate-doc"))
(define implies '("mutate-lib"
                  "mutate-doc"))
(define build-deps '())
(define pkg-desc "Program mutation library")
(define pkg-authors '(lukas))
