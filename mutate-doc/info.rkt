#lang info
(define collection "mutate")
(define deps '("base"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "mutate-lib"
                     "mutate-mutators"))
(define pkg-desc "Program mutation library - docs")
(define pkg-authors '(lukas))
(define scribblings '(("scribblings/mutate.scrbl" (multi-page))))

