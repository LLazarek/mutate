#lang at-exp racket/base

(require racket/contract
         racket/stream
         syntax/parse)

(provide (contract-out
          [make-program-mutator ({mutator/c}
                                 {top-level-selector/c}
                                 . ->* .
                                 full-program-mutator/c)]
          [without-counter
           (full-program-mutator/c
            . -> .
            (program-mutator/c mutated-program?))]
          [syntax-only
           (full-program-mutator/c
            . -> .
            (program-mutator/c syntax?))]

          [program-mutator->module-mutator
           ((program-mutator/c any/c)
            . -> .
            (program-mutator/c any/c))]
          [program-mutator->stream-builder
           ((program-mutator/c any/c)
            . -> .
            (syntax? . -> . (stream/c any/c)))])

         (struct-out mutated-program)
         no-more-mutations-flag)

(require racket/match
         syntax/parse
         "mutate-util.rkt"
         "mutated.rkt"
         "mutator-lib.rkt"
         "top-level-selectors.rkt")

(struct mutated-program (stx mutated-id) #:transparent)
(define no-more-mutations-flag #f)

(define (program-mutator/c result/c)
  ({syntax? mutation-index?} {counter?} . ->* . (or/c result/c no-more-mutations-flag)))
(define full-program-mutator/c
  (program-mutator/c (mutated/c mutated-program?)))

(define (mutation-applied-already? mutation-index counter)
  (> counter mutation-index))

;; Note: distinction between mutate-program and mutate-expr is necessary because
;; mutate-program may want to descend only into certain top level forms, while
;; mutate-expr can descend into everything (mutate-program acts like its
;; gatekeeper)
(define ((make-program-mutator mutate-expr [select select-all])
         stx mutation-index [counter 0])
  (let/ec bail
    (let mutate-program-rest ([stx stx]
                              [mutation-index mutation-index]
                              [counter counter])
      (if (<= counter mutation-index)
          (syntax-parse stx
            [(top-level-e e ...)
             #:do [(match-define (list body-stxs top-level-id reconstruct-e)
                     (or (select #'top-level-e)
                         (list #f #f #f)))]
             #:when body-stxs
             (mutated-do
              #:count-with [__ counter]
              [body-stxs/mutated (mutate-in-sequence body-stxs
                                                     mutation-index
                                                     __
                                                     mutate-expr)]
              #:let [body-stxs-mutated? (mutation-applied-already? mutation-index __)]
              ;; move on to the rest of the program, if necessary
              [(mutated-program program-rest mutated-fn-in-rest)
               (mutate-program-rest (syntax/loc stx (e ...))
                                    mutation-index
                                    __)]
              #:return (mutated-program
                        (quasisyntax/loc stx
                          (#,(reconstruct-e body-stxs/mutated)
                           #,@program-rest))
                        (if body-stxs-mutated?
                            top-level-id
                            mutated-fn-in-rest)))]

            ;; Ignore anything else
            [(other-e e ...)
             (mutated-do-single
              [(mutated-program rest-stxs mutated-fn-in-rest)
               (mutate-program-rest (syntax/loc stx (e ...))
                                    mutation-index
                                    counter)]
              #:return (mutated-program
                        (quasisyntax/loc stx
                          (other-e #,@rest-stxs))
                        mutated-fn-in-rest))]
            [()
             (bail no-more-mutations-flag)])

          (mutated (mutated-program stx #f) counter)))))


(define (without-counter mutate-program)
  (compose1 (match-lambda [(mutated program counter)
                           program]
                          [(== no-more-mutations-flag)
                           no-more-mutations-flag])
            mutate-program))

(define (syntax-only mutate-program)
  (compose1 (match-lambda [(mutated-program stx mutated-id)
                           stx]
                          [(== no-more-mutations-flag)
                           no-more-mutations-flag])
            (without-counter mutate-program)))

(define (transform-stx-in wrapped f)
  (let loop ([maybe-stx wrapped])
    (match maybe-stx
      [(mutated stx counter)
       (mutated (loop stx) counter)]
      [(mutated-program stx id)
       (mutated-program (loop stx) id)]
      [(? syntax? stx) (f stx)]
      [(== no-more-mutations-flag)
       no-more-mutations-flag])))

(define (make-indexed-stream-terminated-by-exn f exn-predicate)
  (let loop ([i 0])
    (with-handlers ([exn-predicate (λ _ empty-stream)])
      (stream-cons #:eager (f i)
                   (loop (add1 i))))))
(define (make-indexed-stream-terminated-by-sentinel-value f sentinel)
  (let loop ([i 0])
    (define head (f i))
    (if (equal? head sentinel)
        empty-stream
        (stream-cons head
                     (loop (add1 i))))))

(define (program-mutator->stream-builder engine)
  (λ (stx)
    (make-indexed-stream-terminated-by-sentinel-value (λ (i) (engine stx i))
                                                      no-more-mutations-flag)))

(define (program-mutator->module-mutator mutate-program)
  (λ (stx index [counter 0])
    (syntax-parse stx
      [(module name lang (mod-begin top-level-e ...))
       (transform-stx-in (mutate-program #'[top-level-e ...] index counter)
                         (syntax-parser
                           [{new-top-level-e ...}
                            #'(module name lang (mod-begin new-top-level-e ...))]))]
      [else
       (raise-argument-error
        'program-mutator->module-mutator
        "a module syntax object, of the form (module name lang (#%module-begin e ...))"
        stx)])))

