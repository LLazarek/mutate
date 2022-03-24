#lang at-exp racket/base

(require racket/contract)

(provide (contract-out
          [make-program-mutator ({mutator/c}
                                 {top-level-selector/c}
                                 . ->* .
                                 ({syntax? mutation-index?}
                                  {counter?}
                                  . ->* .
                                  (mutated/c mutated-program?)))]
          [without-counter
           ((unconstrained-domain-> (mutated/c mutated-program?))
            . -> .
            (unconstrained-domain-> mutated-program?))]
          [syntax-only
           ((unconstrained-domain-> (mutated/c mutated-program?))
            . -> .
            (unconstrained-domain-> syntax?))])

         (struct-out mutated-program)
         mutation-index-exception?)

(require racket/match
         syntax/parse
         "mutate-util.rkt"
         "mutated.rkt"
         "mutator-lib.rkt"
         "top-level-selectors.rkt")

(struct mutated-program (stx mutated-id) #:transparent)
(struct mutation-index-exception ())

(define (mutation-applied-already? mutation-index counter)
  (> counter mutation-index))

;; Note: distinction between mutate-program and mutate-expr is necessary because
;; mutate-program may want to descend only into certain top level forms, while
;; mutate-expr can descend into everything (mutate-program acts like its
;; gatekeeper)
(define ((make-program-mutator mutate-expr [select select-all])
         stx mutation-index [counter 0])
  (let mutate-program-rest ([stx stx]
                            [mutation-index mutation-index]
                            [counter counter])
    (if (<= counter mutation-index)
        (syntax-parse stx
          [(top-level-e e ...)
           #:do [(define-values {body-stxs top-level-id reconstruct-e}
                   (select #'top-level-e))]
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
           ;; signal no more mutations in this module
           (raise (mutation-index-exception))])

        (mutated (mutated-program stx #f) counter))))


(define (without-counter mutate-program)
  (compose1 (match-lambda [(mutated program counter)
                           program])
            mutate-program))

(define (syntax-only mutate-program)
  (compose1 (match-lambda [(mutated-program stx mutated-id)
                           stx])
            (without-counter mutate-program)))
