#lang at-exp racket/base

(require racket/contract)

(struct mutator (function type)
  #:property prop:procedure (struct-field-index function))
(struct dependent-mutator (maker type)
  #:property prop:procedure (struct-field-index maker))

(define mutator-function/c (any/c mutation-index? counter? . -> . mutated?))
(define mutator-type? string?)
(define mutator/c (first-or/c (and/c mutator? (struct/c mutator mutator-function/c mutator-type?))
                              mutator-function/c))
(define (dependent-mutator/c . arg/cs)
  (struct/c dependent-mutator
            (dynamic->* #:mandatory-domain-contracts arg/cs
                        #:range-contracts mutator-function/c)
            mutator-type?))

(provide (contract-out
          [mutator/c contract?]
          [dependent-mutator/c (contract? ... . -> . contract?)]

          [rename try-get-mutator-type
                  mutator-type
                  ({mutator/c} {string?} . ->* . string?)]

          ;; Base mutator
          ;; essentially a more restricted mutator/c
          [maybe-mutate
           ({any/c
             any/c
             mutation-index?
             counter?}
            {#:equivalent? (any/c any/c . -> . boolean?)}
            . ->* .
            mutated?)]
          ;; Mutation sequence applier
          [apply-mutators (any/c
                           (listof mutator/c)
                           mutation-index?
                           counter?
                           . -> .
                           mutated?)]
          ;; Mutator constructor
          ;; applies the mutation function if the guard is satisfied.
          [make-guarded-mutator ((any/c . -> . boolean?)
                                 (any/c . -> . any/c)
                                 . -> .
                                 mutator/c)]
          ;; Composes the given mutators into one which applies each of the
          ;; mutators *in the given order*
          [compose-mutators (mutator/c mutator/c ... . -> . mutator/c)]
          [no-mutation mutator/c])

         (except-out (struct-out mutator) mutator-type)
         (struct-out dependent-mutator)

         define-id-mutator
         define-value-mutator
         define-mutator
         define-dependent-mutator
         define-simple-mutator)

(require racket/dict
         racket/format
         racket/match
         syntax/parse
         syntax/parse/define
         (for-syntax racket/base)
         "logger.rkt"
         "mutated.rkt")

(define (try-get-mutator-type m [default "<?>"])
  (if (mutator? m)
      (mutator-type m)
      default))

(define-simple-macro (define-mutator (name stx mutation-index counter) #:type [type-name type] e ...)
  (define name (let ([type-name type])
                 (mutator (λ (stx mutation-index counter) e ...)
                          type-name))))
(define-simple-macro (define-dependent-mutator (name . args) #:type [type-name type] e ...)
  (define name (let ([type-name type])
                 (dependent-mutator (λ args e ...)
                                    type))))


;; Base mutator: all mutation happens through this function
;;
;; Manages the decision of whether or not to apply a mutation based on
;; `mutation-index` and `counter`, recording the consideration of a
;; valid mutation (in terms of the counter).
;;
;; If `old` is equivalent to `new`, the mutation will not be considered. The
;; meaning of "equivalent" is determined by equivalence predicate given to
;; `#:equivalent?`. In most cases, `old` and `new` are syntax, and "equivalent"
;; means syntactically identical (`exprs-equal?`).
(define (maybe-mutate old new mutation-index counter
                      #:equivalent? [equivalent? exprs-equal?])
  (define should-apply-mutation?
    (and (= mutation-index counter)
         (not (equivalent? old new))))
  (when should-apply-mutation?
    (log-mutation old new))
  (mutated
   (if should-apply-mutation?
       new
       old)
   (if (equivalent? old new)
       counter
       ;; This was a mutation that could be applied, so increment
       ;; counter, indicating that a mutatable expr has been
       ;; considered.
       (add1 counter))))

(define (exprs-equal? a b)
  (equal? (syntax->datum a)
          (syntax->datum b)))



(define (maybe-mutate-value old-v new-v mutation-index counter)
  (maybe-mutate old-v
                new-v
                mutation-index
                counter
                #:equivalent? equal?))


(define-simple-macro (define-id-mutator name:id
                       #:type type:expr
                       {~alt [orig:id #:->  new:id]
                             [left:id #:<-> right:id]} ...)
  #:with [left-right-pair ...] #'[{~@ (left . right) (right . left)} ...]
  (begin
    (define swaps '((orig . new) ... left-right-pair ...))
    (define-mutator (name maybe-atom-stx mutation-index counter) #:type [type-name type]
      (if (syntax->list maybe-atom-stx)
          ;; Value mutators only make sense for atoms, so don't even try to apply
          ;; them on syntax-lists. This prevents accidentally stripping
          ;; syntax-properties from inner syntax objects of syntax-lists due to
          ;; the conversion from stx to datum and back.
          (no-mutation maybe-atom-stx mutation-index counter)
          (mmap (λ (swapped)
                  (datum->syntax maybe-atom-stx
                                 swapped
                                 maybe-atom-stx
                                 maybe-atom-stx))
                (apply-swap-alist (syntax->datum maybe-atom-stx)
                                  swaps
                                  mutation-index
                                  counter
                                  #:type type-name))))))

(define (apply-swap-alist original-value
                          swap-alist
                          mutation-index
                          counter
                          #:type mutation-type)
  (define mutator-sequence
    (for/list ([{orig new} (in-dict swap-alist)])
      (make-guarded-mutator (λ (v) (equal? v orig))
                            (λ (v)
                              (log-mutation-type mutation-type)
                              new))))
  (apply-mutators original-value
                  mutator-sequence
                  mutation-index
                  counter))

(define-simple-macro (define-value-mutator (name:id value-name:id)
                       #:type type:expr
                       [pat:expr #:-> replacement:expr] ...)
  (define-mutator (name maybe-atom-stx mutation-index counter) #:type [the-type type]
    (define mutation-sequence
      (list
       (make-guarded-mutator (λ (v) (match v
                                      [pat #t]
                                      [else #f]))
                             (match-lambda [(and value-name pat)
                                            (log-mutation-type the-type)
                                            replacement]))
       ...))
    (if (syntax->list maybe-atom-stx)
        ;; See note in `define-id-mutator`
        (no-mutation maybe-atom-stx mutation-index counter)
        (mmap (λ (mutated)
                (datum->syntax maybe-atom-stx
                               mutated
                               maybe-atom-stx
                               maybe-atom-stx))
              (apply-mutators (syntax->datum maybe-atom-stx)
                              mutation-sequence
                              mutation-index
                              counter)))))

(define (apply-mutators start
                        mutator-sequence
                        mutation-index
                        counter)
  (for/fold ([current-value (mutated start counter)])
            ([mutate (in-list mutator-sequence)])
    (mbind (λ (v current-counter)
             (mutate v mutation-index current-counter))
           current-value)))

;; A limitation of `make-guarded-mutator` is that you can't use it if your
;; mutator needs to guard any syntax from mutation, because the application of
;; `maybe-mutate` is out of your control, and guarding must be done outside of
;; that.
(define (make-guarded-mutator should-apply? apply [type "<guarded-mutator>"])
  (define-mutator (the-mutator orig-v mutation-index counter) #:type [_ type]
    (if (and (<= counter mutation-index)
             (should-apply? orig-v))
        (maybe-mutate-value orig-v
                            (apply orig-v)
                            mutation-index
                            counter)
        (mutated orig-v
                 counter)))
  the-mutator)

;; See note about limitation of simple mutators above `make-guarded-mutator`
(define-simple-macro (define-simple-mutator (name:id stx-name:id)
                       {~optional {~seq #:type type}
                                  #:defaults ([type #'(~a 'name)])}
                       #:pattern pattern
                       {~optional {~seq #:when guard}}
                       body ...)
  (define-mutator (name stx-name mutation-index counter) #:type [the-type type]
    (syntax-parse stx-name
      [pattern
       {~? {~@ #:when guard}}
       (log-mutation-type the-type)
       (define new-stx (let () body ...))
       (maybe-mutate stx-name new-stx mutation-index counter)]
      [else (no-mutation stx-name mutation-index counter)])))

(define-mutator (no-mutation v mutation-index counter) #:type [_ "<no-mutation>"]
  (mutated v counter))

(define (compose-mutators . mutators)
  (λ (stx mutation-index counter)
    ;; lltodo: this should handle 0 mutators by appending `no-mutation'
    (apply-mutators stx mutators mutation-index counter)))

(module+ test
  (require racket
           syntax/parse
           ruinit
           "mutate-test-common.rkt")
  (test-begin
    #:name maybe-mutate
    ;; Valid mutation, but counter is not yet high enough
    (test-equal? (mmap syntax->datum (maybe-mutate #'a #'b 5 0))
                 (mmap syntax->datum (mutated #'a 1)))
    ;; Valid mutation, counter is right
    (test-equal? (mmap syntax->datum (maybe-mutate #'a #'b 5 5))
                 (mmap syntax->datum (mutated #'b 6)))
    ;; Valid mutation but it is syntactically identical
    ;; This can happen when swapping argument positions
    ;; e.g. for (foo '() '())
    (test-equal? (mmap syntax->datum (maybe-mutate #'('() '()) #'('() '())
                                                   5
                                                   5))
                 (mmap syntax->datum (mutated #'('() '()) 5))))

  (test-begin
    #:name id-mutator
    (ignore (define-id-mutator mutate-id
              #:type "test"
              [car #:->  cdr]
              [+   #:<-> -]

              [foo #:->  bar]
              [foo #:->  boo]))
    (test-mutator mutate-id #'something-else #'something-else)
    (test-mutator mutate-id #'car #'cdr)
    (test-mutator mutate-id #'cdr #'cdr)
    (test-mutator mutate-id #'+ #'-)
    (test-mutator mutate-id #'- #'+)

    (test-mutator* mutate-id #'foo (list #'bar #'boo)))

  (test-begin
    #:name value-mutator
    (ignore (define-value-mutator (mutate-value value)
              #:type "test"
              [(? number?) #:-> (- value)]
              [(? number?) #:-> 0]
              [(? integer?) #:-> (exact->inexact value)]
              [(? string?) #:-> #f]))
    (test-mutator* mutate-value #'5 (list #'-5 #'0 #'5.0))
    (test-mutator* mutate-value #'2.5 (list #'-2.5 #'0))
    (test-mutator* mutate-value #'"hi" (list #'#f))
    (test-mutator* mutate-value #'x (list #'x)))

  (test-begin
    #:name apply-mutators
    (ignore
     (define a-compound-mutator
       (compose-mutators
        (λ (x index counter)
          (maybe-mutate x
                        #'surprise!
                        index
                        counter))
        (make-guarded-mutator
         (λ (x) #t)
         (λ (x) #'surprise-2!))
        (make-guarded-mutator
         (λ (x) #f)
         (λ (x) #'never-happens))
        (make-guarded-mutator
         (syntax-parser [(a b c) #t]
                        [else #f])
         (syntax-parser [(a b c) #'(a 42 c)])))))
    (test-mutator* a-compound-mutator
                   #'an-id
                   (list #'surprise!
                         #'surprise-2!
                         #'an-id))
    (test-mutator* a-compound-mutator
                   #'(+ x 3)
                   (list #'surprise!
                         #'surprise-2!
                         #'(+ 42 3)
                         #'(+ x 3)))))
