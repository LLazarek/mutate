#lang at-exp racket/base

(require racket/contract
         racket/stream)

(struct mutator (function type)
  #:property prop:procedure (struct-field-index function))
(struct dependent-mutator (maker type)
  #:property prop:procedure (struct-field-index maker))

(define mutator-function/c ({any/c mutation-index?} {counter?} . ->* . mutated?))
(define mutator-type? string?)
(define mutator/c (first-or/c (and/c mutator? (struct/c mutator mutator-function/c mutator-type?))
                              mutator-function/c))
(define (dependent-mutator/c . arg/cs)
  (struct/c dependent-mutator
            (dynamic->* #:mandatory-domain-contracts arg/cs
                        #:range-contracts (list mutator-function/c))
            mutator-type?))

(provide (contract-out
          [mutator/c contract?]
          [dependent-mutator/c (contract? ... . -> . contract?)]

          [rename try-get-mutator-type
                  mutator-type
                  ({mutator/c} {string?} . ->* . string?)]
          [current-mutator-type (parameter/c string?)]

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
          [make-guarded-mutator ({(any/c . -> . boolean?)
                                  (any/c . -> . any/c)}
                                 {#:type string?}
                                 . ->* .
                                 mutator/c)]
          [make-stream-mutator ({(syntax? . -> . (stream/c syntax?))}
                                {#:type string?}
                                . ->* .
                                mutator/c)]
          ;; Composes the given mutators into one which applies each of the
          ;; mutators *in the given order*
          [compose-mutators (mutator/c mutator/c ... . -> . mutator/c)]
          [no-mutation mutator/c])

         (except-out (struct-out mutator) mutator-type)
         (struct-out dependent-mutator)

         define-id-mutator
         define-constant-mutator
         define-mutator
         define-dependent-mutator
         define-simple-mutator)

(require racket/bool
         racket/dict
         racket/format
         racket/match
         syntax/parse
         syntax/parse/define
         racket/list
         (for-syntax racket/base)
         "logger.rkt"
         "mutated.rkt")

(define current-mutator-type (make-parameter "<?>"))

(define (try-get-mutator-type m [default "<?>"])
  (if (mutator? m)
      (mutator-type m)
      default))

(define-simple-macro (define-mutator (name stx mutation-index counter) #:type type
                       e ...)
  (define name (let ([the-type type])
                 (mutator (λ (stx mutation-index [counter 0])
                            (parameterize ([current-mutator-type the-type])
                              e ...))
                          the-type))))
(define-simple-macro (define-dependent-mutator (name . args) #:type type
                       e ...)
  (define name (let ([the-type type])
                 (dependent-mutator (λ args
                                      (parameterize ([current-mutator-type the-type])
                                        e ...))
                                    the-type))))


;; Base mutator: all mutation happens through this function
;;
;; Manages the decision of whether or not to apply a mutation based on
;; `mutation-index` and `counter`, recording the consideration of a
;; valid mutation (in terms of the counter).
;;
;; If `old` is equivalent to `new`, the mutation will not be considered. The
;; meaning of "equivalent" is determined by equivalence predicate given to
;; `#:equivalent?`. In most cases, `old` and `new` are syntax, and "equivalent"
;; means syntactically identical (`stx-equal?`).
(define (maybe-mutate old new mutation-index counter
                      #:equivalent? [equivalent? stx-equal?])
  (define should-apply-mutation?
    (and (= mutation-index counter)
         (not (equivalent? old new))))
  (when should-apply-mutation?
    (log-mutation old new (current-mutator-type)))
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

;; Copied from TR internals somewhere
(define (stx-equal? s1 s2)
  (cond [(and (identifier? s1) (identifier? s2))
         (free-identifier=? s1 s2)]
        [else
         (if (and (syntax? s1) (syntax? s2))
             (equal?/recur (syntax-e s1) (syntax-e s2) stx-equal?)
             (equal?/recur s1 s2 stx-equal?))]))



(define (maybe-mutate-value old-v new-v mutation-index counter)
  (maybe-mutate old-v
                new-v
                mutation-index
                counter
                #:equivalent? equal?))


(define-simple-macro (define-id-mutator name:id
                       {~optional {~seq #:type type}
                                  #:defaults ([type #'(~a 'name)])}
                       {~alt [orig:id #:->  new:id]
                             [left:id #:<-> right:id]} ...)
  #:with [left-right-pair ...] #'[{~@ (left . right) (right . left)} ...]
  (begin
    (define swaps '((orig . new) ... left-right-pair ...))
    (define-mutator (name maybe-atom-stx mutation-index counter) #:type type
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
                                  counter))))))

(define (apply-swap-alist original-value
                          swap-alist
                          mutation-index
                          counter)
  (define mutator-sequence
    (for/list ([{orig new} (in-dict swap-alist)])
      (make-guarded-mutator (λ (v) (equal? v orig))
                            (λ (v) new))))
  (apply-mutators original-value
                  mutator-sequence
                  mutation-index
                  counter))

(define-simple-macro (define-constant-mutator (name:id value-name:id)
                       {~optional {~seq #:type type}
                                  #:defaults ([type #'(~a 'name)])}
                       [pat:expr #:-> replacement:expr] ...)
  (define-mutator (name maybe-atom-stx mutation-index counter) #:type type
    (define mutation-sequence
      (list
       (make-guarded-mutator (λ (v) (match v
                                      [pat #t]
                                      [else #f]))
                             (match-lambda [(and value-name pat)
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
(define (make-guarded-mutator should-apply? apply #:type [type (current-mutator-type)])
  (define-mutator (the-mutator orig-v mutation-index counter) #:type type
    (if (and (<= counter mutation-index)
             (should-apply? orig-v))
        (maybe-mutate-value orig-v
                            (apply orig-v)
                            mutation-index
                            counter)
        (mutated orig-v
                 counter)))
  the-mutator)

;; (stx? -> (streamof stx?))
(define (make-stream-mutator make-stream #:type [type (current-mutator-type)])
  ;; One might think we could take a shortcut here by just calling
  ;; `(next-mutation stx (- mutation-index counter))`
  ;; This doesn't work because some change in between there might
  ;; produce a syntactically equivalent mutant!
  (define-mutator (the-mutator stx mutation-index counter) #:type type
    (let loop ([mutated-so-far (no-mutation stx mutation-index counter)]
               [stream (make-stream stx)])
      (cond [(> (mutated-new-counter mutated-so-far) mutation-index)
             mutated-so-far]
            [(stream-empty? stream)
             mutated-so-far]
            [else
             (define next (mbind (λ (stx-so-far current-counter)
                                   (maybe-mutate stx-so-far
                                                 (stream-first stream)
                                                 mutation-index
                                                 current-counter))
                                 mutated-so-far))
             (loop next
                   (stream-rest stream))])))
  the-mutator)


;; See note about limitation of simple mutators above `make-guarded-mutator`
;;
;; Another limitation is that a mutator made with `define-simple-mutator` can only make
;; one possible change to a piece of syntax.
;;
;; TODO: support making multiple possible changes by letting `body` produce a list of syntaxes
;; representing a sequence of mutations.
(define-simple-macro (define-simple-mutator (name:id stx-name:id)
                       {~optional {~seq #:type type}
                                  #:defaults ([type #'(~a 'name)])}
                       #:pattern pattern
                       {~optional {~seq #:when guard}}
                       body ...)
  (define name
    (make-stream-mutator (λ (stx-name)
                           (syntax-parse stx-name
                             [pattern
                              {~? {~@ #:when guard}}
                              (stream-cons (let () body ...)
                                           empty-stream)]
                             [_ empty-stream]))
                         #:type type)))

(define-mutator (no-mutation v mutation-index counter) #:type "<no-mutation>"
  (mutated v counter))

(define (compose-mutators . mutators)
  (if (empty? mutators)
      no-mutation
      (λ (stx mutation-index [counter 0])
        (apply-mutators stx
                        mutators
                        mutation-index
                        counter))))

