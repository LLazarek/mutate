#lang at-exp racket/base

(require racket/contract
         racket/bool)

(provide (contract-out
          [compound-expr? (syntax? . -> . boolean?)]
          [mutate-in-seq ((listof any/c)
                          mutation-index?
                          counter?
                          mutator/c
                          . -> .
                          mutated?)]
          [rearrange-in-seq ((listof syntax?)
                             mutation-index?
                             counter?
                             . -> .
                             (mutated/c (listof syntax?)))]
          [mutate-with-generator (syntax?
                                  (syntax? counter? . -> . (or/c syntax? #f))
                                  mutation-index?
                                  counter?
                                  . -> .
                                  (mutated/c syntax?))]))

(require racket/list
         racket/match
         syntax/parse
         "mutated.rkt"
         "mutator-lib.rkt")

(define (compound-expr? stx)
  (syntax-parse stx
    [(_ ...) #t]
    [datum   #f]))

(define (mutate-in-seq stxs mutation-index counter
                       mutator)
  (for/fold ([mutated-so-far (mutated '() counter)]
             #:result (mmap reverse mutated-so-far))
            ([stx (in-list stxs)])
    (mdo [count-with (__ #f)]
         (def stxs-so-far mutated-so-far)
         (def element (mutator stx
                               mutation-index
                               __))
         [return (cons element stxs-so-far)])))

;; stx?
;; (stx? index? . -> . (or/c stx? #f))
;; index?
;; index?
;; ->
;; mutated?
(define (mutate-with-generator stx next-mutation mutation-index counter)
  ;; One might think we could take a shortcut here by just calling
  ;; `(next-mutation stx (- mutation-index counter))`
  ;; This doesn't work because some change in between there might
  ;; produce a syntactically equivalent mutant!
  (let loop ([mutated-so-far (no-mutation stx mutation-index counter)]
             [i 0])
    (cond [(> (mutated-new-counter mutated-so-far) mutation-index)
           mutated-so-far]
          [else
           (define next
             (mdo [count-with (__ #f)]
                  (def stx-so-far mutated-so-far)
                  (def/value new-stx (next-mutation stx-so-far i))
                  (def swapped (maybe-mutate stx-so-far
                                             new-stx
                                             mutation-index
                                             __))
                  [return (if (false? new-stx)
                              #f
                              swapped)]))
           (if (false? (mutated-stx next))
               mutated-so-far
               (loop next
                     (add1 i)))])))

(module+ test
  (require racket
           syntax/parse
           ruinit
           "mutate-test-common.rkt")
  (test-begin
    #:name mutate-in-seq
    (test-mutation/in-seq
     (list #'#t
           #''a
           #'1
           #'(+ 1 2))
     mutate-in-seq
     (λ (stx mutation-index counter)
       (maybe-mutate stx
                     (syntax-parse stx
                       [_:number #'42]
                       [b:boolean #'(not b)]
                       [other stx])
                     mutation-index
                     counter))
     (curry map syntax->datum)
     `(#;[1 (,#'#t
             ,#''a
             ,#'1
             ,#'(+ 1 2))]
       [0 (,#'(not #t)
           ,#''a
           ,#'1
           ,#'(+ 1 2))]
       [1 (,#'#t
           ,#''a
           ,#'42
           ,#'(+ 1 2))]
       [2 (,#'#t
           ,#''a
           ,#'1
           ,#'(+ 1 2))])))
  (test-begin
    #:name mutate-with-generator
    (ignore
     (define (choose-permutation stx i)
       (define l (syntax->list stx))
       (define perms (permutations l))
       (and (< i (length perms))
            (datum->syntax stx (list-ref perms i))))
     (define mutate-from-permutations
       (λ (stx mutation-index counter)
         (mutate-with-generator stx
                                choose-permutation
                                mutation-index
                                counter))))
    (test-mutator*
     mutate-from-permutations
     #'(a b c)
     (list #'(b a c)
           #'(a c b)
           #'(c a b)
           #'(b c a)
           #'(c b a)))
    (ignore
     (define mutate-from-empty
       (λ (stx mutation-index counter)
         (mutate-with-generator stx
                                (λ _ #f)
                                mutation-index
                                counter))))
    (test-mutator*
     mutate-from-empty
     #'(a b c)
     (list))
    (ignore
     (define mutate-with-a-bigger-number
       (λ (stx mutation-index counter)
         (mutate-with-generator stx
                                (λ (stx i) (datum->syntax stx (+ i (syntax->datum stx))))
                                mutation-index
                                counter))))
    (test-mutator*
     mutate-with-a-bigger-number
     #'0
     (list #'1
           #'2
           #'3
           #'4
           #'5
           #'6
           #'7
           #'8
           #| ... |#))))

(define (rearrange-in-seq args-stxs mutation-index counter)
  (define-values (pairs remainder) (pair-off args-stxs))
  (mdo* (def pairs/swapped (mutate-in-seq pairs
                                          mutation-index
                                          counter
                                          rearrange-pair))
        [return (unpair-off pairs/swapped remainder)]))

(define/contract (rearrange-pair args mutation-index counter)
  ((list/c syntax? syntax?) mutation-index? counter? . -> . (mutated/c syntax?))

  (match-define (list arg1 arg2) args)
  (define unmutated-pair (quasisyntax/loc arg1
                           (#,arg1 #,arg2)))
  (if (> counter mutation-index)
      (mutated unmutated-pair counter)
      ;; note that the swapped pair may be syntactically identical to
      ;; the original. `maybe-mutate` handles this issue
      ;; automatically.
      (maybe-mutate unmutated-pair
                    (quasisyntax/loc arg1
                      (#,arg2 #,arg1))
                    mutation-index
                    counter)))

(define (pair-off lst)
  (for/fold ([paired empty]
             [current-pair empty]
             #:result (values (reverse paired) current-pair))
            ([el (in-list lst)])
    (if (empty? current-pair)
        (values paired (list el))
        (values (cons (list (first current-pair) el)
                      paired)
                empty))))

(define (unpair-off pairs remainder)
  (append (flatten (map syntax->list pairs)) remainder))

(module+ test
  (test-begin
    #:name rearrange-in-seq
    (test-programs-equal?
     #`(#,@(mutated-stx (rearrange-in-seq
                         (syntax->list #'(a (+ 1 2)))
                         0 0)))
     #'((+ 1 2) a))
    (test-programs-equal?
     #`(#,@(mutated-stx (rearrange-in-seq
                         (syntax->list #'(a (+ 1 2) b))
                         0 0)))
     #'((+ 1 2) a b))
    (test-programs-equal?
     #`(#,@(mutated-stx (rearrange-in-seq
                         (syntax->list #'(a (+ 1 2) b (foo 3)))
                         1 0)))
     #'(a (+ 1 2) (foo 3) b))
    (test-programs-equal?
     #`(#,@(mutated-stx (rearrange-in-seq
                         (syntax->list #'(a (+ 1 2) b (foo 3) (bar 3 4 5)))
                         2 0)))
     #'(a (+ 1 2) b (foo 3) (bar 3 4 5)))
    #| ... |#))

