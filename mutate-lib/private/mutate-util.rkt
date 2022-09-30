#lang at-exp racket/base

(require racket/contract)

(provide (contract-out
          [compound-expr? (syntax? . -> . boolean?)]
          [mutate-in-sequence ((listof any/c)
                               mutation-index?
                               counter?
                               mutator/c
                               . -> .
                               mutated?)]
          [rearrange-in-sequence ((listof syntax?)
                                  mutation-index?
                                  counter?
                                  . -> .
                                  (mutated/c (listof syntax?)))]))

(require racket/list
         racket/match
         syntax/parse
         "mutated.rkt"
         "mutator-lib.rkt")

(define (compound-expr? stx)
  (syntax-parse stx
    [(_ ...) #t]
    [datum   #f]))

(define (mutate-in-sequence stxs mutation-index counter
                            mutator)
  (for/fold ([mutated-so-far (mutated '() counter)]
             #:result (mmap reverse mutated-so-far))
            ([stx (in-list stxs)])
    (mutated-do
     #:count-with [__ #f]
     [stxs-so-far mutated-so-far]
     [element (mutator stx
                       mutation-index
                       __)]
     #:return (cons element stxs-so-far))))

(define (rearrange-in-sequence args-stxs mutation-index counter)
  (define-values (pairs remainder) (pair-off args-stxs))
  (mutated-do-single
   [pairs/swapped (mutate-in-sequence pairs
                                      mutation-index
                                      counter
                                      rearrange-pair)]
   #:return (unpair-off pairs/swapped remainder)))

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

