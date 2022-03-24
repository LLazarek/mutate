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

(module+ test
  (require racket
           syntax/parse
           ruinit
           "mutate-test-common.rkt")
  (test-begin
    #:name mutate-in-sequence
    (test-mutation/in-seq
     (list #'#t
           #''a
           #'1
           #'(+ 1 2))
     mutate-in-sequence
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
           ,#'(+ 1 2))]))))

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

(module+ test
  (test-begin
    #:name rearrange-in-sequence
    (test-programs-equal?
     #`(#,@(mutated-stx (rearrange-in-sequence
                         (syntax->list #'(a (+ 1 2)))
                         0 0)))
     #'((+ 1 2) a))
    (test-programs-equal?
     #`(#,@(mutated-stx (rearrange-in-sequence
                         (syntax->list #'(a (+ 1 2) b))
                         0 0)))
     #'((+ 1 2) a b))
    (test-programs-equal?
     #`(#,@(mutated-stx (rearrange-in-sequence
                         (syntax->list #'(a (+ 1 2) b (foo 3)))
                         1 0)))
     #'(a (+ 1 2) (foo 3) b))
    (test-programs-equal?
     #`(#,@(mutated-stx (rearrange-in-sequence
                         (syntax->list #'(a (+ 1 2) b (foo 3) (bar 3 4 5)))
                         2 0)))
     #'(a (+ 1 2) b (foo 3) (bar 3 4 5)))
    #| ... |#))

