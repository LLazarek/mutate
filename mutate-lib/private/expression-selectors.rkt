#lang at-exp racket/base

(require racket/contract)

(provide (contract-out
          [expression-selector/c contract?]
          [select-any-expr expression-selector/c]
          [select-exprs-as-if-untyped expression-selector/c]))

(require racket/bool
         racket/format
         racket/function
         racket/list
         syntax/parse)

(define expression-selector/c
  (syntax?
   . -> .
   (or/c #f
         (list/c syntax?
                 (syntax? . -> . syntax?)
                 (listof (cons/c parameter? any/c))))))

(define (select-any-expr expr)
  (list expr
        identity
        empty))

(define-splicing-syntax-class type-annotation
  #:datum-literals [:]
  #:attributes [(annotation-parts 1)]
  [pattern {~seq : T}
           #:with [annotation-parts ...] #'[: T]]
  [pattern (: e ...)
           #:with [annotation-parts ...] #'[(: e ...)]])

;; If `expr` contains no type annotation subexprs, select the expr as-is.
;; If `expr` contains type annotations, select all subexprs of `expr` not
;; associated with the type annotations; the reconstructor puts the annotations
;; back in.
;;
;; ASSUMPTION: the reconstructor assumes that the syntax it receives will have
;; the same number of subexprs as the selected syntax.
;;
;; Example:
;; (+ 2 (apply (λ (x) x) 42))
;;   selects everything, and the reconstructor is identity
;; (+ 2 (apply (λ ([x : Natural]) : Natural x) 42))
;;   same as above
;; (ann (+ 2 (apply (λ ([x : Natural]) : Natural x) 42)) T)
;;   selects the inner expr, and the reconstructor puts the `(ann ... T)` back
;; (for : T (...) foobar)
;;   selects (for (...) foobar), and the reconstructor puts the `: T` back
(define (select-exprs-as-if-untyped expr)
  (syntax-parse expr
    #:datum-literals [: ann cast inst row-inst quote quasiquote]
    [(: . _) #f]
    [({~and the-annotation-thing
            {~or ann
                 cast
                 inst
                 row-inst}}
      e
      T ...)
     (list #'e
           (λ (new-e)
             (quasisyntax/loc expr
               (the-annotation-thing #,new-e T ...)))
           empty)]
    [({~and e-1 {~not {~or* : (: . _)}}}
      ...+
      {~seq annot:type-annotation
            {~and e-i {~not {~or* : (: . _)}}} ...}
      ...+)
     (define e-1-count (length (attribute e-1)))
     (define e-i-counts (map length (attribute e-i)))
     (list
      #'(e-1 ... {~@ e-i ...} ...)
      (λ (mutated-stx)
        (define mutated-stx-parts (syntax->list mutated-stx))
        (define (flexible-split-at l index)
          (if (> index (length l))
              (values l empty)
              (split-at l index)))
        (define-values {mutated-e-1s remaining-stx-parts}
          (flexible-split-at mutated-stx-parts e-1-count))
        (define mutated-e-is
          (for/fold ([remaining-stx-parts remaining-stx-parts]
                     [mutated-e-is empty]
                     #:result (reverse mutated-e-is))
                    ([e-i-count (in-list e-i-counts)]
                     [e-i-group-index (in-naturals)])
            (define-values {e-is now-remaining-stx}
              (if (= e-i-group-index (sub1 (length e-i-counts)))
                  (values remaining-stx-parts empty)
                  (flexible-split-at remaining-stx-parts e-i-count)))
            (values now-remaining-stx
                    (cons e-is mutated-e-is))))
        (with-syntax ([[mutated-e-1 ...] mutated-e-1s]
                      [[[mutated-e-i ...] ...] mutated-e-is])
          (syntax/loc expr
            (mutated-e-1
             ...
             {~@ annot.annotation-parts ... mutated-e-i ...}
             ...))))
      empty)]
    ;; ll: this is a bit naive, see tests below for #''(: a b c)
    [{~or* ({~or* quote quasiquote} atom)
           atom
           ({~and e-1 {~not :}} ...)}
     #:when (or (not (attribute atom))
                (false? (syntax->list #'atom)))
     (list this-syntax
           identity
           empty)]
    [other
     (error 'select-exprs-as-if-untyped
            @~a{Syntax @#'other doesn't match any patterns.})]))

