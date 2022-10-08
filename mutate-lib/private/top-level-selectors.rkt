#lang at-exp racket/base

(require racket/contract)
(provide (contract-out
          [top-level-selector/c                     contract?]
          [select-all                               top-level-selector/c]
          [select-define/contract-body              top-level-selector/c]
          [select-any-define-named-form-body        top-level-selector/c]

          [select-define-body                       top-level-selector/c]
          [select-type-annotations+define-body      top-level-selector/c]

          [leftmost-identifier-in                   (syntax? . -> . symbol?)]))

(require racket/bool
         racket/format
         racket/function
         racket/list
         racket/match
         racket/syntax
         syntax/parse)

(define-syntax-class contracted-definition
  #:description "define/contract form"
  (pattern ((~and (~datum define/contract)
                  def/c)
            id/sig ctc body ...)))
(define-syntax-class definition
  #:description "define form"
  (pattern (def-form id/sig body ...)
           #:when (regexp-match? #rx"define"
                                 (symbol->string (syntax->datum #'def-form)))))

(define (leftmost-identifier-in stx)
  (match (flatten (list (syntax->datum stx)))
    [(list* (? symbol? s) _) s]
    [else '<no-name-found>]))

(define top-level-selector/c
  (syntax? . -> . (or/c #f
                        (list/c (listof syntax?)
                                any/c
                                ((listof syntax?) . -> . syntax?)))))

(define (select-all stx)
  (define name (leftmost-identifier-in stx))
  (list (list stx)
        name
        (match-lambda
          [(list stx/datum/mutated)
           stx/datum/mutated]
          [a-bigger-list
           (error 'select-all
                  @~a{
                      Mutation produced multiple stxs from one stx?
                      Original: @stx
                      Mutated: @a-bigger-list
                      })])))

(define (select-define/contract-body stx)
  (syntax-parse stx
    [def:contracted-definition
      (match-define (list to-mutate id reconstructor)
        (select-define-body #'(define def.id/sig def.body ...)))
      (define (reconstruct-definition body-stxs/mutated)
        (syntax-parse (reconstructor body-stxs/mutated)
          [(define _ mutated-body-e ...)
           (syntax/loc stx
             (def.def/c def.id/sig def.ctc
               mutated-body-e ...))]))
      (list to-mutate
            (leftmost-identifier-in #'def.id/sig)
            reconstruct-definition)]
    [_ #f]))

(define (select-any-define-named-form-body stx)
  (syntax-parse stx
    [def:definition
      (define body-stxs (syntax->list (syntax/loc stx
                                        (def.body ...))))
      (define (reconstruct-definition body-stxs/mutated)
        (quasisyntax/loc stx
          (def.def-form def.id/sig
            #,@body-stxs/mutated)))
      (list body-stxs
            (leftmost-identifier-in #'def.id/sig)
            reconstruct-definition)]
    [_ #f]))

(define (select-define-body stx)
  (syntax-parse stx
    #:datum-literals [define :]
    [({~and define def} id/sig
                        {~optional {~seq : type}}
                        body ...)
     (define function? (and (syntax->list #'id/sig) #t))
     (define body-stxs
       (if function?
           (list (syntax/loc stx (begin body ...)))
           (attribute body)))
     (define (reconstruct-definition body-stxs/mutated)
       (define body-stxs/no-begin
         (if function?
             (syntax-parse body-stxs/mutated
               [[{~describe "begin-wrapped function body (from select-define-body reconstructor)"
                            (begin mutated-body-e ...)}]
                (attribute mutated-body-e)])
             body-stxs/mutated))
       (quasisyntax/loc stx
         (def id/sig {~? {~@ : type}} #,@body-stxs/no-begin)))
     (list body-stxs
           (leftmost-identifier-in #'id/sig)
           reconstruct-definition)]
    [_ #f]))

(define (select-type-annotations+define-body stx)
  (syntax-parse stx
    #:datum-literals [define :]
    [({~and : colon} name:id . _)
     (list (list stx)
           (format-symbol "~a:annotation"
                          (leftmost-identifier-in #'name))
           first)]
    [({~and {~datum define} def} id/sig {~optional {~seq {~and : colon} T}}
                                 body ...)
     (define body-stxs (syntax-e (syntax/loc stx
                                   ({~? (colon gensym T)} body ...))))
     (define (reconstruct-definition body-stxs/mutated)
       (if (attribute T)
           (syntax-parse body-stxs/mutated
             [(({~datum :} _ mutated-T) mutated-body-stx ...)
              (syntax/loc stx
                (def id/sig colon mutated-T mutated-body-stx ...))])
           (quasisyntax/loc stx
             (def id/sig #,@body-stxs/mutated))))
     (list body-stxs
           (format-symbol "~a:body"
                          (leftmost-identifier-in #'id/sig))
           reconstruct-definition)]
    [else #f]))

