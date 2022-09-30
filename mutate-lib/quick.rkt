#lang at-exp racket/base

(provide build-mutation-engine)

(require (for-syntax racket/base
                     racket/format
                     syntax/contract
                     syntax/parse/lib/function-header)
         racket/contract/base
         racket/format
         syntax/parse/define
         "define.rkt"
         "program.rkt")

(begin-for-syntax
  (define-syntax-class mutator-definition
    #:description "a mutator definition"
    #:commit
    #:attributes [id]
    #:literals [define-id-mutator
                 define-constant-mutator
                 define-mutator
                 define-dependent-mutator
                 define-simple-mutator]
    (pattern (define-id-mutator id:id . _))
    (pattern ({~or* define-constant-mutator
                    define-mutator
                    define-dependent-mutator
                    define-simple-mutator}
              (id:id . _) . _))
    (pattern (define id:id . _))
    (pattern (define head:function-header . _)
             #:with id #'head.name)))

(define mutator/c-first-order
  (flat-named-contract 'mutator/c-first-order
                       (or/c mutator?
                             (and/c procedure? (procedure-arity-includes/c 3)))))

(define (mutator-id-ctc name)
  (suggest/c
   (suggest/c (and/c mutator/c-first-order mutator/c)
              "summary"
              @~a{@name does not satisfy the mutator/c contract})
   "suggestion"
   "if this is a helper definition, move it outside of build-mutation-engine"))

(define-simple-macro (build-mutation-engine
                      #:mutators
                      mutator:mutator-definition ...
                      {~alt {~optional {~seq #:expression-selector expr-selector:expr}}
                            {~optional {~seq #:top-level-selector top-level-selector:expr}}
                            {~once {~or* {~and #:with-mutated-id mode-id}
                                         {~and #:syntax-only     mode-stx}}}
                            {~optional {~and #:streaming stream-kw}}
                            {~optional {~and #:module-mutator module-kw}}} ...)
  #:with [checked-mutator-id ...] (for/list ([id (in-list (attribute mutator.id))])
                                    (wrap-expr/c #`(mutator-id-ctc '#,id)
                                                 id
                                                 #:context this-syntax
                                                 #:name @~a{the mutator named @(syntax->datum id)}))
  #:with program-mutator-transformer #`(compose1
                                        #,@(for/list ([include? (list (attribute stream-kw)
                                                                      (attribute mode-id)
                                                                      (attribute mode-stx)
                                                                      (attribute module-kw))]
                                                      [stx (list #'program-mutator->stream-builder
                                                                 #'without-counter
                                                                 #'syntax-only
                                                                 #'program-mutator->module-mutator)]
                                                      #:when include?)
                                             stx))
  (let ()
    mutator ...
    (define combined-mutators (compose-mutators checked-mutator-id ...))
    (define mutate-expr (make-expr-mutator combined-mutators
                                           {~? {~@ #:select expr-selector} {~@}}))
    (define mutate-program (make-program-mutator mutate-expr
                                                 {~? {~@ #:select top-level-selector} {~@}}))
    (program-mutator-transformer mutate-program)))
