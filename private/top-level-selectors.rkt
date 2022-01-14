#lang at-exp racket/base

(require racket/contract)
(provide (contract-out
          [top-level-selector/c    contract?]
          [select-all                          top-level-selector/c]
          [select-define/contract              top-level-selector/c]
          [select-any-define-named-form        top-level-selector/c]

          [select-define-body                  top-level-selector/c]
          [select-type-annotations+define-body top-level-selector/c]

          [leftmost-identifier-in  (syntax? . -> . symbol?)]))

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
  (->i ([stx syntax?])
       (values [parts-to-mutate (or/c #f (listof syntax?))]
               [mutated-id (or/c #f symbol?)]
               [reconstruct-stx (or/c #f ((listof syntax?) . -> . syntax?))])
       #:post/desc {parts-to-mutate mutated-id reconstruct-stx}
       (or (andmap false? (list parts-to-mutate
                                mutated-id
                                reconstruct-stx
                                ;; force to bool
                                #f))
           (andmap identity (list parts-to-mutate
                                  mutated-id
                                  reconstruct-stx
                                  ;; force to bool
                                  #t))
           "Either all results must be #f or all must be non-#f.")))

(define (select-all stx)
  (define name (leftmost-identifier-in stx))
  (match (syntax->list stx)
    [(? list? stx/list)
     (values stx/list
             name
             (λ (stxs/mutated)
               (datum->syntax stx stxs/mutated)))]
    [else
     (values (list stx)
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
                           })]))]))

(define (select-define/contract stx)
  (syntax-parse stx
    [def:contracted-definition
      (define-values {to-mutate id reconstructor}
        (select-define-body #'(define def.id/sig def.body ...)))
      (define (reconstruct-definition body-stxs/mutated)
        (syntax-parse (reconstructor body-stxs/mutated)
          [(define _ mutated-body-e ...)
           (syntax/loc stx
             (def.def/c def.id/sig def.ctc
               mutated-body-e ...))]))
      (values to-mutate
              (leftmost-identifier-in #'def.id/sig)
              reconstruct-definition)]
    [_ (values #f #f #f)]))

(define (select-any-define-named-form stx)
  (syntax-parse stx
    [def:definition
      (define body-stxs (syntax->list (syntax/loc stx
                                        (def.body ...))))
      (define (reconstruct-definition body-stxs/mutated)
        (quasisyntax/loc stx
          (def.def-form def.id/sig
            #,@body-stxs/mutated)))
      (values body-stxs
              (leftmost-identifier-in #'def.id/sig)
              reconstruct-definition)]
    [_ (values #f #f #f)]))

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
     (values body-stxs
             (leftmost-identifier-in #'id/sig)
             reconstruct-definition)]
    [_ (values #f #f #f)]))

(module+ test
  (require ruinit
           racket)
  (define-test (test-stx=? a b)
    (test-equal? (syntax->datum a) (syntax->datum b)))
  (define-test (test-selector selector
                              stx
                              body-stxs/expected
                              id/expected
                              test-reconstruct)
    (define-values {body-stxs id reconstruct}
      (selector stx))
    (and/test/message
     [(for/and/test ([part (in-list body-stxs)]
                     [part/expected (in-list body-stxs/expected)])
                    (test-stx=? part part/expected))
      @~a{Body stxs are different:}]
     [(test-equal? id id/expected)
      @~a{Mutated ids are different:}]
     [(test-reconstruct reconstruct)
      @~a{Reconstruction test failed:}]))
  (test-begin
    #:name select-define-body
    (test-selector select-define-body
                   #'(define x 5)
                   (list #'5)
                   'x
                   (λ (reconstruct)
                     (test-stx=? (reconstruct (list #'42))
                                 #'(define x 42))))
    (test-selector select-define-body
                   #'(define (f x) 5)
                   (list #'(begin 5))
                   'f
                   (λ (reconstruct)
                     (test-stx=? (reconstruct (list #'(begin 42)))
                                 #'(define (f x) 42))))))

(define (select-type-annotations+define-body stx)
  (syntax-parse stx
    #:datum-literals [define :]
    [({~and : colon} name:id . _)
     (values (list stx)
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
     (values body-stxs
             (format-symbol "~a:body"
                            (leftmost-identifier-in #'id/sig))
             reconstruct-definition)]
    [else (values #f #f #f)]))

(module+ test
  (test-begin
    #:name select-type-annotations+define-body
    (test-selector select-type-annotations+define-body
                   #'(: foo Any)
                   (list #'(: foo Any))
                   'foo:annotation
                   (λ (reconstruct)
                     (test-stx=? (reconstruct (list #'(: foo Something-Else)))
                                 #'(: foo Something-Else))))
    (test-selector select-type-annotations+define-body
                   #'(: foo : Any)
                   (list #'(: foo : Any))
                   'foo:annotation
                   (λ (reconstruct)
                     (test-stx=? (reconstruct (list #'(: foo : Something-Else)))
                                 #'(: foo : Something-Else))))
    (test-selector select-type-annotations+define-body
                   #'(define (f x)
                       (: y A)
                       (define y 32)
                       (: z B)
                       (define z 40)
                       (+ y z))
                   (list #'(: y A)
                         #'(define y 32)
                         #'(: z B)
                         #'(define z 40)
                         #'(+ y z))
                   'f:body
                   (λ (reconstruct)
                     (test-stx=? (reconstruct (list #'(: y A)
                                                    #'(define y 32)
                                                    #'(: z Any)
                                                    #'(define z 40)
                                                    #'(+ y z)))
                                 #'(define (f x)
                                     (: y A)
                                     (define y 32)
                                     (: z Any)
                                     (define z 40)
                                     (+ y z)))))
    (test-selector select-type-annotations+define-body
                   #'(define (f x) : Number
                       (: y A)
                       (define y 32)
                       (: z B)
                       (define z 40)
                       (+ y z))
                   (list #'(: gensym Number)
                         #'(: y A)
                         #'(define y 32)
                         #'(: z B)
                         #'(define z 40)
                         #'(+ y z))
                   'f:body
                   (λ (reconstruct)
                     (test-stx=? (reconstruct (list #'(: gensym Any)
                                                    #'(: y A)
                                                    #'(define y 32)
                                                    #'(: z B)
                                                    #'(define z 40)
                                                    #'(+ y z)))
                                 #'(define (f x) : Any
                                     (: y A)
                                     (define y 32)
                                     (: z B)
                                     (define z 40)
                                     (+ y z)))))
    (test-selector select-type-annotations+define-body
                   #'(define f
                       (λ (x)
                         (: y A)
                         x))
                   (list #'(λ (x)
                             (: y A)
                             x))
                   'f:body
                   (λ (reconstruct)
                     (test-stx=? (reconstruct (list #'(λ (x)
                                                        (: y Foobar)
                                                        x)))
                                 #'(define f
                                     (λ (x)
                                       (: y Foobar)
                                       x)))))))
