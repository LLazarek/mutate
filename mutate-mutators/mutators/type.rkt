#lang at-exp racket/base

(require racket/contract)
(provide (contract-out
          [GTP-base-type->Any   mutator/c]
          [complex-type->Any    mutator/c]
          [function-arg-swap    mutator/c]
          [function-result-swap mutator/c]
          [struct-field-swap    mutator/c]
          [class-field-swap     mutator/c]))

(require mutate/define
         mutate/low-level
         racket/class
         racket/stream
         racket/format
         syntax/parse
         (for-syntax racket/base)
         syntax/parse/define
         "private/common.rkt")

(define type:base-type-substitution "base-type->Any")
(define-id-mutator GTP-base-type->Any
  #:type type:base-type-substitution
  [Real #:-> Any]
  [Integer #:-> Any]
  [Natural #:-> Any]
  [Nonnegative-Integer #:-> Any]
  [Positive-Integer #:-> Any]
  [Index #:-> Any]
  [Exact-Rational #:-> Any]
  [Float #:-> Any]

  [Symbol #:-> Any]
  [String #:-> Any]
  [Boolean #:-> Any]
  )

(define known-special-form?
  (syntax-parser
    [({~or {~datum struct} {~datum struct:}} _ ...) #t]
    [([field:id {~datum :} type ...] ...) #t]
    [({~or {~datum field} {~datum init-field}} _ ...) #t]
    [[field-name:id {~datum :} type ...] #t]
    [[method-name:id ({~or {~datum ->} {~datum ->*}} _ ...)] #t]
    [({~or {~datum values} {~datum Values}} _ ...) #t]
    [else #f]))
(define type:complex-type->Any "complex-type->Any")
(define-simple-mutator (complex-type->Any stx)
  #:type type:complex-type->Any
  #:pattern (_ ...)
  #:when (not (known-special-form? stx))
  #'Any)

(define-simple-macro (define-swapping-mutator name type
                       pattern
                       attribute-to-swap #:-> swapped-attribute-name
                       {~optional {~seq #:guard guard-f}}
                       re-pattern)
  (define name
    (make-stream-mutator
     #:type type
     (syntax-parser
       [pattern
        (for/stream ([rearranged-stxs (in-swaps (attribute attribute-to-swap)
                                                {~? guard-f})])
          (syntax-parse rearranged-stxs
            [[swapped-attribute-name (... ...)]
             (quasisyntax/loc this-syntax
               re-pattern)]))]
       [else empty-stream]))))


(define type:function-arg-swap "function-arg-swap")
(define-swapping-mutator ->-arg-swap type:function-arg-swap
  ({~and {~datum ->} head} e ... range)
  e #:-> new-e
  (head new-e ... range))
(define-swapping-mutator ->*-mandatory-arg-swap type:function-arg-swap
  ({~and {~datum ->*} head} (e ...) . rest)
  e #:-> new-e
  (head (new-e ...) . rest))
(define-swapping-mutator ->*-optional-arg-swap type:function-arg-swap
  ({~and {~datum ->*} head} mandatory (e ...) . rest)
  e #:-> new-e
  #:guard (λ left+right
            (not (ormap (compose1 keyword? syntax->datum) left+right)))
  (head mandatory (new-e ...) . rest))
(define function-arg-swap (mutator (compose-mutators ->-arg-swap
                                                     ->*-mandatory-arg-swap
                                                     ->*-optional-arg-swap)
                                   type:function-arg-swap))


(define type:function-result-swap "function-result-swap")
(define-swapping-mutator function-result-swap type:function-result-swap
  ({~and head {~or {~datum ->} {~datum ->*}}} arg ... ({~and {~datum values} values} e ...))
  e #:-> new-e
  (head arg ... (values new-e ...)))

(define type:struct-field-swap "struct-field-swap")
(define-swapping-mutator struct-field-swap type:struct-field-swap
  ({~and {~or* {~datum struct} {~datum struct:}} head}
      {~and name* {~or name:id (name:id parent:id)}}
      ([field-name:id {~datum :} field-t] ...)
      extras ...)
  field-t #:-> new-field-t
  (head name* ([field-name : new-field-t] ...) extras ...))

(define type:class-field-swap "class-field-swap")
(define-swapping-mutator class-field-swap type:class-field-swap
  ({~and {~or {~datum init-field}
              {~datum field}}
         field}
   [field-id:id field-type:expr]
   ...)
  field-type #:-> new-field-type
  (field [field-id new-field-type]
         ...))



(module+ test
  (require ruinit
           mutate/tests/testing-util)
  (test-begin
    #:name class-field-swap
    (for/and/test
     ([field-name (in-list (list #'field #'init-field))])
     (extend-test-message
      (test-mutator* class-field-swap
                     #`(#,field-name
                        [a T1]
                        [b T2]
                        [c T3])
                     (list #`(#,field-name
                              [a T2]
                              [b T1]
                              [c T3])
                           #`(#,field-name
                              [a T3]
                              [b T2]
                              [c T1])
                           #`(#,field-name
                              [a T1]
                              [b T3]
                              [c T2])
                           #`(#,field-name
                              [a T1]
                              [b T2]
                              [c T3])))
      @~a{Field: @field-name}))))

(module+ test
  (test-begin
    #:name function-arg-swap
    (test-mutator* function-arg-swap
                   #'(-> A B C D)
                   (list #'(-> B A C D)
                         #'(-> C B A D)
                         #'(-> A C B D)

                         #'(-> A B C D)
                         ))
    (test-mutator* function-arg-swap
                   #'(->* (A B C) D)
                   (list #'(->* (B A C) D)
                         #'(->* (C B A) D)
                         #'(->* (A C B) D)

                         #'(->* (A B C) D)
                         ))
    (test-mutator* function-arg-swap
                   #'(->* () (A B C) D)
                   (list #'(->* () (B A C) D)
                         #'(->* () (C B A) D)
                         #'(->* () (A C B) D)

                         #'(->* () (A B C) D)
                         ))
    (test-mutator* function-arg-swap
                   #'(->* (A B) (A B C) D)
                   (list #'(->* (B A) (A B C) D)
                         #'(->* (A B) (B A C) D)
                         #'(->* (A B) (C B A) D)
                         #'(->* (A B) (A C B) D)

                         #'(->* (A B) (A B C) D)
                         ))
    (test-mutator* function-arg-swap
                   #'(->* (A B) (A B #:c C) D)
                   (list #'(->* (B A) {A B #:c C} D)
                         #'(->* (A B) (B A #:c C) D)
                         #'(->* (A B) (C B #:c A) D)
                         #'(->* (A B) (A C #:c B) D)

                         #'(->* (A B) (A B #:c C) D)
                         )))
  (test-begin
    #:name function-result-swap
    (test-mutator* function-result-swap
                   #'(-> A B C D)
                   (list #'(-> A B C D)))
    (test-mutator* function-result-swap
                   #'(-> X (values A B C D))
                   (list #'(-> X (values B A C D))
                         #'(-> X (values C B A D))
                         #'(-> X (values D B C A))
                         #'(-> X (values A C B D))
                         #'(-> X (values A D C B))
                         #'(-> X (values A B D C))

                         #'(-> X (values A B C D))
                         )))

  (test-begin
    #:name struct-field-swap
    (test-mutator* struct-field-swap
                   #'(struct: foo ([x : Number]
                                   [y : String]
                                   [z : Bar])
                       #:prefab
                       #:type-name Foo)
                   (list #'(struct: foo ([x : String]
                                         [y : Number]
                                         [z : Bar])
                             #:prefab
                             #:type-name Foo)
                         #'(struct: foo ([x : Bar]
                                         [y : String]
                                         [z : Number])
                             #:prefab
                             #:type-name Foo)
                         #'(struct: foo ([x : Number]
                                         [y : Bar]
                                         [z : String])
                             #:prefab
                             #:type-name Foo)
                         #'(struct: foo ([x : Number]
                                         [y : String]
                                         [z : Bar])
                             #:prefab
                             #:type-name Foo))))

  (test-begin
    #:name complex-type->Any
    (test-mutator* complex-type->Any
                   #'(-> A B C)
                   (list #'Any
                         #'(-> A B C)))
    (test-mutator* complex-type->Any
                   #'(Listof String)
                   (list #'Any
                         #'(Listof String)))
    (test-mutator* complex-type->Any
                   #'Natural
                   (list #'Natural))
    (test-mutator* complex-type->Any
                   #'(struct stream ([head : Number]
                                     [tail : stream]))
                   (list #'(struct stream ([head : Number]
                                           [tail : stream]))))))
