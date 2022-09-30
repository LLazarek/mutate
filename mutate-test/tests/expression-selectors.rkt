#lang at-exp racket

(require ruinit
         mutate/private/expression-selectors
         "testing-util.rkt")

(define-test (test-selector selector
                            stx
                            expected
                            [params-test (const #t)])
  (define result (selector stx))
  (match* {result expected}
    [{(list new-stx reconstructor params) (not #f)}
     (and/test (test-programs-equal? new-stx expected)
               (test-programs-equal? (reconstructor new-stx) stx)
               (params-test params))]
    [{(not #f) #f}
     (fail @~a{Selector matches with result: @result})]
    [{#f (not #f)}
     (fail @~a{Selector does not match when it should.})]
    [{#f #f} #t]))
(test-begin
  #:name select-exprs-as-if-untyped
  (test-selector select-exprs-as-if-untyped
                 #'x
                 #'x)
  (test-selector select-exprs-as-if-untyped
                 #'42
                 #'42)
  (test-selector select-exprs-as-if-untyped
                 #'()
                 #'())
  (test-selector select-exprs-as-if-untyped
                 #'(: a T)
                 #f)
  (test-selector select-exprs-as-if-untyped
                 #'(ann x T)
                 #'x)
  (test-selector select-exprs-as-if-untyped
                 #'(cast x T)
                 #'x)
  (test-selector select-exprs-as-if-untyped
                 #'(inst x T1 T2)
                 #'x)
  (test-selector select-exprs-as-if-untyped
                 #'(row-inst x T1 T2 T3)
                 #'x)
  (test-selector select-exprs-as-if-untyped
                 #'(f a b 42 c)
                 #'(f a b 42 c))
  (test-selector select-exprs-as-if-untyped
                 #'[a : Natural 42]
                 #'[a 42])
  (test-selector select-exprs-as-if-untyped
                 #'(λ ([x : T]) : R (+ 2 2))
                 #'(λ ([x : T]) (+ 2 2)))
  (test-selector select-exprs-as-if-untyped
                 #'(for : T ([v : Boolean (in-list bools)])
                        (displayln v))
                 #'(for ([v : Boolean (in-list bools)])
                     (displayln v)))
  (test-selector select-exprs-as-if-untyped
                 #'(define (f [x : T]) : R (+ x 2))
                 #'(define (f [x : T]) (+ x 2)))
  (test-selector select-exprs-as-if-untyped
                 #'(define (f [x : T]) (: y R) (+ x 2))
                 #'(define (f [x : T]) (+ x 2)))
  (test-selector select-exprs-as-if-untyped
                 #'(define x ':)
                 #'(define x ':))
  (test-selector select-exprs-as-if-untyped
                 #'':
                 #'':)
  (test-selector select-exprs-as-if-untyped
                 #'`:
                 #'`:)
  (test-selector select-exprs-as-if-untyped
                 #'(quote :)
                 #'(quote :))
  ;; Note that the simple handling of the above makes this sort of thing
  ;; happen. For now as long as we can reconstruct the sexp I'm going to say
  ;; it's fine.
  (test-selector select-exprs-as-if-untyped
                 #''(: a b c)
                 #'(quote)))

(test-begin
  #:name select-exprs-as-if-untyped/reconstructor
  (ignore (match-define (list selected reconstruct params)
            (select-exprs-as-if-untyped #'(+ 2 2))))
  (test-programs-equal? (reconstruct #'(- 2 2))
                        #'(- 2 2)))

(test-begin
  #:name select-exprs-as-if-untyped/reconstruct/add-remove-swap-exprs
  ;; swap
  (ignore (match-define (list selected reconstruct params)
            (select-exprs-as-if-untyped #'(begin x : T y (: x T2) z))))
  (test-programs-equal? (reconstruct #'(begin x y))
                        #'(begin x : T y (: x T2)))
  (test-programs-equal? (reconstruct #'(begin x z y))
                        #'(begin x : T z (: x T2) y))
  ;; add
  (ignore (match-define (list selected reconstruct params)
            (select-exprs-as-if-untyped #'(class parent
                                            (field a b c)
                                            (: f : Number -> Number)
                                            (define/public (f x) x)))))
  (test-programs-equal? (reconstruct
                         #'(class parent
                             (define/public (a-nonexistant-method x) x)
                             (field a b c)
                             (define/public (f x) x)))
                        #'(class parent
                            (define/public (a-nonexistant-method x) x)
                            (: f : Number -> Number)
                            (field a b c)
                            (define/public (f x) x)))
  ;; remove
  (ignore (match-define (list selected reconstruct params)
            (select-exprs-as-if-untyped #'(class parent
                                            (field a b c)
                                            (super-new)
                                            (: f : Number -> Number)
                                            (define/public (f x) x)))))
  (test-programs-equal? (reconstruct
                         #'(class parent
                             (field a b c)
                             (define/public (f x) x)))
                        #'(class parent
                            (field a b c)
                            (define/public (f x) x)
                            (: f : Number -> Number))))

(test-begin
  #:name select-exprs-as-if-untyped/random-testing
  (not (for/or ([i (in-range 1000)])
         (define random-stx-datum
           (contract-random-generate (listof symbol?) 1))
         (define stx
           (datum->syntax #f random-stx-datum))
         (define seems-untyped? (member ': random-stx-datum))
         (and seems-untyped?
              (test-fail? (test-selector select-exprs-as-if-untyped
                                         stx
                                         stx))
              random-stx-datum))))
