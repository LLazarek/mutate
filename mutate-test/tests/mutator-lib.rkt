#lang at-exp racket

(require syntax/parse
         ruinit
         mutate/private/mutator-lib
         mutate/private/mutated
         "testing-util.rkt")

(test-begin
  #:name maybe-mutate
  ;; Valid mutation, but counter is not yet high enough
  (test-equal? (mmap syntax->datum (maybe-mutate #'a #'b 5 0))
               (mmap syntax->datum (mutated #'a 1)))
  ;; Valid mutation, counter is right
  (test-equal? (mmap syntax->datum (maybe-mutate #'a #'b 5 5))
               (mmap syntax->datum (mutated #'b 6)))
  ;; Valid mutation but it is syntactically identical
  ;; This can happen when swapping argument positions
  ;; e.g. for (foo '() '())
  (test-equal? (mmap syntax->datum (maybe-mutate #'('() '()) #'('() '())
                                                 5
                                                 5))
               (mmap syntax->datum (mutated #'('() '()) 5))))

(test-begin
  #:name id-mutator
  (ignore (define-id-mutator mutate-id
            #:type "test"
            [car #:->  cdr]
            [+   #:<-> -]

            [foo #:->  bar]
            [foo #:->  boo]))
  (test-mutator mutate-id #'something-else #'something-else)
  (test-mutator mutate-id #'car #'cdr)
  (test-mutator mutate-id #'cdr #'cdr)
  (test-mutator mutate-id #'+ #'-)
  (test-mutator mutate-id #'- #'+)

  (test-mutator* mutate-id #'foo (list #'bar #'boo)))

(test-begin
  #:name value-mutator
  (ignore (define-constant-mutator (mutate-value value)
            #:type "test"
            [(? number?) #:-> (- value)]
            [(? number?) #:-> 0]
            [(? integer?) #:-> (exact->inexact value)]
            [(? string?) #:-> #f]))
  (test-mutator* mutate-value #'5 (list #'-5 #'0 #'5.0))
  (test-mutator* mutate-value #'2.5 (list #'-2.5 #'0))
  (test-mutator* mutate-value #'"hi" (list #'#f))
  (test-mutator* mutate-value #'x (list #'x)))

(test-begin
  #:name simple-mutator
  (ignore (define-simple-mutator (if-swap stx)
            #:pattern ({~datum if} test t e)
            #'(if test e t)))
  (test-mutator if-swap #'(not an if) #'(not an if))
  (test-mutator if-swap #'(if #t 1 2) #'(if #t 2 1))

  (ignore (define-simple-mutator (permute-args stx)
            #:pattern ({~datum ->} arg ... result)
            (for/stream ([args (in-permutations (attribute arg))])
              #`(-> #,@args result))))
  (test-mutator* permute-args
                 #'(-> A B C D)
                 (append (map (λ (s)
                                #`(-> #,@(datum->syntax #f s) D))
                              (sequence->list (in-permutations '(A B C))))
                         (list #'(-> A B C D)))))

(test-begin
  #:name apply-mutators
  (ignore
   (define a-compound-mutator
     (compose-mutators
      (λ (x index [counter 0])
        (maybe-mutate x
                      #'surprise!
                      index
                      counter))
      (make-guarded-mutator
       (λ (x) #t)
       (λ (x) #'surprise-2!))
      (make-guarded-mutator
       (λ (x) #f)
       (λ (x) #'never-happens))
      (make-guarded-mutator
       (syntax-parser [(a b c) #t]
                      [else #f])
       (syntax-parser [(a b c) #'(a 42 c)])))))
  (test-mutator* a-compound-mutator
                 #'an-id
                 (list #'surprise!
                       #'surprise-2!
                       #'an-id))
  (test-mutator* a-compound-mutator
                 #'(+ x 3)
                 (list #'surprise!
                       #'surprise-2!
                       #'(+ 42 3)
                       #'(+ x 3)))
  (test-mutator (compose-mutators)
                #'(+ x 3)
                #'(+ x 3)))

(test-begin
  #:name make-stream-mutator
  (ignore
   (define (permutation-stream stx)
     (for/stream ([p (in-permutations (syntax->list stx))])
       (datum->syntax stx p)))
   (define mutate-from-permutations (make-stream-mutator permutation-stream)))
  (test-mutator*
   mutate-from-permutations
   #'(a b c)
   (map (λ (s) (datum->syntax #f s))
        (remove '(a b c) ;; maybe-mutate won't let this one through!
                (sequence->list (in-permutations '(a b c))))))
  (ignore
   (define mutate-from-empty
     (make-stream-mutator (λ _ empty-stream))))
  (test-mutator*
   mutate-from-empty
   #'(a b c)
   (list #'(a b c)))
  (ignore
   (define mutate-with-a-bigger-number
     (make-stream-mutator
      (λ (stx)
        (for/stream ([i (in-naturals 1)])
          (datum->syntax stx (+ i (syntax->datum stx))))))))
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
         #| ... |#)))

(test-begin
  #:name nesting-mutators
  (ignore
   (local-require mutate/low-level)
   (define-mutator (drop-case->-cases stx mutation-index counter)
     #:type "drop-case->-case"
     (define-mutator (drop-case stx mutation-index counter)
       #:type (current-mutator-type)
       (maybe-mutate stx
                     #'[]
                     mutation-index
                     counter))

     (syntax-parse stx
       [({~and arrow {~datum case->}}
         case ...)
        (mutated-do-single
         [mutated-cases (mutate-in-sequence (syntax->list #'([case] ...))
                                            mutation-index
                                            counter
                                            drop-case)]
         #:return
         (syntax-parse mutated-cases
           [({~or* [case*] []} ...)
            (syntax/loc stx
              (arrow {~? case*} ...))]))]
       [else
        (no-mutation stx mutation-index counter)])))
  (test-logged-mutation drop-case->-cases
                        #'(case-> a b c)
                        "drop-case->-case"))
