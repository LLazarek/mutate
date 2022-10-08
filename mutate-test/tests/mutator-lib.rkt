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
  #:name apply-mutators
  (ignore
   (define a-compound-mutator
     (compose-mutators
      (λ (x index counter)
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
                       #'(+ x 3))))

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
   (list #'(c b a)
         #'(b c a)
         #'(c a b)
         #'(a c b)
         #'(b a c)
         #'(a b c)
         #'(a b c)))
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