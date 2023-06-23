#lang racket/base

(require racket/function
         racket/list
         racket/stream)

(provide in-swaps)

(define (in-swaps l [swap-ok? (const #t)])
  (for*/stream ([i (in-range (length l))]
                [k (in-range (add1 i) (length l))]

                [I (in-value (list-ref l i))]
                [K (in-value (list-ref l k))]
                #:when (swap-ok? I K))
    (list-set (list-set l i K)
              k
              I)))

(module+ test
  (require ruinit)

  (test-begin
    #:name in-swaps
    (test-equal? (stream->list (in-swaps '()))
                 '[])
    (test-equal? (stream->list (in-swaps '(1)))
                 '[])
    (test-equal? (stream->list (in-swaps '(1 2)))
                 '[(2 1)])
    (test-equal? (stream->list (in-swaps '(1 2 3)))
                 '[(2 1 3)
                   (3 2 1)
                   (1 3 2)])
    (test-equal? (stream->list (in-swaps '(1 2 3 4)))
                 '[(2 1 3 4)
                   (3 2 1 4)
                   (4 2 3 1)
                   (1 3 2 4)
                   (1 4 3 2)
                   (1 2 4 3)])
    (test-equal? (stream->list (in-swaps '(1 2 3 4)
                                         (λ (a b)
                                           (not (or (= a 2)
                                                    (= b 2))))))
                 '[(3 2 1 4)
                   (4 2 3 1)
                   (1 2 4 3)])))
