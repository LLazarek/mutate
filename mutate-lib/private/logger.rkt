#lang at-exp racket/base

(provide log-mutate-info
         log-mutate-debug
         log-mutation
         mutate-logger)

(require racket/format)

(define-logger mutate)

(define (log-mutation before after type)
  (log-message mutate-logger
               'info
               @~a{Mutating with @type : @~s[before] -> @~s[after]}
               (list type before after)))
