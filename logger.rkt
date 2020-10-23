#lang at-exp racket/base

(provide log-mutate-info
         log-mutate-debug
         log-mutation-type
         log-mutation
         mutate-logger)

(require racket/format)

(define-logger mutate)

(define (log-mutation-type type)
  (log-mutate-info "type: ~a" type))

(define (log-mutation before after)
  (log-message mutate-logger
               'info
               @~a{Mutating @before -> @after}
               (list before after)))
