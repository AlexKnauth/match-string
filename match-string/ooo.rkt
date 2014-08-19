#lang racket/base

(provide ooo? ooo-k)

(require racket/match)

(define (ooo? sym)
  (match sym
    ['... 0]
    ['...+ 1]
    [(? symbol? (app symbol->string str))
     #:when (and (< 2 (string-length str))
                 (equal? ".." (substring str 0 2)))
     (define k (string->number (substring str 2)))
     (cond [(exact-nonnegative-integer? k) k]
           [else #f])]
    [_ #f]))

(define (ooo-k sym)
  (or (ooo? sym) (error 'ooo-k "expects ooo?, given: ~v" sym)))

