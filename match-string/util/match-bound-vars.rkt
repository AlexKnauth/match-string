#lang racket/base

(provide matcher/bound-vars
         (for-syntax pat/bound-vars))

(require racket/match
         syntax/parse/define
         "matcher.rkt"
         (for-syntax racket/base
                     racket/lazy-require
                     syntax/parse))

(begin-for-syntax
  (lazy-require
   [racket/match/parse (parse)]
   [racket/match/patterns (bound-vars pats->bound-vars)]))

(begin-for-syntax
  (define (pat->bound-vars pat)
    (pats->bound-vars parse (list pat)))

  (define-syntax-class pat/bound-vars
    #:attributes [pat [x 1]]
    [pattern pat:expr
      #:with [x ...] (pat->bound-vars #'pat)])
  )

(define-simple-macro (matcher/bound-vars pat:pat/bound-vars)
  (matcher pat [pat.x ...]))

