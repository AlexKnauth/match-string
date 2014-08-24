#lang racket/base

(provide list/@)

(require racket/match
         match-string
         (for-syntax racket/base
                     syntax/parse
                     "../match-string/syntax-classes.rkt"
                     ))

(module+ test
  (require rackunit))

(begin-for-syntax
  (define-syntax-class splice
    #:literals (quasiquote unquote-splicing)
    [pattern ,@spliced-expr:expr]
    [pattern `,@spliced-expr:expr])
  (define-syntax-class rst
    [pattern (~and :expr (~not (_ . _)))])
  
  (define (parse-list/@-match-expander stx)
    (syntax-parse stx
      [(list/@)
       #''()]
      [(list/@ s:splice)
       #'s.spliced-expr]
      [(list/@ . rst:rst)
       #'rst]
      [(list/@ pat:pat)
       #'(list pat)]
      [(list/@ s:splice ooo:ooo pat:pat-or-elipsis ...)
       #'(append s.spliced-expr ooo.norm (list/@ pat.norm ...))]
      [(list/@ pat0:pat ooo:ooo pat:pat-or-elipsis ...)
       #'(append (list pat0 ooo.norm) (list/@ pat.norm ...))]
      [(list/@ s:splice pat1:pat pat:pat-or-elipsis ...)
       #'(append s.spliced-expr (list/@ pat1 pat.norm ...))]
      [(list/@ pat0:pat pat1:pat pat:pat-or-elipsis ...)
       #'(cons pat0 (list/@ pat1 pat.norm ...))]
      [(list/@ s:splice ooo:ooo pat:pat-or-elipsis ... . rst:rst)
       #'(append s.spliced-expr ooo.norm (list/@ pat.norm ... . rst))]
      [(list/@ pat0:pat ooo:ooo pat:pat-or-elipsis ... . rst:rst)
       #'(list-rest pat0 ooo.norm (list/@ pat.norm ... . rst))]
      [(list/@ s:splice pat1:pat pat:pat-or-elipsis ... . rst:rst)
       #'(append s.spliced-expr (list/@ pat1 pat.norm ... . rst))]
      [(list/@ pat0:pat pat1:pat pat:pat-or-elipsis ... . rst:rst)
       #'(cons pat0 (list/@ pat1 pat.norm ... . rst))]))
  
  (define (parse-list/@-macro stx)
    (syntax-parse stx
      [(list/@)
       #''()]
      [(list/@ s:splice)
       #'s.spliced-expr]
      [(list/@ . rst:rst)
       #'rst]
      [(list/@ s:splice expr:expr ...)
       #'(append s.spliced-expr (list/@ expr ...))]
      [(list/@ expr0:expr expr:expr ...)
       #'(cons expr0 (list/@ expr ...))]
      [(list/@ s:splice expr:expr ... . rst:rst)
       #'(append s.spliced-expr (list/@ expr ... . rst))]
      [(list/@ expr0:expr expr:expr ... . rst:rst)
       #'(cons expr0 (list/@ expr ... . rst))]
      [(~literal list/@)
       #'list])))

(define-match-expander list/@
  parse-list/@-match-expander
  parse-list/@-macro)

(module+ test
  (check-equal? (list/@) '())
  (check-equal? (list/@ 1) '(1))
  (check-equal? (list/@ 1 2 3) '(1 2 3))
  (check-equal? (list/@ ,@1) 1)
  (check-equal? (list/@ ,@'()) '())
  (check-equal? (list/@ 1 ,@2) '(1 . 2))
  (check-equal? (list/@ 1 . 2) '(1 . 2))
  (check-match '() (list/@))
  (check-match '(1) (list/@ 1))
  (check-match '(1 2 3) (list/@ 1 2 3))
  (check-match 1 (list/@ ,@1))
  (check-match '() (list/@ ,@'()))
  (check-match '(1 . 2) (list/@ 1 ,@2))
  ;(check-match '(1 . 2) (list/@ 1 . 2))
  (check-match '(1 2 3 4 5 6) (list/@ 1 2 3 ,@p) (equal? p '(4 5 6)))
  (check-match '(1 2 3 . 4) (list/@ 1 2 ,@p) (equal? p '(3 . 4)))
  (check-match '(1 2 3 4 5 6) (not (list/@ 1 2 2.5 3 ,@p)))
  (check-match '(1 2 3 4 5 . 6) (list/@ 1 ,@p1 4 ,@p2) (and (equal? p1 '(2 3)) (equal? p2 '(5 . 6))))
  )
