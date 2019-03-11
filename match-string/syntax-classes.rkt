#lang racket/base

(provide ooo
         pat
         pat-or-elipsis
         str-pat
         str-pat-or-elipsis
         )

(require syntax/parse
         racket/syntax
         "ooo.rkt"
         )

(define-syntax-class ooo
  #:description "elipsis"
  #:attributes (k norm)
  [pattern (~and ooo (~or (~literal ...) (~datum ...)))
           #:attr k 0
           #:with norm #'ooo]
  [pattern (~and ooo (~or (~literal ...+) (~datum ...+)))
           #:attr k 1
           #:with norm (datum->syntax #'ooo '..1 #'ooo #'ooo)]
  [pattern ooo
           #:attr k (ooo? (syntax->datum #'ooo))
           #:when (exact-nonnegative-integer? (attribute k))
           #:with norm (format-id #'ooo "..~a" (attribute k) #:source #'ooo #:props #'ooo)])

(define-syntax-class pat
  #:description "match pattern"
  [pattern (~and :expr (~not :ooo))])

(define-syntax-class pat-or-elipsis
  #:description "match pattern or elipsis"
  #:attributes (norm)
  [pattern ooo:ooo #:with norm #'ooo.norm]
  [pattern pat:pat #:with norm #'pat])

(define-syntax-class rx
  #:description "regexp literal"
  [pattern pat
           #:attr rx (syntax-e #'pat)
           #:when (or (regexp? (attribute rx))
                      (byte-regexp? (attribute rx)))])

(define-syntax-class str-pat
  #:description "string-append pattern"
  #:attributes (norm)
  [pattern rx:rx #:with norm #'(regexp rx)]
  [pattern pat:pat #:with norm #'pat])

(define-syntax-class str-pat-or-elipsis
  #:description "string-append pattern or elipsis"
  #:attributes (norm)
  [pattern ooo:ooo #:with norm #'ooo.norm]
  [pattern pat:str-pat #:with norm #'pat.norm]
  )



