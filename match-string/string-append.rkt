#lang racket/base

(provide (rename-out
          [new-string string]
          [new-string-append string-append]
          ))

(require racket/match
         (prefix-in rkt: racket/base)
         "util/match-bound-vars.rkt"
         "util/matcher.rkt"
         "util/string-seq-matcher.rkt"
         (for-syntax racket/base
                     syntax/parse
                     syntax/transformer
                     "syntax-classes.rkt"))

;; ---------------------------------------------------------

(begin-for-syntax
  (define (make-var-like-transformer id)
    (set!-transformer-procedure (make-variable-like-transformer id)))

  (define-syntax-class string
    [pattern {~or {~literal new-string} {~literal rkt:string} {~datum string}}])

  (define-syntax-class string-append
    [pattern {~or {~literal new-string-append} {~literal rkt:string-append} {~datum string-append}}])

  (define-syntax-class blank
    [pattern {~or {~literal _} {~literal rkt:_} {~datum _}}])

  (define-syntax-class quot
    [pattern {~or {~literal quote} {~literal rkt:quote} {~datum quote}}])

  (define-syntax-class aand
    [pattern {~or {~literal and} {~literal rkt:and} {~datum and}}])
  )

;; ---------------------------------------------------------

(begin-for-syntax
  (define-syntax-class seq-pat
    #:attributes [seq-matcher [out 1] match-pat]
    [pattern :string-pat]
    [pattern :string-append-pat]
    [pattern :any-seq-pat]
    [pattern :var-seq-pat]
    [pattern :quote-seq-pat]
    [pattern :and-seq-pat]
    [pattern s:str
      #:with match-pat #'s
      #:with seq-matcher #'(string=/sp 's)
      #:with [out ...] '()]
    [pattern rx
      #:when (regexp? (syntax-e #'rx))
      #:with seq-matcher #'(regexp/sp 'rx)
      #:with [out ...] '()
      #:with match-pat #'(string-seq-matcher: seq-matcher [out ...])]
    [pattern {~and {~not {~or :ooo :blank}} p:pat/bound-vars}
      #:with match-pat (attribute p.pat)
      #:with [out ...] #'[p.pat]
      #:with seq-matcher #'var/sp]
    )

  (define-syntax-class elem-pat
    #:attributes [matcher [out 1] match-pat]
    [pattern {~and {~not :ooo} p:pat/bound-vars}
      #:with match-pat (attribute p.pat)
      #:with [out ...] (attribute p.x)
      #:with matcher #'(matcher p.pat [p.x ...])])

  (define-splicing-syntax-class repeat-elem-pat
    #:attributes [seq-matcher [out 1] [match-pat 1]]
    [pattern {~seq a:elem-pat {~peek-not :ooo}}
      #:with [match-pat ...] #'[a.match-pat]
      #:with [out ...] (attribute a.out)
      #:with seq-matcher #'(string/sp a.matcher)]
    [pattern {~seq a:elem-pat {~and {~literal ...} ooo}}
      #:with [match-pat ...] #'[a.match-pat ooo]
      #:with [out ...] (attribute a.out)
      #:with n (length (attribute a.out))
      #:with seq-matcher #'(repeat/sp 'n (string/sp a.matcher))]
    [pattern {~seq a:elem-pat ooo:ooo}
      #:with [match-pat ...] #'[a.match-pat ooo.norm]
      #:with [out ...] (attribute a.out)
      #:with k (attribute ooo.k)
      #:with n (length (attribute a.out))
      #:with seq-matcher #'(repeat-at-least/sp 'k 'n (string/sp a.matcher))])

  (define-splicing-syntax-class repeat-seq-pat
    #:attributes [seq-matcher [out 1]]
    [pattern {~seq :seq-pat {~peek-not :ooo}}]
    [pattern {~seq a:seq-pat {~and {~literal ...} ooo}}
      #:with [out ...] (attribute a.out)
      #:with n (length (attribute a.out))
      #:with seq-matcher #'(repeat/sp 'n a.seq-matcher)]
    [pattern {~seq a:seq-pat ooo:ooo}
      #:with [out ...] (attribute a.out)
      #:with k (attribute ooo.k)
      #:with n (length (attribute a.out))
      #:with seq-matcher #'(repeat-at-least/sp 'k 'n a.seq-matcher)])

  (define-syntax-class and-seq-pat
    #:attributes [seq-matcher [out 1] match-pat]
    [pattern (:aand)
      #:with match-pat #'(and)
      #:with seq-matcher #'var/sp
      #:with [out ...] '()]
    [pattern (:aand :seq-pat)]
    [pattern (:aand {~and x:id {~not {~or :ooo :blank}}} p:seq-pat)
      #:with match-pat #'(and x p.match-pat)
      #:with seq-matcher #'(var-and/sp p.seq-matcher)
      #:with [out ...] #'[x p.out ...]]
    [pattern (:aand p:seq-pat {~and x:id {~not {~or :ooo :blank}}})
      #:with match-pat #'(and p.match-pat x)
      #:with seq-matcher #'(and-var/sp p.seq-matcher)
      #:with [out ...] #'[p.out ... x]]
    [pattern (:aand p:seq-pat ...)
      #:with match-pat #'(and p.match-pat ...)
      #:with seq-matcher #'(and/sp p.seq-matcher ...)
      #:with [out ...] #'[p.out ... ...]])

  ;; -------------------------------------------------------

  (define-syntax-class any-seq-pat
    #:attributes [seq-matcher [out 1] match-pat]
    [pattern :blank
      #:with match-pat #'_
      #:with seq-matcher #'any/sp
      #:with [out ...] '()])

  (define-syntax-class var-seq-pat
    #:attributes [seq-matcher [out 1] match-pat]
    [pattern {~and {~not {~or :ooo :blank}} x:id match-pat}
      #:with seq-matcher #'var/sp
      #:with [out ...] #'[x]])

  (define-syntax-class quote-seq-pat
    #:attributes [seq-matcher [out 1] match-pat]
    [pattern (:quot datum:str)
      #:with match-pat #'(quote datum)
      #:with seq-matcher #'(string=/sp 'datum)
      #:with [out ...] '()])

  (define-syntax-class string-pat
    #:attributes [seq-matcher [out 1] match-pat]
    [pattern (:string e:elem-pat ...)
      #:with match-pat #'(? string? (app string->list (list e.match-pat ...)))
      #:with seq-matcher #'(string/sp e.matcher ...)
      #:with [out ...] #'[e.out ... ...]]
    [pattern (:string e:repeat-elem-pat ...)
      #:with match-pat #'(? string? (app string->list (list e.match-pat ... ...)))
      #:with seq-matcher #'(append/sp e.seq-matcher ...)
      #:with [out ...] #'[e.out ... ...]])

  (define-syntax-class string-append-pat
    #:attributes [seq-matcher [out 1] match-pat]
    [pattern (:string-append a:repeat-seq-pat ...)
      #:with seq-matcher #'(append/sp a.seq-matcher ...)
      #:with [out ...] #'[a.out ... ...]
      #:with match-pat #'(string-seq-matcher: seq-matcher [out ...])])
  )

;; ---------------------------------------------------------

(define-match-expander new-string
  (λ (stx) (syntax-parse stx [p:string-pat #'p.match-pat]))
  (make-var-like-transformer #'rkt:string))

(define-match-expander new-string-append
  (λ (stx) (syntax-parse stx [p:string-append-pat #'p.match-pat]))
  (make-var-like-transformer #'rkt:string-append))

;; ---------------------------------------------------------
