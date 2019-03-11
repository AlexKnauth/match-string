#lang racket/base

(provide matcher
         matcher:
         ;---
         any/p
         var/p
         pred/p
         equal/p
         and/p
         ;---
         empty/p
         cons/p
         list*/p
         list/p
         string/p)

(require racket/list
         racket/match
         (only-in srfi/1 append-reverse)
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse))
(module+ test
  (require rackunit))

;; -----------------------------------------------

;; A [Matcher X (Y ...)] is a function:
;;   X -> [Maybe [List Y ...]]

(define-simple-macro (matcher pat:expr [id:id ...])
  (match-lambda [pat (list id ...)] [_ #false]))

(define-match-expander matcher:
  (syntax-parser
    [(_ mer:expr [id:id ...])
     #'(app mer (list id ...))]))

;; -----------------------------------------------

;; Generic patterns

;; any/p : [Matcher X ()]
(define (any/p x) '())

;; var/p : [Matcher X (X)]
(define (var/p x) (list x))

;; pred/p : [X -> Bool] -> [Matcher X ()]
(define ((pred/p x?) x)
  (and (x? x) '()))

;; equal/p : X -> [Matcher X ()]
(define ((equal/p x) y)
  (and (equal? x y) '()))

;; and/p : [Matcher X (Y ...)] ... -> [Matcher X (Y ... ...)]
(define ((and/p . ps) x)
  (let loop ([ps ps] [acc '()])
    (match ps
      ['() (reverse acc)]
      [(cons p ps)
       (define r (p x))
       (and r
            (loop ps (append-reverse r acc)))])))

;; -----------------------------------------------

;; Patterns for data structures, lists and strings

;; empty/p : [Matcher Any ()]
(define (empty/p x)
  (and (empty? x) '()))

;; cons/p :
;;   [Matcher X1 (Y1 ...)]
;;   [Matcher X2 (Y2 ...)]
;;   ->
;;   [Matcher [Cons X1 X2] (Y1 ... Y2 ...)]
(define ((cons/p p1 p2) x)
  (and
   (pair? x)
   (let ([r1 (p1 (car x))])
     (and
      r1
      (let ([r2 (p2 (cdr x))])
        (and
         r2
         (append r1 r2)))))))

;; list*/p :
;;   [Matcher X1 (Y1 ...)] ...
;;   [Matcher X2 (Y2 ...)]
;;   ->
;;   [Matcher [List* X1 ... X2] (Y1 ... ... Y2 ...)]
(define (list*/p p1 . p2s)
  (define-values [p1s lst-p2] (split-at-right (cons p1 p2s) 1))
  (define p2 (first lst-p2))
  (foldr cons/p p2 p1s))

;; list/p :
;;   [Matcher X (Y ...)] ...
;;   ->
;;   [Matcher [List X ...] (Y ... ...)]
(define ((list/p . ps) xs)
  (and
   (list? xs)
   (let loop ([ps ps] [xs xs] [acc '()])
     (match* [ps xs]
       [['() '()] (reverse acc)]
       [[(cons p ps) (cons x xs)]
        (define r (p x))
        (and r (loop ps xs (append-reverse r acc)))]
       [[_ _] #false]))))

;; string/p :
;;   [Matcher Char (Y ...)] ...
;;   ->
;;   [Matcher String (Y ... ...)]
(define (string/p . ps)
  (define np (length ps))
  ;; str/p : [Matcher String (Y ... ...)]
  (define (str/p x)
    (and
     (string? x)
     (= np (string-length x))
     (let loop ([ps ps] [i 0] [acc '()])
       (match ps
         ['() (reverse acc)]
         [(cons p ps)
          (define r (p (string-ref x i)))
          (and
           r
           (loop ps (add1 i) (append-reverse r acc)))]
         [_ #false]))))
  str/p)

;; -----------------------------------------------

(module+ test
  (check-equal? (empty/p '()) '())
  (check-equal? (empty/p 123) #false)
  (check-equal? ((cons/p any/p any/p) (cons 1 2)) '())
  (check-equal? ((cons/p any/p any/p) "the words") #false)
  (check-equal? ((cons/p var/p var/p) (cons 1 2)) (list 1 2))
  (check-equal? ((cons/p var/p (cons/p var/p var/p))
                 (list "a" "b" "c" 1 2 3))
                (list "a" "b" (list "c" 1 2 3)))
  (check-equal? ((list*/p var/p var/p var/p)
                 (list 1 2 3 "do" "re" "mi"))
                (list 1 2 (list 3 "do" "re" "mi")))
  
  (check-equal? ((list/p (equal/p 1) var/p (equal/p 3))
                 (list 1 'boo! 3))
                (list 'boo!))
  (check-equal? ((list/p (equal/p 1) var/p (equal/p 3))
                 (list 1 2 5))
                #false)
  (check-equal? ((string/p (equal/p #\a) var/p (equal/p #\c))
                 "abc")
                (list #\b))
  )

