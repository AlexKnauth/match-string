#lang racket/base

(provide seq-matcher->matcher
         ;---
         fail/sp
         empty/sp
         any/sp
         var/sp
         ;---
         list/sp
         listof/sp
         ;---
         append/sp
         )

(require racket/list
         racket/match
         (only-in srfi/1 append-reverse))
(module+ test
  (require rackunit
           "matcher.rkt"))

;; -----------------------------------------------

;; A [SeqMatcher X (Y ...)] can do one of:
;;  - fail outright, represented by an empty list
;;  - continue with a set of options:
;;     - succeed completely, consuming some amount of
;;       input
;;     - partially succeed, consuming some amount of
;;       input but still needing some pattern to pass
;;       on the rest
;; Where if "succeed completely" cases exist, they are
;; at the front.

;; A [SeqMatcher X (Y ...)] is a function:
;;   [Listof X]
;;   ->
;;   [Listof [SeqMatchContinue X (Y ...)]]

;; A [SeqMatchContinue X (Y ...)] is one of:
;;  - (complete [List Y ...] [Listof X])
;;  - (partial [SeqMatcher X (Y ...)] [Listof X])
(struct complete [values rest] #:transparent)
(struct partial [rest-matcher rest] #:transparent)

;; -----------------------------------------------

;; smc-done? : [SeqMatchContinue X (Y ...)] -> Bool
(define (smc-done? smc)
  (and (complete? smc) (empty? (complete-rest smc))))

;; seq-matcher->matcher :
;;   [SeqMatcher X (Y ...)]
;;   ->
;;   [Matcher [Listof X] (Y ...)]
(define ((seq-matcher->matcher seq/sp) xs)
  (let loop ([seq/sp seq/sp] [xs xs])
    (define cs (seq/sp xs))
    (cond
      [(empty? cs) #false]
      [else
       (or
        (for/first ([c (in-list cs)]
                    #:when (smc-done? c))
          (complete-values c))
        (for/or ([c (in-list cs)]
                 #:when (partial? c))
          (loop (partial-rest-matcher c) (partial-rest c))))])))

;; -----------------------------------------------

;; fail/sp : [SeqMatcher X ()]
(define (fail/sp xs) '())

;; empty/sp/bind : [List Y ...] -> [SeqMatcher X (Y ...)]
(define ((empty/sp/bind ys) xs) (list (complete ys xs)))

;; empty/sp : [SeqMatcher X ()]
(define (empty/sp xs) (list (complete '() xs)))


;; any/sp : [SeqMatcher X ()]
(define (any/sp xs)
  (cond
    [(empty? xs) (list (complete '() xs))]
    [else (list (complete '() xs)
                (partial any/sp (rest xs)))]))

;; var/sp/acc : [Listof X] -> [SeqMatcher X ([Listof X])]
(define ((var/sp/acc acc) xs)
  (cond
    [(empty? xs) (list (complete (list (reverse acc)) xs))]
    [else
     (list (complete (list (reverse acc)) xs)
           (partial (var/sp/acc (cons (first xs) acc))
                    (rest xs)))]))

;; var/sp : [SeqMatcher X ([Listof X])]
(define var/sp (var/sp/acc '()))

;; -----------------------------------------------

;; list/sp :
;;   [Matcher X (Y ...)] ...
;;   ->
;;   [SeqMatcher X (Y ... ...)]
(define ((list/sp . ps) xs)
  (let loop ([ps ps] [xs xs] [acc '()])
    (match* [ps xs]
      [['() xs] (list (complete (reverse acc) xs))]
      [[(cons p ps) (cons x xs)]
       (define r (p x))
       (cond
         [r (loop ps xs (append-reverse r acc))]
         [else '()])]
      [[_ _] '()])))

;; listof/sp/acc :
;;   [Listof [List Y ...]]
;;   Natural
;;   [Matcher X (Y ...)]
;;   ->
;;   [SeqMatcher X ([Listof Y] ...)]
(define ((listof/sp/acc acc n elem/p) xs)
  (define loloy
    (cond [(empty? acc) (make-list n '())]
          [else (apply map list (reverse acc))]))
  (cond
    [(empty? xs)
     (list (complete loloy xs))]
    [else
     (define r (elem/p (first xs)))
     (cond
       [r
        (list (complete loloy xs)
              (partial (listof/sp/acc (cons r acc) n elem/p)
                       (rest xs)))]
       [else
        (list (complete loloy xs))])]))

;; listof/sp :
;;   Natural
;;   [Matcher X (Y ...)]
;;   ->
;;   [SeqMatcher X ([Listof Y] ...)]
(define (listof/sp n elem/p)
  (listof/sp/acc '() n elem/p))

;; -----------------------------------------------

;; append/sp :
;;   [SeqMatcher X (Y ...)] ...
;;   ->
;;   [SeqMatcher X (Y ... ...)]
(define ((append/sp . ps) xs)
  (let loop ([ps ps] [xs xs] [acc '()])
    (match ps
      ['() (list (complete (reverse acc) xs))]
      [(cons p ps-rst)
       (define rs (p xs))
       (append*
        (for/list ([r (in-list rs)])
          (match r
            [(complete vs xs)
             (loop ps-rst xs (append-reverse vs acc))]
            [(partial p* xs)
             (loop (cons p* ps-rst) xs acc)])))])))

;; repeat/sp/acc :
;;   [Listof [List Y ...]]
;;   Natural
;;   [SeqMatcher X (Y ...)]
;;   [SeqMatcher X (Y ...)]
;;   ->
;;   [SeqMatcher X ([Listof Y] ...)]
(define ((repeat/sp/acc acc n p1 p2) xs)
  (define loloy
    (cond [(empty? acc) (make-list n '())]
          [else (apply map list (reverse acc))]))
  (cond
    [(empty? xs)
     (list (complete loloy xs))]
    [else
     (define rs (p1 xs))
     (append*
      (for/list ([r (in-list rs)])
        (match r
          [(complete vs xs*)
           (when (eqv? xs xs*)
             (error "ellipsis pattern matched empty sequence"))
           (list (complete loloy xs)
                 (partial (repeat/sp/acc (cons vs acc) n p2 p2)
                          xs*))]
          [(partial p1* xs*)
           (when (eqv? xs xs*)
             (error "ellipsis pattern matched empty sequence"))
           (list (partial (repeat/sp/acc acc n p1* p2)
                          xs*))])))]))

;; repeat/sp :
;;   Natural
;;   [SeqMatcher X (Y ...)]
;;   ->
;;   [SeqMatcher X ([Listof Y] ...)]
(define (repeat/sp n p)
  (repeat/sp/acc '() n p p))

;; -----------------------------------------------

(module+ test
  (define-check (check-seq-match val sp rs)
    (check-equal? ((seq-matcher->matcher sp) val) rs))

  (check-seq-match (list 1 2 3) var/sp (list (list 1 2 3)))
  (check-seq-match (list 1 2 3) (list/sp var/p (equal/p 2) var/p) (list 1 3))
  (check-seq-match (list 1 2 3) (list/sp var/p (equal/p 2)) #false)
  (check-seq-match (list 1 2 3) (append/sp var/sp) (list (list 1 2 3)))
  (check-seq-match (list 1 2 3)
                   (append/sp (list/sp var/p) var/sp)
                   (list 1 (list 2 3)))
  (check-seq-match (list 1 2 3)
                   (append/sp var/sp (list/sp var/p))
                   (list (list 1 2) 3))
  (check-seq-match (list 1 2 3 4)
                   (append/sp (list/sp var/p) var/sp (list/sp var/p))
                   (list 1 (list 2 3) 4))
  (check-seq-match (list 1 2 3 "do" "re" "mi")
                   (append/sp (list/sp var/p (equal/p 2) var/p)
                              var/sp)
                   (list 1 3 (list "do" "re" "mi")))
  (check-seq-match (list 1 2 3 "do" "re" "mi")
                   (append/sp var/sp
                              (list/sp var/p (equal/p "re") var/p))
                   (list (list 1 2 3) "do" "mi"))
  (check-seq-match (list "a" 1 "do" "b" 2 "re" "c" 3 "mi")
                   (repeat/sp 3 (list/sp var/p var/p var/p))
                   (list (list "a" "b" "c") (list 1 2 3) (list "do" "re" "mi")))
  (check-seq-match (list ":" 1 2 ":" 3 ":" ":" 4 5 6)
                   (repeat/sp 1
                              (append/sp
                               (list/sp (equal/p ":"))
                               (listof/sp 1 (and/p (pred/p number?) var/p))))
                   (list (list '(1 2) '(3) '() '(4 5 6))))
  )

;; -----------------------------------------------
