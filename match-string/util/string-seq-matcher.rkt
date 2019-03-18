#lang racket/base

(provide string-seq-matcher->matcher
         string-seq-matcher:
         ;---
         fail/sp
         empty/sp
         any/sp
         var/sp
         and-var/sp
         var-and/sp
         and/sp
         ;---
         string=/sp
         regexp/sp
         string/sp
         ;---
         cons/sp
         append/sp
         repeat/sp
         repeat-at-least/sp
         )

(require racket/list
         racket/match
         (only-in srfi/1 append-reverse)
         (for-syntax racket/base
                     syntax/parse))
(module+ test
  (require rackunit
           "matcher.rkt"))

;; -----------------------------------------------

;; A [StrSeqMatcher (Y ...)] can do one of:
;;  - fail outright, represented by an empty list
;;  - continue with a set of options:
;;     - succeed completely, consuming some amount of
;;       input
;;     - partially succeed, consuming some amount of
;;       input but still needing some pattern to pass
;;       on the rest
;; Where if "succeed completely" cases exist, they are
;; at the front.

;; A [StrSeqMatcher (Y ...)] is a function:
;;   String
;;   Natural
;;   Natural
;;   ->
;;   [Listof [StrSeqMatchContinue (Y ...)]]
;; Where the naturals are the starting and ending indexes
;; into the string.

;; A [StrSeqMatchContinue (Y ...)] is one of:
;;  - (complete [List Y ...] Natural)
;;  - (partial [StrSeqMatcher (Y ...)] Natural)
(struct complete [values rest] #:transparent)
(struct partial [rest-matcher rest] #:transparent)
;; where the `rest` nats are ending indexes into the string.

(define (complete-rests-all-equal? rs)
  (or (empty? rs)
      (let ([r0 (first rs)]
            [r1s (rest rs)])
        (and (complete? r0)
             (let ([r0r (complete-rest r0)])
               (for/and ([r1 (in-list r1s)])
                 (and (complete? r1)
                      (= r0r (complete-rest r1)))))))))

;; -----------------------------------------------

;; smc-done? : [SeqMatchContinue (Y ...)] -> Bool
(define (smc-done? smc n)
  (and (complete? smc) (= n (complete-rest smc))))

;; string-seq-match-exact-substring :
;;   [StrSeqMatcher (Y ...)]
;;   String
;;   Natural
;;   Natural
;;   ->
;;   [Maybe [List Y ...]]
(define (string-seq-match-exact-substring seq/sp s i j)
  (let loop ([seq/sp seq/sp] [i i])
    (define cs (seq/sp s i j))
    (cond
      [(empty? cs) #false]
      [else
       (or
        (for/first ([c (in-list cs)]
                    #:when (smc-done? c j))
          (complete-values c))
        (for/or ([c (in-list cs)]
                 #:when (and (partial? c)
                             (<= (partial-rest c) j)))
          (loop (partial-rest-matcher c) (partial-rest c))))])))

;; string-seq-matcher->matcher :
;;   [StrSeqMatcher (Y ...)]
;;   ->
;;   [Matcher String (Y ...)]
(define ((string-seq-matcher->matcher seq/sp) s)
  (define i 0)
  (define n (string-length s))
  (string-seq-match-exact-substring seq/sp s i n))

;; string-seq-matcher->possibilities :
;;   [StrSeqMatcher (Y ...)]
;;   ->
;;   [String -> [Listof [List Y ...]]
(define ((string-seq-matcher->possibilities seq/sp) s)
  (define i 0)
  (define n (string-length s))
  (let loop ([seq/sp seq/sp] [i i])
    (define cs (seq/sp s i n))
    (cond
      [(empty? cs) '()]
      [else
       (append*
        (for/list ([c (in-list cs)]
                    #:when (smc-done? c n))
          (complete-values c))
        (for/list ([c (in-list cs)]
                   #:when (and (partial? c)
                               (<= (partial-rest c) n)))
          (loop (partial-rest-matcher c) (partial-rest c))))])))

;; (string-seq-matcher: smer [pat ...])
;;   smer : [StrSeqMatcher (Y ...)]
(define-match-expander string-seq-matcher:
  (syntax-parser
    [(_ smer:expr [pat:expr ...])
     #:with ooo (quote-syntax ...)
     #'(app (string-seq-matcher->possibilities smer)
            (list-rest _ ooo (list pat ...) _))]))

;; -----------------------------------------------

;; fail/sp : [StrSeqMatcher ()]
(define (fail/sp s i n) '())

;; empty/sp : [StrSeqMatcher ()]
(define (empty/sp s i n) (list (complete '() i)))


;; any/sp : [StrSeqMatcher ()]
(define (any/sp s i n)
  (cond
    [(< i n) (list (complete '() i)
                   (partial any/sp (add1 i)))]
    [else (list (complete '() i))]))

;; var/sp/acc : [Listof Char] -> [StrSeqMatcher (String)]
(define ((var/sp/acc acc) s i n)
  (cond
    [(< i n)
     (list (complete (list (list->string (reverse acc))) i)
           (partial (var/sp/acc (cons (string-ref s i) acc))
                    (add1 i)))]
    [else (list (complete (list (list->string (reverse acc))) i))]))

;; var/sp : [StrSeqMatcher (String)]
(define var/sp (var/sp/acc '()))

;; and-var/sp : [StrSeqMatcher (Y ...)] -> [StrSeqMatcher (Y ... String)]
;; var-and/sp : [StrSeqMatcher (Y ...)] -> [StrSeqMatcher (String Y ...)]
(define ((and-var/sp p) s i n) ((and-var-i0/sp i p) s i n))
(define ((var-and/sp p) s i n) ((var-and-i0/sp i p) s i n))

(define ((and-var-i0/sp i0 p) s i n)
  (define rs (p s i n))
  (for/list ([r (in-list rs)])
    (match r
      [(partial p i)
       (partial (and-var-i0/sp i0 p) i)]
      [(complete vs j)
       (complete (append vs (list (substring s i0 j))) j)])))

(define ((var-and-i0/sp i0 p) s i n)
  (define rs (p s i n))
  (for/list ([r (in-list rs)])
    (match r
      [(partial p i)
       (partial (and-var-i0/sp i0 p) i)]
      [(complete vs j)
       (complete (cons (substring s i0 j) vs) j)])))

;; and/sp : [StrSeqMatcher (Y ...)] ... -> [StrSeqMatcher (Y ... ...)]
(define ((and/sp . ps) s i n)
  (match ps
    ['()         any/sp]
    [(cons p ps) ((and-i0*/sp i p ps) s i n)]))

(define ((and-i0*/sp i0 p ps) s i n)
  (define rs (p s i n))
  (append*
   (for/list ([r (in-list rs)])
     (match r
       [(partial p i)
        (list (partial (and-var-i0/sp i0 p) i))]
       [(complete vs j)
        (define rst-vs
          (let loop ([ps ps] [acc '()])
            (match ps
              ['() (reverse acc)]
              [(cons p ps)
               (define r (string-seq-match-exact-substring p s i j))
               (and r (loop ps (append-reverse r acc)))])))
        (cond
          [rst-vs (list (complete (append vs rst-vs) j))]
          [else   '()])]))))

;; -----------------------------------------------

;; string=/sp : String -> [StrSeqMatcher ()]
(define ((string=/sp sub) str i n)
  (define j (+ i (string-length sub)))
  (cond
    [(and (<= j n) (string=? (substring str i j) sub))
     (list (complete '() j))]
    [else
     '()]))

;; regexp/sp : Regexp -> [StrSeqMatcher ()]
(define ((regexp/sp rx) s i n)
  (match (regexp-match-positions rx s i n)
    [(cons (cons (== i) j) _)
     #:when (<= j n)
     (list (complete '() j))]
    [_
     '()]))

;; string/sp :
;;   [Matcher Char (Y ...)] ...
;;   ->
;;   [StrSeqMatcher (Y ... ...)]
(define ((string/sp . ps) s i n)
  (define j (+ i (length ps)))
  (cond
    [(<= j n)
     (let loop ([ps ps] [i i] [acc '()])
       (match ps
         ['() (list (complete (reverse acc) i))]
         [(cons p ps)
          (define r (p (string-ref s i)))
          (cond
            [r (loop ps (add1 i) (append-reverse r acc))]
            [else '()])]))]
    [else
     '()]))

;; -----------------------------------------------

;; cons/sp :
;;   [Matcher Char (Y1 ...)]
;;   [StrSeqMatcher (Y2 ...)]
;;   ->
;;   [StrSeqMatcher (Y1 ... Y2 ...)]
(define ((cons/sp p sp) s i n)
  (cond
    [(not (< i n)) '()]
    [else
     (define vs1 (p (string-ref s i)))
     (cond
       [(not vs1) '()]
       [else
        (let loop ([sp sp] [i (add1 i)])
          (define rs (sp s i n))
          (append*
           (for/list ([r (in-list rs)])
             (match r
               [(complete vs2 i)
                (list (complete (append vs1 vs2) i))]
               [(partial sp i)
                (loop sp i)]))))])]))

;; append/sp :
;;   [StrSeqMatcher (Y ...)] ...
;;   ->
;;   [StrSeqMatcher (Y ... ...)]
(define ((append/sp . ps) s i n)
  (let loop ([ps ps] [i i] [acc '()])
    (match ps
      ['() (list (complete (reverse acc) i))]
      [(cons p ps-rst)
       (define rs (p s i n))
       (append*
        (for/list ([r (in-list rs)])
          (match r
            [(complete vs i)
             (loop ps-rst i (append-reverse vs acc))]
            [(partial p* i)
             (loop (cons p* ps-rst) i acc)])))])))

;; repeat/sp/acc :
;;   [Listof [List Y ...]]
;;   Natural
;;   Natural
;;   [StrSeqMatcher (Y ...)]
;;   [StrSeqMatcher (Y ...)]
;;   ->
;;   [StrSeqMatcher ([Listof Y] ...)]
;; The `k` argument represents the minimum number of repetitions.
;; The `np` argument represents the number of outputs `p1` and `p2` have
;;   (they both should have the same number np).
(define ((repeat/sp/acc acc k np p1 p2) s i n)
  (define loloy
    (cond [(empty? acc) (make-list np '())]
          [else (apply map list (reverse acc))]))
  (cond
    [(< i n)
     (define rs (p1 s i n))
     (append*
      (cond [(zero? k) (list (complete loloy i))]
            [else      '()])
      (for/list ([r (in-list rs)])
        (match r
          [(complete vs i*)
           (when (= i i*)
             (error "ellipsis pattern matched empty sequence"))
           (list
            (partial (repeat/sp/acc (cons vs acc)
                                    (if (zero? k) 0 (sub1 k))
                                    np
                                    p2
                                    p2)
                     i*))]
          [(partial p1* i*)
           (when (= i i*)
             (error "ellipsis pattern matched empty sequence"))
           (list (partial (repeat/sp/acc acc k np p1* p2)
                          i*))])))]
    [(zero? k)
     (list (complete loloy i))]
    [else
     '()]))

;; repeat/sp :
;;   Natural
;;   [StrSeqMatcher (Y ...)]
;;   ->
;;   [StrSeqMatcher ([Listof Y] ...)]
(define (repeat/sp np p)
  (repeat/sp/acc '() 0 np p p))

;; repeat-at-least/sp :
;;   Natural
;;   Natural
;;   [StrSeqMatcher (Y ...)]
;;   ->
;;   [StrSeqMatcher ([Listof Y] ...)]
;; The `k` argument represents the minimum number of repetitions.
;; The `np` argument represents the number of outputs `p` has.
(define (repeat-at-least/sp k np p)
  (repeat/sp/acc '() k np p p))

;; -----------------------------------------------

(module+ test
  (define-check (check-seq-match val sp rs)
    (check-equal? ((string-seq-matcher->matcher sp) val) rs))

  (check-seq-match "123" var/sp (list "123"))
  (check-seq-match "123" (string/sp var/p (equal/p #\2) var/p) (list #\1 #\3))
  (check-seq-match "123" (string/sp var/p (equal/p #\2)) #false)
  (check-seq-match "1234"
                   (cons/sp var/p (cons/sp (equal/p #\2) var/sp))
                   (list #\1 "34"))
  (check-seq-match "123" (append/sp var/sp) (list "123"))
  (check-seq-match "123"
                   (append/sp (string/sp var/p) var/sp)
                   (list #\1 "23"))
  (check-seq-match "123"
                   (append/sp var/sp (string/sp var/p))
                   (list "12" #\3))
  (check-seq-match "1234"
                   (append/sp (string/sp var/p) var/sp (string/sp var/p))
                   (list #\1 "23" #\4))
  (check-seq-match "123abc"
                   (append/sp (string/sp var/p (equal/p #\2) var/p)
                              var/sp)
                   (list #\1 #\3 "abc"))
  (check-seq-match "123abc"
                   (append/sp var/sp
                              (string/sp var/p (equal/p #\b) var/p))
                   (list "123" #\a #\c))
  (check-seq-match "a1db2ec3f"
                   (repeat/sp 3 (string/sp var/p var/p var/p))
                   (list '(#\a #\b #\c) '(#\1 #\2 #\3) '(#\d #\e #\f)))
  (check-seq-match "0246813579"
                   (repeat/sp
                    1
                    (string/sp (and/p (pred/p char-numeric?) var/p)))
                   (list (string->list "0246813579")))
  (check-seq-match "123:456"
                   (append/sp var/sp (string=/sp ":") var/sp)
                   (list "123" "456"))
  (check-seq-match ":132435465"
                   (append/sp
                    (string=/sp ":")
                    (repeat/sp
                     1
                     (string/sp (and/p (pred/p char-numeric?) var/p))))
                   (list '(#\1 #\3 #\2 #\4 #\3 #\5 #\4 #\6 #\5)))
  (check-seq-match "1324:"
                   (append/sp
                    (repeat/sp
                     1
                     (string/sp (and/p (pred/p char-numeric?) var/p)))
                    (string=/sp ":"))
                   (list '(#\1 #\3 #\2 #\4)))
  (check-seq-match ":12:3::456"
                   (repeat/sp
                    1
                    (append/sp
                     (string=/sp ":")
                     (repeat/sp
                      1
                      (string/sp (and/p (pred/p char-numeric?) var/p)))))
                   (list (list '(#\1 #\2) '(#\3) '() '(#\4 #\5 #\6))))
  (check-match ":12:3::456"
               (string-seq-matcher:
                (repeat/sp
                 1
                 (append/sp
                  (string=/sp ":")
                  (repeat/sp
                   1
                   (string/sp (and/p (pred/p char-numeric?) var/p)))))
                [xss])
               (equal? xss (list '(#\1 #\2) '(#\3) '() '(#\4 #\5 #\6))))

  (check-match "12:34:56:78:910"
               (string-seq-matcher:
                (append/sp var/sp
                           (string=/sp ":")
                           var/sp
                           (string=/sp ":")
                           var/sp)
                [xs "34" ys])
               (and (equal? xs "12")
                    (equal? ys "56:78:910")))
  (check-match "12:34:56:78:910"
               (string-seq-matcher:
                (append/sp var/sp
                           (string=/sp ":")
                           var/sp
                           (string=/sp ":")
                           var/sp)
                [xs "56" ys])
               (and (equal? xs "12:34")
                    (equal? ys "78:910")))
  (check-match "12:34:56:78:910"
               (string-seq-matcher:
                (append/sp var/sp
                           (string=/sp ":")
                           var/sp
                           (string=/sp ":")
                           var/sp)
                [xs "78" ys])
               (and (equal? xs "12:34:56")
                    (equal? ys "910")))
  )
