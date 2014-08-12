#lang racket/base

(provide append
         string-append
         string
         append/c
         string-append/c
         )

(require racket/list
         racket/local
         racket/match
         racket/contract/base
         racket/contract/region
         rackjure/conditionals
         (only-in racket/base
                  [append rkt:append]
                  [string rkt:string]
                  [string-append rkt:string-append])
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     racket/match
                     rackjure/threading))

(module+ test
  (require rackunit
           (only-in lang/htdp-intermediate-lambda string-whitespace?)))


(define (string-first s)
  (string (string-ref s 0)))

(define (string-rest s)
  (substring s 1))




(begin-for-syntax
  (define-syntax-class rx
    #:description "regexp literal"
    [pattern pat
             #:attr rx (syntax-e #'pat)
             #:when (or (regexp? (attribute rx))
                        (byte-regexp? (attribute rx)))])
  (define-syntax-class ooo
    #:description "elipsis"
    #:attributes (k norm)
    [pattern (~and ooo (~or (~literal ...) (~datum ...)))
             #:attr k 0
             #:with norm #'ooo]
    [pattern (~and ooo (~or (~literal ...+) (~datum ...+)))
             #:attr k 1
             #:with norm (datum->syntax #'ooo '..1 #'ooo #'ooo)]
    [pattern ..k:id
             #:do [(define str (~> #'..k syntax-e symbol->string))]
             #:when (and (< 2 (string-length str))
                         (equal? ".." (substring str 0 2)))
             #:attr k (string->number (substring str 2))
             #:when (exact-nonnegative-integer? (attribute k))
             #:with norm #'..k])
  
  (define-syntax-class pat-maybe-elipsis
    #:attributes (norm)
    [pattern ooo:ooo #:with norm #'ooo.norm]
    [pattern pat:expr #:with norm #'pat])
  
  (define-syntax-class str-pat
    #:description "string-append pattern"
    #:attributes (norm)
    [pattern rx:rx #:with norm #'(regexp rx)]
    [pattern (~and pat:expr (~not :ooo)) #:with norm #'pat])
  
  (define-syntax-class str-pat-maybe-elipsis
    #:description "string-append pattern"
    #:attributes (norm)
    [pattern ooo:ooo #:with norm #'ooo.norm]
    [pattern pat:str-pat #:with norm #'pat.norm])
  )

(define-match-expander string
  (lambda (stx) ; as a match pattern
    (syntax-parse stx
      [(string) #'""]
      [(string pat:pat-maybe-elipsis ...)
       #'(? string? (app string->list (list pat.norm ...)))]))
  (lambda (stx) ; as normal string
    (syntax-parse stx
      [(string c ...) #'(rkt:string c ...)]
      [string #'rkt:string])))





(define-match-expander string-append
  (lambda (stx) ; as a pattern
    (syntax-parse stx
      [(string-append) #'""]
      [(string-append pat:str-pat)
       #'(? string? pat.norm)]
      [(string-append pat:str-pat ooo:ooo)
       (with-syntax ([pat #'pat.norm] [ooo #'ooo.norm] [k (attribute ooo.k)])
         #'(app (local [;; try : (Listof String) Natural -> (U (Listof String) #f)
                        (define (try lst i)
                          (match lst
                            [(list)
                             (cond [(<= k i) '()]
                                   [else (match ""
                                           [pat
                                            (if-let [it (try '() (add1 i))]
                                                    (cons "" it)
                                                    #false)]
                                           [_ #false])])]
                            [(list "" ..0)
                             (try '() i)]
                            [(list-rest (and s1 pat) rest)
                             (if-let [it (try rest (add1 i))]
                                     (cons s1 it)
                                     (failure-cont))]
                            [(list-rest (string cs ..0 c) s2 rest)
                             (try (list* (list->string cs) (rkt:string-append (string c) s2) rest) i)]
                            [(list-rest "" rest)
                             #false]
                            [(list val)
                             (try (list val "") i)]
                            [_ (error "!!!") #false]))]
                  (lambda (val)
                    (and (string? val)
                         (try (list val) 0))))
                (list pat ooo)))]
      [(string-append pat1:str-pat pat2:str-pat)
       (with-syntax ([pat1 #'pat1.norm] [pat2 #'pat2.norm])
         #'(app (local [;; String String -> (or/c (list String String) #f)
                        ;; matches s1 and s2 against pat1 and pat2, and
                        ;; if the match fails, then it tries again with slightly different strings
                        (define (try s1 s2)
                          (match* (s1 s2)
                            [(pat1 pat2) (list s1 s2)]
                            [((string cs ..0 c) s2)
                             (try (list->string cs) (rkt:string-append (string c) s2))]
                            [("" _) #false]))]
                  (lambda (val)
                    (and (string? val)
                         (try val ""))))
                (list pat1 pat2)))]
      [(string-append pat1:str-pat ooo:ooo pat2:str-pat-maybe-elipsis ...)
       #'(string-append (string-append pat1.norm ooo.norm) pat2.norm ...)]
      [(string-append pat1:str-pat pat2:str-pat-maybe-elipsis ...)
       #'(string-append pat1.norm (string-append pat2.norm ...))]
      ))
  (lambda (stx) ; as normal string-append
    (syntax-parse stx
                  [(string-append . rest)
                   (quasisyntax/loc stx
                     (#,(syntax/loc #'string-append rkt:string-append) . rest))]
                  [string-append
                   (syntax/loc stx rkt:string-append)])))

(define-match-expander append
  (lambda (stx) ; as a pattern
    (syntax-parse stx
      [(append) #''()]
      [(append pat) #'pat]
      [(append pat ooo:ooo)
       (with-syntax ([ooo #'ooo.norm] [k (attribute ooo.k)])
         #'(app (local [;; try : (Listof Any) Natural -> (U (Listof Any) #f)
                        (define (try lst i)
                          (match lst
                            [(list)
                             (cond [(<= k i) '()]
                                   [else (match '()
                                           [pat
                                            (if-let [it (try '() (add1 i))]
                                                    (cons '() it)
                                                    #false)]
                                           [_ #false])])]
                            [(list '() ..0)
                             (try '() i)]
                            [(list-rest (and lst1 pat) rest)
                             (if-let [it (try rest (add1 i))]
                                     (cons lst1 it)
                                     (failure-cont))]
                            [(list-rest (list lst1 ..0 last) lst2 rest)
                             (try (list* lst1 (cons last lst2) rest) i)]
                            [(list-rest '() rest)
                             #false]
                            [(list (list-rest lst1 ..0 (and rst (not (? pair?)))))
                             (try (list lst1 rst) i)]
                            [_ (error 'append "!!! lst = ~v" lst) #false]))]
                  (lambda (val)
                    (try (list val) 0)))
                (list pat ooo)))]
      [(append pat1 pat2)
       #'(app (local [;; List Any -> (or/c (list List Any) #f)
                      ;; matches p1 and p2 against pat1 and pat2, and
                      ;; if the match fails, then it tries again with slightly different lists
                      (define (try p1 p2)
                        (match* (p1 p2)
                          [(pat1 pat2) (list p1 p2)]
                          [((list lst1 ..0 last) lst2)
                           (try lst1 (cons last lst2))]
                          [(_ _) #false]))]
                (lambda (val)
                  (match val
                    [(list-rest lst1 ..0 (and rst (not (? pair?))))
                     (try lst1 rst)])))
              (list pat1 pat2))]
      [(append pat1:expr ooo:ooo pat2:pat-maybe-elipsis ...)
       #'(append (append pat1 ooo.norm) pat2.norm ...)]
      [(append pat1 pat2 ...)
       #'(append pat1 (append pat2 ...))]
      ))
  (lambda (stx) ; as normal append
    (syntax-parse stx
                  [(append p ...) #'(#%app rkt:append p ...)]
                  [append #'rkt:append])))




(define/contract append/c (case-> (#:rest (listof flat-contract?) . -> . flat-contract?))
  (procedure-rename
   (case-lambda
     [() (flat-named-contract 'empty? empty?)]
     [(c) (flat-named-contract (contract-name c) c)]
     [(c1 c2)
      (let ([c1? (flat-named-contract (contract-name c1) c1)]
            [c2? (flat-named-contract (contract-name c2) c2)])
        (flat-named-contract
         `(append/c ,(contract-name c1) ,(contract-name c2))
         (lambda (p)
           (local [(define (try p1 p2)
                     (cond [(and (c1? p1)
                                 (c2? p2))
                            #true]
                           [(not (pair? p2)) #false]
                           [else
                            (let* ([p2-first (car p2)]
                                   [p2-rest (cdr p2)]
                                   [p1+p2-first (append p1 (list p2-first))])
                              (try p1+p2-first p2-rest))]))]
             (try '() p)))))]
     [(c1 . rest-args)
      (flat-named-contract
       `(append/c ,(contract-name c1) ,@(map contract-name rest-args))
       (append/c c1 (apply append/c rest-args)))])
   'append/c))

(define/contract string-append/c (case-> (#:rest (listof flat-contract?) . -> . flat-contract?))
  (case-lambda
    [() (flat-named-contract "" "")]
    [(c) (flat-named-contract `(string-append-c ,(contract-name c)) (and/c string? c))]
    [(c1 c2)
     (let ([c1? (flat-named-contract (contract-name c1) c1)]
           [c2? (flat-named-contract (contract-name c2) c2)])
       (flat-named-contract
        `(string-append/c ,(contract-name c1) ,(contract-name c2))
        (lambda (s)
          (and (string? s)
               (local [(define (try s1 s2)
                         (cond [(and (c1? s1)
                                     (c2? s2))
                                #true]
                               [(equal? "" s2) #false]
                               [else
                                (let* ([s2-first (string-first s2)]
                                       [s2-rest (string-rest s2)]
                                       [s1+s2-first (string-append s1 s2-first)])
                                  (try s1+s2-first s2-rest))]))]
                 (try "" s))))))]
    [(c1 . rest-args)
     (flat-named-contract
      `(string-append/c ,(contract-name c1) ,@(map contract-name rest-args))
      (string-append/c c1 (apply string-append/c rest-args)))]))




(module+ test
  
  (check-true (#%app (append/c (list/c 1 2 3) list?)
                     (list 1 2 3 4 5 6)))
  
  (check-true (#%app (append/c (list/c 1 2 3) 4)
                     '(1 2 3 . 4)))
  
  (check-false (#%app (append/c (list/c 1 2 2.5 3) list?)
                      (list 1 2 3 4 5 6)))
  
  (check-true (#%app (string-append/c "abc" string?)
                     "abcdef"))
  
  (check-false (#%app (string-append/c "abc" string?)
                      "ab cdef"))
  
  (check-true (#%app (string-append/c (or/c "abc" "ab c") string?)
                     "ab cdef"))
  
  (local [(define ws? string-whitespace?)
          (define abc?
            (string-append/c "a" ws? "b" ws? "c"))]
    (check-true (#%app (string-append/c abc? string?)
                       "a    b c   stuff")))
  
  (check-equal? (match (list 1 2 3 4 5 6)
                  [(append (list 1 2 3) p) p])
                (list 4 5 6))
  
  (check-equal? (match '(1 2 3 . 4)
                  [(append (list 1 2) p) p])
                '(3 . 4))
  
  (check-false (match (list 1 2 3 4 5 6)
                 [(append (list 1 2 2.5 3) p) p]
                 [_ #f]))
  
  (check-equal? (match "abcdef"
                  [(string-append "abc" s) s])
                "def")
  
  (check-false (match "ab cdef"
                 [(string-append "abc" s) s]
                 [_ #f]))
  
  (check-equal? (match "ab cdef"
                  [(string-append (or "abc" "ab c") s) s])
                "def")
  
  (local [(define ws? string-whitespace?)]
    (check-equal? (match "a    b c   stuff"
                    [(string-append "a" (? ws? ab) "b" (? ws? bc) "c" s) (list ab bc s)])
                  (list "    "
                        " "
                        "   stuff")))
  
  ;; Examples based on examples in racket guide for regexps
  
  (define-syntax-rule
    (my-string-match-positions pat str)
    (match str
      [(string-append (app string-length start-pos)
                      (and pat s (app string-length s.length))
                      _)
       (define end-pos (+ start-pos s.length))
       (list (cons start-pos end-pos))]
      [_ #f]))
  
  (define-syntax-rule
    (my-string-match pat str)
    (match str
      [(string-append _ (and pat s) _)
       (list s)]
      [_ #f]))
  
  ;; instead of:
  ;; > (regexp-match-positions #rx"brain" "bird")
  ;; #f
  (check-equal? (my-string-match-positions "brain" "bird")
                #;(match "bird"
                  [(string-append (app string-length a) "brain" _)
                   (list (cons a (+ a (string-length "brain"))))]
                  [_ #f])
                #f)
  
  ;; instead of:
  ;; > (regexp-match-positions #rx"needle" "hay needle stack")
  ;; '((4 . 10))
  (check-equal? (my-string-match-positions "needle" "hay needle stack")
                #;(match "hay needle stack"
                  [(string-append (app string-length a) "needle" _)
                   (list (cons a (+ a (string-length "needle"))))])
                '((4 . 10)))
  
  ;; instead of:
  ;; > (regexp-match #rx"needle" "hay needle stack")
  ;; '("needle")
  (check-equal? (match "hay needle stack"
                  [(string-append _ (and "needle" s) _)
                   (list s)])
                '("needle"))
  
  ;; instead of:
  ;; > (regexp-replace #rx"te" "liberte" "ty")
  ;; "liberty"
  (check-equal? (match "liberte"
                  [(string-append a "te" b)
                   (string-append a "ty" b)])
                "liberty")
  
  ;; instead of:
  ;; > (regexp-match-positions #rx"^contact" "first contact")
  ;; #f
  (check-equal? (match "first contact"
                  [(string-append (and s "contact") _)
                   (list (cons 0 (string-length s)))]
                  [_ #f])
                #f)
  
  ;; instead of:
  ;; > (regexp-match-positions #rx"laugh$" "laugh laugh laugh laugh")
  ;; '((18 . 23))
  (check-equal? (match "laugh laugh laugh laugh"
                  [(string-append (app string-length a) (and s "laugh"))
                   (list (cons a (+ a (string-length s))))])
                '((18 . 23)))
  
  ;; instead of:
  ;; > (regexp-match-positions #px"yack\\b" "yackety yack")
  ;; '((8 . 12))
  (check-equal? (match "yackety yack"
                  [(string-append (app string-length a) "yack" (or "" (string-append (? string-whitespace? (not "")) _)))
                   (list (cons a (+ a (string-length "yack"))))])
                '((8 . 12)))
  
  ;; instead of:
  ;; > (regexp-match-positions #px"an\\B" "an analysis")
  ;; '((3 . 5))
  (check-equal? (match "an analysis"
                  [(string-append (app string-length a) "an" (not (or "" (string-append (? string-whitespace? (not "")) _))))
                   (list (cons a (+ a (string-length "an"))))])
                '((3 . 5)))
  
  ;; instead of:
  ;; > (regexp-match #rx"p.t" "pet")
  ;; '("pet")
  (check-equal? (match "pet"
                  [(string-append _ (and (string-append "p" (string _) "t") s) _)
                   (list s)])
                '("pet"))
  
  ;; instead of:
  ;; > (regexp-match #rx"p[aeiou]t" "pet")
  ;; '("pet")
  (check-equal? (match "pet"
                  [(string-append _ (and (string-append "p" (or "a" "e" "i" "o" "u") "t") s) _)
                   (list s)])
                '("pet"))
  
  ;; instead of:
  ;; > (regexp-match #rx"ta[b-dgn-p]" "tab")
  ;; '("tab")
  (check-equal? (match "tab"
                  [(string-append _ (and (string-append "ta" (or (regexp #rx"[b-d]") "g" (regexp #rx"[n-p]"))) s) _)
                   (list s)])
                '("tab"))
  
  ;; instead of:
  ;; > (regexp-match #rx"do[^g]" "dot")
  ;; '("tab")
  (check-equal? (match "dot"
                  [(string-append _ (and (string-append "do" (and (string _) (not "g"))) s) _)
                   (list s)])
                '("dot"))
  
  ;; instead of:
  ;; > (regexp-match-positions #rx"c[ad]*r" "cadaddadddr")
  ;; '((0 . 11))
  (check-equal? (match "cadaddadddr"
                  [(string-append (app string-length a) (and s (string-append "c" (or "a" "d") ... "r")) _)
                   (list (cons a (+ a (string-length s))))])
                '((0 . 11)))
  (check-equal? (match "cadaddadddr"
                  [(string-append (app string-length a) (and (string #\c (or #\a #\d) ... #\r) s) _)
                   (list (cons a (+ a (string-length s))))])
                '((0 . 11)))
  
  ;; instead of:
  ;; > (regexp-match-positions #rx"c[ad]*r" "cr")
  ;; '((0 . 2))
  (check-equal? (match "cr"
                  [(string-append (app string-length a) (and (string-append "c" (or "a" "d") ... "r") s) _)
                   (list (cons a (+ a (string-length s))))])
                '((0 . 2)))
  (check-equal? (match "cr"
                  [(string-append (app string-length a) (and (string #\c (or #\a #\d) ... #\r) s) _)
                   (list (cons a (+ a (string-length s))))])
                '((0 . 2)))
  
  ;; instead of:
  ;; > (regexp-match-positions #rx"c[ad]+r" "cadaddadddr")
  ;; '((0 . 11))
  (check-equal? (match "cadaddadddr"
                  [(string-append (app string-length a) (and (string-append "c" (or "a" "d") ...+ "r") s) _)
                   (list (cons a (+ a (string-length s))))])
                '((0 . 11)))
  (check-equal? (match "cadaddadddr"
                  [(string-append (app string-length a) (and (string #\c (or #\a #\d) ...+ #\r) s) _)
                   (list (cons a (+ a (string-length s))))])
                '((0 . 11)))
  
  ;; instead of:
  ;; > (regexp-match-positions #rx"c[ad]+r" "cr")
  ;; #f
  (check-equal? (match "cr"
                  [(string-append (app string-length a) (and (string-append "c" (or "a" "d") ...+ "r") s) _)
                   (list (cons a (+ a (string-length s))))]
                  [_ #f])
                #f)
  (check-equal? (match "cr"
                  [(string-append (app string-length a) (and (string #\c (or #\a #\d) ...+ #\r) s) _)
                   (list (cons a (+ a (string-length s))))]
                  [_ #f])
                #f)
  
  ;; instead of:
  ;; > (regexp-match-positions #rx"c[ad]?r" "cadaddadddr")
  ;; #f
  (check-equal? (match "cadaddadddr"
                  [(string-append (app string-length a) (and (string-append "c" (or (or "a" "d") "") "r") s) _)
                   (list (cons a (+ a (string-length s))))]
                  [_ #f])
                #f)
  
  (check-equal? (match "abababab"
                  [(string-append (and los (or "ab" "abab")) ..3)
                   los])
                '("abab" "ab" "ab"))
  
  (check-equal? (match "abab"
                  [(string-append (and los (or "ab" "abab")) ..2)
                   los])
                '("ab" "ab"))
  
  (check-equal? (match "abababab"
                  [(string-append (and los (string-append "ab" ...+)) ..3)
                   los])
                '("abab" "ab" "ab"))
  (check-equal? (match "abababab"
                  [(string-append (and los (string-append "ab" ...)) ..3)
                   los])
                '("abababab" "" ""))
  (check-equal? (match "abababab"
                  [(string-append (and los (string-append "ab" ..2)) ..2)
                   los])
                '("abab" "abab"))
  (check-false (match "abababab"
                 [(string-append (and los (string-append "ab" ..2)) ..3)
                  los]
                 [_ #f]))
  
  (check-true (match "abab"
                [(string-append "ab" ..2) #t]))
  (check-false (match "abab"
                 [(string-append "ab" ..3) #t]
                 [_ #f]))
  
  (check-equal? (match '(a b a b a b a b)
                  [(append (and lol (or '(a b) '(a b a b))) ..3)
                   lol])
                '((a b a b) (a b) (a b)))
  
  (check-equal? (match '(a b a b)
                  [(append (and lol (or '(a b) '(a b a b))) ..2)
                   lol])
                '((a b) (a b)))
  
  (check-equal? (match '(a b a b a b a b)
                  [(append (and lol (append '(a b) ...+)) ..3)
                   lol])
                '((a b a b) (a b) (a b)))
  (check-equal? (match '(a b a b a b a b)
                  [(append (and lol (append '(a b) ...)) ..3)
                   lol])
                '((a b a b a b a b) () ()))
  (check-equal? (match '(a b a b a b a b)
                  [(append (and lol (append '(a b) ..2)) ..2)
                   lol])
                '((a b a b) (a b a b)))
  (check-false (match '(a b a b a  b a b)
                 [(append (and lol (append '(a b) ..2)) ..3)
                  lol]
                 [_ #f]))
  
  (check-true (match '(a b a b)
                [(append '(a b) ..2) #t]))
  (check-false (match '(a b a b)
                 [(append '(a b) ..3) #t]
                 [_ #f]))
  
  (check-equal? (match '(a b a b a b)
                  [(append (and lol (or '(a b) '(a b a b))) ..2)
                   lol])
                '((a b a b) (a b)))
  
  (check-true (match '(1 2 3 . 4)
                [(append (or '(1) '(2) '(3) 4) ..4) #t]))
  
  )
