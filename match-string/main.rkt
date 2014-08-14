#lang racket/base

(provide append
         string-append
         string
         append/c
         string-append/c
         string/c
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
  
  (define-syntax-class pat
    #:description "match pattern"
    [pattern (~and :expr (~not :ooo))])
  
  (define-syntax-class pat-maybe-elipsis
    #:description "match pattern or elipsis"
    #:attributes (norm)
    [pattern ooo:ooo #:with norm #'ooo.norm]
    [pattern pat:pat #:with norm #'pat])
  
  (define-syntax-class str-pat
    #:description "string-append pattern"
    #:attributes (norm)
    [pattern rx:rx #:with norm #'(regexp rx)]
    [pattern pat:pat #:with norm #'pat])
  
  (define-syntax-class str-pat-maybe-elipsis
    #:description "string-append pattern"
    #:attributes (norm)
    [pattern ooo:ooo #:with norm #'ooo.norm]
    [pattern pat:str-pat #:with norm #'pat.norm])
  )

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



(define-match-expander string
  (lambda (stx) ; as a match pattern
    (syntax-parse stx
      [(string) #'""]
      [(string pat1:pat pat:pat-maybe-elipsis ...)
       #'(? string? (app string->list (list pat1 pat.norm ...)))]))
  (lambda (stx) ; as normal string
    (syntax-parse stx
      [(string c ...) #'(rkt:string c ...)]
      [string #'rkt:string])))

(define (string/c . args)
  (flat-named-contract
   `(string/c ,@(for/list ([arg (in-list args)]) (if (ooo? arg) arg (contract-name arg))))
   (local [(define (try s-lst args)
             (match args
               [(list) (empty? s-lst)]
               [(list-rest c (app ooo? (and k (not #f))) rest-args)
                (define c? (flat-contract-predicate c))
                (match s-lst
                  [(list-rest (? c? cs) ... rest-cs)
                   #:when (<= k (length cs))
                   (try rest-cs rest-args)]
                  [_ #f])]
               [(list-rest c rest-args)
                (define c? (flat-contract-predicate c))
                (match s-lst
                  [(list-rest (? c?) rest-cs)
                   (try rest-cs rest-args)]
                  [_ #f])]))]
     (lambda (s)
       (try (string->list s) args)))))



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
      [(append pat:pat) #'pat]
      [(append pat:pat ooo:ooo)
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
      [(append pat1:pat pat2:pat)
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
      [(append pat1:pat ooo:ooo pat2:pat-maybe-elipsis ...)
       #'(append (append pat1 ooo.norm) pat2.norm ...)]
      [(append pat1:pat pat2:pat pat:pat-maybe-elipsis ...)
       #'(append pat1 (append pat2 pat ...))]
      ))
  (lambda (stx) ; as normal append
    (syntax-parse stx
      [(append . rst)
       (quasisyntax/loc stx (#,(syntax/loc #'append rkt:append) . rst))]
      [append
       (syntax/loc stx rkt:append)])))




(define/contract append/c (case-> (#:rest (listof flat-contract?) . -> . flat-contract?))
  (case-lambda
    [() (flat-named-contract 'empty? empty?)]
    [(c) (flat-named-contract (contract-name c) c)]
    [(c1 c2)
     (define ooo?/k (ooo? c2))
     (cond [ooo?/k
            (let ([c c1] [ooo c1] [c.name (contract-name c1)])
              (flat-named-contract
               `(append/c ,c.name ,ooo)
               (local [(define k ooo?/k)
                       (define c? (flat-named-contract c.name c))
                       (define (try lst i)
                         (match lst
                           [(list)
                            (cond [(<= k i) #true]
                                  [(c? '()) #true]
                                  [else #false])]
                           [(list '() ..0)
                            (try '() i)]
                           [(list-rest (and lst1 (? c?)) rest)
                            (or (try rest (add1 i))
                                (failure-cont))]
                           [(list-rest (list lst1 ..0 last) lst2 rest)
                            (try (list* lst1 (cons last lst2) rest) i)]
                           [(list-rest '() rest)
                            #false]
                           [(list (list-rest lst1 ..0 (and rst (not (? pair?)))))
                            (try (list lst1 rst) i)]
                           [_ (error 'append/c "!!! lst = ~v" lst) #false]))]
                 (lambda (val)
                   (try (list val) 0)))))]
           [else
            (let ([c1.name (contract-name c1)]
                  [c2.name (contract-name c2)])
              (flat-named-contract
               `(append/c ,c1.name ,c2.name)
               (local [(define c1? (flat-named-contract c1.name c1))
                       (define c2? (flat-named-contract c2.name c2))
                       (define (try p1 p2)
                         (match p1
                           [_ #:when (and (c1? p1) (c2? p2)) #true]
                           [(list lst1 ..0 last)
                            (try lst1 (cons last p2))]
                           ['() #false]))]
                 (lambda (val)
                   (match val
                     [(list-rest lst1 ..0 (and rst (not (? pair?))))
                      (try lst1 rst)])))))])]
    [(c1 c2 . rest-args)
     (cond [(ooo? c2)
            (flat-named-contract
             `(append/c ,(contract-name c1) ,c2 ,@(map contract-name rest-args))
             (apply append/c (append/c c1 c2) rest-args))]
           [else
            (flat-named-contract
             `(append/c ,(contract-name c1) ,(contract-name c2) ,@(map contract-name rest-args))
             (append/c c1 (apply append/c c2 rest-args)))])]))

(define/contract string-append/c (case-> (#:rest (listof flat-contract?) . -> . flat-contract?))
  (case-lambda
    [() (flat-named-contract "" "")]
    [(c) (flat-named-contract `(string-append-c ,(contract-name c)) (and/c string? c))]
    [(c1 c2)
     (define ooo?/k (ooo? c2))
     (cond [ooo?/k
            (let ([c c1] [ooo c1] [c.name (contract-name c1)])
              (flat-named-contract
               `(string-append/c ,c.name ,ooo)
               (local [(define k ooo?/k)
                       (define c? (flat-named-contract c.name c))
                       (define (try lst i)
                         (match lst
                           [(list)
                            (cond [(<= k i) #true]
                                  [(c? "") #true]
                                  [else #false])]
                           [(list "" ..0)
                            (try '() i)]
                           [(list-rest (and lst1 (? c?)) rest)
                            (or (try rest (add1 i))
                                (failure-cont))]
                           [(list-rest (string cs ..0 c) s2 rest)
                            (try (list* (list->string cs) (string-append (string c) s2) rest) i)]
                           [(list-rest "" rest)
                            #false]
                           [(list s)
                            (try (list s "") i)]
                           [_ (error 'string-append/c "!!! lst = ~v" lst) #false]))]
                 (lambda (val)
                   (try (list val) 0)))))]
           [else
            (let ([c1.name (contract-name c1)]
                  [c2.name (contract-name c2)])
              (flat-named-contract
               `(string-append/c ,c1.name ,c2.name)
               (local [(define c1? (flat-named-contract c1.name c1))
                       (define c2? (flat-named-contract c2.name c2))
                       (define (try s1 s2)
                         (match s1
                           [_ #:when (and (c1? s1) (c2? s2)) #true]
                           [(string cs ... c)
                            (try (list->string cs) (string-append (string c) s2))]
                           ["" #false]))]
                 (lambda (s)
                   (and (string? s)
                        (try s ""))))))])]
    [(c1 . rest-args)
     (flat-named-contract
      `(string-append/c ,(contract-name c1) ,@(map contract-name rest-args))
      (string-append/c c1 (apply string-append/c rest-args)))]))




(module+ test
  
  (check-pred (append/c (list/c 1 2 3) list?)
              (list 1 2 3 4 5 6))
  
  (check-pred (append/c (list/c 1 2 3) 4)
              '(1 2 3 . 4))
  
  (check-pred (not/c (append/c (list/c 1 2 2.5 3) list?))
              (list 1 2 3 4 5 6))
  
  (check-pred (string-append/c "abc" string?)
              "abcdef")
  
  (check-pred (not/c (string-append/c "abc" string?))
              "ab cdef")
  
  (check-pred (string-append/c (or/c "abc" "ab c") string?)
              "ab cdef")
  
  (local [(define ws? string-whitespace?)
          (define abc?
            (string-append/c "a" ws? "b" ws? "c"))]
    (check-pred (string-append/c abc? string?)
                "a    b c   stuff"))
  
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
  (check-pred (string-append/c (or/c "ab" "abab") '..3)
              "abababab")
  
  (check-equal? (match "abab"
                  [(string-append (and los (or "ab" "abab")) ..2)
                   los])
                '("ab" "ab"))
  (check-pred (string-append/c (or/c "ab" "abab") '..2)
              "abab")
  
  (check-equal? (match "abababab"
                  [(string-append (and los (string-append (and lol "ab") ...+)) ..3)
                   (list los lol)])
                '(("abab" "ab" "ab") (("ab" "ab") ("ab") ("ab"))))
  (check-pred (string-append/c (string-append/c "ab" '...+) '..3)
              "ababab")
  (check-equal? (match "abababab"
                  [(string-append (and los (string-append "ab" ...)) ..3)
                   los])
                '("abababab" "" ""))
  (check-equal? (match "abababab"
                  [(string-append (and los (string-append "ab" ..2)) ..2)
                   los])
                '("abab" "abab"))
  (check-pred (string-append/c (string-append/c "ab" '..2) '..2)
              "abababab")
  (check-false (match "abababab"
                 [(string-append (and los (string-append "ab" ..2)) ..3)
                  los]
                 [_ #f]))
  (check-pred (not/c (string-append/c (string-append/c "ab" '..2) '..3))
              "abababab")
  
  (check-true (match "abab"
                [(string-append "ab" ..2) #t]))
  (check-false (match "abab"
                 [(string-append "ab" ..3) #t]
                 [_ #f]))
  (check-pred (string-append/c "ab" '..2) "abab")
  (check-pred (not/c (string-append/c "ab" '..3)) "abab")
  
  (check-equal? (match '(a b a b a b a b)
                  [(append (and lol (or '(a b) '(a b a b))) ..3)
                   lol])
                '((a b a b) (a b) (a b)))
  (check-pred (append/c (or/c (list/c 'a 'b) (list/c 'a 'b 'a 'b)) '..3)
              '(a b a b a b a b))
  
  (check-equal? (match '(a b a b)
                  [(append (and lol (or '(a b) '(a b a b))) ..2)
                   lol])
                '((a b) (a b)))
  (check-pred (append/c (or/c (list/c 'a 'b) (list/c 'a 'b 'a 'b)) '..2)
              '(a b a b a b))
  
  (check-equal? (match '(a b a b a b a b)
                  [(append (and lol (append (and lolol '(a b)) ...+)) ..3)
                   (list lol lolol)])
                '(((a b a b) (a b) (a b))
                  (((a b) (a b)) ((a b)) ((a b)))))
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
  (check-pred (append/c (list/c 'a 'b) '..2) '(a b a b))
  (check-pred (not/c (append/c (list/c 'a 'b) '..3)) '(a b a b))
  
  (check-equal? (match '(a b a b a b)
                  [(append (and lol (or '(a b) '(a b a b))) ..2)
                   lol])
                '((a b a b) (a b)))
  
  (check-true (match '(1 2 3 . 4)
                [(append (or '(1) '(2) '(3) 4) ..4) #t]))
  (check-pred (append/c (or/c (list/c (or/c 1 2 3)) 4) '..4) '(1 2 3 . 4))
  
  (check-pred (string/c char-upper-case? '..3) "ABC")
  (check-pred (not/c (string/c char-upper-case? '..3)) "ABc")
  
  )
