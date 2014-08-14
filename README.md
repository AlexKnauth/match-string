match-string
============

string-append and append as a match patterns

Examples:
```racket
> string-append
#<procedure:string-append>
> (string-append "abc" "def")
"abcdef"
> (match "abcdef"
    [(string-append "abc" s) s])
"def"
> (match "ab cdef"
    [(string-append (or "abc" "ab c") s) s])
"def"
> (match "cadaddadddr"
    [(string-append "c" (and x (or "a" "d")) ... "r")
     x])
'("a" "d" "a" "d" "d" "a" "d" "d" "d")
> (match "abababab"
    [(string-append (and los (or "ab" "abab")) ..3)
     los])
'("abab" "ab" "ab")
> (match "abababab"
    [(string-append (string-append (and lol "ab") ...+) ..3)
     lol])
'(("ab" "ab") ("ab") ("ab"))
> (match (list 1 2 3 4 5 6)
    [(append (list 1 2 3) p) p])
'(4 5 6)
> (match '(1 2 3 . 4)
    [(append (list 1 2) p) p])
'(3 . 4)
> (match '(1 2 3 . 4)
    [(append (list 1 2 3) p) p])
4
> (match '(0 1 #:kw-1 kw-arg-1 2 #:kw-2 kw-arg-2 3 4)
    [(append (and lol (or (list (? keyword?) _) (list (not (? keyword?))))) ...)
     lol])
'((0) (1) (#:kw-1 kw-arg-1) (2) (#:kw-2 kw-arg-2) (3) (4))
> (match '(a b a b a b a b)
    [(append (append (and lolol '(a b)) ...+) ..3)
     lolol])
'(((a b) (a b)) ((a b)) ((a b)))
> (match '(1 2 3 . 4)
    [(append (and l (or '(1) '(2) '(3) 4)) ..4) l])
'((1) (2) (3) 4)
```
