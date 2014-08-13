#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          (for-label (rename-in racket
                                [string-append rkt:string-append]
                                [append rkt:append]
                                [string rkt:string])
                     match-string))

@(define evaluator (make-base-eval))
@(void (evaluator '(require (rename-in racket
                                       [string-append rkt:string-append]
                                       [append rkt:append]
                                       [string rkt:string])
                            match-string)))

@defmodule[match-string]{
provides @racket[string-append], @racket[append], and @racket[string] as @racket[match] expanders,
and @racket[string-append/c], @racket[append/c], and @racket[string/c] as contract constructers.
}

@defform*[((string-append pat ...)
           (string-append expr ...)
           string-append)
          #:grammar ([pat match-pattern
                          (code:line pat ooo)]
                     [ooo ...
                          ...+
                          ..k])]{
a @racket[match] expander for matching strings.  
When it's not used as a @racket[match] expander, it behaves like normal @racket[rkt:string-append].

@examples[
  #:eval evaluator
  (match "abcdef"
    [(string-append "abc" s) s])
  (match "ab cdef"
    [(string-append (or "abc" "ab c") s) s])
  (match "cadaddadddr"
    [(string-append "c" (and x (or "a" "d")) ... "r")
     x])
  (match "abababab"
    [(string-append (and los (or "ab" "abab")) ..3)
     los])
  (match "abababab"
    [(string-append (string-append (and lol "ab") ...+) ..3)
     lol])
]}

@defform*[((append pat ...)
           (append expr ...)
           append)
          #:grammar ([pat match-pattern
                          (code:line pat ooo)]
                     [ooo ...
                          ...+
                          ..k])]{
a @racket[match] expander for matching lists.  
When it's not used as a @racket[match] expander, it behaves like normal @racket[rkt:append].

@examples[
  #:eval evaluator
  (match (list 1 2 3 4 5 6)
    [(append (list 1 2 3) p) p])
  (match '(1 2 3 . 4)
    [(append (list 1 2) p) p])
  (match '(1 2 3 . 4)
    [(append (list 1 2 3) p) p])
  (match '(0 1 #:kw-1 kw-arg-1 2 #:kw-2 kw-arg-2 3 4)
    [(append (and lol (or (list (? keyword?) _) (list (not (? keyword?))))) ...)
     lol])
  (match '(a b a b a b a b)
    [(append (append (and lolol '(a b)) ...+) ..3)
     lolol])
  (match '(1 2 3 . 4)
    [(append (and l (or '(1) '(2) '(3) 4)) ..4) l])
]}

@defform*[((string pat ...)
           (string expr ...)
           string)
          #:grammar ([pat match-pattern
                          (code:line match-pattern ooo)]
                     [ooo ...
                          ..k])]{
a @racket[match] expander for matching string character by character.
Whet it's used as a @racket[match] expander, it expands to @racket[(app string->list (list pat ...))].
When it's not used as a @racket[match] expander, it behaves like normal @racket[rkt:string].
}

@defproc[(string-append/c [arg (or/c flat-contract? '... '...+ ..k?)] ...) flat-contract?]{

}

@defproc[(append/c [arg (or/c flat-contract? '... '...+ ..k?)] ...) flat-contract?]{

}

@defproc[(string/c [arg flat-contract?] ...) flat-contract?]{

}

