;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname assignment7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Assignment 7
(require 2htdp/image)
;; Problem 1
;; An Atom is one of:
;; - Number
;; - Symbol
;; - String

;; An SExp is one of:
;; - Atom
;; - [List-of SExp]

;; Problem a
;; sexp->string: SExp -> String
;; produces the textual representation of the given SExp an-sexp
(check-expect (sexp->string 1) "1")
(check-expect (sexp->string 'bar) "bar")
(check-expect (sexp->string "foo") "\"foo\"")
(check-expect (sexp->string '()) "()")
(check-expect (sexp->string '(a (37 "foo") c)) "(a (37 \"foo\" ) c )")
(define (sexp->string an-sexp)
  (cond [(atom? an-sexp) (atom->string an-sexp)]
        [else (string-append "(" (los->string an-sexp) ")")]))

;; atom?: SExp -> Boolean
;; is the given SExp an-sexp an atom?
(check-expect (atom? 1) #true)
(check-expect (atom? "foo") #true)
(check-expect (atom? 'bar) #true)
(check-expect (atom? '()) #false)
(check-expect (atom? '(1 2)) #false)
(define (atom? an-sexp)
  (or (number? an-sexp) (string? an-sexp) (symbol? an-sexp)))


;; atom->string: Atom -> String
;; produces the textual representation of the given atom an-atom
(check-expect (atom->string 1) "1")
(check-expect (atom->string 'bar) "bar")
(check-expect (atom->string "foo") "\"foo\"")
(define (atom->string an-atom)
  (cond [(number? an-atom) (number->string an-atom)]
        [(symbol? an-atom) (symbol->string an-atom)]
        [(string? an-atom) (string-append "\"" an-atom "\"")]))

;; los->string: [List-of SExp] -> String
;; produces the textual representation of the given list of SExp without the
;; outermost parentheses
(check-expect (los->string '()) "")
(check-expect (los->string '(1 2)) "1 2 ")
(check-expect (los->string '(a (37 "foo") c)) "a (37 \"foo\" ) c ")
(define (los->string a-los)
  (foldr (λ (s base) (string-append (sexp->string s) " " base)) "" a-los))

;; Problem b
;; two SExps template                         
#; (define (two-sexp-template sexp1 sexp2)
     (cond [(and (atom? sexp1) (atom? sexp2))
            (...(atom-temp sexp1)
                ...(atom-temp sexp2))]
           [(and (atom? sexp1) (list? sexp2))
            (...(atom-temp sexp1)
                ...(los-temp sexp2))]
           [(and (list? sexp1) (atom? sexp2))
            (...(los-temp sexp1)
                ...(atom-temp sexp2))]
           [(and (list? sexp1) (list? sexp2))
            (...(los-temp sexp1)
                ...(los-temp sexp2))]))

;; atom-temp: Atom -> ???
#; (define (atom-temp an-atom)
     (cond [(number? an-atom) ...]
           [(symbol? an-atom) ...]
           [(string? an-atom) ...]))

;; sexp-temp: SExp -> ???
#; (define (sexp-temp an-sexp)
     (cond [(or (number? an-sexp) (symbol? an-sexp) (string? an-sexp))
            (atom-temp an-sexp)]
           [else (los-temp an-sexp)]))

;; los-temp: [List-of SExp] -> ???
#; (define (los-temp a-los)
     (cond [(empty? a-los) ...]
           [else ...(sexp-temp (first a-los))
                 ...(los-temp (rest a-los))]))

;; Problem c
;; contains-same-atoms?: SExp SExp -> Boolean
;; do the two SExpressions contain same atoms regardless of the ordering?
(check-expect (contains-same-atoms? '(1 2 3 () ("r" b)) '("r" 1 (2) 3 b)) #true)
(check-expect (contains-same-atoms? '() '((()))) #true)
(check-expect (contains-same-atoms? "a" '(() ("a"))) #true)
(check-expect (contains-same-atoms? "a" '(() "a" ("a"))) #true)
(check-expect (contains-same-atoms? '("a" "mass") '("mass")) #false)
(define (contains-same-atoms? s1 s2)
  (and (contains-atoms? s1 s2)
       (contains-atoms? s2 s1)))

;; contains-atoms?: SExp SExp -> Boolean
;; does s2 contains all the atoms in s1?
(check-expect (contains-atoms? "a" "a") #true)
(check-expect (contains-atoms? "a" 1) #false)
(check-expect (contains-atoms? "a" '()) #false)
(check-expect (contains-atoms? "a" '(1 ("a"))) #true)
(check-expect (contains-atoms? '() "a") #true)
(check-expect (contains-atoms? '("a") "a") #true)
(check-expect (contains-atoms? '(1 2 "b") '(1 ("b"))) #false)
(check-expect (contains-atoms? '(1 "b") '("b" (1 2))) #true)
(check-expect (contains-atoms? '(3) '("a" "b")) #false)
(check-expect (contains-atoms? '((()) (() 1)) 1) #true)
(define (contains-atoms? s1 s2)
  (cond [(and (atom? s1) (atom? s2)) (atom=? s1 s2)]
        [(and (atom? s1) (list? s2)) (atom-in-list? s1 s2)]
        [(list? s1)
         (contains-list? s1 s2)]))

;; contains-list?: [List-of SExp] SExp -> Boolean
;; does the SExp s contains all the atoms on the list los?
(check-expect (contains-list? '() "a") #true)
(check-expect (contains-list? '(() "a") "a") #true)
(check-expect (contains-list? '(b) "a") #false)
(check-expect (contains-list? '("a" "b") "a") #false)
(check-expect (contains-list? '() '(a b)) #true)
(check-expect (contains-list? '(a) '()) #false)
(check-expect (contains-list? '(() ()) '(a)) #true)
(define (contains-list? los s)
  (cond [(empty? los) #true]
        [(cons? los)
         (and (contains-atoms? (first los) s)
              (contains-list? (rest los) s))]))

;; atom=?: Atom Atom -> Boolean
;; are atom a and b equal?
(check-expect (atom=? "a" "a") #true)
(check-expect (atom=? "a" "b") #false)
(check-expect (atom=? "a" 'a) #false)
(check-expect (atom=? 'a 'a) #true)
(check-expect (atom=? 'a 1) #false)
(check-expect (atom=? 'a 'b) #false)
(check-expect (atom=? 1 1) #true)
(check-expect (atom=? 1 2) #false)
(check-expect (atom=? 1 "a") #false)
(define (atom=? a1 a2)
  (cond [(symbol? a1) (and (symbol? a2) (symbol=? a1 a2))]
        [(string? a1) (and (string? a2) (string=? a1 a2))]
        [(number? a1) (and (number? a2) (= a1 a2))]))

;; atom-in-list? Atom [List-of SExpression] -> Boolean
;; is the atom an-atom in the given list of SExpression los
(check-expect (atom-in-list? "a" '()) #false)
(check-expect (atom-in-list? "r" '(1 2 3 () ("r" b))) #true)
(check-expect (atom-in-list? "c" '(1 2 3 () ("r" b))) #false)
(define (atom-in-list? an-atom los)
  (ormap (λ (sexp) (contains-atoms? an-atom sexp)) los))

;;;;;;;;;;;;;;;;; LEGO!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define LEGO-HEIGHT 10)
(define-struct lego (label color width))
;; A Lego is a structure:
;; (make-lego Number Symbol Number)
;; interpretation: (make-lego l c w) is the lego brick
;; with label l, color c, and width w (in pixels).
(define lego-1 (make-lego 1 'red 10))
(define lego-2 (make-lego 2 'blue 10))
(define lego-3 (make-lego 3 'black 10))
(define lego-4 (make-lego 4 'red 10))
(define lego-5 (make-lego 5 'red 10))
(define lego-6 (make-lego 6 'pink 10))
;; lego-temp: Lego -> ???
#; (define (lego-temp a-lego)
     ...(lego-label a-lego)
     ...(lego-color a-lego)
     ...(lego-width a-lego))

(define-struct bigger (lego left right))
;; A LegoBldg (lego building) is one of:
;; - Lego
;; - (make-bigger Lego LegoBldg LegoBldg)
;; interpretation: (make-bigger lg lft rgt) makes a bigger
;; lego building by putting a lego brick lg on top of two lego
;; buildings lft (left) and rgt (right).
(define legobldg-1 lego-1)
(define legobldg-2 (make-bigger lego-2 legobldg-1 legobldg-1))
(define legobldg-3 (make-bigger lego-3 legobldg-2 legobldg-1))
(define legobldg-4 (make-bigger lego-1 legobldg-2 legobldg-3))
(define legobldg-5 (make-bigger lego-3 (make-bigger lego-2 lego-1 lego-4)
                                lego-5))
(define legobldg-6 (make-bigger lego-3 (make-bigger lego-2 lego-1 lego-4)
                                lego-6))
;; legobldg-temp: LegoBldg -> ???
#; (define (legobldg-temp a-bldg)
     (cond [(lego? a-bldg) ...]
           [(bigger? a-bldg)
            (lego-temp (bigger-lego a-bldg))
            (legobldg-temp (bigger-left a-bldg))
            (legobldg-temp (bigger-right a-bldg))]))

;; A MaybeLego is one of:
;; - false
;; - Lego
;; maybe-lego-temp: MaybeLego -> ???
#; (define (maybe-lego-temp a-maybe)
     (cond [(lego? a-maybe) (lego-temp a-maybe)]
           [else ...]))

;; Problem 2
;; count-bricks: LegoBldg -> Number
;; counts the number of bricks in the LegoBldg a-bldg
(check-expect (count-bricks lego-1) 1)
(check-expect (count-bricks legobldg-1) 1)
(check-expect (count-bricks legobldg-2) 3)
(define (count-bricks a-bldg)
  (cond [(lego? a-bldg) 1]
        [(bigger? a-bldg)
         (+ 1
            (count-bricks (bigger-left a-bldg))
            (count-bricks (bigger-right a-bldg)))]))

;; Problem 3
;; how-high: LegoBldg -> PosInt
;; produces the total height of the lego building
(check-expect (how-high lego-1) 10)
(check-expect (how-high legobldg-2) 20)
(check-expect (how-high legobldg-3) 30)
(check-expect (how-high legobldg-4) 40)
(define (how-high a-bldg)
  (* LEGO-HEIGHT (bldg-depth a-bldg)))

;; bldg-depth: LegoBldg -> PosInt
;; returns the maximum depth of a lego building
(check-expect (bldg-depth lego-1) 1)
(check-expect (bldg-depth legobldg-2) 2)
(check-expect (bldg-depth legobldg-3) 3)
(check-expect (bldg-depth legobldg-4) 4)
(define (bldg-depth a-bldg)
  (cond [(lego? a-bldg) 1]
        [else
         (+ 1 (max (bldg-depth (bigger-left a-bldg))
                   (bldg-depth (bigger-right a-bldg))))]))

;; Problem 4
;; contains-colored-brick?: LegoBldg Symbol -> Boolean
;; is the color a-color in the lego building a-bldg?
(check-expect (contains-colored-brick? lego-1 'red) #true)
(check-expect (contains-colored-brick? lego-1 'blue) #false)
(check-expect (contains-colored-brick? legobldg-2 'red) #true)
(check-expect (contains-colored-brick? legobldg-2 'pink) #false)
(define (contains-colored-brick? a-bldg a-color)
  (cond [(lego? a-bldg) (color=? a-bldg a-color)]
        [else
         (or (color=? (bigger-lego a-bldg) a-color)
             (contains-colored-brick? (bigger-left a-bldg) a-color)
             (contains-colored-brick? (bigger-right a-bldg) a-color))]))

;; color=?: Lego Symbol -> Boolean
;; does the lego a-lego have the color c?
(check-expect (color=? lego-1 'pink) #false)
(check-expect (color=? lego-1 'red) #true)
(define (color=? a-lego c)
  (symbol=? (lego-color a-lego) c))


;; Problem 5
;; find-colored-brick?: LegoBldg Symbol -> MaybeLego
;; finds any lego with the given color a-color, and #false otherwise
(check-expect (find-colored-brick? legobldg-1 'red) lego-1)
(check-expect (find-colored-brick? legobldg-1 'pink) #false)
(check-expect (find-colored-brick? legobldg-3 'black) lego-3)
(check-expect (find-colored-brick? legobldg-3 'blue) lego-2)
(check-expect (find-colored-brick? legobldg-6 'pink) lego-6)
(check-expect (find-colored-brick? legobldg-6 'yellow) #false)
(check-expect (find-colored-brick? legobldg-5 'red) lego-1)
(define (find-colored-brick? a-bldg a-color)
  (cond [(lego? a-bldg) (colored-brick? a-bldg a-color)]
        [else
         (cond [(color=? (bigger-lego a-bldg) a-color) (bigger-lego a-bldg)]
               [(lego? (find-colored-brick? (bigger-left a-bldg) a-color))
                (find-colored-brick? (bigger-left a-bldg) a-color)]
               [(lego? (find-colored-brick? (bigger-right a-bldg) a-color))
                (find-colored-brick? (bigger-right a-bldg) a-color)]
               [else #false])]))

;; colored-brick?: Lego Symbol -> MaybeLego
(check-expect (colored-brick? lego-1 'red) lego-1)
(check-expect (colored-brick? lego-1 'blue) #false)
(define (colored-brick? a-lego a-color)
  (if (color=? a-lego a-color) a-lego #false))

;; Priblem 6
;; lb->image: LegoBldg -> Image
;; draws a lego building
(check-expect (lb->image legobldg-1) (lego->image lego-1))
(check-expect (lb->image (make-bigger (make-lego 4 'purple 80)
                                      (make-bigger (make-lego 2 'blue 60)
                                                   (make-lego 1 'yellow 40)
                                                   (make-lego 3 'red 40))
                                      (make-bigger (make-lego 6 'orange 60)
                                                   (make-lego 5 'green 40)
                                                   (make-lego 7 'red 40))))
              (above (rectangle 80 10 'solid 'purple)
                     (beside/align
                      "top"
                      (above (rectangle 60 10 'solid 'blue)
                             (beside/align "top"
                                           (rectangle 40 10 'solid 'yellow)
                                           (rectangle 40 10 'solid 'red)))
                      (above (rectangle 60 10 'solid 'orange)
                             (beside/align "top"
                                           (rectangle 40 10 'solid 'green)
                                           (rectangle 40 10 'solid 'red))))))
(check-expect (lb->image (make-bigger (make-lego 4 'purple 80)
                                      (make-bigger (make-lego 2 'blue 60)
                                                   (make-lego 1 'yellow 40)
                                                   (make-lego 3 'red 40))
                                      (make-lego 6 'orange 60)))
              (above (rectangle 80 10 'solid 'purple)
                     (beside/align
                      "top"
                      (above (rectangle 60 10 'solid 'blue)
                             (beside/align
                              "top"
                              (rectangle 40 10 'solid 'yellow)
                              (rectangle 40 10 'solid 'red)))
                      (rectangle 60 10 'solid 'orange))))
(define (lb->image a-bldg)
  (cond [(lego? a-bldg) (lego->image a-bldg)]
        [else
         (above (lego->image (bigger-lego a-bldg))
                (beside/align "top"
                              (lb->image (bigger-left a-bldg))
                              (lb->image (bigger-right a-bldg))))]))
                        
;; lego->image: Lego -> Image
;; draws a lego
(check-expect (lego->image lego-1) (rectangle 10 10 'solid 'red))
(define (lego->image a-lego)
  (rectangle (lego-width a-lego) LEGO-HEIGHT 'solid (lego-color a-lego)))


