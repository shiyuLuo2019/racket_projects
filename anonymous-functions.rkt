(require 2htdp/image)
(require 2htdp/universe)
; A Ball is a (make-ball Nat Mode Color [Nat -> Posn])
(define-struct ball (r mode color placement))
; - where r is the ball's radius
; - mode is the ball's mode
; - color is the ball's color
; - and placement is a function that, given the current time,
; outputs a new coordinate for the ball to be drawn at
; A Mode is one of:
; - 'solid
; - 'outline
(define HEIGHT 500)
(define WIDTH 500)
(define BALL-1 (make-ball 5 'solid 'red (λ (t) (make-posn
                                                20 (modulo t HEIGHT)))))
(define BALL-2 (make-ball 7 'outline 'blue (λ (t) (make-posn
                                                   (modulo t WIDTH) 100))))

; ball-temp : Ball -> ???
#;(define (ball-temp b)
    (... (ball-r b) ... (mode-temp (ball-mode b)) ...
         (ball-color b) ... (ball-placement b) ...))
; mode-temp : Mode -> ???
#;(define (mode-temp m)
    (... (cond [(symbol=? m 'solid) ...]
               [(symbol=? m 'outline) ...]) ...))

; A World is a (make-world Nat [List-of Ball])
(define-struct world (t balls))
; - where t is the amount of time that has passed
; - and balls is the balls of the world

(define WORLD-1 (make-world 0 '()))
(define WORLD-2 (make-world 10 (list BALL-1 BALL-2)))


; world-temp : World -> ???
#;(define (world-temp w)
    (... (world-t w) ... (ball-list-temp (world-balls w)) ...))
; ball-list-temp : [List-of Ball] -> ???
#;(define (ball-list-temp alob)
    (... (cond [(empty? alob) ...]
               [(cons? alob)
                ... (ball-temp (first alob)) ...
                ... (ball-list-temp (rest alob)) ...]) ...))

; main : [List-of Ball] -> World
; Run this game with this list of initial balls
(define (main init-list)
    (big-bang (make-world 0 init-list)
              [on-tick tick]
              [to-draw draw]
              [on-mouse place-ball]))

; tick: World -> World
; increments the world time by 1
(check-expect (tick WORLD-1) (make-world 1 '()))
(check-expect (tick WORLD-2) (make-world 11 (list BALL-1 BALL-2)))
(define (tick w)
  (make-world (add1 (world-t w)) (world-balls w)))

; draw-ball: Ball Posn Image -> Image
; draws that ball onto the image
(define MT (empty-scene WIDTH HEIGHT))
(check-expect (draw-ball BALL-1 (make-posn 250 250) MT)
              (place-image (circle 5 'solid 'red) 250 250 MT))
(check-expect (draw-ball BALL-2 (make-posn 100 100) MT)
              (place-image (circle 7 'outline 'blue) 100 100 MT))
(define (draw-ball a-ball a-posn img)
  (place-image (circle (ball-r a-ball) (ball-mode a-ball) (ball-color a-ball))
               (posn-x a-posn) (posn-y a-posn) img))

;; make-drawer: Nat -> [Ball Image -> Image]
;; Creates a function that will takes a ball and an image and will draw it
(check-expect ((make-drawer 0) BALL-1 MT)
              (place-image (circle 5 'solid 'red) 20 0 MT))
(check-expect ((make-drawer 100) BALL-2 MT)
              (place-image (circle 7 'outline 'blue) 100 100 MT))
(define (make-drawer t)
  (λ (a-ball img) (draw-ball a-ball ((ball-placement a-ball) t) img)))

;; draw: World -> Image
;; draws the current world
(check-expect (draw WORLD-1) MT)
(check-expect (draw WORLD-2)
              (place-image (circle 5 'solid 'red) 20 10
                           (place-image (circle 7 'outline 'blue)
                                        10 100 MT)))
(define (draw w)
  (foldr (make-drawer (world-t w)) MT (world-balls w)))

; A BallGenerator is a [Nat Nat Nat -> [Nat -> Posn]]
; Given the time, x-coordinate, and y-coordinate of when and where a
; ball is created, create a function that, given the current time of
; the world, will output a Posn
; Example:
; move-horizontally : BallGenerator
(define (move-horizontally t0 x0 y0)
  (λ (t) (make-posn (modulo (+ x0 (- t t0)) WIDTH) y0)))
(check-expect ((move-horizontally 3 5 8) 10)
              (make-posn 12 8))

; move-vertically: Nat Nat Nat -> [Nat -> Posn]
(check-expect ((move-vertically 0 10 20) 10) (make-posn 10 30))
(check-expect ((move-vertically 0 10 499) 1) (make-posn 10 0))
(define (move-vertically t0 x0 y0)
  (λ (t) (make-posn x0 (modulo (+ (- t t0) y0) HEIGHT))))

(define GENERATORS (list move-horizontally move-vertically))

; place-ball: World Nat Nat MouseEvent -> World
; adds a ball at (x0, y0) if the user clicked
(define (place-ball w x0 y0 mouse)
  (if (string=? mouse "button-down")
      (make-world (world-t w)
                   (cons (make-ball (+ (random 49) 1)
                                    (select-random '(solid outline))
                                    (make-color (random 255) (random 255) (random 255))
                                    ((select-random GENERATORS) (world-t w) x0 y0))
                         (world-balls w)))
      w))

; select-random: [NeList-of ITEM] -> ITEM
; select randomly an item from a nonempty list
(check-member-of (((select-random GENERATORS) 0 0 0) 10)
                 (make-posn 10 0)
                 (make-posn 0 10))
(define (select-random l)
    (list-ref l (random (length l))))

