;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname frogger) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
;;;;; Frogger

#|
TABLE OF CONTENTS
1 Constants
   1.1 Constants
      1.1.1 constants for lanes and rivers
      1.1.2 constants for the frog
      1.1.3 constants for vehicles
      1.1.4 constants for turtles
      1.1.5 constants for planks
   1.2 Images
      1.2.1 images of lanes
      1.2.2 image of rivers
      1.2.3 image of goal
      1.2.4 images of background
      1.2.5 frog images
      1.2.6 vehicle images
      1.2.7 plank image
      1.2.8 turtle image
      1.2.9 win or lose images
   1.3 Termination Points
2 Data Definitions
   2.1 Itemizations
   2.2 Structures
   2.3 [List-of]
   2.4 World Definition
3 Main Function
4 Event Handlers for main function
   4.1 To-draw function
   4.2 On-tick handler
   4.3 On-key handler
   4.4 Stop-when functions
|#

;;;; 1. Constants

;;; 1.1 Constants

(define WIDTH 500)
(define HEIGHT 650)
;; 1.1.1 constants for lanes and rivers
; numbers of lanes and rivers
; lane 1 and lane 7 are safe zones for the frog
; when the frog reaches goal the player wins
(define LANE-NUM 7)
(define RIVER-NUM 5)
(define GOAL 1)
; DIVISION-HEIGHT is the height of each lane and river
; lanes and rivers are of equal height
(define DIVISION-HEIGHT (/ HEIGHT (+ LANE-NUM RIVER-NUM GOAL)))
; LANEx-CENTER is the y-coordinate of the center of each lane
(define LANE1-CENTER (- HEIGHT (/ DIVISION-HEIGHT 2)))
(define LANE2-CENTER (- LANE1-CENTER DIVISION-HEIGHT))
(define LANE3-CENTER (- LANE2-CENTER DIVISION-HEIGHT))
(define LANE4-CENTER (- LANE3-CENTER DIVISION-HEIGHT))
(define LANE5-CENTER (- LANE4-CENTER DIVISION-HEIGHT))
(define LANE6-CENTER (- LANE5-CENTER DIVISION-HEIGHT))
(define LANE7-CENTER (- LANE6-CENTER DIVISION-HEIGHT))
; LANE-X is the x-coordinate of the center of each lane
(define LANE-X (/ WIDTH 2))
; RIVERx-CENTER is the y-coordinate of the center of each river
(define RIVER1-CENTER (- LANE7-CENTER DIVISION-HEIGHT))
(define RIVER2-CENTER (- RIVER1-CENTER DIVISION-HEIGHT))
(define RIVER3-CENTER (- RIVER2-CENTER DIVISION-HEIGHT))
(define RIVER4-CENTER (- RIVER3-CENTER DIVISION-HEIGHT))
(define RIVER5-CENTER (- RIVER4-CENTER DIVISION-HEIGHT))
; RIVER-X is the x-coordinate of the center of each river
(define RIVER-X LANE-X)
; GOAL-Y is the y-coordinate of the center of goal
(define GOAL-Y (/ DIVISION-HEIGHT 2))
; GOAL-X is the x-coordinate of the center of goal
(define GOAL-X LANE-X)
; list of positions of lanes, rivers and goal
(define list-of-posns
  (list (make-posn LANE-X LANE1-CENTER)
        (make-posn LANE-X LANE2-CENTER)
        (make-posn LANE-X LANE3-CENTER)
        (make-posn LANE-X LANE4-CENTER)
        (make-posn LANE-X LANE5-CENTER)
        (make-posn LANE-X LANE6-CENTER)
        (make-posn LANE-X LANE7-CENTER)
        (make-posn RIVER-X RIVER1-CENTER)
        (make-posn RIVER-X RIVER2-CENTER)
        (make-posn RIVER-X RIVER3-CENTER)
        (make-posn RIVER-X RIVER4-CENTER)
        (make-posn RIVER-X RIVER5-CENTER)
        (make-posn GOAL-X GOAL-Y)))
;; 1.1.2 constants for the frog
(define FROG-BODY-SIZE (/ DIVISION-HEIGHT 3))
(define FROG-EYE-SIZE (/ FROG-BODY-SIZE 5))
; FROG-STEP is the length of step of each jump the frog makes
(define FROG-STEP DIVISION-HEIGHT)
;; 1.1.3 constants for vehicles
(define VEHICLE-BODY-WIDTH (* DIVISION-HEIGHT 1))
(define VEHICLE-BODY-HEIGHT (/ VEHICLE-BODY-WIDTH 3))
(define VEHICLE-WHEEL-SIZE (/ VEHICLE-BODY-WIDTH 8))
; SPEED is the distance each vehicle moves on every tick
(define SPEED (/ VEHICLE-BODY-WIDTH 50))
; VEHICLE-SPACE is the space between two adjacent vehicles
(define VEHICLE-SPACE (* FROG-BODY-SIZE 3))
;; 1.1.4 constants for turtles
(define TURTLE-RADIUS (/ DIVISION-HEIGHT 2))
; TURTLE-SPACE is the space between two adjacent turtles
; here the frog is NOT allowed to jump between two adjacent turtles
; or it will fall into river
(define TURTLE-SPACE (* FROG-BODY-SIZE 2))
(define TSPEED SPEED)
;; 1.1.5 constants for planks
(define PLANK-WIDTH (* 1.6 VEHICLE-BODY-WIDTH))
(define PLANK-HEIGHT VEHICLE-BODY-HEIGHT)
(define PSPEED SPEED)
; PLANK-SPACE is the space between two adjacent planks
(define PLANK-SPACE (* 1.5 VEHICLE-SPACE))

;;; 1.2 Images

;; 1.2.1 images of lanes
; LANE1-IMAGE is the image of lane 1 (safe zone)
(define LANE1-IMAGE (rectangle WIDTH DIVISION-HEIGHT "solid" "MediumPurple"))
; LANE7-IMAGE is the image of lane 7 (safe zone)
(define LANE7-IMAGE (rectangle WIDTH DIVISION-HEIGHT "solid" "LightPink"))
; LANE2-6-IMAGE is the image of the lane where vehicles move
(define LANE2-6-IMAGE (rectangle WIDTH DIVISION-HEIGHT "solid" "Gainsboro"))
;; 1.2.2 image of rivers
(define RIVER-IMAGE (rectangle WIDTH DIVISION-HEIGHT "solid" "PowderBlue"))
;; 1.2.3 image of goal
(define GOAL-IMAGE (place-image
                    (text/font "GOAL" 30 "LightPink" #f 'system 'slant 'bold #f)
                    LANE-X (/ DIVISION-HEIGHT 2)
                    (rectangle WIDTH DIVISION-HEIGHT "solid" "MistyRose")))
; list of lanes, rivers and goal
(define list-of-lanes-rivers-goal
  (append (list LANE1-IMAGE)
          (make-list 5 LANE2-6-IMAGE)
          (list LANE7-IMAGE)
          (make-list 5 RIVER-IMAGE)
          (list GOAL-IMAGE)))
;; 1.2.4 images of background
; scene where the background is placed
(define SCENE (empty-scene WIDTH HEIGHT))
; background image
(define BACKGROUND
  (local
    [; place-image*: [List-of Image] [List-of Posns] Image -> [List-of Image]
     ; places images in loi to img according to lop
     ; assume loi and lop have equal length
     (define (place-image* loi lop img)
       (cond [(empty? loi) img]
             [(cons? loi) (place-image
                           (first loi)
                           (posn-x (first lop)) (posn-y (first lop))
                           (place-image* (rest loi) (rest lop) img))]))]
    (place-image* list-of-lanes-rivers-goal list-of-posns SCENE)))
;; 1.2.5 frog images
(define FROG-BODY (circle FROG-BODY-SIZE "solid" "olive"))
(define FROG-EYE (circle FROG-EYE-SIZE "solid" "black"))
(define FROG-EYES (overlay/xy FROG-EYE
                              (/ FROG-BODY-SIZE 1.5) 0
                              FROG-EYE))
(define FROG-IMAGE (overlay/xy FROG-EYES
                               (* FROG-EYE-SIZE -2) FROG-EYE-SIZE
                               FROG-BODY))

;; 1.2.6 vehicle images
(define VEHICLE-WHEEL (circle VEHICLE-WHEEL-SIZE "solid" "SlateGray"))
(define VEHICLE-BODY (rectangle VEHICLE-BODY-WIDTH VEHICLE-BODY-HEIGHT "solid"
                                "HotPink"))
(define WHEELS (overlay/xy VEHICLE-WHEEL
                           (- VEHICLE-BODY-WIDTH (* VEHICLE-WHEEL-SIZE 2)) 0
                           VEHICLE-WHEEL))
(define VEHICLE-IMAGE (overlay/xy WHEELS
                                  0 (* VEHICLE-WHEEL-SIZE -2)
                                  VEHICLE-BODY))
;; 1.2.7 plank image
(define PLANK-IMAGE (rectangle PLANK-WIDTH PLANK-HEIGHT "solid" "Sienna"))
;; 1.2.8 turtle image
(define TURTLE-IMAGE (circle TURTLE-RADIUS "solid" "Coral"))

;; 1.2.9 win or lose images
(define WIN-IMAGE (text/font "YOU WIN" 50 "red" #f 'system 'normal'bold #f))
(define LOSE-IMAGE (text/font "YOU LOSE" 50 "red" #f 'system 'normal 'bold #f))

;;; 1.3 Termination points

;; Minimum distance between the player and a vehicle before collision
(define MIN-PLAYER-VEHICLE-DISTANCE
  (/ (+ (image-width VEHICLE-IMAGE) (image-width FROG-IMAGE)) 2.5))
;; Maximum distance between the center of the player and a turtle
;; so that the frog can ride on the turtle
(define MAX-PLAYER-TURTLE-DISTANCE (floor (* (image-width FROG-IMAGE) 0.8)))
;; Maximum distance between the center of the player and a plank
;; so that the frog can ride on the plank
(define MAX-PLAYER-PLANK-DISTANCE (/ PLANK-WIDTH 1.6))


;;;; 2. Data Definitions

;;; 2.1 Itemizations

;; A Direction is one of:
;; - "up"
;; - "down"
;; - "left"
;; - "right"

;; An Obstacle is one of:
;; - Vehicle
;; - Plank
;; - Turtle

;;; 2.2 Structures

(define-struct player (x y dir))
;; A Player is a (make-player Integer Integer Direction)
;; INTERP: represents the coordinate of the player on the scene
;; and the direction in which the player is moving
(define player0 (make-player (/ WIDTH 2) LANE1-CENTER "up"))

(define-struct vehicle (x y dir))
;; A Vehicle is a (make-vehicle Integer Integer Direction)
;; INTERP: represents the coordinate of a vehicle on the scene
;; and the direction in which the vehicle is moving
;; vehicles on lane 2
(define vehicle2-1 (make-vehicle (/ WIDTH 10) LANE2-CENTER "left"))
(define vehicle2-2 (make-vehicle (+ (vehicle-x vehicle2-1) VEHICLE-BODY-WIDTH
                                    VEHICLE-SPACE) LANE2-CENTER "left"))
(define vehicle2-3 (make-vehicle (+ (vehicle-x vehicle2-2) VEHICLE-BODY-WIDTH
                                    VEHICLE-SPACE) LANE2-CENTER "left"))
(define vehicle2-4 (make-vehicle (+ (vehicle-x vehicle2-3) VEHICLE-BODY-WIDTH
                                    VEHICLE-SPACE) LANE2-CENTER "left"))
;; vehicles on lane 3
(define vehicle3-1 (make-vehicle (vehicle-x vehicle2-1) LANE3-CENTER "right"))
(define vehicle3-2 (make-vehicle (vehicle-x vehicle2-2) LANE3-CENTER "right"))
(define vehicle3-3 (make-vehicle (vehicle-x vehicle2-3) LANE3-CENTER "right"))
(define vehicle3-4 (make-vehicle (vehicle-x vehicle2-4) LANE3-CENTER "right"))
;; vehicles on lane 4
(define vehicle4-1 (make-vehicle (vehicle-x vehicle2-1) LANE4-CENTER "left"))
(define vehicle4-2 (make-vehicle (vehicle-x vehicle2-2) LANE4-CENTER "left"))
(define vehicle4-3 (make-vehicle (vehicle-x vehicle2-3) LANE4-CENTER "left"))
(define vehicle4-4 (make-vehicle (vehicle-x vehicle2-4) LANE4-CENTER "left"))
;; vehicles on lane 5
(define vehicle5-1 (make-vehicle (vehicle-x vehicle2-1) LANE5-CENTER "right"))
(define vehicle5-2 (make-vehicle (vehicle-x vehicle2-2) LANE5-CENTER "right"))
(define vehicle5-3 (make-vehicle (vehicle-x vehicle2-3) LANE5-CENTER "right"))
(define vehicle5-4 (make-vehicle (vehicle-x vehicle2-4) LANE5-CENTER "right"))
;;vehicles on lane 6
(define vehicle6-1 (make-vehicle (vehicle-x vehicle2-1) LANE6-CENTER "left"))
(define vehicle6-2 (make-vehicle (vehicle-x vehicle2-2) LANE6-CENTER "left"))
(define vehicle6-3 (make-vehicle (vehicle-x vehicle2-3) LANE6-CENTER "left"))
(define vehicle6-4 (make-vehicle (vehicle-x vehicle2-4) LANE6-CENTER "left"))

(define-struct plank (x y))
;; A Plank is a (make-plank Integer Integer)
;; INTERP: represents a plank always moving right
;; planks are on river 1, 3 and 5, 3 planks for each river
;; planks on river 1
(define plank1-1 (make-plank 70 RIVER1-CENTER))
(define plank1-2 (make-plank (+ 70 PLANK-WIDTH PLANK-SPACE) RIVER1-CENTER))
(define plank1-3 (make-plank (+ (plank-x plank1-2) PLANK-WIDTH PLANK-SPACE)
                             RIVER1-CENTER))
;; planks on river 3
(define plank3-1 (make-plank 90 RIVER3-CENTER))
(define plank3-2 (make-plank (+ 90 PLANK-WIDTH PLANK-SPACE) RIVER3-CENTER))
(define plank3-3 (make-plank (+ (plank-x plank3-2) PLANK-WIDTH PLANK-SPACE)
                             RIVER3-CENTER))
;; planks on river 5
(define plank5-1 (make-plank (plank-x plank1-1) RIVER5-CENTER))
(define plank5-2 (make-plank (plank-x plank1-2) RIVER5-CENTER))
(define plank5-3 (make-plank (plank-x plank1-3) RIVER5-CENTER))

(define-struct turtle (x y))
;; A Turtle is a (make-turtle Integer Integer)
;; INTERP: represents a turtle always moving left
;; turtles are on river 2 and 4, 3 turtles each river
;; turtles on river 2
(define turtle2-1 (make-turtle 60 RIVER2-CENTER))
(define turtle2-2 (make-turtle (+ 60 (* 2 TURTLE-RADIUS) TURTLE-SPACE)
                               RIVER2-CENTER))
(define turtle2-3 (make-turtle (+ (turtle-x turtle2-2) (* 2 TURTLE-RADIUS)
                                  TURTLE-SPACE) RIVER2-CENTER))
;; turtles on river 4
(define turtle4-1 (make-turtle (turtle-x turtle2-1) RIVER4-CENTER))
(define turtle4-2 (make-turtle (turtle-x turtle2-2) RIVER4-CENTER))
(define turtle4-3 (make-turtle (turtle-x turtle2-3) RIVER4-CENTER))

;;; 2.3 [List-of]

;; A VSet is a [List-of Vehicle]
;; INTERP: stores all vehicles into a list
(define vset0 (list vehicle2-1 vehicle2-2 vehicle2-3 vehicle2-4
                    vehicle3-1 vehicle3-2 vehicle3-3 vehicle3-4
                    vehicle4-1 vehicle4-2 vehicle4-3 vehicle4-4
                    vehicle5-1 vehicle5-2 vehicle5-3 vehicle5-4
                    vehicle6-1 vehicle6-2 vehicle6-3 vehicle6-4))

;; A PSet is a [List-of Plank]
;; INTERP: stores all planks into a list
(define pset0 (list plank1-1 plank1-2 plank1-3
                    plank3-1 plank3-2 plank3-3
                    plank5-1 plank5-2 plank5-3))

;; A TSet is a [List-of Turtle]
;; INTERP: stores all turtles into a list
(define tset0 (list turtle2-1 turtle2-2 turtle2-3
                    turtle4-1 turtle4-2 turtle4-3))

;;; 2.4 World definition

;; A World is (make-world Player VSet PSet TSet)
;; INTERP: represents the state of the world
(define-struct world (player vset pset tset))
(define world0 (make-world player0 vset0 pset0 tset0))



;;;; 3. Main function

;; main: World -> World
;; launches the frogger game
(define (main w)
  (big-bang w
            [to-draw draw-world]
            [on-tick move]
            [on-key move-player]
            [stop-when end-game? last-picture]
            ))
;;;; 4. Event handlers for main function

;;; 4.1 To-draw handler

;; draw-world: World -> Image
(check-expect (draw-world (make-world (make-player 250 450 "up")
                                      (list (make-vehicle 100 100 "left"))
                                      (list (make-plank 300 300))
                                      (list (make-turtle 400 400))))
              (place-image
               FROG-IMAGE 250 450
               (place-image
                VEHICLE-IMAGE 100 100
                (place-image
                 PLANK-IMAGE 300 300
                 (place-image
                  TURTLE-IMAGE 400 400 BACKGROUND)))))
(check-expect (draw-world (make-world (make-player 250 450 "right")
                                      (list (make-vehicle 100 100 "left"))
                                      (list (make-plank 300 300))
                                      (list (make-turtle 400 400))))
              (place-image
               (rotate 270 FROG-IMAGE) 250 450
               (place-image
                VEHICLE-IMAGE 100 100
                (place-image
                 PLANK-IMAGE 300 300
                 (place-image
                  TURTLE-IMAGE 400 400 BACKGROUND)))))
(check-expect (draw-world (make-world (make-player 250 450 "down")
                                      (list (make-vehicle 100 100 "left"))
                                      (list (make-plank 300 300))
                                      (list (make-turtle 400 400))))
              (place-image
               (rotate 180 FROG-IMAGE) 250 450
               (place-image
                VEHICLE-IMAGE 100 100
                (place-image
                 PLANK-IMAGE 300 300
                 (place-image
                  TURTLE-IMAGE 400 400 BACKGROUND)))))
(check-expect (draw-world (make-world (make-player 250 450 "left")
                                      (list (make-vehicle 100 100 "left"))
                                      (list (make-plank 300 300))
                                      (list (make-turtle 400 400))))
              (place-image
               (rotate 90 FROG-IMAGE) 250 450
               (place-image
                VEHICLE-IMAGE 100 100
                (place-image
                 PLANK-IMAGE 300 300
                 (place-image
                  TURTLE-IMAGE 400 400 BACKGROUND)))))
                            
(define (draw-world w)
  (draw-player (world-player w)
               (draw-obstacles (world-vset w) (world-pset w) (world-tset w))))

;; draw-player: Player, Image -> Image
;; given a Player p and an image img, draw p onto img
(check-expect (draw-player player0 BACKGROUND)
              (place-image FROG-IMAGE 250 625 BACKGROUND))
(check-expect (draw-player (make-player 250 625 "left") BACKGROUND)
              (place-image (rotate 90 FROG-IMAGE) 250 625 BACKGROUND))
(check-expect (draw-player (make-player 250 625 "down") BACKGROUND)
              (place-image (rotate 180 FROG-IMAGE) 250 625 BACKGROUND))
(check-expect (draw-player (make-player 250 625 "right") BACKGROUND)
              (place-image (rotate 270 FROG-IMAGE) 250 625 BACKGROUND))
(define (draw-player p img)
  (place-image
   (local [(define frog-image-with-direction
             (cond [(string=? (player-dir p) "up") FROG-IMAGE]
                   [(string=? (player-dir p) "right")
                    (rotate 270 FROG-IMAGE)]
                   [(string=? (player-dir p) "down")
                    (rotate 180 FROG-IMAGE)]
                   [(string=? (player-dir p) "left")
                    (rotate 90 FROG-IMAGE)]))]
     frog-image-with-direction)
   (player-x p) (player-y p) img))

;; draw-obstacles: VSet PSet TSet -> Image
;; draws v, p, and t onto the background
(check-expect (draw-obstacles '() '() '()) BACKGROUND)
(check-expect (draw-obstacles '() '() (list (make-turtle 375 300)))
              (place-image TURTLE-IMAGE 375 300 BACKGROUND))
(check-expect (draw-obstacles '()  (list (make-plank 300 300)) '())
              (place-image PLANK-IMAGE 300 300 BACKGROUND))
(check-expect (draw-obstacles '()  (list (make-plank 300 300))
                              (list (make-turtle 375 300)))
              (place-image PLANK-IMAGE 300 300
                           (place-image TURTLE-IMAGE 375 300 BACKGROUND)))
(check-expect (draw-obstacles (list (make-vehicle 125 585 "right")) '() '())
              (place-image VEHICLE-IMAGE 125 585 BACKGROUND))
(check-expect (draw-obstacles (list (make-vehicle 125 585 "right")) '()
                              (list (make-turtle 375 300)))
              (place-image VEHICLE-IMAGE 125 585
                           (place-image TURTLE-IMAGE 375 300 BACKGROUND)))
(check-expect (draw-obstacles (list (make-vehicle 125 585 "right"))
                              (list (make-plank 300 300)) '())
              (place-image
               VEHICLE-IMAGE 125 585
               (place-image
                PLANK-IMAGE 300 300 BACKGROUND)))
(check-expect (draw-obstacles (list (make-vehicle 125 585 "right"))
                              (list (make-plank 300 300))
                              (list (make-turtle 375 300)))
              (place-image
               VEHICLE-IMAGE 125 585
               (place-image
                PLANK-IMAGE 300 300
                (place-image
                 TURTLE-IMAGE 375 300 BACKGROUND))))
(define (draw-obstacles v p t)
  (local [; draw-an-obstacle: Obstacle Image -> Image
          ; draws obs onto img
          (define (draw-an-obstacle obs img)
            (cond [(vehicle? obs)
                   (place-image VEHICLE-IMAGE
                                (vehicle-x obs) (vehicle-y obs) img)]
                  [(plank? obs)
                   (place-image PLANK-IMAGE (plank-x obs) (plank-y obs) img)]
                  [else
                   (place-image
                    TURTLE-IMAGE (turtle-x obs) (turtle-y obs) img)]))]
    (foldr draw-an-obstacle BACKGROUND (append v p t))))


;;; 4.2 On-tick handler

;; move: World -> World
;; changes w every time move is called
(check-expect (move (make-world player0
                                (list (make-vehicle 135 300 "left")
                                      (make-vehicle 135 325 "right"))
                                (list (make-plank 135 275)
                                      (make-plank 135 250))
                                (list (make-turtle 135 225)
                                      (make-turtle 135 200))))
              (make-world player0
                          (list (make-vehicle 134 300 "left")
                                (make-vehicle 136 325 "right"))
                          (list (make-plank 136 275)
                                (make-plank 136 250))
                          (list (make-turtle 134 225)
                                (make-turtle 134 200))))
(check-expect (move (make-world player0
                                (list (make-vehicle -25 100 "left")
                                      (make-vehicle 135 325 "right"))
                                (list (make-plank 135 275)
                                      (make-plank 135 250))
                                (list (make-turtle 135 225)
                                      (make-turtle 135 200))))
              (make-world player0
                          (list (make-vehicle 524 100 "left")
                                (make-vehicle 136 325 "right"))
                          (list (make-plank 136 275)
                                (make-plank 136 250))
                          (list (make-turtle 134 225)
                                (make-turtle 134 200))))
(check-expect (move (make-world player0
                                (list (make-vehicle 135 300 "left")
                                      (make-vehicle 525 100 "right"))
                                (list (make-plank 135 275)
                                      (make-plank 135 250))
                                (list (make-turtle 135 225)
                                      (make-turtle 135 200))))
              (make-world player0
                          (list (make-vehicle 134 300 "left")
                                (make-vehicle -24 100 "right"))
                          (list (make-plank 136 275)
                                (make-plank 136 250))
                          (list (make-turtle 134 225)
                                (make-turtle 134 200))))
(check-expect (move (make-world player0
                                (list (make-vehicle 135 300 "left")
                                      (make-vehicle 135 325 "right"))
                                (list (make-plank 135 275)
                                      (make-plank 540 250))
                                (list (make-turtle 135 225)
                                      (make-turtle 135 200))))
              (make-world player0
                          (list (make-vehicle 134 300 "left")
                                (make-vehicle 136 325 "right"))
                          (list (make-plank 136 275)
                                (make-plank -39 250))
                          (list (make-turtle 134 225)
                                (make-turtle 134 200))))
(check-expect (move (make-world player0
                                (list (make-vehicle 135 300 "left")
                                      (make-vehicle 135 325 "right"))
                                (list (make-plank 135 275)
                                      (make-plank 135 250))
                                (list (make-turtle -25 250)
                                      (make-turtle 135 200))))
              (make-world player0
                          (list (make-vehicle 134 300 "left")
                                (make-vehicle 136 325 "right"))
                          (list (make-plank 136 275)
                                (make-plank 136 250))
                          (list (make-turtle 524 250)
                                (make-turtle 134 200))))
(check-expect (move
               (make-world (make-player 70 275 "up")
                           (list (make-vehicle 50 475 "left")
                                 (make-vehicle 150 475 "left"))
                           (list (make-plank 70 275)
                                 (make-plank 100 275))
                           (list (make-turtle 60 225)
                                 (make-turtle 60 125))))
              (make-world (make-player 71 275 "up")
                          (list (make-vehicle 49 475 "left")
                                (make-vehicle 149 475 "left"))
                          (list (make-plank 71 275)
                                (make-plank 101 275))
                          (list (make-turtle 59 225)
                                (make-turtle 59 125))))
(check-expect (move
               (make-world (make-player 60 225 "up")
                           (list (make-vehicle 50 475 "left")
                                 (make-vehicle 150 475 "left"))
                           (list (make-plank 70 275)
                                 (make-plank 100 275))
                           (list (make-turtle 60 225)
                                 (make-turtle 60 125))))
              (make-world (make-player 59 225 "up")
                          (list (make-vehicle 49 475 "left")
                                (make-vehicle 149 475 "left"))
                          (list (make-plank 71 275)
                                (make-plank 101 275))
                          (list (make-turtle 59 225)
                                (make-turtle 59 125)))) 
(define (move w)
  (make-world (tick-player w)
              (move-vset (world-vset w))
              (move-pset (world-pset w))
              (move-tset (world-tset w))))

;; tick-player: World -> Player
;; moves the frog when it rides on a plank or a turtle in the direction
;; of what it rides on
(check-expect (tick-player world0) (world-player world0))
(check-expect (tick-player
               (make-world (make-player 50 375 "up") vset0 pset0 tset0))
              (make-player 50 375 "up"))
(check-expect (tick-player
               (make-world (make-player 225 275 "up") vset0 pset0 tset0))
              (make-player 226 275 "up"))
(check-expect (tick-player
               (make-world (make-player 60 125 "up") vset0 pset0 tset0))
              (make-player 59 125 "up"))
(define (tick-player w)
  (local [; move-with-obs: Player Direction -> Player
          ; moves player p in direction dir
          ; dir can only be "right" or "left" (as planks or turtles) 
          (define (move-with-obs p dir)
            (cond [(string=? dir "right") (make-player (+ PSPEED (player-x p))
                                                       (player-y p)
                                                       (player-dir p))]
                  [(string=? dir "left") (make-player (- (player-x p) TSPEED)
                                                      (player-y p)
                                                      (player-dir p))]))
          ; plank-river?: Player -> Boolean
          ; is the player p in river 1, 3 or 5?
          (define (plank-river? p)
            (or (= RIVER1-CENTER (player-y p))
                (= RIVER3-CENTER (player-y p))
                (= RIVER5-CENTER (player-y p))))
          ; turtle-river?: Player -> Boolean
          ; is the player p in river 2 or 4?
          (define (turtle-river? p)
            (or (= RIVER2-CENTER (player-y p))
                (= RIVER4-CENTER (player-y p))))]
    (cond [(and (plank-river? (world-player w))
                (on-plank? (world-player w) (world-pset w)))
           (move-with-obs
            (world-player w) "right")]
          [(and (turtle-river? (world-player w))
                (on-turtle? (world-player w) (world-tset w)))
           (move-with-obs (world-player w) "left")]
          [else (world-player w)])))

;; move-vset: VSet -> VSet
;; moves the vehicles in a-vset
(check-expect (move-vset '()) '())
(check-expect (move-vset (list (make-vehicle -25 100 "left")
                               (make-vehicle 25 100 "left")
                               (make-vehicle 75 100 "left")
                               (make-vehicle 125 100 "left")))
              (list (make-vehicle 524 100 "left")
                    (make-vehicle 24 100 "left")
                    (make-vehicle 74 100 "left")
                    (make-vehicle 124 100 "left")))
(check-expect (move-vset (list (make-vehicle 375 100 "right")
                               (make-vehicle 425 100 "right")
                               (make-vehicle 475 100 "right")
                               (make-vehicle 525 100 "right")))
              (list (make-vehicle 376 100 "right")
                    (make-vehicle 426 100 "right")
                    (make-vehicle 476 100 "right")
                    (make-vehicle -24 100 "right")))
(check-expect (move-vset (list (make-vehicle -25 100 "up")
                               (make-vehicle 25 100 "up")
                               (make-vehicle 75 100 "down")
                               (make-vehicle 125 100 "down")))
              (list (make-vehicle -25 100 "up")
                    (make-vehicle 25 100 "up")
                    (make-vehicle 75 100 "down")
                    (make-vehicle 125 100 "down")))
(define (move-vset a-vset)
  (local [; move-a-vehicle: Vehicle -> Vehicle
          ; moves the vehicle v according to its direction,
          ; and ensures the vehicle always stays on the background
          (define (move-a-vehicle v)
            (cond [(string=? (vehicle-dir v) "left")
                   (move-to v (* SPEED -1))]
                  [(string=? (vehicle-dir v) "right")
                   (move-to v SPEED)]
                  [else v]))
          ; move-to: Vehicle Number -> Vehicle
          ; moves the x-coordinate of the vehicle v
          ; n pixels and keeps it on background
          (define reappear-from-right
            (- (+ WIDTH (* VEHICLE-BODY-WIDTH 0.5)) SPEED))
          (define reappear-from-left (+ (* VEHICLE-BODY-WIDTH -0.5) SPEED))
          (define (move-to v n)
            (cond [(< n 0)
                   (if (<= (vehicle-x v) (* VEHICLE-BODY-WIDTH -0.5))
                       (make-vehicle reappear-from-right
                                     (vehicle-y v) (vehicle-dir v))
                       (make-vehicle (+ (vehicle-x v) n)
                                     (vehicle-y v) (vehicle-dir v)))]
                  [(> n 0)
                   (if (>= (vehicle-x v)
                           (+ WIDTH (* VEHICLE-BODY-WIDTH 0.5)))
                       (make-vehicle reappear-from-left
                                     (vehicle-y v) (vehicle-dir v))
                       (make-vehicle (+ (vehicle-x v) SPEED)
                                     (vehicle-y v) (vehicle-dir v)))]))]
    ; [Vehicle -> Vehicle] VSet -> VSet
    (map move-a-vehicle a-vset)))

;; move-pset: PSet -> PSet
;; moves the planks in a-pset and ensures the number of planks unchanged
(check-expect (move-pset (list (make-plank 100 250)))
              (list (make-plank 101 250)))
(check-expect (move-pset (list (make-plank 539 250)))
              (list (make-plank 540 250)))
(check-expect (move-pset (list (make-plank 540 250)))
              (list (make-plank -39 250)))
(define (move-pset a-pset)
  (local [(define reappear (+ (* (/ PLANK-WIDTH 2) -1) SPEED))
          ; move-a-plank: Plank -> Plank
          ; moves a plank to the right at a speed of PSPEED
          ; meanwhile ensures the plank is always on the scene
          (define (move-a-plank p)
            (if (< (plank-x p) (+ WIDTH (/ PLANK-WIDTH 2)))
                (make-plank (+ (plank-x p) PSPEED) (plank-y p))
                (make-plank reappear (plank-y p))))]
    ; [Plank -> Plank] PSet -> PSet
    (map move-a-plank a-pset)))

;; move-tset: TSet -> TSet
;; moves the turtles in t and ensures the number of turtles unchanged
(check-expect (move-tset (list (make-turtle 250 250)))
              (list (make-turtle 249 250)))
(check-expect (move-tset (list (make-turtle -25 250)))
              (list (make-turtle 524 250)))
(define (move-tset a-tset)
  (local [(define reappear (- (+ WIDTH TURTLE-RADIUS) SPEED))
          ; move-a-turtle: Turtle -> Turtle
          ; moves a turtle to the right at a speed of TSPEED
          ; meanwhile ensures the plank is always on the scene
          (define (move-a-turtle t)
            (if (<= (turtle-x t) (* -1 TURTLE-RADIUS))
                (make-turtle reappear (turtle-y t))
                (make-turtle (- (turtle-x t) TSPEED) (turtle-y t))))]
    ; [Turtle -> Turtle] TSet -> TSet
    (map move-a-turtle a-tset)))

;;; 4.3 On-key handler
  
;; move-player: World, key -> World
;; moves the player 1 step in the given direction
;; and limits the player in the scene
(check-expect (move-player (make-world (make-player 250 125 "up")
                                       vset0 pset0 tset0) "up")
              (make-world (make-player 250 (- 125 FROG-STEP) "up")
                          vset0 pset0 tset0))
(check-expect (move-player (make-world (make-player 250 125 "up")
                                       vset0 pset0 tset0) "down")
              (make-world (make-player 250 (+ 125 FROG-STEP) "down")
                          vset0 pset0 tset0))
(check-expect (move-player (make-world (make-player 250 125 "up")
                                       vset0 pset0 tset0) "left")
              (make-world (make-player (- 250 FROG-STEP) 125 "left")
                          vset0 pset0 tset0))
(check-expect (move-player (make-world
                            (make-player 250 125 "up")
                            vset0 pset0 tset0) "right")
              (make-world (make-player (+ 250 FROG-STEP) 125 "right")
                          vset0 pset0 tset0))
(check-expect (move-player (make-world
                            (make-player 480 125 "up")
                            vset0 pset0 tset0) "right")
              (make-world (make-player (- WIDTH FROG-BODY-SIZE) 125 "right")
                          vset0 pset0 tset0))
(check-expect (move-player (make-world (make-player 20 125 "up")
                                       vset0 pset0 tset0) "left")
              (make-world (make-player FROG-BODY-SIZE 125 "left")
                          vset0 pset0 tset0))
(check-expect (move-player (make-world (make-player 250 25 "up")
                                       vset0 pset0 tset0) "up")
              (make-world (make-player 250 25 "up") vset0 pset0 tset0))
(check-expect (move-player (make-world (make-player 250 625 "up")
                                       vset0 pset0 tset0) "down")
              (make-world (make-player 250 (- HEIGHT (/ DIVISION-HEIGHT 2))
                                       "down") vset0 pset0 tset0))
(check-expect (move-player (make-world (make-player 20 20 "up")
                                       vset0 pset0 tset0) "up")
              (make-world (make-player 20 25 "up") vset0 pset0 tset0))
(check-expect (move-player (make-world (make-player 20 630 "up")
                                       vset0 pset0 tset0) "down")
              (make-world (make-player 20 625 "down") vset0 pset0 tset0))
(check-expect (move-player (make-world
                            (make-player (+ FROG-BODY-SIZE FROG-STEP)
                                         275 "up") vset0 pset0 tset0) "left")
              (make-world (make-player FROG-BODY-SIZE 275 "left")
                          vset0 pset0 tset0))
(check-expect (move-player (make-world
                            (make-player (- WIDTH FROG-BODY-SIZE FROG-STEP)
                                         275 "up")
                            vset0 pset0 tset0) "right")
              (make-world (make-player (- WIDTH FROG-BODY-SIZE)
                                       275 "right") vset0 pset0 tset0))
(check-expect (move-player world0 "a") world0)
(define (move-player w akey)
  (cond [(not (or (string=? akey "up")
                  (string=? akey "down")
                  (string=? akey "left")
                  (string=? akey "right"))) w]
        [else
         (make-world
          ; local returns a player
          (local [(define upper-bound (/ DIVISION-HEIGHT 2))
                  (define lower-bound LANE1-CENTER)
                  (define left-bound FROG-BODY-SIZE)
                  (define right-bound (- WIDTH FROG-BODY-SIZE))
                  ; scene-bounds?: Player Key -> Boolean
                  ; determins whether the player p is at the boundary
                  (define (scene-bounds? p k)
                    (cond [(string=? k "up")
                           (<= (- (player-y p) FROG-STEP) upper-bound)]
                          [(string=? k "down")
                           (>= (+ (player-y p) FROG-STEP) lower-bound)]
                          [(string=? k "left")
                           (<= (- (player-x p) FROG-STEP) left-bound)]
                          [else
                           (>= (+ (player-x p) FROG-STEP) right-bound)]))
                  ; update-player-at-bounds: Player Key -> Player
                  ; updates the player p's position when p is at the boundary
                  (define (update-player-at-bounds p k)
                    (cond [(string=? k "up")
                           (make-player (player-x p) upper-bound "up")]
                          [(string=? k "down")
                           (make-player (player-x p) lower-bound "down")]
                          [(string=? k "left")
                           (make-player left-bound (player-y p) "left")]
                          [else
                           (make-player right-bound (player-y p) "right")]))
                  ; update-player-off-bounds: Player Key -> Player
                  ; updates the player p's position when p is off the boundary
                  (define (update-player-off-bounds p k)
                    (cond [(string=? k "up")
                           (make-player (player-x p)
                                        (- (player-y p) FROG-STEP) "up")]
                          [(string=? k "down")
                           (make-player (player-x p)
                                        (+ (player-y p) FROG-STEP) "down")]
                          [(string=? k "left")
                           (make-player (- (player-x p) FROG-STEP)
                                        (player-y p) "left")]
                          [else
                           (make-player (+ (player-x p) FROG-STEP)
                                        (player-y p) "right")]))]
            (if (scene-bounds? (world-player w) akey)
                (update-player-at-bounds (world-player w) akey)
                (update-player-off-bounds (world-player w) akey)))
          (world-vset w) (world-pset w) (world-tset w))]))

;;;  4.3 Stop-when function
;;; NOTICE: BOUNDARIES ARE CHECKED IN HELPERS
;;; "leave the bounds" is interpreted as the frog leaves the river border

;; end-game?: World -> Boolean
;; is the game about to end?
;; termination event: the player reaches the goal, collides with a vehicle,
;; leaves the river border, or falls into the river
;; tests: Conditions that will not happen in the real game are still tested
;; to ensure the correctness of the function
; reaches, not collides, not falls, not leaves river borders
(check-expect (end-game?
               (make-world (make-player 250 25 "up") vset0 pset0 tset0)) #true)
; reaches, not collides, falls, not leaves river borders:
; impossible because GOAL is on top of rivers
; reaches, collides, not falls, not leaves river borders
(check-expect (end-game?
               (make-world (make-player 250 25 "up")
                           (list (make-vehicle 250 25 "right")) pset0 tset0))
              #true)
; reaches, collides, falls, not leaves river borders:
; impossible because GOAL is on top of rivers
; not reaches, not collides, not falls, not leaves river borders
(check-expect (end-game? world0) #false) 
; not reaches, not collides, falls, not leaves river borders
(check-expect (end-game?
               (make-world (make-player 250 250 "left") vset0
                           (list (make-plank 250 125) (make-plank 400 150))
                           (list (make-turtle 376 175) (make-turtle 391 200))))
              #true)
; not reaches, collides, not falls, not leaves river borders
(check-expect (end-game?
               (make-world (make-player 50 375 "right")
                           vset0 pset0 tset0))
              #true)
; not reaches, collides, falls, not leaves river borders
(check-expect (end-game?
               (make-world (make-player 250 250 "left")
                           (list (make-vehicle 250 250 "left"))
                           (list (make-plank 250 125) (make-plank 400 150))
                           (list (make-turtle 376 175) (make-turtle 391 200))))
              #true)
; leaves river borders, not reaches, not collides, not falls
(check-expect (end-game?
               (make-world (make-player 550 275 "up") vset0
                           (list (make-plank 550 275)) tset0)) #true)
; leaves river borders, not reaches, not collides, falls
(check-expect (end-game?
               (make-world (make-player 550 275 "up") vset0 pset0 tset0)) #true)
; leaves river borders, not reaches, collides, not falls: impossible
; leaves river borders, not reaches, collides, falls: impossible
; leaves river borders, reaches, not collides, not falls: impossible
; leaves river borders, reaches, not collides, falls: impossible
; leaves river borders, reaches, collides, not falls: impossible
; leaves river borders, reaches, collides, falls: impossible
(define (end-game? w)
  (or (reach-goal? w)
      (collision? w)
      (leave-river-border? w)
      (fall-into-river? w)))

;; reach-goal?: World -> Boolean
;; does the player reach the goal?
(check-expect (reach-goal? world0) #false)
(check-expect (reach-goal?
               (make-world (make-player 250 25 "up") vset0 pset0 tset0)) #true)
(check-expect (reach-goal?
               (make-world (make-player 250 24 "up") vset0 pset0 tset0)) #true)
(check-expect (reach-goal?
               (make-world (make-player 250 26 "up") vset0 pset0 tset0)) #false)
(define (reach-goal? w)
  (<= (player-y (world-player w)) GOAL-Y))

;; fall-into-river?: World -> Boolean
;; does player p fall into rivers?
;; tests: Conditions that will not happen in the real game are still tested
;; to ensure the correctness of the function
; off river, off plank, off turtle
(check-expect (fall-into-river? world0) #false)
; off river, off plank, on turtle
(check-expect (fall-into-river?
               (make-world (make-player 250 600 "up")
                           vset0
                           (list (make-plank 250 210)
                                 (make-plank 250 100))
                           (list (make-turtle 250 600)
                                 (make-turtle 600 250)))) #false)
; off river, on plank, off turtle
(check-expect (fall-into-river?
               (make-world (make-player 250 600 "up")
                           vset0
                           (list (make-plank 250 600)
                                 (make-plank 250 100))
                           (list (make-turtle 250 100)
                                 (make-turtle 100 250)))) #false)
; off river, on plank, on turtle
(check-expect (fall-into-river?
               (make-world (make-player 250 600 "up")
                           vset0
                           (list (make-plank 250 600)
                                 (make-plank 250 100))
                           (list (make-turtle 250 600)
                                 (make-turtle 600 250)))) #false)
; in river, on plank, off turtle
(check-expect (fall-into-river?
               (make-world (make-player 250 250 "up")
                           vset0
                           (list (make-plank 250 210)
                                 (make-plank 250 100))
                           (list (make-turtle 250 600)
                                 (make-turtle 600 250)))) #false)
; in river, on plank, on turtle
(check-expect (fall-into-river?
               (make-world (make-player 250 250 "up")
                           vset0
                           (list (make-plank 250 230)
                                 (make-plank 250 100))
                           (list (make-turtle 250 250)
                                 (make-turtle 250 500)))) #false)
; in river, off plank, off turtle
(check-expect (fall-into-river?
               (make-world (make-player 250 250 "up")
                           vset0
                           (list (make-plank 100 250)
                                 (make-plank 250 100))
                           (list (make-turtle 250 600)
                                 (make-turtle 250 500)))) #true)
; in river, off plank, on turtle
(check-expect (fall-into-river?
               (make-world (make-player 250 250 "up")
                           vset0
                           (list (make-plank 250 600)
                                 (make-plank 250 100))
                           (list (make-turtle 250 250)
                                 (make-turtle 250 500)))) #false)
(define (fall-into-river? w)
  (and (in-river? (world-player w))
       (not (or (on-turtle? (world-player w) (world-tset w))
                (on-plank? (world-player w) (world-pset w))))))

;; in-river?: Player -> Boolean
;; is the player p in rivers?
(check-expect (in-river? player0) #false)
(check-expect (in-river? (make-player 250 75 "up")) #true)
(check-expect (in-river? (make-player 250 275 "up")) #true)
(check-expect (in-river? (make-player 250 125 "up")) #true)
(check-expect (in-river? (make-player 250 74 "up")) #false)
(check-expect (in-river? (make-player 250 276 "up")) #false)
(define (in-river? p)
  (and (<= (player-y p) RIVER1-CENTER)
       (>= (player-y p) RIVER5-CENTER)))

;; on-turtle?: Player TSet -> Boolean
;; does the player successfully ride on at least one turtle in tset?

(check-expect (on-turtle? player0 tset0) #false)
(check-expect (on-turtle? player0
                          (list (make-turtle 224 625)
                                (make-turtle 100 100))) #true)
(check-expect (on-turtle? player0
                          (list (make-turtle 250 599)
                                (make-turtle 100 100))) #true)
(check-expect (on-turtle? player0
                          (list (make-turtle 225 625)
                                (make-turtle 100 100))) #true)
(check-expect (on-turtle? player0
                          (list (make-turtle 250 600)
                                (make-turtle 100 100))) #true)
(check-expect (on-turtle? player0
                          (list (make-turtle 100 598)
                                (make-turtle 50 100))) #false)
(check-expect (on-turtle? player0
                          (list (make-turtle 223 625)
                                (make-turtle 50 100))) #false)
(define (on-turtle? p tset)
  (local [; on-a-turtle?: Turtle -> Boolean
          ; does the player rides on turt?
          (define (on-a-turtle? turt)
            (<= (distance p turt) MAX-PLAYER-TURTLE-DISTANCE))]
    (ormap on-a-turtle? tset)))

;; on-plank?: Player TSet -> Boolean
;; does the player p successfully ride on at least one plank in pset?
(check-expect (on-plank? player0 pset0) #false)
(check-expect (on-plank? player0
                         (list (make-plank 200 625)
                               (make-plank 190 625))) #true)
(check-expect (on-plank? player0
                         (list (make-plank 190 625)
                               (make-plank 190 625))) #false)
(check-expect (on-plank? player0
                         (list (make-plank 250 575))) #true)
(check-expect (on-plank? player0
                         (list (make-plank 250 574))) #false)
(define (on-plank? p pset)
  (local [; on-a-plank?: Plank -> Boolean
          ; does the player rides on plk?
          (define (on-a-plank? plk)
            (<= (distance p plk) MAX-PLAYER-PLANK-DISTANCE))]
    ; [Plank -> Boolean] PSet -> Boolean
    (ormap on-a-plank? pset)))

;; distance: Player Obstacle -> Number
;; calculates the distance between p and obs
(check-expect (distance (make-player 0 0 "up") (make-vehicle 30 40 "right")) 50)
(check-expect (distance (make-player 0 0 "up") (make-turtle 50 120)) 130)
(check-expect (distance (make-player 100 50 "up") (make-plank 100 100)) 50)
(define (distance p obs)
  (local [; distance*: Number Number Number Number -> Number
          ; calculates the distance between (x1, y1) and (x2, y2)
          ; given: 3 0 0 4, wanted: 5
          (define (distance* x1 y1 x2 y2)
            (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2)))))]
    (cond [(vehicle? obs) (distance* (vehicle-x obs) (vehicle-y obs)
                                     (player-x p) (player-y p))]
          [(turtle? obs) (distance* (turtle-x obs) (turtle-y obs)
                                    (player-x p) (player-y p))]
          [(plank? obs) (distance* (plank-x obs) (plank-y obs)
                                   (player-x p) (player-y p))])))
;; leave-river-border?: World -> Boolean
;; does the player leaves the screen?
(check-expect (leave-river-border? world0) #false)
(check-expect (leave-river-border?
               (make-world
                (make-player (+ FROG-BODY-SIZE WIDTH) RIVER1-CENTER "up")
                vset0 pset0 tset0))
              #false)
(check-expect (leave-river-border?
               (make-world
                (make-player (* FROG-BODY-SIZE -1) RIVER1-CENTER "up")
                vset0 pset0 tset0)) #false)
(check-expect (leave-river-border?
               (make-world
                (make-player 550 RIVER1-CENTER "up") vset0 pset0 tset0)) #true)
(check-expect (leave-river-border?
               (make-world
                (make-player -50 RIVER1-CENTER "up") vset0 pset0 tset0)) #true)
(define (leave-river-border? w)
  (local [; leave-borders?: Player -> Boolean
          ; does the player p leave the border of screen?
          (define (leave-borders? p)
            (or (> (player-x p) (+ FROG-BODY-SIZE WIDTH))
                (< (player-x p) (* FROG-BODY-SIZE -1))))]
    (and (in-river? (world-player w))
         (leave-borders? (world-player w)))))
          
;; collision?: World -> Boolean
;; does the player collide with vehicles?
(check-expect (collision? world0) #false)
(check-expect (collision? (make-world
                           (make-player 100 100 "up")
                           (list (make-vehicle 10 10 "left")
                                 (make-vehicle 100 90 "left"))
                           pset0 tset0))
              #true)
(define (collision? w)
  (local [; collide-a-vehicle?: Vehicle -> Boolean
          ; does the player collides with v?
          (define (collide-a-vehicle? v)
            (<= (distance (world-player w) v) MIN-PLAYER-VEHICLE-DISTANCE))] 
    (ormap collide-a-vehicle? (world-vset w))))

;; last-picture: World -> Image
;; returns different images to inform the player whether they win or lose
(check-expect (last-picture (make-world (make-player 250 25 "up")
                                        (list (make-vehicle 20 30 "right"))
                                        (list (make-plank 30 40))
                                        (list (make-turtle 50 60))))
              (place-image WIN-IMAGE 250 325
                           (place-image
                            FROG-IMAGE 250 25
                            (place-image
                             VEHICLE-IMAGE 20 30
                             (place-image
                              PLANK-IMAGE 30 40
                              (place-image
                               TURTLE-IMAGE 50 60
                               BACKGROUND))))))
(check-expect (last-picture
               (make-world
                (make-player 250 225 "up")
                (list (make-vehicle 250 225  "left"))
                (list (make-plank 400 500))
                (list (make-turtle 400 600))))
              (place-image
               LOSE-IMAGE 250 325
               (place-image
                FROG-IMAGE 250 225
                (place-image
                 VEHICLE-IMAGE 250 225
                 (place-image
                  PLANK-IMAGE 400 500
                  (place-image
                   TURTLE-IMAGE 400 600
                   BACKGROUND))))))
(check-expect (last-picture
               (make-world
                (make-player 250 75 "up")
                (list (make-vehicle 100 100 "left"))
                (list (make-plank 400 75))
                (list (make-turtle 450 75))))
              (place-image
               LOSE-IMAGE 250 325
               (place-image
                FROG-IMAGE 250 75
                (place-image
                 VEHICLE-IMAGE 100 100
                 (place-image
                  PLANK-IMAGE 400 75
                  (place-image
                   TURTLE-IMAGE 450 75
                   BACKGROUND))))))
(check-expect (last-picture
               (make-world (make-player 550 275 "up")
                           '() (list (make-plank 550 275)) '()))
              (place-image LOSE-IMAGE 250 325
                           (place-image FROG-IMAGE 550 275
                                        (place-image PLANK-IMAGE 550 275
                                                     BACKGROUND))))
(define (last-picture w)
  (cond [(or (collision? w) (fall-into-river? w) (leave-river-border? w))
         (place-image LOSE-IMAGE
                      (/ WIDTH 2) (/ HEIGHT 2)
                      (draw-world w))]
        [else (place-image WIN-IMAGE
                           (/ WIDTH 2) (/ HEIGHT 2)
                           (draw-world w))]))

      
     
                               



                         
                                     

