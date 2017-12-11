(require 2htdp/image)
(require 2htdp/universe)

;;; Problem 1

;; A List of Passwords (LoP) is one of:
;; - '()
;; - (cons String LoP)
#; (define (lop-temp alop)
     (cond [(empty? alop)...]
           [(cons? alop)...(first alop)...
                        ...(rest alop)...]))
(define lop0 (cons "asdfgh" (cons "cnv-da" (cons "18$8-d" '()))))
(define lop1 (cons "asdfgh" (cons "nvidia!!" (cons "goooooooooooooooogle" '()))))

;; LoP -> Boolean
;; given a LoP, returns #true if all the strings in the list is at least 6 characters
;; but no more than 10 characters long
(check-expect (check-pass-6-10? '()) #true)
(check-expect (check-pass-6-10? lop0) #true)
(check-expect (check-pass-6-10? lop1) #false)
(define (check-pass-6-10? alop)
  (cond [(empty? alop) #true]
        [(cons? alop) (and (and (>= (string-length (first alop)) 6)
                                (<= (string-length (first alop)) 10))
                      (check-pass-6-10? (rest alop)))]))

;; LoP, Nonnegative Integer, Nonnegative Integer -> Boolean
;; given a LoP, a minimum and a maximum length, returns #true if
;; all the passwords' lengths are within the length span, otherwise #false
(check-expect (check-pass? lop0 4 7) #true)
(check-expect (check-pass? lop0 7 4) #true)
(check-expect (check-pass? lop0 8 9) #false)
(check-expect (check-pass? lop0 6 6) #true)
(define (check-pass? alop min max)
  (if (<= min max)
      (cond [(empty? alop) #true]
        [(cons? alop) (and (and (>= (string-length (first alop)) min)
                                (<= (string-length (first alop)) max))
                     (check-pass? (rest alop) min max))])
      (cond [(empty? alop) #true]
        [(cons? alop) (and (and (>= (string-length (first alop)) max)
                                (<= (string-length (first alop)) min))
                     (check-pass? (rest alop) min max))])))


;;; Problem 2
;; A List of Symbols (LoS) is one of:
;; - '()
;; - (cons Symbol LoS)
#; (define (los-temp alos)
     (cond [(empty? alos)...]
           [(cons? alos)...(first alos)...
                        ...(rest alos)...]))

(define los1 (cons 'wurst
                   (cons 'huevos
                         (cons 'pizza
                               (cons 'pants '())))))
(define los2 (cons 'wurst
                   (cons 'huevos
                         (cons 'pants '()))))

;; LoS -> LoS
;; given a LoS, returns the same list but with every instance of 'pizza doubled.
(check-expect (cesarify los1) (cons 'wurst
                                    (cons 'huevos
                                          (cons 'pizza
                                                (cons 'pizza
                                                      (cons 'pants '()))))))
(check-expect (cesarify los2) (cons 'wurst
                                    (cons 'huevos
                                          (cons 'pants '()))))
(define (cesarify alos)
  (cond [(empty? alos) '()]
        [(cons? alos) (cons (first alos)
                            (if (symbol=? (first alos) 'pizza)
                                (cons 'pizza (cesarify (rest alos)))
                                (cesarify (rest alos))))]))

;;; Problem 3

;;; Constants
(define BACKGROUND (empty-scene 500 500))
(define circle0 (place-image (circle 200 "solid" "red") 250 250 BACKGROUND))
(define rectangle0 (place-image (rectangle 200 300 "solid" "olive") 250 250 BACKGROUND))
(define square0 (place-image (square 100 "solid" "black") 250 250 BACKGROUND))
(define circle1 (place-image (circle 50 "solid" "blue") 250 250 BACKGROUND))
(define rectangle1 (place-image (rectangle 100 150 "solid" "olive") 250 250 BACKGROUND))
(define square1 (place-image (square 150 "solid" "yellow") 250 250 BACKGROUND))

;;; Data Definition
;; A LoImg (List-of-Image) is one of:
;; - '()
;; - (cons Image LoImg)
#; (define (loimg-temp aloimg)
     (cond [(empty? aloimg) ...]
           [(cons? aloimg)...(first aloimg)...
                          ...(rest aloimg)...]))
(define loimg0 (cons circle0 (cons rectangle0
                                   (cons square0
                                         (cons circle1
                                               (cons rectangle1
                                                     (cons square1 '())))))))

(define-struct cr (index images))
;; A CR (Camera Roll) is a (make-cr PosInteger LoImg)
;; intepretation:
;; The index represents the position of the current image.
;; The index must be between 0 (inclusive) and the number of images in LoImg (exclusive).
#; (define (cr-temp acr)
     ...(cr-index acr)...
     ...(cr-images acr)...)
(define cr0 (make-cr 2 loimg0))
(define cr1 (make-cr 0 loimg0))
(define cr2 (make-cr 5 loimg0))



;; An Arrow is one of:
;; - "left"
;; - "right"
#; (define (arrow-temp anarrow)
     (cond [(string=? "left")...]
           [(string=? "right")...]
           [else...]))

;; CR -> CR
;; launches the cameral-roll program
(define (main acr)
  (big-bang acr
  [to-draw display-photo]
  [on-key change-photo]))

;; display-photo: CR -> Image
;; draws the photo; if there's no photo in the roll, returns the background
(check-expect (display-photo (make-cr 0 '())) BACKGROUND)
(check-expect (display-photo cr0) square0)
(define (display-photo acr)
  (cond [(empty? (cr-images acr)) (empty-scene 500 500)]
        [(cons? (cr-images acr)) (if (zero? (cr-index acr))
             (first (cr-images acr))
             (display-photo
              (make-cr (- (cr-index acr) 1) (rest (cr-images acr)))))]))


;; change-photo: CR, Arrow -> CR
;; changes the index of WorldState when a key ("left" or "right") is pressed
(check-expect (change-photo cr0 "left")
              (make-cr 1 loimg0))
(check-expect (change-photo cr0 "right")
             (make-cr 3 loimg0))
(check-expect (change-photo cr1 "left") cr1)
(check-expect (change-photo cr2 "right") cr2)
(check-expect (change-photo cr0 "up") cr0)
(check-expect (change-photo (make-cr 0 '()) "left") (make-cr 0 '()))
(check-expect (change-photo (make-cr 0 '()) "right") (make-cr 1 '()))
(define (change-photo acr arrow)
  (cond [(string=? arrow "left")
         (if (= (cr-index acr) 0)
             acr
             (make-cr (- (cr-index acr) 1) (cr-images acr)))]
        [(string=? arrow "right")
         (if (= (cr-index acr) (- (length-of-loi (cr-images acr)) 1))
             acr
             (make-cr (+ (cr-index acr) 1) (cr-images acr)))]
        [else acr]))

;; length-of-cr: LoImg -> Nonnegative Integer
;; given a LoImg, returns the number of items in the list
(check-expect (length-of-loi loimg0) 6)
(check-expect (length-of-loi '()) 0)
(define (length-of-loi aloimg)
  (cond [(empty? aloimg) 0]
        [(cons? aloimg) (+ 1 (length-of-loi (rest aloimg)))]))



  

      
