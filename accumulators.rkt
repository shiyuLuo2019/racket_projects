;;; Accumulators

;; part 1 fibonacci

;; fibonacci: Nat -> Nat
;; produces the nth fibonacci number
(check-expect (fibonacci 0) 0)
(check-expect (fibonacci 1) 1)
(check-expect (fibonacci 2) 1)
(check-expect (fibonacci 11) 89)
(define (fibonacci n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [else
         (+ (fibonacci (sub1 n))
            (fibonacci (sub1 (sub1 n))))]))

;; fibonacci*: Nat -> Nat
;; produces the nth fibonacci number using accumulator
(check-expect (fibonacci* 0) 0)
(check-expect (fibonacci* 1) 1)
(check-expect (fibonacci* 2) 1)
(check-expect (fibonacci* 11) 89)
(define (fibonacci* n)
  (local [; f: Nat Nat Nat -> Nat
          ; produces the fibonacci number of n
          ; ACCUMULATOR f1: fibonacci number of k
          ; ACCUMULATOR f2: fibonacci number of (k + 1)
          ; k = (the number of time that function f has been executed) - 1
          ; n acts like a counter
          (define (f n f1 f2)
            (cond [(= n 0) f1]
                  [else
                   (f (sub1 n) f2 (+ f1 f2))]))]
    (f n 0 1)))

;; part 2 tweeter

(define-struct user (handle tweeps))
;; A Network is a [List-of TU]
;; A TU (twitter user) is a structure: (make-user Symbol [List-of Symbol])
(define network-5 (list (make-user 'A (list 'B 'C 'D))
                        (make-user 'B (list 'C))
                        (make-user 'C '())
                        (make-user 'D (list 'C 'E))
                        (make-user 'E (list 'A))))

;; NAMED: in a Network, all TUs in tweeps must be named nodes

;; list-handles: Network -> [List-of Symbol]
;; lists all the handles in the network
(check-expect (list-handles '()) '())
(check-expect (list-handles network-5) '(E D C B A))
(define (list-handles ntwk)
  (local [; list-handles*: Network [List-of Symbol] -> [List-of Symbol]
          ; lists all the handles in the network
          ; ACCUMULATOR: keeps track of all the visited users
          (define (list-handles* nw acc)
            (cond [(empty? nw) acc]
                  [(cons? nw)
                   (list-handles* (rest nw)
                                  (cons (user-handle (first nw)) acc))]))] 
    (list-handles* ntwk '())))

;; most-followers: Network -> Symbol
;; produces the handle in the network that has the most followers
(check-error (most-followers '()) "user not found")
(check-expect (most-followers (list (make-user 'billy '()))) 'billy)
(check-expect (most-followers network-5) 'A)
(define (most-followers ntwk)
  (local [; most-followers*: Network TU -> TU
          ; finds the most popular user in the network
          ; ACCUMULATOR: keeps track of the most popular user so far
          (define (most-followers nw acc)
            (cond [(empty? nw) (error "user not found")]
                  [(empty? (rest nw)) (select-popular (first nw) acc)]
                  [else
                   (if (>= (count-followers (first nw)) (count-followers acc))
                       (most-followers (rest nw) (first nw))
                       (most-followers (rest nw) acc))]))]
    (user-handle (most-followers ntwk (make-user 'foo '())))))

;; count-followers: TU -> Nat
;; counts the number of followers of a single user
(check-expect (count-followers (make-user 'bar '())) 0)
(check-expect (count-followers (make-user 'baz '(buz buddy))) 2)
(define (count-followers usr)
  (foldr (λ (fo base) (add1 base)) 0 (user-tweeps usr)))

;; select-popular: TU TU -> TU
;; returns the user that has more followers
(check-expect (select-popular (make-user 'billy '())
                              (make-user 'bobby '(aaron bunny)))
              (make-user 'bobby '(aaron bunny)))
(check-expect (select-popular (make-user 'billy '(fumble))
                              (make-user 'bobby '(aaron bunny)))
              (make-user 'bobby '(aaron bunny)))
(check-expect (select-popular (make-user 'billy '(bubble bloomberg))
                              (make-user 'bobby '(aaron bunny)))
              (make-user 'billy '(bubble bloomberg)))
(check-expect (select-popular (make-user 'billy '(bubble bloomberg buz))
                              (make-user 'bobby '(aaron bunny)))
              (make-user 'billy '(bubble bloomberg buz)))
(define (select-popular usr1 usr2)
  (if (>= (count-followers usr1) (count-followers usr2)) usr1 usr2))
                   
;; friends?: Network -> Boolean
;; are there two users who follow each other?
;; algorithm: 1. check if the first user mutually follow certain users in
;;               (rest ntwk)
;;            2. check if two users mutually follow each other in (rest ntwk)
(check-expect (friends? network-5) #false)
(check-expect (friends? (list (make-user 'A '(B C D))
                              (make-user 'B '(C D))
                              (make-user 'C '(D A))
                              (make-user 'D '()))) #true)
(check-expect (friends? '()) #false)
(define (friends? ntwk)
  (cond [(empty? ntwk) #false]
        [else
         (or (ormap (λ (usr2) (mutually-follow? (first ntwk) usr2)) (rest ntwk))
             (friends? (rest ntwk)))]))


;; mutually-follow?: TU TU -> Boolean
;; do the two users follow each other?
(check-expect (mutually-follow? (make-user 'foo '())
                                (make-user 'bar '(foo))) #false)
(check-expect (mutually-follow? (make-user 'bar '(foo))
                                (make-user 'foo '())) #false)
(check-expect (mutually-follow? (make-user 'bar '())
                                (make-user 'foo '())) #false)
(check-expect (mutually-follow? (make-user 'foo '(buz bud))
                                (make-user 'bar '(foo))) #false)
(check-expect (mutually-follow? (make-user 'foo '(bar buz))
                                (make-user 'bar '(foo))) #true)
(define (mutually-follow? usr1 usr2)
  (local [;; follower?: TU TU -> Boolean
          ;; is u1 a follower of u2?
          (define (follower? u1 u2)
            (member? (user-handle u1) (user-tweeps u2)))]
    (and (follower? usr1 usr2)
         (follower? usr2 usr1))))

;; part 3 palindrome

;; make-palindrome: Non-empty String -> Non-empty String
;; makes a palindrome using s
(check-expect (make-palindrome "string") "stringnirts")
(check-expect (make-palindrome "a") "a")
(check-expect (make-palindrome "fundies") "fundieseidnuf")
(define (make-palindrome s)
  (implode (mirror (explode s))))

;; mirror: [Non-empty List-of String] -> [Non-empty List-of String]
;; mirrors the given list of strings los
(check-expect (mirror '("a")) '("a"))
(check-expect (mirror '("a" "b")) '("a" "b" "a"))
(define (mirror los)
  (append (all-but-last los)
          (list (last los))
          (reverse* (all-but-last los))))

;; all-but-last: [Non-empty List-of 1String] -> [List-of 1String]
;; deletes the last 1 String in los
(check-expect (all-but-last '("a")) '())
(check-expect (all-but-last '("a" "b")) '("a"))
(define (all-but-last los)
  (local [; [Non-empty List-of 1String] 1String -> [List-of 1String]
          ; deletes the last 1String in a-los
          ; ACCUMULATOR: keeps track of all the 1String already visited in los
          (define (all-but-last-acc a-los acc)
            (cond [(empty? (rest a-los)) acc]
                  [else
                   (all-but-last-acc (rest a-los)
                                     (append acc (list (first a-los))))]))]
    (all-but-last-acc los '())))

;; last: [Non-empty List-of 1String] -> 1String
;; extracts the last 1String in los
(check-expect (last '("a")) "a")
(check-expect (last '("a" "b")) "b")
(define (last los)
  (cond [(empty? (rest los)) (first los)]
        [else (last (rest los))]))

;; reverse*: [List-of ITEM] -> [List-of ITEM]
;; reverses the given list
(check-expect (reverse* '()) '())
(check-expect (reverse* '("a")) '("a"))
(check-expect (reverse* '("a" "b")) '("b" "a"))
(check-expect (reverse* '(1 2 3)) '(3 2 1))
(define (reverse* los)
  (local [; reverse-acc: [List-of 1String][List-of 1String]-> [List-of 1String]
          ; reverses the given list of 1String
          ; ACCUMULATOR: stores 1String already visited
          (define (reverse-acc a-los acc)
            (cond [(empty? a-los) acc]
                  [else
                   (reverse-acc (rest a-los) (cons (first a-los) acc))]))]
    (reverse-acc los '())))

;; is-palindrome?: Non-empty String -> Boolean
;; is the string a palindrome?
(check-expect (is-palindrome? "a") #true)
(check-expect (is-palindrome? "ab") #false)
(check-expect (is-palindrome? "aba") #true)
(check-expect (is-palindrome? "abba") #true)
(check-expect (is-palindrome? "abc") #false)
(check-expect (is-palindrome? "stringnirts") #true)
(check-expect (is-palindrome? "string") #false)
(define (is-palindrome? s)
  (string=? s (string-reverse s)))

;; string-reverse: Non-empty String -> Non-empty String
;; reverses the string s
(check-expect (string-reverse "a") "a")
(check-expect (string-reverse "ab") "ba")
(check-expect (string-reverse "aba") "aba")
(define (string-reverse s)
  (implode (reverse* (explode s))))
        

;; bubble sort

;; bsort: [List-of Number] -> [List-of Number]
;; sorts lon in ascending order using bubble sort
(check-expect (bsort '()) '())
(check-expect (bsort '(1 2 3)) '(1 2 3))
(check-expect (bsort '(3 2 1)) '(1 2 3))
(check-expect (bsort '(1 3 2)) '(1 2 3))
(define (bsort lon)
  (local [; bsort*: [List-of Number] Nat -> [List-of Number]
          ; sorts lon in ascending order using bubble sort
          ; ACCUMULATOR: keeps track of the swap
          (define (bsort a-lon swapped)
            (cond [(= swapped 1)
                   (if (lon=? a-lon (bsort-aux a-lon))
                       (bsort a-lon 0)
                       (bsort (bsort-aux a-lon) 1))]
                  [(= swapped 0)
                   a-lon]))]
    (bsort lon 1)))

;; bsort-aux: [List-of Number] -> [List-of Number]
;; an individual pass of bubble sort
(check-expect (bsort-aux '()) '())
(check-expect (bsort-aux '(1)) '(1))
(check-expect (bsort-aux '(5 1 4 2 8)) '(1 4 2 5 8))
(define (bsort-aux lon)
  (local [; bsort-aux*: [List-of Number] [List-of Number] -> [List-of Number]
          ; an individual pass of bubble sort; conducts swap for n times;
          ; n is the length of the list
          ; ACCUMULATOR: keeps track of visited numbers
          (define (bsort-aux* l acc)
            (cond [(empty? l) '()]
                  [(empty? (rest l)) (cons (first l) acc)]
                  [else
                   (if (< (first l) (second l))
                       (bsort-aux* (rest l) (cons (first l) acc))
                       (bsort-aux* (cons (first l) (rest (rest l)))
                                   (cons (second l) acc)))]))]
    (reverse* (bsort-aux* lon '()))))

;; lon=?: [List-of Number] [List-of Number] -> Boolean
;; are the two lists of numbers equal?
;; assume the two lists are of equal length
(check-expect (lon=? '() '()) #true)
(check-expect (lon=? '(1 2 3) '(1 2 3)) #true)
(check-expect (lon=? '(1 3 2) '(1 2 3)) #false)
(check-expect (lon=? '(1 2 3) '(2 3 4)) #false)
(define (lon=? lon-1 lon-2)
  (cond [(empty? lon-1) #true]
        [(cons? lon-1)
         (and (= (first lon-1) (first lon-2))
              (lon=? (rest lon-1) (rest lon-2)))]))


  


