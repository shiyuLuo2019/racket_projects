;;; Binary Search Trees

(define-struct leaf())
(define-struct branch (key left right))
(define lf (make-leaf))
;; A IBST (Integer Binary Search Tree) is one of:
;; - (make-leaf)
;; - (make-branch Integer IBST IBST)

;; Constraints:
;;             1. all the keys on the left of the root are less than the root
;;             2. all the keys on the right of the root are greater than
;;             the root
;;             3. #1 and #2 are true for every subtree

;; A LoN (list of numbers) is one of:
;; -'()
;; - (cons Number LoN)

(define ibst1 (make-branch
               -5
               (make-branch
                -10
                (make-branch
                 -20
                 (make-branch
                  -30
                  lf lf)
                 lf)
                (make-branch
                 -9 
                 lf lf))
               (make-branch
                -1
                (make-branch
                 -3 
                 lf lf)
                (make-branch
                 5 
                 lf lf))))

(define ibst2
  (make-branch
   -5 
   lf
   (make-branch
    0
    (make-branch
     -1 
     lf lf)
    (make-branch
     2 
     lf lf))))

(define ibst3 (make-branch
               6 
               (make-branch
                2 
                (make-branch
                 1 
                 lf lf)
                lf)
               (make-branch
                11
                (make-branch
                 8 
                 lf
                 (make-branch
                  9 
                  lf
                  (make-branch
                   10 
                   lf lf)))
                (make-branch
                 15 lf lf))))
             
;; Problem 1
;; ibst-contains?: IBST, Number -> Boolean
;; returns true if the given IBST contains the given number,
;; and false otherwise
(check-expect (ibst-contains? lf 5) false)
(check-expect (ibst-contains? ibst1 -40) false)
(check-expect (ibst-contains? ibst1 30) false)
(check-expect (ibst-contains? ibst1 -20) true)
(check-expect (ibst-contains? ibst1 -9) true)
(check-expect (ibst-contains? ibst1 -3) true)
(check-expect (ibst-contains? ibst1 5) true)
(define (ibst-contains? ibst num)
  (cond [(leaf? ibst) false]
        [(branch? ibst)
         (or (= (branch-key ibst) num)
             (ibst-contains? (branch-left ibst) num)
             (ibst-contains? (branch-right ibst) num))]))

;; Problem 2
;; ibst-add: IBST, Integer -> IBST
;; adds the new number into the given IBST
(check-expect (ibst-add lf 1) (make-branch 1 lf lf))
(check-expect (ibst-add ibst1 7)
              (make-branch
               -5 
               (make-branch
                -10
                (make-branch
                 -20
                 (make-branch
                  -30
                  lf lf)
                 lf)
                (make-branch
                 -9
                 lf lf))
               (make-branch
                -1
                (make-branch
                 -3 
                 lf lf)
                (make-branch
                 5 
                 lf
                 (make-branch
                  7
                  lf lf)))))
(check-expect (ibst-add ibst1 -15)
              (make-branch
               -5
               (make-branch
                -10
                (make-branch
                 -20
                 (make-branch
                  -30
                  lf lf)
                 (make-branch
                  -15
                  lf lf))
                (make-branch
                 -9
                 lf lf))
               (make-branch
                -1
                (make-branch
                 -3
                 lf lf)
                (make-branch
                 5
                 lf lf))))
(check-expect (ibst-add ibst2 -5)
              (make-branch
               -5
               lf
               (make-branch
                0
                (make-branch
                 -1
                 lf lf)
                (make-branch
                 2
                 lf lf))))
(define (ibst-add ibst num)
  (cond [(leaf? ibst) (make-branch num lf lf)]
        [(branch? ibst)
         (cond [(= (branch-key ibst) num)
                (make-branch num (branch-left ibst) (branch-right ibst))]
               [(> (branch-key ibst) num)
                (make-branch
                 (branch-key ibst)
                 (ibst-add (branch-left ibst) num) (branch-right ibst))]
               [else
                (make-branch
                 (branch-key ibst)
                 (branch-left ibst) (ibst-add (branch-right ibst) num))])]))
               
;; Problem 3
;; ibst-inorder: IBST -> LoN
;; traverses the IBST in-order and returns a LoN
;; containing all the numbers in the tree
(check-expect (ibst-inorder lf) '())
(check-expect (ibst-inorder ibst1) '(-30 -20 -10 -9 -5 -3 -1 5))
(define (ibst-inorder ibst)
  (cond [(leaf? ibst) '()]
        [(branch? ibst)
         (append (ibst-inorder (branch-left ibst))
                 (list (branch-key ibst))
                 (ibst-inorder (branch-right ibst)))]))

;; Problem 4
;; ibst-postorder: IBST -> LoN
;; traverses the IBST pre-order and returns a LoN
;; containing all the numbers in the tree
(check-expect (ibst-postorder lf) '())
(check-expect (ibst-postorder ibst1) '(-30 -20 -9 -10 -3 5 -1 -5))
(define (ibst-postorder ibst)
  (cond [(leaf? ibst) '()]
        [(branch? ibst)
         (append (ibst-postorder (branch-left ibst))
                 (ibst-postorder (branch-right ibst))
                 (list (branch-key ibst)))]))

;; Problem 5
;; ibst-preorder: IBST -> LoN
;; traverses the IBST pre-order and returns a LoN
;; containing all the number in the tree
(check-expect (ibst-preorder lf) '())
(check-expect (ibst-preorder ibst1) '(-5 -10 -20 -30 -9 -1 -3 5))
(define (ibst-preorder ibst)
  (cond [(leaf? ibst) '()]
        [(branch? ibst)
         (append (list (branch-key ibst))
                 (ibst-preorder (branch-left ibst))
                 (ibst-preorder (branch-right ibst)))]))

;; Problem 6
;; ibst-depth: IBST -> Number
;; returns the maximum depth of the given tree
(check-expect (ibst-depth lf) 0)
(check-expect (ibst-depth ibst1) 4)
(check-expect (ibst-depth ibst2) 3)
(define (ibst-depth ibst)
  (cond [(leaf? ibst) 0]
        [(branch? ibst)
         (if (>= (ibst-depth (branch-left ibst))
                 (ibst-depth (branch-right ibst)))
             (+ (ibst-depth (branch-left ibst)) 1)
             (+ (ibst-depth (branch-right ibst)) 1))]))

;; Problem 8
;; ibst-remove: IBST Integer -> IBST
;; removes the node with the given key from the given tree
(check-expect (ibst-remove lf 1) lf)
(check-expect (ibst-remove (make-branch 1 lf lf) 1) lf)
(check-expect (ibst-remove (make-branch 1 lf lf) 2)
              (make-branch 1 lf lf))
(check-expect (ibst-remove ibst3 6)
              (make-branch
               8
               (make-branch
                2
                (make-branch 1 lf lf)
                lf)
               (make-branch
                11
                (make-branch
                 9
                 lf
                 (make-branch
                  10
                  lf lf))
                (make-branch
                 15
                 lf lf))))
(check-expect (ibst-remove ibst3 11)
              (make-branch
               6
               (make-branch
                2
                (make-branch
                 1 lf lf) lf)
               (make-branch
                15
                (make-branch
                 8 lf
                 (make-branch
                  9 lf
                  (make-branch
                   10 lf lf))) lf)))
(check-expect (ibst-remove ibst3 20) ibst3)              
(define (ibst-remove ibst num)
  (cond [(leaf? ibst) lf]
        [(and (leaf? (branch-left ibst))
              (leaf? (branch-right ibst)))
         (if (= (branch-key ibst) num)
             lf
             ibst)]
        [else
         (cond [(= (branch-key ibst) num)
                (make-branch
                 (branch-key (minimum (branch-right ibst)))
                 (branch-left ibst)
                 (ibst-remove
                  (branch-right ibst)
                  (branch-key (minimum (branch-right ibst)))))]
               [(> (branch-key ibst) num)
                (make-branch
                 (branch-key ibst)
                 (ibst-remove (branch-left ibst) num)
                 (branch-right ibst))]
               [else
                (make-branch
                 (branch-key ibst)
                 (branch-left ibst)
                 (ibst-remove (branch-right ibst) num))])]))

;; minimum: IBST -> IBST
;; finds the node with the minimum key in a tree
;; assume the given IBST is (make-branch Integer IBST IBST)
(check-expect (minimum (make-branch 1 lf lf)) (make-branch 1 lf lf))
(check-expect (minimum ibst3) (make-branch 1 lf lf))                      
(define (minimum ibst)
  (cond [(leaf? (branch-left ibst)) ibst]
        [else
         (minimum (branch-left ibst))]))

;;; Sets

;; A Set is one of:
;; - '()
;; - (cons AnyValue Set)
;; Constraints:
;;             a. items in a set is unordered
;;             b. all items in a set is unique
;;             c. Any Value refers to any value that is a legal Racket Value

(define set1 '())
(define set2 (list 1 2 "trees" 'forest))

;; Problem 1

;; set-add: Set, Any Value -> Set
;; adds the given value into the set if it is not in the set
(check-expect (set-add set1 "star") '("star"))
(check-expect (set-add set2 '("star" problem))
              '(("star" problem) 1 2 "trees" forest))
(check-expect (set-add set2 1) set2)
(define (set-add a-set a-value)
  (cond [(empty? a-set) (list a-value)]
        [(cons? a-set)
         (if (in-set? a-set a-value)
             a-set
             (cons a-value a-set))]))

;; in-set?: Set, Any Value -> Boolean
;; checks if the given value is already in the given set
(check-expect (in-set? '() "star") #false)
(check-expect (in-set? '("star" house 1 2 3) "star") #true)
(check-expect (in-set? '("star" house 1 2 3) 7) #false)
(define (in-set? a-set a-value)
  (cond [(empty? a-set) #false]
        [(cons? a-set)
         (or (equal? (first a-set) a-value)
             (in-set? (rest a-set) a-value))]))

;; Problem 2

;; set-remove: Set, Any Value -> Set
;; removes the given value from the given set,
;; and set remains unchanged if the value is not in the set
(check-expect (set-remove '() 'a) '())
(check-expect (set-remove set2 'a) set2)
(check-expect (set-remove set2 "trees") '(1 2 forest))
(check-expect (set-remove set2 1) '(2 "trees" forest))
(define (set-remove a-set a-value)
  (cond [(empty? a-set) '()]
        [(cons? a-set)
         (if (equal? (first a-set) a-value)
             (rest a-set)
             (cons (first a-set)
              (set-remove (rest a-set) a-value)))]))
             
             
;; Problem 3

;; set-contains?: Set, Any Value -> Boolean
;; checks if the given value is already in the given set
(check-expect (set-contains? '() "star") #false)
(check-expect (set-contains? '("star" house 1 2 3) "star") #true)
(check-expect (set-contains? '("star" house 1 2 3) 7) #false)
(define (set-contains? a-set a-value)
  (cond [(empty? a-set) #false]
        [(cons? a-set)
         (or (equal? (first a-set) a-value)
             (set-contains? (rest a-set) a-value))]))


;; Problem 4

;; set-equals?: Set, Set -> Boolean
;; checks if two sets are equal
(check-expect (set-equals? '() '()) #true)
(check-expect (set-equals? '(1 2 3) '(2 3 1)) #true)
(check-expect (set-equals? '(2 3 1) '(2 3 4)) #false)
(check-expect (set-equals? '(1 2 3) '(1 2)) #false)
(define (set-equals? set-a set-b)
  (and (all-a-in-b? set-a set-b)
       (all-a-in-b? set-b set-a)))

;; all-a-in-b?: Set, Set -> Boolean
;; checks all the items in set-a is already in set-b
(check-expect (all-a-in-b? '() '(1 2 3)) #true)
(check-expect (all-a-in-b? '(1 2) '(1 2)) #true)
(check-expect (all-a-in-b? '(1 2) '(2 1)) #true)
(check-expect (all-a-in-b? '(1 2) '(1 3 2)) #true)
(check-expect (all-a-in-b? '(1 2) '(2 3)) #false)
(define (all-a-in-b? set-a set-b)
  (cond [(empty? set-a) #true]
        [(cons? set-a)
         (and (set-contains? set-b (first set-a))
              (all-a-in-b? (rest set-a) set-b))]))


;; Problem 5

;; set-union: Set, Set -> Set
;; returns the union of two sets
(check-expect (set-union '() '()) '())
(check-expect (set-union '() '(1 2 3)) '(1 2 3))
(check-expect (set-union '(1 2 3) '()) '(1 2 3))
(check-expect (set-union '(1 2) '(2 3)) '(1 2 3))
(check-expect (set-union '(1 2) '(3 4)) '(1 2 3 4))
(define (set-union set-a set-b)
  (cond [(empty? set-a) set-b]
        [(cons? set-a)
         (cond [(empty? set-b) set-a]
               [(cons? set-b)
                (if (set-contains? set-b (first set-a))
                    (set-union (rest set-a) set-b)
                    (cons (first set-a)
                          (set-union (rest set-a) set-b)))])]))
                    
         
              
;; Problem 6
;; set-intersection: Set, Set -> Set
;; returns the intersection of two sets
(check-expect (set-intersection '() '()) '())
(check-expect (set-intersection '() '(1 2 3)) '())
(check-expect (set-intersection '(1 2 3) '()) '())
(check-expect (set-intersection '(1 2 3) '(4 5)) '())
(check-expect (set-intersection '(1 2 3 5 4) '(2 5 7)) '(2 5))
(define (set-intersection set-a set-b)
  (cond [(or (empty? set-b) (empty? set-a)) '()]
        [else
         (if (set-contains? set-b (first set-a))
             (cons (first set-a)
                   (set-intersection (rest set-a) set-b))
             (set-intersection (rest set-a) set-b))]))

;; Problem 7
;; set-symmetric-diff: Set, Set -> Set
;; returns the symmetric difference of two sets
(check-expect (set-symmetric-diff '() '()) '())
(check-expect (set-symmetric-diff '(1 2 3) '()) '(1 2 3))
(check-expect (set-symmetric-diff '() '(1 2 3)) '(1 2 3))
(check-expect (set-symmetric-diff '(1 2) '(1 2)) '())
(check-expect (set-symmetric-diff '(1 2 3) '(3 4 5)) '(1 2 4 5))
(define (set-symmetric-diff set-a set-b)
  (cond [(empty? set-a) set-b]
        [(cons? set-a)
         (if (set-contains? set-b (first set-a))
             (set-remove (set-symmetric-diff (rest set-a) set-b)
                         (first set-a))
             (set-add (set-symmetric-diff (rest set-a) set-b)
                      (first set-a)))]))

;; Problem 8
;; is-set?: Any Value -> Boolean
;; checks if the given value is a Set
(check-expect (is-set? '()) true)
(check-expect (is-set? '(1 2 "tree" (a b "forest"))) true)
(check-expect (is-set? 'a) false)
(check-expect (is-set? 7) false)
(check-expect (is-set? "string") false)
(define (is-set? a-value)
  (if (list? a-value)
      (unique? a-value)
      false))

;; unique?: List -> Boolean
;; checks if every item in the given list is unique
(check-expect (unique? '()) true)
(check-expect (unique? '(1 2 3)) true)
(check-expect (unique? '(1 1 2)) false)
(define (unique? a-list)
  (cond [(empty? a-list) true]
        [(cons? a-list)
         (and (not (list-contains? (rest a-list) (first a-list)))
             (unique? (rest a-list)))]))

;; list-contains?: List, Any Value -> Boolean
;; checks if the given value is already in the given list
(check-expect (list-contains? '() 'a) false)
(check-expect (list-contains? '() '()) false)
(check-expect (list-contains? '(1 2 3) 1) true)
(check-expect (list-contains? '(1 2 3) 4) false)
(define (list-contains? a-list a-value)
  (cond [(empty? a-list) false]
        [(cons? a-list)
         (or (equal? a-value (first a-list))
             (list-contains? (rest a-list) a-value))]))
