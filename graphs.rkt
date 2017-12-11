;;;;; Assignment 9
;;;;; Shiyu Luo

;; A Node is a Symbol 
;; INTERP: represents the name of a node in a graph

;; A Distance is a PosInt
;; INTERP: represents distance in miles

;; An Edge is (list Node Distance Node)
;; e.g. (list 'A 10 'B)
;; INTERP: represents an edge from 'A to 'B (directed) with the distance
;;         from 'A to 'B being 10 miles

;; A Path is a [List-of Edge]

;; A Graph is a [Set-of Edge]
;; Constraint:
;;           Two edges that share a common starting node and end node are
;;           considered identical and therefore not allowed

;; A [Set-of ITEM] is a [List-of ITEM]
;; where ITEMs on the set are unique and unordered

;; A MaybePath is one of:
;; - #false
;; - Path

(define sample-graph
  '((A 2 B)
    (A 1 C)
    (A 7 H)
    (B 3 D)
    (B 5 E)
    (B 10 G)
    (C 3 H)
    (C 10 G)
    (D 7 B)
    (E 1 G)
    (F 5 B)
    (H 4 G)))

;;;; Problem 1

;; valid-path?: Graph Path -> Boolean
;; returns #true if every edge in the given Path p is also in the given Graph g
;; and we can follow the suggested path, and #false otherwise
(check-expect (valid-path? '() '()) #true)
(check-expect (valid-path? '() '((A 2 B) (B 3 D))) #false)
(check-expect (valid-path? '() '((A 2 B))) #false)
(check-expect (valid-path? sample-graph '()) #true)
(check-expect (valid-path? sample-graph '((A 2 B))) #true)
(check-expect (valid-path? sample-graph '((A 2 B) (B 3 D) (D 7 B))) #true)
(check-expect (valid-path? sample-graph '((A 2 B) (B 3 D) (E 1 G))) #false)
(check-expect (valid-path? sample-graph '((A 2 B) (B 3 D) (D 8 B))) #false)
(check-expect (valid-path? sample-graph '((A 2 B) (B 3 D) (D 3 g))) #false)
(check-expect (valid-path? sample-graph '((A 2 B) (B 3 D) (D 3 Z))) #false)
(define (valid-path? g p)
  (local [; valid-path-aux?: Path -> Boolean
          ; can we follow the given Path p0 all the way to the end?
          ; given: '(), wanted: #true
          ; given: '((A 2 B)), wanted: #true
          ; given: '((A 2 B) (B 3 D)), wanted: #true
          ; given: '((B 10 G) (C 10 G)), wanted: #false
          (define (valid-path-aux? p0)
            (cond [(empty? p0) #true]
                  [(empty? (rest p0)) #true]
                  [else (and (continuous? (first p0) (second p0))
                             (valid-path-aux? (rest p0)))]))
          ; continuous?: Edge Edge -> Boolean
          ; are edge e1 and e2 continuous?
          ; given: '(A 2 B) '(B 3 D), wanted: #true
          ; given: '(A 2 B) '(C 10 G), wanted: #false
          (define (continuous? e1 e2)
            (symbol=? (dest e1) (ori e2)))]
    (cond [(path-in-graph? p g) (valid-path-aux? p)]
          [else #false])))

;;; helpers

;; path-in-graph?: Path Graph -> Boolean
;; is the given path p in the graph g?
(check-expect (path-in-graph? '() '()) #true)
(check-expect (path-in-graph? '() sample-graph) #true)
(check-expect (path-in-graph? '((A 10 B)) '()) #false)
(check-expect (path-in-graph? '((A 2 B)) sample-graph) #true)
(check-expect (path-in-graph? '((B 2 A)) sample-graph) #false)
(check-expect (path-in-graph? '((A 2 B) (F 5 B)) sample-graph) #true)
(check-expect (path-in-graph? '((A 2 B) (A 2 B)) sample-graph) #true)
(define (path-in-graph? p g)
  (andmap (λ (edg) (member? edg g)) p))

;; ori: Edge -> Node
;; returns the first node (origin) of an edge
(check-expect (ori '(A 10 B)) 'A)
(check-expect (ori '(B 10 A)) 'B)
(define (ori e) (first e))

;; dist: Edge -> Distance
;; returns the distance of an edge
(check-expect (dist '(A 10 B)) 10)
(check-expect (dist '(A 5 B)) 5)
(define (dist e) (second e))

;; dest: Edge -> Distance
;; returns the last node (destination) of an edge
(check-expect (dest '(A 10 B)) 'B)
(check-expect (dest '(B 10 A)) 'A)
(define (dest e) (first (rest (rest e))))

;;;; Problem 2

;; valid-st-path?: Graph Node Node Path -> Boolean
;; can we follow the Path suggested by p in g to get
;; from Node s to Node t?
;; if p is invalid or does not start with s, returns #false (reference: piazza)
;; for empty p:
;;            if s and t are the same and in g, returns #true, otherwise #false
(check-expect (valid-st-path? '() 'A 'B '()) #false)
(check-expect (valid-st-path? sample-graph 'A 'I '()) #false)
(check-expect (valid-st-path? sample-graph 'I 'A '()) #false)
(check-expect (valid-st-path? sample-graph 'Z 'I '((A 2 B) (B 3 D))) #false)
(check-expect (valid-st-path? sample-graph 'Z 'I '((Z 10 I))) #false)
(check-expect (valid-st-path? sample-graph 'A 'B '()) #false)
(check-expect (valid-st-path? sample-graph 'A 'A '()) #true)
(check-expect (valid-st-path? sample-graph 'A 'B '((A 2 B))) #true)
(check-expect (valid-st-path? sample-graph
                              'A 'B
                              '((A 2 B) (B 3 D) (D 7 B))) #true)
(check-expect (valid-st-path? sample-graph 'B 'G '((B 5 E) (E 1 G))) #true)
(check-expect (valid-st-path? sample-graph 'B 'D '((A 2 B) (B 3 D))) #false)
(check-expect (valid-st-path? sample-graph 'B 'E '((B 5 E) (E 1 G))) #true)
(check-expect (valid-st-path? sample-graph 'A 'H '((A 7 H) (H 1 B))) #false)
(define (valid-st-path? g s t p)
  (local [; valid-st-path-aux?: [Non-empty Path] -> Boolean
          ; can we follow the path suggested by p0 to get from s to t?
          ; assume p0 is a non-empty valid Path and starts with s
          ; suppose s is 'A, t is 'B, g is sample-graph
          ; given: '((A 2 B)), wanted: #true
          ; given: '((A 2 B) (B 3 D)), wanted: #true
          ; given: '((A 1 C) (C 10 G)), wanted: #false
          (define (valid-st-path-aux? s0 p0)
            (cond [(symbol=? s0 t) #true]
                  [(empty? (rest p0)) (and (symbol=? (ori (first p0)) s0)
                                           (symbol=? (dest (first p0)) t))]
                  [else (valid-st-path-aux? (dest (first p0)) (rest p0))]))]
    (cond
      ; for empty p:
      [(empty? p)
       (and (node-in-graph? s g) (symbol=? s t))]
      ; when p is valid, not empty, and starts with s
      [(and (valid-path? g p) (symbol=? s (ori (first p))))
       (valid-st-path-aux? s p)]
      [else #false])))

;;; helper

;; node-in-graph?: Node Graph -> Boolean
;; is the given node nd in g?
(check-expect (node-in-graph? 'A sample-graph) #true)
(check-expect (node-in-graph? 'I sample-graph) #false)
(check-expect (node-in-graph? 'A '()) #false)
(define (node-in-graph? nd g)
  (local [; node-in-edge?: Node Edge -> Boolean
          ; is the node nd in Edge e?
          (define (node-in-edge? nd e)
            (or (symbol=? nd (ori e)) (symbol=? nd (dest e))))]
    (ormap (λ (edge) (node-in-edge? nd edge)) g)))

;;;; Problem 3

;; find-st-path: Node Node Graph -> MaybePath
;; finds a path from Node s to Node t in Graph g
;; if there is no Path, returns #false
;; if s is same as t, returns '()
;; if s or t is not in g, signals an error
(check-error (find-st-path 'A 'A '()) "Node(s) not found")
(check-error (find-st-path 'A 'J sample-graph) "Node(s) not found")
(check-error (find-st-path 'J 'A sample-graph) "Node(s) not found")
(check-expect (find-st-path 'A 'A sample-graph) '())
(check-expect (find-st-path 'A 'B sample-graph) '((A 2 B)))
(check-expect (find-st-path 'C 'G sample-graph)
              '((C 3 H) (H 4 G)))
(check-expect (find-st-path 'A 'G sample-graph)
              '((A 2 B) (B 5 E) (E 1 G)))
(check-expect (find-st-path 'B 'F sample-graph) #false)
(check-expect (find-st-path 'A 'F sample-graph) #false)
(define (find-st-path s t g)
  (local [; find-st-path*: Node Node Graph [List-of Node] -> MaybePath
          ; finds a path from s to t in g
          ; if there is no path, the function returns #false
          ; if s is the same as t, the function returns '((s 0 s))
          ; if s or t is not in g, signals error
          ; ACCUMULATOR: keeps track of visited nodes
          (define (find-st-path* s t g seen)
            (cond [(not (and (node-in-graph? s g)
                             (node-in-graph? t g)))
                   (error "Node(s) not found")]
                  [(symbol=? s t) (list (list s 0 s))]
                  [(member? s seen) #false]
                  [else
                   (local ((define next (neighbors s g))
                           (define candidate
                             (find/list next t g (cons s seen))))
                     (cond [(boolean? candidate) #false]
                           [else (make-path s candidate g)]))]))
          ; find/list: [List-of Node] Node Graph [List-of Node] -> MaybePath
          ; finds a path from some nodes on lo-originations to destination
          (define (find/list lo-ori destination g seen)
            (cond [(empty? lo-ori) #false]
                  [else (local ((define candidate
                                  (find-st-path* (first lo-ori)
                                                 destination g seen)))
                          (cond
                            [(boolean? candidate)
                             (find/list (rest lo-ori) destination g seen)]
                            [else candidate]))]))
          (define candidate (find-st-path* s t g '()))]
    (cond [(boolean? candidate) #false]
          [else (drop-last candidate)])))

;;; helpers

;; make-path: Node Path Graph -> Path
;; links s to the path p according to g
;; constraint 1: s and edges in p must be in g
;; constraint 2: there must be an edge in g that starts with s and ends with
;;               the start node of first edge in p 
(check-expect (make-path 'A '((H 4 G)) sample-graph) '((A 7 H) (H 4 G)))
(check-expect (make-path 'A '((B 5 E) (E 1 G)) sample-graph)
              '((A 2 B) (B 5 E) (E 1 G)))
(define (make-path s p g)
  (local [; find-edge: Graph Node Node -> Edge
          ; finds the edge starting with s and ending with t in g
          ; assume s and t are both in g and there is an edge in g that
          ; starts with s and ends with t
          (define (find-edge g s t)
            (first (filter (λ (edg) (and (symbol=? s (ori edg))
                                         (symbol=? t (dest edg)))) g)))]
    (cons (find-edge g s (ori (first p))) p)))

;; neighbors: Node Graph -> [List-of Node]
;; finds the direct neighbor of node s
;; assume s is in g
(check-expect (neighbors 'A sample-graph) '(B C H))
(check-expect (neighbors 'B sample-graph) '(D E G))
(check-expect (neighbors 'G sample-graph) '())
(define (neighbors s g)
  (cond [(empty? g) '()]
        [else (if (symbol=? (ori (first g)) s)
                  (cons (dest (first g)) (neighbors s (rest g)))
                  (neighbors s (rest g)))]))

;; drop-last: MaybePath -> MaybePath
;; drops the last edge in the path or returns #false if the MaybePath is #false
(check-expect (drop-last #false) #false)
(check-expect (drop-last '()) '())
(check-expect (drop-last '((A 10 B) (B 2 C))) '((A 10 B)))
(define (drop-last mbp)
  (local [; drop-last-aux: Path -> Path
          ; drops the last edge in the given path p
          (define (drop-last-aux p)
            (cond [(empty? p) '()]
                  [(empty? (rest p)) '()]
                  [else (cons (first p) (drop-last-aux (rest p)))]))]
    (cond [(boolean? mbp) #false]
          [else (drop-last-aux mbp)])))


;;;; Problem 4

;; find-shortest-path: Node Node Graph [List-of Node] -> MaybePath
;; returns the shortest path from s to t
;; returns #false if there is no path from s to t
;; returns '() if s is the same as t
;; signals error if s or t is not in g
(check-error (find-shortest-path 'A 'A '()) "Node(s) not found")
(check-error (find-shortest-path 'A 'J sample-graph) "Node(s) not found")
(check-error (find-shortest-path 'J 'A sample-graph) "Node(s) not found")
(check-expect (find-shortest-path 'A 'A sample-graph) '())
(check-expect (find-shortest-path 'A 'B sample-graph) '((A 2 B)))
(check-expect (find-shortest-path 'A 'C sample-graph) '((A 1 C)))
(check-expect (find-shortest-path 'A 'G sample-graph)
              '((A 1 C) (C 3 H) (H 4 G)))
(check-expect (find-shortest-path 'C 'G sample-graph)
              '((C 3 H) (H 4 G)))
(check-expect (find-shortest-path 'A 'F sample-graph) #false)
(define (find-shortest-path s t g)
  (local [;; find-shortest-path*: Node [List-of Node] -> MaybePath
          ;; returns the shortest path from s to t
          ;; returns #false if there is no path from s to t
          ;; returns '() if s is the same as t
          ;; signals error if s or t is not in g
          ;; ACCUMULATOR: keeps track of nodes already visited
          (define (find-shortest-path* s seen)
            (cond [(not (and (node-in-graph? s g)
                             (node-in-graph? t g)))
                   (error "Node(s) not found")]
                  [(symbol=? s t) (list (list s 0 s))]
                  [(member? s seen) #false]
                  [else
                   (local [(define next (neighbors s g))
                           (define candidates
                             (find/list next (cons s seen)))
                           (define lo-paths (make-paths s candidates g))]
                     (shortest-or-false lo-paths))]))
          ;; find/list: [List-of Node] [List-of Node] -> [List-of MaybePath]
          ;; finds the shortest paths for each source s in lon to destination t
          ;; ACCUMULATOR: keeps track of visited nodes
          (define (find/list lon-s seen)
            (cond [(empty? lon-s) '()]
                  [else (cons (find-shortest-path* (first lon-s) seen)
                              (find/list (rest lon-s) seen))]))]
    (drop-last (find-shortest-path* s '()))))

;;; helpers

;; make-paths: Node [List-of MaybePath] Graph -> [List-of MaybePath]
;; links the source to its corresponding paths in g
;; returns #false if lo-maybe is #false
;; constraint 1: the source and Paths must be in g
;; constraint 2: there must be an edge from source to each starting node
;;               in each Path in lo-maybe
(check-expect (make-paths 'A '(((B 3 D)) ((C 3 H) (H 4 G))) sample-graph)
              '(((A 2 B) (B 3 D)) ((A 1 C) (C 3 H) (H 4 G))))
(check-expect (make-paths 'A (list #false) sample-graph) (list #false))
(check-expect (make-paths 'A '(((C 3 H) (H 4 G)) #false) sample-graph)
              '(((A 1 C) (C 3 H) (H 4 G)) #false))
(define (make-paths s lo-maybe g)
  (cond [(empty? lo-maybe) (list #false)]
        [else
         (map (λ (maybe-path)
                (cond [(boolean? maybe-path) #false]
                      [else (make-path s maybe-path g)])) lo-maybe)]))

;; shortest-or-false: [List-of MaybePath] -> MaybePath
;; returns the shortest path from a list of MaybePath if possible,
;; if all MaybePaths on lo-maybe are #false, returns #false
(check-expect (shortest-or-false '(#false #false #false)) #false)
(check-expect (shortest-or-false '()) #false)
(check-expect (shortest-or-false '(#false ((a 2 b) (b 3 c))))
              '((a 2 b) (b 3 c)))
(check-expect (shortest-or-false
               '(((b 5 e) (e 1 g)) ((c 10 g)) ((d 1 c) (c 2 b) (b 3 d))))
              (list (list 'd 1 'c) (list 'c 2 'b) (list 'b 3 'd)))
(define (shortest-or-false lo-maybe)
  (local [(define valid-path
            (filter (λ (maybe-path) (list? maybe-path)) lo-maybe))
          ; shortest: [Non-empty List-of Path] -> Path
          ; selects the shortest path from lop
          (define (shortest lop)
            (foldr (λ (path b) (if (< (distance path) (distance b))
                                   path b))'(("base" +inf.0 "base")) lop))]
    (cond [(empty? valid-path) #false]
          [else (shortest valid-path)])))
                  
;; distance: Path -> Number
;; calculates the total distance of the given Path p
(check-expect (distance '((A 1 B) (B 2 C))) 3)
(check-expect (distance '()) 0)
(define (distance p)
  (foldr (λ (edge base) (+ (dist edge) base)) 0 p))
