; Bipartite graph

(load "../basic-data-types/set.scm")
(load "../basic-data-types/pair.scm")

; --------------------------------------------------------------------------------------------------------------------------------------------

; Placeholder for graph stuff
(define (make-empty-graph) '())

(define (add-node graph node)
  (if (assoc node graph) graph
      (cons (pair node (set-create-empty)) graph)))

(define (remove-node graph node)
  (cond ((null? graph) '())
        ((equal? (first (car graph)) node) (cdr graph))
        (else (cons (car graph) (remove-node (cdr graph) node)))))

(define (add-edge graph node1 node2)
  (if (or (null? graph) (not (assoc node1 graph)) (not (assoc node2 graph)))
      graph
      (let* ((entry (car graph))
             (node (first entry))
             (neighbors (second entry)))
        (if (equal? node node1)
            (cons (pair node (set-insert node2 neighbors)) (cdr graph))
            (cons entry (add-edge (cdr graph) node1 node2))))))
; For directed graphs

(define (make-directed-graph)
  '())
(define (add-directed-edge graph node1 node2)
  (if (null? graph) '()
      (let* ((entry (car graph))
             (node (first entry))
             (neighbors (second entry)))
        (if (equal? node node1)
            (cons (pair node (set-insert node2 neighbors)) (cdr graph))
            (cons entry (add-edge (cdr graph) node1 node2))))))

; Weighted graph
; This is a temp implementation, we'll imporve the interface later

(define (weighted-graph)
  '())

(define (add-weighted-edge graph node1 node2 weight)
  (if (or (null? graph) (not (assoc node1 graph)) (not (assoc node2 graph)))
      graph
      (let* ((entry (car graph))
             (node (first entry))
             (neighbors (second entry)))
        (if (equal? node node1)
            (cons (pair node (set-insert (pair node2 weight) neighbors)) (cdr graph))
            (cons entry (add-weighted-edge (cdr graph) node1 node2 weight))))))


(define (remove-edge graph node1 node2)
  (if (null? graph) '()
      (let* ((entry (car graph))
             (node (first entry))
             (neighbors (second entry)))
        (if (equal? node node1)
            (cons (pair node (set-remove node2 neighbors)) (cdr graph))
            (cons entry (remove-edge (cdr graph) node1 node2))))))

(define (neighbors graph node)
  (let ((node-assoc (assoc node graph)))
    (if node-assoc
        (if (pair? (cdr node-assoc)) ; check if pair?
            (cdr (cdr node-assoc)) ; get the second element of the pair (the neighbors)
            '())
        '())))

(define (adjacent? graph node1 node2)
  (if (or (not (assoc node1 graph)) (not (assoc node2 graph)))
      #f
      (let ((n (neighbors graph node1)))
        (set-member? node2 n))))


(define (nodes graph)
  (map car graph))

(define (edges graph)
  (if (null? graph) '()
      (let* ((entry (car graph))
             (node (first entry))
             (connected-nodes (second entry)))
        (set-union (map (lambda (node2) (pair node node2)) connected-nodes) (edges (cdr graph))))))

(define (number-of-nodes graph)
  (length (nodes graph)))

(define wg (weighted-graph))
(define wg (add-node wg 'A))
(define wg (add-node wg 'B))
(define wg (add-node wg 'C))
(define wg (add-node wg 'D))
(define wg (add-node wg 'E))
(define wg (add-node wg 'F))

(define wg (add-weighted-edge wg 'A 'B 1))
(define wg (add-weighted-edge wg 'C 'B 3))
(define wg (add-weighted-edge wg 'F 'B 5))
(define wg (add-weighted-edge wg 'D 'C 9))
;(display wg)
;(newline)
;(newline)

(define g (make-empty-graph))
(define g (add-node g 'A))
(define g (add-node g 'B))
(define g (add-node g 'C))
(define g (add-node g 'D))
(define g (add-node g 'E))
(define g (add-node g 'F))
(define g (add-edge g 'A 'B))
(define g (add-edge g 'B 'C))
(define g (add-edge g 'C 'A)) ; Cycle of odd length (3), making the graph non-bipartite.
(display g)
(newline)
(newline)
; --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

; Okay John I couldn't figure out Bipartite graphs, so I'm just gonna keep doing specifications with chatgpt until I get an algorithm for it

; Design Idea/Proof

; Base case: If there are no remaining nodes to visit, it returns true, indicating the graph is bipartite.
; Inductive Hypothesis (IH): Assume that the function dfs-visit correctly checks if the current node and
; its neighbors can be colored in a way that makes the graph bipartite.
; Inductive Step (IS): For each node, it colors the node red (if it's not colored yet) and then checks
; if its neighbors can be colored with the opposite color (blue) without conflict.
; Precondition: The is-bipartite? function takes a graph as an argument. The graph must be a valid data structure where nodes and edges can be fetched.

; color-node: This function colors a node in the graph. If the node is already in the graph, it replaces the old color with the new one.
; If the node is not in the graph, it adds the node with the new color.
(define (color-node graph node color)
  (let ((entry (assoc node graph)))
    (if entry
        (let* ((node-data (car entry))
               (neighbors-set (cdr entry))
               (old-color (if (pair? neighbors-set) (car neighbors-set) '())))
          (if (equal? old-color '())
              (cons (cons node-data (cons color (cdr neighbors-set))) (cdr graph))
              (cons entry (color-node (cdr graph) node color))))
        graph)))
; get-color: This function retrieves the color of a node in the graph.
(define (get-color graph node)
  (let ((node-assoc (assoc node graph)))
    (if (and node-assoc (pair? (cdr node-assoc)))
        (car (cdr node-assoc))
        '())))

; dfs-visit: This function uses a depth-first search (DFS) approach to color a node and its neighbors. It colors the node
; with a given color, then recursively calls itself on its uncolored neighbors with the opposite color. If a neighbor has the
; same color as the node, it returns #f, indicating that the graph is not bipartite.
(define (dfs-visit graph node color)
  (let ((graph (color-node graph node color))
        (adj-nodes (neighbors graph node)))
    (cond ((null? adj-nodes) #t)
          (else
           (let* ((adj-node (car adj-nodes))
                  (adj-color (get-color graph adj-node)))
             (cond ((equal? adj-color color) #f)
                   ((equal? adj-color '()) 
                    (dfs-visit (color-node graph adj-node (if (equal? color 'red) 'blue 'red)) adj-node (if (equal? color 'red) 'blue 'red)))
                   (else (dfs-visit graph (cdr adj-nodes) color))))))))
; is-bipartite?: This is the main function that checks whether the graph is bipartite or not. It starts by creating a list of nodes.
; It then iteratively colors each node (if it's not colored yet) and checks if the graph can be colored in a bipartite manner using dfs-visit.
(define (is-bipartite? graph)
  (let ((nodes-list (map car graph)))
    (let loop ((remaining-nodes nodes-list)
               (colored-graph graph))
      (cond ((null? remaining-nodes) #t)
            ((not (dfs-visit colored-graph (car remaining-nodes) 'red)) #f)
            (else (loop (cdr remaining-nodes) (color-node colored-graph (car remaining-nodes) 'red)))))))

; Postcondition: The is-bipartite? function will return a boolean value indicating whether the graph is bipartite or not.
; If the graph is bipartite, it will return #t, otherwise, it will return #f.





;Test

(is-bipartite? g)

