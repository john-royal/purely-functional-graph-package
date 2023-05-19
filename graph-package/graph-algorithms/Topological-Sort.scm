; Topological Search

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
        (second node-assoc)
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


; --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

; Let's try changing up topological sort now

; Old design plan:
 ;This will be a bit tricky, it seems, but essentially this sorting method, just follow a direct line of vertices connected by the edge
; so like this A -> B -> C -> D

; So first a graph can only be topological if it's acyclic, lucky for us we already have a function that does that, so we can check that off.
; Next we can use DFS to help us out with this.
; First we can create an empty stack to hold all the vertices that we sort
; Then create a set to keep track of visited vertices
; We want to go through adjacient vertices. and push the visited ones up to the stack
; It will sort the order from top to bottom, if we can execute this right LOL.

; First let's implement the DFS helper function we need
; We need to slightlty modify the previous ones, not too much work for that

; Design Plan/Proof

; Base case: If the current vertex has been visited already, then it returns the current visited set and order.
; If there are no vertices left to check in the iter function, it returns the current order.
; Inductive Hypothesis (IH): Assume that the function dfs-topo-sort correctly visits all the vertices reachable from
; the current vertex and updates the visited set and order accordingly. Assume that the function iter correctly performs
; a topological sort of the remaining vertices given the current visited set and order.
; Inductive Step (IS): For each vertex, if it hasn't been visited yet, the dfs-topo-sort function is called to visit the vertex
; and all vertices reachable from it. The visited set and order are updated based on the result of the dfs-topo-sort function, and
; the iter function is called on the remaining vertices.
; Precondition: The topological-sort function takes a graph as an argument. The graph must be a valid data structure where nodes and
; their neighbors can be fetched, and it must be a DAG (Directed Acyclic Graph), meaning there are no cycles.

; dfs-topo-sort: This function performs a depth-first traversal of the graph from the current vertex. It updates the visited set and order
; list based on the traversal. It iterates through the neighbors of the current vertex and recursively calls itself on each neighbor that hasn't been visited yet.

(define (dfs-topo-sort current visited order graph)
  (if (set-member? current visited)
      (pair visited order)
      (let ((neighbors (neighbors graph current)))
        (define (iter-connect nbrs visited order)
          (if (null? nbrs)
              (pair (set-insert current visited) (cons current order))  ;; Add current node to visited and order after visiting all neighbors
              (let ((nbr (car nbrs)))
                (if (not (set-member? nbr visited))
                    (let ((result (dfs-topo-sort nbr (set-insert current visited) order graph)))
                      (iter-connect (cdr nbrs) (first result) (second result)))  ;; Recursively visit neighbors
                    (iter-connect (cdr nbrs) visited order)))))
        (iter-connect neighbors visited order))))

;topological-sort: This function performs a topological sort of the graph. It initializes an empty set of visited nodes and an
; empty list for the order. It then iterates through all the nodes in the graph, and for each node, it calls the dfs-topo-sort
; function to visit the node and all nodes reachable from it.

(define (topological-sort graph) ; the graph is the input value
  (letrec ((iter (lambda (vertices visited order) ; letrec helps us store a local function for iteration
      (if (null? vertices) ; if we run out of vertices to check, it returns us the visited vertices 
          order
          (let ((vertex (car vertices)))
            (if (set-member? vertex visited)
                (iter (cdr vertices) visited order)
                (let ((result (dfs-topo-sort vertex visited order graph)))
                  (iter (cdr vertices) (first result) (second result)))))))))
    (iter (nodes graph) (set-create-empty) '())))

; Postcondition: The topological-sort function will return a list of nodes in topological order, meaning that for every directed edge from
; node U to node V, U comes before V in the ordering. If the graph is not a DAG, the result will not be a valid topological order.

; Test
(define dg (make-directed-graph))
(define dg (add-node dg 'A))
(define dg (add-node dg 'B))
(define dg (add-node dg 'C))
(define dg (add-node dg 'D))
(define dg (add-node dg 'E))
(define dg (add-node dg 'F))
(define dg (add-directed-edge dg 'A 'B))
(define dg (add-directed-edge dg 'C 'B))
(define dg (add-directed-edge dg 'F 'B))
(define dg (add-directed-edge dg 'D 'C))
(display dg)
(newline)
(newline)
(define sorted-nodes (topological-sort dg))
(display sorted-nodes)
