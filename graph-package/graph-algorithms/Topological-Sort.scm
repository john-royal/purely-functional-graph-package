; Topological Search

(load "../basic-data-types/set.scm")
(load "../basic-data-types/pair.scm")
(load "../basic-data-types/graph.scm")
(load "../util.scm")


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

(define (dfs-topo-sort node graph sorted)
  (if (set-member? node sorted)
      sorted
      (let iter-connect ((neighbors (neighbors graph node))
                         (sorted sorted))
        (if (null? neighbors)
            (set-insert node sorted)  ;; Add current node to sorted after visiting all neighbors
            (iter-connect (cdr neighbors) (dfs-topo-sort (car neighbors) graph sorted)))))) ;; Recursively visit neighbors

;topological-sort: This function performs a topological sort of the graph. It initializes an empty set of visited nodes and an
; empty list for the order. It then iterates through all the nodes in the graph, and for each node, it calls the dfs-topo-sort
; function to visit the node and all nodes reachable from it.

(define (topological-sort graph)
  (let iter ((unvisited (nodes graph))
             (sorted (set-create-empty)))
    (if (null? unvisited) (reverse sorted)
        (let* ((node (car unvisited))
               (sorted (dfs-topo-sort node graph sorted)))
          (iter (set-difference unvisited sorted)
                sorted)))))

; Postcondition: The topological-sort function will return a list of nodes in topological order, meaning that for every directed edge from
; node U to node V, U comes before V in the ordering. If the graph is not a DAG, the result will not be a valid topological order.





; Test
(define dg (make-empty-graph))
(define dg (add-node dg 0))
(define dg (add-node dg 1))
(define dg (add-node dg 2))
(define dg (add-node dg 3))
(define dg (add-node dg 4))
(define dg (add-node dg 5))
(define dg (add-node dg 6))
(define dg (add-node dg 7))
(define dg (add-edge dg (make-edge 0 1)))
(define dg (add-edge dg (make-edge 0 4)))
(define dg (add-edge dg (make-edge 1 3)))
(define dg (add-edge dg (make-edge 1 5)))
(define dg (add-edge dg (make-edge 1 6)))
(define dg (add-edge dg (make-edge 2 6)))
(define dg (add-edge dg (make-edge 5 7)))
(display dg)
(newline)
(newline)
(topological-sort dg)
