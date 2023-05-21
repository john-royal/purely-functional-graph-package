; Topological Search

(load "graph-package/basic-data-types/set.scm")
(load "graph-package/basic-data-types/pair.scm")
(load "graph-package/basic-data-types/graph.scm")
(load "graph-package/util.scm")

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
(display (topological-sort dg)) ; (0 1 2 3 4 5 6 7)
