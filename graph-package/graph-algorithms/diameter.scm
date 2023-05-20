; graph-algorithms/diameter.scm


(load "../basic-data-types/set.scm")
(load "./dijkstra.scm")

; Returns all paths in a graph, i.e. all possible combinations of nodes
(define (all-paths graph)
  (let ((nodes (nodes graph)))
    (let iter ((unchecked-nodes nodes)
               (paths '()))
      (if (null? unchecked-nodes) paths
          (iter (cdr unchecked-nodes) (set-union
                                       (map (lambda (node) (pair (car unchecked-nodes) node)) nodes)
                                       paths))))))

; Computes the diameter of a graph.
(define (diameter graph)
  ; Compute the length of the shortest path between all nodes in the graph.
  ; Return the maximum shortest path.
  ; Note: `apply` invokes a function with a list of arguments, e.g. (apply max (list 1 2 3)) = (max 1 2 3) = 3
  (let ((paths (map (lambda (path) (dijkstra graph (first path) (second path))) (all-paths graph))))
    (apply max (filter number? paths))))

(define g (make-empty-graph))
(define g (add-node g 'A))
(define g (add-node g 'B))
(define g (add-node g 'C))
(define g (add-node g 'D))
(define g (add-edge g (make-edge 'A 'B 2)))
(define g (add-edge g (make-edge 'A 'C 2)))
(define g (add-edge g (make-edge 'B 'C 2)))
(define g (add-edge g (make-edge 'C 'D 2)))

(diameter g) ; 5

