; graph-algorithms/diameter.scm


(load "../basic-data-types/set.scm")
(load "./dijkstra.scm")


; Given a list of edges, returns a set of nodes, e.g. ((A B 1) (B A 1)) => (A B)
(define (nodes graph)
  (set-create (map car graph)))


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
  (apply max (map
              (lambda (path) (dijkstra graph (first path) (second path)))
              (all-paths graph))))


(define graph
  (list (make-edge 'A 'B 1)
        (make-edge 'B 'A 1)
        (make-edge 'A 'C 3)
        (make-edge 'C 'A 3)
        (make-edge 'B 'C 2)
        (make-edge 'C 'B 2)))

(diameter graph) ; 3

