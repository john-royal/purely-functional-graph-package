; Determine if a graph if acyclic.
; This currently doesn’t work correctly.

(load "graph-package/basic-data-types/set.scm")
(load "graph-package/basic-data-types/graph.scm")

(define (dfs-acyclic current_vertex parent graph visited)
  (if (set-member? current_vertex visited)
      (if (equal? parent current_vertex)
          #f
          #t)
      (let loop ((neighbors (neighbors graph current_vertex)))
        (if (null? neighbors)
            #f
            (let ((neighbor (car neighbors)))
              (if (not (equal? parent neighbor))
                  (if (dfs-acyclic neighbor current_vertex graph (set-insert current_vertex visited))
                      #t
                      (loop (cdr neighbors)))
                  (loop (cdr neighbors))))))))

(define (acyclic? graph)
  (let ((starting_vertex (first (nodes graph)))) 
    (dfs-acyclic starting_vertex #f graph (set-create-empty))))

(define g (make-empty-graph))
(define g (add-node g 'C))
(define g (add-node g 'B))
(define g (add-node g 'A))
(define g (add-edge g (make-edge 'A 'B)))
(define g (add-edge g (make-edge 'A 'C)))
(define g (add-edge g (make-edge 'B 'A)))
(define g (add-edge g (make-edge 'C 'B)))
(display g)
(display (acyclic? g)) ; should be #f, but is #t
