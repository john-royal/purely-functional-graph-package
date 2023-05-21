; Determines if the graph is connected.
; This doesn't work (infinite loop).

(load "graph-package/basic-data-types/set.scm")
(load "graph-package/basic-data-types/pair.scm")
(load "graph-package/basic-data-types/graph.scm")
(load "graph-package/util.scm")

(define (dfs-connected current visited graph)
  (if (set-member? current visited)
      visited
      (let ((neighbors (neighbors graph current)))
        (define (iter-connect nbrs)
          (if (null? nbrs)
              visited 
              (let ((nbr (first nbrs)))
                (if (not (set-member? nbr visited))
                    (iter-connect (dfs-connected nbr (set-insert current visited) graph))
                    (iter-connect (cdr nbrs))))))
        (iter-connect neighbors))))


(define (connected? graph)
  (let* ((start-node (car (nodes graph)))
         (visited (dfs-connected start-node (set-create-empty) graph)))
    (= (length (nodes graph)) (length visited))))

(define g (make-empty-graph))
(define g (add-node g 'C))
(define g (add-node g 'B))
(define g (add-node g 'A))
(define g (add-edge g (make-edge 'A 'B)))
(define g (add-edge g (make-edge 'A 'C)))
(define g (add-edge g (make-edge 'B 'A)))
(define g (add-edge g (make-edge 'C 'B)))

(display (connected? g))
