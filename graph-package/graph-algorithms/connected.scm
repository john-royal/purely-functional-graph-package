; Determines if the graph is connected.

(load "graph-package/basic-data-types/set.scm")
(load "graph-package/basic-data-types/pair.scm")
(load "graph-package/basic-data-types/graph.scm")
(load "graph-package/util.scm")

(define (dfs-connected current visited graph)
  (let iter ((node current)
             (visited visited))
      (if (set-member? node visited)
          visited
          (accumulate (lambda (neighbor acc)
                              (iter neighbor acc))
                      (set-insert node visited)
                      (neighbors graph node)))))


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
