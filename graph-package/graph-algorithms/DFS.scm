; Depth-first search

(load "graph-package/basic-data-types/set.scm")
(load "graph-package/basic-data-types/pair.scm")
(load "graph-package/basic-data-types/graph.scm")
(load "graph-package/util.scm")

(define g (make-empty-graph))
(define g (add-node g 'C))
(define g (add-node g 'B))
(define g (add-node g 'A))
(define g (add-edge g (make-edge 'A 'B)))
(define g (add-edge g (make-edge 'A 'C)))
(define g (add-edge g (make-edge 'B 'A)))
(define g (add-edge g (make-edge 'C 'B)))

(define (DFS graph start)
  (letrec ((DFS-recursive (lambda (node visited)
                            (if (set-member? node visited)
                                visited
                                (accumulate (lambda (neighbour acc)
                                         (DFS-recursive neighbour acc))
                                       (set-insert node visited)
                                       (neighbors graph node))))))
    (DFS-recursive start (set-create-empty))))

(define g1 (make-empty-graph))
(define g1 (add-node g1 'a))
(define g1 (add-node g1 'b))
(define g1 (add-node g1 'c))
(define g1 (add-node g1 'd))
(define g1 (add-node g1 'e))
(define g1 (add-edge g1 (make-edge 'a 'b)))
(define g1 (add-edge g1 (make-edge 'a 'c)))
(define g1 (add-edge g1 (make-edge 'b 'd)))
(define g1 (add-edge g1 (make-edge 'b 'e)))
(define g1 (add-edge g1 (make-edge 'c 'e)))
(display g1)