; Determine if a path exists between the given start and target nodes using DFS.

(define (dfs-path-exists? graph start target)
  (letrec ((DFS-recursive (lambda (node visited)
                            (if (equal? node target)
                                #t
                                (if (set-member? node visited)
                                    #f
                                    (accumulate (lambda (neighbour acc)
                                             (or acc (DFS-recursive neighbour (set-insert node visited))))
                                           #f
                                           (neighbors graph node)))))))
    (DFS-recursive start (set-create-empty))))

(define g (make-empty-graph))
(define g (add-node g 'C))
(define g (add-node g 'B))
(define g (add-node g 'A))
(define g (add-edge g (make-edge 'A 'B)))
(define g (add-edge g (make-edge 'A 'C)))
(define g (add-edge g (make-edge 'B 'A)))
(define g (add-edge g (make-edge 'C 'B)))
(display g)
(display (dfs-path-exists? g 'A 'B))
