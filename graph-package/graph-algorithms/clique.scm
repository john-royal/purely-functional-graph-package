; Clique for a graph (Brute force)

(load "../basic-data-types/set.scm")
(load "../basic-data-types/pair.scm")
(load "../basic-data-types/abstract-graph.scm")
(load "../util.scm")

(define g (make-empty-graph))

(define g (make-empty-graph))
(define g (add-node g 'A))
(define g (add-node g 'B))
(define g (add-node g 'C))
(define g (add-node g 'D))
(define g (add-node g 'E))

(define g (add-edge g (make-edge 'A 'B)))
(define g (add-edge g (make-edge 'A 'C)))
(define g (add-edge g (make-edge 'B 'A)))
(define g (add-edge g (make-edge 'B 'C)))
(define g (add-edge g (make-edge 'B 'D)))
(define g (add-edge g (make-edge 'C 'A)))
(define g (add-edge g (make-edge 'C 'B)))
(define g (add-edge g (make-edge 'C 'E)))
(define g (add-edge g (make-edge 'D 'B)))
(define g (add-edge g (make-edge 'D 'E)))
(define g (add-edge g (make-edge 'E 'C)))
(define g (add-edge g (make-edge 'E 'D)))

(display g)
(newline)


; --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


;compare This function takes two items, x and y, and a comparator function cmp. If the
;comparator function returns true for x and y, x is returned; otherwise, y is returned.
(define (compare x y cmp)
  (if (cmp x y) x y))

;all-pairs-adjacent?: This function checks if all pairs of nodes in a given set are adjacent in a graph.
;It returns true if all pairs are adjacent and false otherwise. The adjacency check is performed by taking
;a node from the set and checking if it is adjacent to all other nodes in the set.
(define (all-pairs-adjacent? graph nodes)
  (if (null? nodes) #t
      (let ((node1 (car nodes))
            (remaining-nodes (cdr nodes)))
        (and (every (lambda (node2) (adjacent? graph node1 node2)) remaining-nodes)
             (all-pairs-adjacent? graph remaining-nodes)))))

;maximal-clique: This function uses DFS to find the maximal clique in a graph.
; It does this by recursively exploring all possible cliques and selecting the
; largest one that meets the adjacency requirement.

(define (maximal-clique graph)
  (let dfs ((clique '())
            (remaining-nodes (nodes graph)))
    (if (null? remaining-nodes)
        clique
        (let* ((node (car remaining-nodes))
               (rest (cdr remaining-nodes))
               (new-clique (cons node clique)))
          (if (all-pairs-adjacent? graph new-clique)
              (dfs new-clique rest)
              (compare (dfs clique rest) (dfs new-clique rest) (lambda (lst1 lst2) (> (length lst1) (length lst2)))))))))


; Test
(maximal-clique g) ; (a b c d e)





