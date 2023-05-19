; Clique for a graph (Brute force)

(load "../basic-data-types/set.scm")
(load "../basic-data-types/pair.scm")

; --------------------------------------------------------------------------------------------------------------------------------------------

; Placeholder for graph stuff

(define (make-empty-graph) '())

(define (add-node graph node)
  (if (assoc node graph) graph
      (cons (pair node (set-create-empty)) graph)))

(define (remove-node graph node)
  (cond ((null? graph) '())
        ((equal? (first (car graph)) node) (cdr graph))
        (else (cons (car graph) (remove-node (cdr graph) node)))))

(define (add-edge graph node1 node2)
  (if (or (null? graph) (not (assoc node1 graph)) (not (assoc node2 graph)))
      graph
      (let* ((entry (car graph))
             (node (first entry))
             (neighbors (second entry)))
        (if (equal? node node1)
            (cons (pair node (set-insert node2 neighbors)) (cdr graph))
            (cons entry (add-edge (cdr graph) node1 node2))))))


(define (remove-edge graph node1 node2)
  (if (null? graph) '()
      (let* ((entry (car graph))
             (node (first entry))
             (neighbors (second entry)))
        (if (equal? node node1)
            (cons (pair node (set-remove node2 neighbors)) (cdr graph))
            (cons entry (remove-edge (cdr graph) node1 node2))))))

(define (neighbors graph node)
  (let ((node-assoc (assoc node graph)))
    (if node-assoc
        (second node-assoc)
        '())))
(define (adjacent? graph node1 node2)
  (if (or (not (assoc node1 graph)) (not (assoc node2 graph)))
      #f
      (let ((n (neighbors graph node1)))
        (set-member? node2 n))))
(define (nodes graph)
  (map car graph))

(define (edges graph)
  (if (null? graph) '()
      (let* ((entry (car graph))
             (node (first entry))
             (connected-nodes (second entry)))
        (set-union (map (lambda (node2) (pair node node2)) connected-nodes) (edges (cdr graph))))))

(define g (make-empty-graph))

(define g (make-empty-graph))
(define g (add-node g 'A))
(define g (add-node g 'B))
(define g (add-node g 'C))
(define g (add-node g 'D))
(define g (add-node g 'E))

(define g (add-edge g 'A 'B))
(define g (add-edge g 'A 'C))
(define g (add-edge g 'B 'A))
(define g (add-edge g 'B 'C))
(define g (add-edge g 'B 'D))
(define g (add-edge g 'C 'A))
(define g (add-edge g 'C 'B))
(define g (add-edge g 'C 'E))
(define g (add-edge g 'D 'B))
(define g (add-edge g 'D 'E))
(define g (add-edge g 'E 'C))
(define g (add-edge g 'E 'D))

(display g)
(newline)


(newline)
(newline)


; --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

; Okay so I learned the hardway that using my previous method won't work, so let's try again with DFS
; This can actually work really well, when I thought it out
; A clique is basically a group of connected edges, and we want to find biggest of that
; How shall we do it?
; Well we can use DFS go through one vertex and find all of the connected vertices with it, and label that
; Then we can keep doing that with different vertices and after compare them all and see which set is the biggest
; Possibly straight forward

;Base case: The base case is when the list of remaining nodes is empty. If there are no remaining nodes,
;the current clique (which could be empty or contain some nodes) is returned.
; IH: Assume that the function dfs correctly finds the maximal clique for a subset of the remaining nodes.
; IS: For each remaining node, it is considered for addition to the current clique. If the addition of the
; node to the clique forms a valid clique and increases its size, the node is added to the clique, and the
; dfs function is called on the rest of the remaining nodes with the new clique. If not, the node is not added
; to the clique, and the dfs function is called twice: once with the original clique and once with the new clique.
; The larger of the two cliques returned by these calls is selected as the result.
; This is used to substitue the max builty-function to find the max clique

;Precondition: The maximal-clique function takes a graph with its nodes and edges defined.

;max-lst: This function takes two lists x and y and a comparator function cmp. If the
;comparator function returns true for x and y, x is returned; otherwise, y is returned.
(define (max-lst x y cmp)
  (if (cmp x y) x y))

;every: This function takes a predicate function pred and a list lst and returns true if the predicate
; is true for all elements of the list; otherwise, it returns false.
(define (every pred lst)
  (cond ((null? lst) #t)
        ((pred (car lst)) (every pred (cdr lst)))
        (else #f)))

;all-pairs-adjacent?: This function checks if all pairs of nodes in a given set are adjacent in a graph.
;It returns true if all pairs are adjacent and false otherwise. The adjacency check is performed by taking
;a node from the set and checking if it is adjacent to all other nodes in the set.
(define (all-pairs-adjacent? graph nodes)
  (if (null? nodes)
      #t
      (let ((node (car nodes))
            (rest-nodes (cdr nodes)))
        (and (every (lambda (n) (adjacent? graph node n)) rest-nodes)
             (all-pairs-adjacent? graph rest-nodes)))))

;maximal-clique: This function uses DFS to find the maximal clique in a graph.
; It does this by recursively exploring all possible cliques and selecting the
; largest one that meets the adjacency requirement.

(define (maximal-clique graph)
  (letrec ((dfs (lambda (clique remaining)
                  (if (null? remaining)
                      clique
                      (let ((node (car remaining))
                            (rest (cdr remaining)))
                        (let ((new-clique (cons node clique)))
                          (if (and (all-pairs-adjacent? graph new-clique)
                                   (> (length new-clique) (length clique)))
                              (dfs new-clique rest)
                              (max-lst (dfs clique rest) (dfs new-clique rest) (lambda (x y) (> (length x) (length y)))))))))))
    (dfs '() (nodes graph))))
; This function uses DFS, it has an empty clique and then the graph of adjacient nodes that haven't been added to the clique
; If there are node nodes left in the remaining, then there is a clique
; If there are nodes remaining, we take the first node, and test if it's a valid clique
; if it is bigger than our current clique, we fun the function all over again.
; if the new clique isn't bigger than the current, we then run DFS two times to compare
; which clique is bigger.
; This processes until all possible cliques are found

 ;Postcondition: The maximal-clique function returns the maximal clique in the graph, which is a
; list of nodes where every two nodes are connected by an edge. If no such clique exists, it returns an empty list.

; Test
(maximal-clique g)

; Let's goooooo it works finally 




