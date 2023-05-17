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

; This is used to substitue the max builty-function to find the max clique 
(define (max-lst x y cmp)
  (if (cmp x y) x y))

; This function takes a predicate (boolean) and a lst, returs true for all elements of the list or false
(define (every pred lst)
  (cond ((null? lst) #t)
        ((pred (car lst)) (every pred (cdr lst)))
        (else #f)))


(define (all-pairs-adjacent? graph nodes)
  (if (null? nodes)
      #t
      (let ((node (car nodes))
            (rest-nodes (cdr nodes)))
        (and (every (lambda (n) (adjacent? graph node n)) rest-nodes)
             (all-pairs-adjacent? graph rest-nodes)))))

; This function checks if all nodes in the graph are adjacient to each other. This is done by
; checking if the first node is adjacient to every other node, then repeating the process
; If all nodes are adjacient, they form a clique

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

; Test
(maximal-clique g)

; Let's goooooo it works finally 




