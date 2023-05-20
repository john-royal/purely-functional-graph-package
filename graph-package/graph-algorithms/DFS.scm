
; Depth-first Search
(load "../basic-data-types/set.scm")
(load "../basic-data-types/pair.scm")
(load "../basic-data-types/graph.scm")

; --------------------------------------------------------------------------------------------------------------------------------------------

; Placeholder for graph stuff


(define g (make-empty-graph))
(define g (add-node g 'C))
(define g (add-node g 'B))
(define g (add-node g 'A))
(define g (add-edge g 'A 'B))
(define g (add-edge g 'A 'C))
(define g (add-edge g 'B 'A))
(define g (add-edge g 'C 'B))
;(display g)

;(newline)
;(newline)

; --------------------------------------------------------------------------------------------------------------------------------------------

(define (foldl f initial lst)
  (if (null? lst)
      initial
      (foldl f (f (car lst) initial) (cdr lst))))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
        

(define (DFS graph start)
  (letrec ((DFS-recursive (lambda (node visited)
                            (if (set-member? node visited)
                                visited
                                (foldl (lambda (neighbour acc)
                                         (DFS-recursive neighbour acc))
                                       (set-insert node visited)
                                       (neighbors graph node))))))
    (DFS-recursive start (set-create-empty))))
; Test

;(DFS g 'a)



; ------------------------------------------------------------------------------------------------------------------------------------------


(define (DFS-path graph start target)
  (letrec ((DFS-recursive (lambda (node visited)
                            (if (equal? node target)
                                #t
                                (if (set-member? node visited)
                                    #f
                                    (foldl (lambda (neighbour acc)
                                             (or acc (DFS-recursive neighbour (set-insert node visited))))
                                           #f
                                           (neighbors graph node)))))))
    (DFS-recursive start (set-create-empty))))



; Test
;(display (DFS-path g 'A 'C))
;(newline)
;(newline)
; -------------------------------------------------------------------------------------------------------------------------------------------------------------------


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

(define (is-acyclic graph)
  (let ((starting_vertex (first (nodes graph)))) 
    (dfs-acyclic starting_vertex #f graph (set-create-empty))))


;Test

;(is-acyclic g)

; -------------------------------------------------------------------------------------------------------------------------------

; Now to check if the graph is connected or not, which will also be done through DFS
; Original design plan
; So we want to check if the graph is connected, which means that all the vertices are connected through one parent vertex
; Honestly from all we've done so far this isn't diffuclt, we essentially can just run DFS, and calculate from this
; We'll reimplement DFS, and then a helper function that checks the number of vertices in the graph
; The point of that is to compare the visited elements of DFS with how many vertices there are in the graph
; Then we create a function connect? that essentially started DFS on the first vertex.
; Once that is done, it compares the visted vertices from DFS to the number of vertices in the graph
; If the number of vertices are the same, we have a connected graph!

; I was able to just replace some of the code with the abstract datatypes, which wasn't too hard to do

(define (node-value node)
  (car node))


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

(define (number-of-nodes graph)
  (length (nodes graph)))


(define (connected? graph)
  (let* ((start-node (car (nodes graph)))
         (visited (dfs-connected start-node (set-create-empty) graph)))
    (= (number-of-nodes graph) (length visited))))

;Test
;(connected? g)

; For the part that checks if it's an acyclic graph doesn't have to change at all, the data types are already under the hood

(define (spanning? graph)
  (and (is-acyclic graph) (connected? graph)))

; Test
;(spanning? g)

; Lastly we can use DFS to also gives us a spanning tree made from the graph;
; I created this before for the more abstract version, let's try it again;
; We now are using set-member? to check if the current vertex is in the visited set
; We used first to get the first neighbor, without using car
; set-insert is used to insert te current vertex into the visited set
; Pair is used to pair the current vertex into a tree
; Laslty set-empty-set creates an empty set for the tree

;(define (dfs-spanning-tree current-node visited-nodes output-tree graph)
 ; (if (set-member? current-node visited-nodes)
  ;    output-tree
   ;   (let iter ((neighbors (neighbors graph current-node)))
    ;    (if (null? neighbors)
     ;       output-tree
      ;      (let ((first-neighbor (car neighbors))) ; logic error: first is for pairs; neighbors should be a set; using `car` temporarily instead; this should work if neighbors is non-null
       ;       (if (set-member? first-neighbor visited-nodes)
        ;          (iter (cdr neighbors))
 ;                 (dfs-spanning-tree first-neighbor (set-insert current-node visited-nodes) (set-insert (list current-node first-neighbor) output-tree) graph)))))))


(define (add-edge-and-nodes graph node1 node2)
  (add-edge (add-node (add-node graph node1) node2) node1 node2))

(define (dfs-spanning-tree graph start-node)
  (let dfs-visit ((node start-node)
                  (parent #f)
                  (spanning-tree (set-create-empty)))
    (if (set-member? node (nodes spanning-tree))
        spanning-tree
        (foldl (lambda (neighbor spanning-tree-acc) (dfs-visit neighbor node spanning-tree-acc))
               (if (not parent) spanning-tree (add-edge-and-nodes spanning-tree parent node))
               (neighbors graph node)))))
;(define (dfs-spanning-tree current visited tree graph)
;  (if (set-member? current visited)
;      tree
;      (let ((neighbors (neighbors graph current)))
;        (define (iter-tree nbrs)
;          (if (null? nbrs)
;              tree
;              (let ((nbr (first nbrs)))
;                (if (not (set-member? nbr visited))
;                    (iter-tree (dfs-spanning-tree nbr (set-insert current visited) (set-insert (list current nbr) tree) graph))
;                    (iter-tree (cdr nbrs))))))
;        (iter-tree neighbors))))


(define g1 (make-empty-graph))
(define g1 (add-node g1 'a))
(define g1 (add-node g1 'b))
(define g1 (add-node g1 'c))
(define g1 (add-node g1 'd))
(define g1 (add-node g1 'e))
(define g1 (add-edge g1 'a 'b))
(define g1 (add-edge g1 'a 'c))
(define g1 (add-edge g1 'b 'd))
(define g1 (add-edge g1 'b 'e))
(define g1 (add-edge g1 'c 'e))
(display g1)
(newline)
(john-dfs g1 'a)
(nodes g1)
(dfs-spanning-tree g1 'a)