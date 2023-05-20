; Kruskal Algorithm
(load "../basic-data-types/set.scm")
(load "../basic-data-types/pair.scm")
(load "../basic-data-types/graph.scm")

; --------------------------------------------------------------------------------------------------------------------------------------------

(define (number-of-nodes graph)
  (length (nodes graph)))

(define wg (make-empty-graph))
(define wg (add-node wg 'A))
(define wg (add-node wg 'B))
(define wg (add-node wg 'C))
(define wg (add-node wg 'D))
(define wg (add-node wg 'E))
(define wg (add-node wg 'F))

(define wg (add-edge wg (make-edge 'A 'B 1)))
(define wg (add-edge wg (make-edge 'C 'B 3)))
(define wg (add-edge wg (make-edge 'F 'B 5)))
(define wg (add-edge wg (make-edge 'D 'C 9)))
(display wg)
(newline)
(newline)

;(edges wg)


; --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

; Previous mumbojumbo

; Actually we need to start this from scratch

; Rough idea
; First we need to sort all of our edges from the smallest weight to the highest
; Then we start edging edges to a new set for the minimum spanning tree, from the smallest edge, in a way that it doesn't form a cycle
; The nodes need to be connected for this to work.
; We repeat this process until we form a tree with the smallest weight

; First let's extract our edges
; we have a data-type for that
; (edges wg)

; Now we can sort the edges.
; we can just implement an insertion-sort algorithm
(define (insertion-sort lst less-than?)
  (define (insert x sorted-lst)
    (if (null? sorted-lst)
        (list x)
        (if (less-than? x (car sorted-lst))
            (cons x sorted-lst)
            (cons (car sorted-lst) (insert x (cdr sorted-lst))))))
  (if (null? lst)
      '()
      (insert (car lst) (insertion-sort (cdr lst) less-than?))))

(define (sort-list lst comparator)
  (insertion-sort lst comparator))

(define (edge-weight-comparator edge1 edge2)
  (< (edge-weight edge1) (edge-weight edge2)))

;(define edges (edges wg))
;(sort-list edges edge-weight-comparator)

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (remove x s)
  (filter (lambda (pair) (not (eq? x (car pair)))) s))
;So we need to implement a find function that finds the parent of the vertex, this means that we have keep asking a vertex where it comes from, and goes down until one identifies itself as the parent
(define (find x s)
  (display "find: ")
  (display x)
  (display " in ")
  (display s)
  (newline)
  (let ((entry (assoc x s)))
    (if entry
        (if (eq? x (car entry))
            x
            (find (second entry) s))
        #f)))  ; or handle `#f` as you need


(define (union x y s)
  (let ((x-root (find x s))
        (y-root (find y s)))

     (display "union: ")
    (display x)
    (display " and ")
    (display y)
    (display " in ")
    (display s)
    (display " with roots ")
    (display x-root)
    (display " and ")
    (display y-root)
    (newline)
    (if (not (eq? x-root y-root))
        (cons (pair x-root y-root)
              (remove x-root (remove y-root s))))))

(define (kruskal graph)
  (let* ((edges (sort-list (edges graph) edge-weight-comparator))
         (nodes (nodes graph))
         (disjoint-set (map (lambda (node) (pair node node)) nodes)))
    (define (kruskal-iter edges disjoint-set mst)
      (if (null? edges)
          mst
          (let* ((edge (car edges))
                 (node1 (edge-from edge))
                 (node2 (edge-to edge))
                 (root1 (find node1 disjoint-set))
                 (root2 (find node2 disjoint-set)))
            (if (not (eq? root1 root2))
                (kruskal-iter (cdr edges) 
                              (union node1 node2 disjoint-set) 
                              (cons edge mst))
                (kruskal-iter (cdr edges) disjoint-set mst)))))

    (kruskal-iter edges disjoint-set '())))

;Test
(define wg2 '((a ((b 1) (c 4)))
             (b ((a 1) (c 2) (d 5)))
             (c ((a 4) (b 2) (d 1)))
             (d ((b 5) (c 1)))))

(define wg2 (make-empty-graph))
(define wg2 (add-node wg2 'a))
(define wg2 (add-node wg2 'b))
(define wg2 (add-node wg2 'c))
(define wg2 (add-node wg2 'd))
(define wg2 (add-edge wg2 (make-edge 'a 'b 1)))
(define wg2 (add-edge wg2 (make-edge 'a 'c 4)))
(define wg2 (add-edge wg2 (make-edge 'b 'a 1)))
(define wg2 (add-edge wg2 (make-edge 'b 'c 2)))
(define wg2 (add-edge wg2 (make-edge 'b 'd 5)))
(define wg2 (add-edge wg2 (make-edge 'c 'a 4)))
(define wg2 (add-edge wg2 (make-edge 'c 'b 2)))
(define wg2 (add-edge wg2 (make-edge 'c 'd 1)))
(define wg2 (add-edge wg2 (make-edge 'd 'b 5)))
(define wg2 (add-edge wg2 (make-edge 'd 'c 1)))



(kruskal wg2) ; ((a (b 1)) (b (a 1)) (c (d 1)) (d (c 1)))

; I'm too tired to explain, but I had to modify the union function, because it didn't work with our implementation, maybe consider adopting this union data-type instead
; or creating this as a main data type too?
; We can also use find as a data-type, if you need I can clarify for you
; We can aldo adopt edge-weight as a main data type
; but this was extremely hard to redo, so idk my brain is fried, but this def probably needs work for abstraction
; Will take any ideas you have lol



                  
            
                                      
    
    


           



  




