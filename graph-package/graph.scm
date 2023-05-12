(load "./basic-data-types/pair.scm")
(load "./basic-data-types/set.scm")



; GENERAL HELPER FUNCTIONS

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (sort pred lst)
  (define (insert el lst)
    (cond ((null? lst) (list el))
          ((pred el (car lst)) (cons el lst))
          (else (cons (car lst) (insert el (cdr lst))))))
  (if (null? lst) '()
      (insert (car lst) (sort pred (cdr lst)))))

(define (accumulate op init seq)
  (if (null? seq) init
      (op (car seq) (accumulate op init (cdr seq)))))



; GRAPH HELPER FUNCTIONS

(define (make-edge src dest weight)
  (list src dest weight))

(define (edge-src edge)
  (car edge))

(define (edge-dest edge)
  (cadr edge))

(define (edge-weight edge)
  (caddr edge))

(define (make-node node weight)
  (pair node weight))

(define (node node-weight-pair)
  (first node-weight-pair))

(define (node-weight node-weight-pair)
  (second node-weight-pair))



; DIJKSTRA

(define (find-edges-from-node node graph)
  (filter (lambda (edge) (equal? (edge-src edge) node)) graph))

; I was going to try and explain this, but I’m tired. I’ll try again some other time.
(define (update-distances-from-node current graph distances)
  (let* ((node (node current))                      ; current node
         (weight (node-weight current))             ; weight of current node
         (edges (find-edges-from-node node graph))) ; edges reachable from current node
    ; Analyze each edge from this node to find the shortest path to each destination node
    (accumulate
     (lambda (edge distances)
       (let* ((dest (edge-dest edge))
              (edge-weight (edge-weight edge))
              (total-weight (+ weight edge-weight)))
         ; Record the length of this edge if it’s the shortest path to this node that we can find.
         (if (assoc dest distances)
             ; Compare the length of this edge to the length we already have for this node.
             (if (< total-weight (node-weight (assoc dest distances)))
                 ; If this edge is shorter, replace the saved length with this one.
                 (cons (make-node dest total-weight) (filter (lambda (x) (equal? (node x) dest)) distances))
                 ; Otherwise, use the length we already have (i.e. do not modify the list).
                 distances)
             ; If we don’t have a length for this node, use the one from this edge.
             (cons (make-node dest total-weight) distances))))
     distances
     edges)))

; Returns the length of the shortest path between a source node and a destination node.
; If no such path exists, returns #f.
(define (dijkstra graph src dest)
  (let iter ((queue (list (make-node src 0)))      ; nodes that need to be processed, beginning with source
             (visited '())                         ; nodes that have already been processed
             (distances (list (make-node src 0)))) ; distances from source to all other nodes
    ; If the queue is empty and we haven't found the destination, there's no path.
    (if (null? queue)
        #f
        ; Find the node with the shortest distance from the queue.
        (let* ((queue (sort (lambda (x y) (< (node-weight x) (node-weight y))) queue))
               (current (car queue))
               (queue (cdr queue)))
          ; If the current node is the destination, return the associated length — this is the shortest path.
          (cond ((equal? (node current) dest) (node-weight current))
                ; If we've already visited this node, continue to the next one.
                ((member (node current) visited) (iter queue visited distances))
                ; Otherwise, process distances from the current node and add the node to the visited list.
                (else (iter (append queue (update-distances-from-node current graph distances))
                            (cons (node current) visited)
                            (update-distances-from-node current graph distances))))))))



; DIAMETER

; Given a list of edges, returns a set of nodes, e.g. ((A B 1) (B A 1)) => (A B)
(define (nodes graph)
  (set-create (map car graph)))

; Returns all paths in a graph, i.e. all possible combinations of nodes
(define (all-paths graph)
  (let ((nodes (nodes graph)))
    (let iter ((unchecked-nodes nodes)
               (paths '()))
      (if (null? unchecked-nodes) paths
          (iter (cdr unchecked-nodes) (set-union
                                       (map (lambda (node) (pair (car unchecked-nodes) node)) nodes)
                                       paths))))))

; Computes the diameter of a graph.
(define (diameter graph)
  ; Compute the length of the shortest path between all nodes in the graph.
  ; Return the maximum shortest path.
  ; Note: `apply` invokes a function with a list of arguments, e.g. (apply max (list 1 2 3)) = (max 1 2 3) = 3
  (apply max (map
              (lambda (path) (dijkstra graph (first path) (second path)))
              (all-paths graph))))



; TESTS

(define graph
  (list (make-edge 'A 'B 1)
        (make-edge 'B 'A 1)
        (make-edge 'A 'C 3)
        (make-edge 'C 'A 3)
        (make-edge 'B 'C 2)
        (make-edge 'C 'B 2)))

(diameter graph) ; 3
(dijkstra graph 'A 'B) ; 1
