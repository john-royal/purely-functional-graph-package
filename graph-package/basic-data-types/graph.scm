; basic-data-types/graph.scm

(load "./set.scm")
(load "./pair.scm")

(define (make-empty-graph) '())

(define (add-node graph node)
  (if (assoc node graph) graph
      (cons (pair node (set-create-empty)) graph)))

(define (remove-node graph node)
  (cond ((null? graph) '())
        ((equal? (first (car graph)) node) (cdr graph))
        (else (cons (car graph) (remove-node (cdr graph) node)))))

(define (add-edge graph node1 node2)
  (if (null? graph) '()
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
  (second (assoc node graph)))

(define (adjacent? graph node1 node2)
  (let ((n (neighbors graph node1)))
    (if (null? n) #f
        (set-member? node2 n))))

(define (nodes graph)
  (map car graph))

(define (edges graph)
  (if (null? graph) '()
      (let* ((entry (car graph))
             (node (first entry))
             (connected-nodes (second entry)))
        (set-union (map (lambda (node2) (pair node node2)) connected-nodes) (edges (cdr graph))))))

(define (number-of-nodes graph)
  (length (nodes graph)))

(define g (make-empty-graph))
(define g (add-node g 'C))
(define g (add-node g 'B))
(define g (add-node g 'A))
(define g (add-edge g 'A 'B))
(define g (add-edge g 'A 'C))
(display g)
(newline)
(nodes g)
(edges g)
(neighbors g 'A)
;(adjacent? g 'A 'B)
;(adjacent? g 'B 'A)
;(adjacent? g 'D 'A)
(define g (remove-edge g 'A 'B))
(display g)
(newline)