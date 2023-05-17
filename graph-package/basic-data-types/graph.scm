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
  (if (or (null? graph) (not (assoc node1 graph)) (not (assoc node2 graph)))
      graph
      (let* ((entry (car graph))
             (node (first entry))
             (neighbors (second entry)))
        (if (equal? node node1)
            (cons (pair node (set-insert node2 neighbors)) (cdr graph))
            (cons entry (add-edge (cdr graph) node1 node2))))))
; For directed graphs

(define (make-directed-graph)
  '())
(define (add-directed-edge graph node1 node2)
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

(define (number-of-nodes graph)
  (length (nodes graph)))

(define g (make-empty-graph))
(define g (add-node g 'C))
(define g (add-node g 'B))
(define g (add-node g 'A))
(define g (add-edge g 'A 'B))
(define g (add-edge g 'A 'C))
;(display g)
(newline)
;(nodes g)
;(edges g)
;(neighbors g 'A)
;(adjacent? g 'A 'B)
;(adjacent? g 'B 'A)
;(adjacent? g 'D 'A)
;(define g (remove-edge g 'A 'B))
;(display g)
(newline)

(define dg (make-directed-graph))
(define dg (add-node dg 'A))
(define dg (add-node dg 'B))
(define dg (add-node dg 'C))
(define dg (add-node dg 'D))
(define dg (add-node dg 'E))
(define dg (add-node dg 'F))
(define dg (add-directed-edge dg 'A 'B))
(define dg (add-directed-edge dg 'C 'B))
(define dg (add-directed-edge dg 'F 'B))
(define dg (add-directed-edge dg 'D 'C))
(display dg)