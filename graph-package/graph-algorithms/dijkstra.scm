; graph-algorithms/dijkstra.scm


(load "../basic-data-types/graph.scm")
(load "../basic-data-types/pair.scm")
(load "../util.scm")

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

(define (find-first pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) (car lst))
        (else (find-first pred (cdr lst)))))

(define (ascending-distances? node1 node2)
  (let ((d1 (second node1))
        (d2 (second node2)))
    (cond ((eq? d1 #f) #f)
          ((eq? d2 #f) #t)
          (else (< d1 d2)))))

(define (initialize-distances graph start)
  (insertion-sort
   (map (lambda (node) (if (eq? node start) (pair start 0) (pair node #f))) (nodes graph))
   ascending-distances?))

(define (compute-neighbor-distances distance neighbors)
    (map (lambda (neighbor)
           (let ((node (if (pair? neighbor) (car neighbor) neighbor))
                 (length (if (pair? neighbor) (cadr neighbor) 1)))
             (pair node (+ distance length))))
       neighbors))

(define (insert-distance node-distance lst)
  (cond ((null? lst) (list node-distance))
        ((eq? (first node-distance) (first (car lst)))
         (if (ascending-distances? node-distance (car lst))
             (cons node-distance (cdr lst))
             lst))
        (else (cons (car lst) (insert-distance node-distance (cdr lst))))))

(define (merge-distances lst1 lst2)
  (if (null? lst1)
      (insertion-sort lst2 ascending-distances?)
      (merge-distances (cdr lst1) (insert-distance (car lst1) lst2))))

(define (dijkstra graph start end)
  (let iter ((unvisited (nodes graph))
             (distances (initialize-distances graph start)))
    (if (null? unvisited) #f
        (let* ((node-distance (find-first (lambda (node-distance) (set-member? (car node-distance) unvisited)) distances))
               (node (car node-distance))
               (distance (cadr node-distance)))
          (if (eq? node end) distance
              (let ((neighbor-distances (compute-neighbor-distances (if (not distance) 0 distance) (neighbors graph node))))
                (iter (set-remove node unvisited)
                      (merge-distances neighbor-distances distances))))))))

(define g (make-empty-graph))
(define g (add-node g 'A))
(define g (add-node g 'B))
(define g (add-node g 'C))
(define g (add-node g 'D))
(define g (add-edge g (make-edge 'A 'B 1)))
(define g (add-edge g (make-edge 'A 'C 4)))
(define g (add-edge g (make-edge 'B 'C 2.5)))

(dijkstra g 'A 'C) ; 3.5

