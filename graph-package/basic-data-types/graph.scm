; basic-data-types/graph.scm


(load "./set.scm")


; In an adjacency matrix, a graph is represented as a square matrix where each row and column represents a vertex, and each entry represents an edge.
; If the entry at (i, j) is a 1, that means the vertices are connected by an edge.
; Otherwise, if the entry at (i, j) is a 0, the vertices are not connected.
; For example:
;
;   A B C
; A 0 1 0
; B 1 0 1
; C 0 1 0
;
; Here, the vertices are A, B, and C; the edges are AB and BC.
; Let’s put that into code:
(define example '((0 1 0)
                  (1 0 1)
                  (0 1 0)))

; We can create an empty matrix the same way we’d create any other graph — I think.
(define (make-empty-graph) '())

; Adding a vertex will be somewhat gnarly.
; To make things simpler, let’s try assuming that vertices are in alphabetical order — if there are 10 rows and columns, then the vertices are the first 10 letters of the alphabet.
(define alphabet '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))
(list-ref alphabet 0)
(length alphabet)

; Returns the zero-based index of an element in a list, or #f if no such element is found.
(define (index-of el lst)
  (cond ((null? lst) #f)
        ((equal? (car lst) el) 0)
        (else (+ 1 (index-of el (cdr lst))))))

(index-of 'Z alphabet)


(define (list-append el lst)
  (if (null? lst) (list el)
      (cons (car lst) (list-append el (cdr lst)))))

(define (list-repeating el len)
  (if (zero? len) '()
      (cons el (list-repeating el (- len 1)))))

(define (add-vertex matrix)
  (if (null? matrix) '((0)) ; return a graph with one vertex, "A"
      (let* ((appended-rows (map (lambda (row) (list-append 0 row)) matrix)) ; add a 0 column to each row
             (n (length (list-ref appended-rows 0)))                         ; determine the new value of n
             (empty-row (list-repeating 0 n)))                               ; generate an empty new row
        (list-append empty-row appended-rows))))                             ; append the empty row to the existing rows

(define (list-set-value index value lst)
  (cond ((>= index (length lst)) #f)
        ((zero? index) (cons value (cdr lst)))
        (else (cons (car lst) (list-set-value (- index 1) value (cdr lst))))))

(list-set-value 3 'Z '(A B C))

(define (set-matrix-entry row col val matrix)
  (list-set-value row (list-set-value col val (list-ref matrix row)) matrix))
    

(define (add-edge graph v1 v2)
  (let ((i (index-of v1 alphabet))
        (j (index-of v2 alphabet)))
    (if (or (not i) (not j) (> i (length graph)) (> j (length graph))) #f ; return #f for invalid indices
        (set-matrix-entry i j 1 (set-matrix-entry j i 1 graph)))))        ; set (i, j) and (j, i) to 1



(define (list-init op len)
  (if (zero? len) '()
      (let ((i (- len 1)))
        (list-append (op i) (list-init op i)))))

; This is a version of `map` where the lambda also receives the index of the given entry.
(define (list-map op lst)
  (let iter ((index 0)
             (lst-in lst)
             (lst-out '()))
    (if (null? lst-in) (reverse lst-out)
        (iter (+ index 1) (cdr lst-in) (cons (op (car lst-in) index) lst-out)))))

(define (print-matrix matrix)
  (define (print-lst lst)
    (if (null? lst) 'end
        (begin (display (car lst))
               (newline)
               (print-lst (cdr lst)))))
  (let* ((header (list-init (lambda (i) (list-ref alphabet i)) (length matrix)))
         (fheader (cons " " header))
         (fmatrix (cons fheader (list-map (lambda (row i) (cons (list-ref alphabet i) row)) matrix))))
    (print-lst fmatrix)))


;(define (print-matrix matrix)
;  (if (null? matrix)
;      'done
;      (begin
;        (for-each (lambda (x) (display x) (display " ")) (car matrix))
;        (newline)
;        (print-matrix (cdr matrix)))))

;(define (print-matrix-with-labels matrix)
;  (let ((rows (my-map (lambda (row i) (cons (list-ref alphabet i) row)) matrix))
;        (first-row (cons " " (my-map (lambda (_ i) (list-ref alphabet i)) (list-repeating 0 (length matrix))))))
;    (print-matrix (cons first-row rows))))


(define graph (make-empty-graph))
(define graph (add-vertex graph))
(define graph (add-vertex graph))
(define graph (add-vertex graph))
(define graph (add-edge graph 'A 'B))
(define graph (add-edge graph 'B 'C))
(print-matrix graph)
(display graph)