; Determine if a graph if acyclic.

(load "graph-package/basic-data-types/set.scm")
(load "graph-package/basic-data-types/pair.scm")
(load "graph-package/basic-data-types/graph.scm")
(load "graph-package/util.scm")

; restructuring the DFS
; So I removed the parent parameter
; The function will return false if it sees a node alread in the visited list
; It loops over all the neighbors of the current node, to check if it leads to a cycle
; The biggest issue was the parents, I'm glad we removed it, didn't like it anyways, but originall thought we needed it

(define (dfs-acyclic? graph current-node visited)
  (let ((neighbors (neighbors graph current-node)))
    (cond ((set-member? current-node visited) #f)   
          ((null? neighbors) #t)                    
          (else 
            (let loop ((rest-neighbors neighbors)) 
              (if (null? rest-neighbors)
                  #t  
                  (let ((next-node (car rest-neighbors)))
                    (if (dfs-acyclic? graph next-node (cons current-node visited))
                        (loop (cdr rest-neighbors))
                        #f))))))))

(define (acyclic? graph)
  (dfs-acyclic? graph (car (nodes graph)) '()))

; Test 1
(define g (make-empty-graph))
(define g (add-node g 'C))
(define g (add-node g 'B))
(define g (add-node g 'A))
(define g (add-edge g (make-edge 'A 'B)))
(define g (add-edge g (make-edge 'A 'C)))
(define g (add-edge g (make-edge 'B 'A)))
(define g (add-edge g (make-edge 'C 'B)))
(display g)
(newline)
(newline)
(display (acyclic? g)); should be #f, but is #t
(newline)
(newline)

; Test 2

(define h (make-empty-graph))
(define h (add-node h 'D))
(define h (add-node h 'E))
(define h (add-node h 'F))
(define h (add-node h 'G))
(define h (add-edge h (make-edge 'D 'E)))
(define h (add-edge h (make-edge 'E 'F)))
(define h (add-edge h (make-edge 'F 'G)))
(display h)
(newline)
(newline)
(display (acyclic? h))
(newline)
(newline)

; Awesome it works now!
