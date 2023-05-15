; basic-data-types/adjacency-list.scm
;
; These helper functions for an adjacency list graph representation are used by the Dijkstra and diameter procedures.
; We’ll need to adapt them to work for all graph representations.


(load "../basic-data-types/pair.scm")


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

