(load "graph-package/graph-algorithms/connected.scm")
(load "graph-package/graph-algorithms/acyclic.scm")

(define (spanning? graph)
  (and (acyclic? graph) (connected? graph)))