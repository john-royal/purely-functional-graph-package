(load "graph-package/basic-data-types/set.scm")
(load "graph-package/basic-data-types/pair.scm")
(load "graph-package/util.scm")

(define (make-node node) node)
(define (make-labeled-node node label) (list node label))
  
(define (make-edge node1 node2 . weight)
  (if (pair? node2) (cons node1 node2)
      (cons node1 (cons node2 weight))))
(define (edge-from edge)
  (car edge))
(define (edge-to edge)
  (cadr edge))
(define (is-weighted? edge)
  (= (length edge) 3))
(define (edge-to-weighted edge)
  (if (is-weighted? edge) (cdr edge) (cadr edge)))
(define (edge-from-weighted src dest)
  (if (pair? dest) (cons src dest)
      (list src dest)))
(define (edge-weight edge)
  (if (is-weighted? edge) (caddr edge) 1))
(define (edge-includes? edge node)
  (or (equal? (edge-from edge) node) (equal? (edge-to edge) node)))
(define (edge-matches? edge1 edge2)
  (and
   (equal? (edge-from edge1) (edge-from edge2))
   (equal? (edge-to edge1) (edge-to edge2))))
(define (make-weighted-edge node1 node2 weight) (list node1 node2 weight))

(define (graph-base)
  ; Returns an empty graph.
  (define (make-empty-graph)
    (pair (set-create-empty)
          (set-create-empty)))

  ; Returns the set of all nodes in the graph.
  (define (nodes graph)
    (first graph))

  ; Returns the set of all edges in the graph.
  ; For unweighted graphs, edges are represented as a pair of two nodes, e.g. (a b).
  ; For weighted graphs, edges are represented as the two nodes followed by their weight, e.g. (a b 1).
  (define (edges graph)
    (second graph))

  ; Adds the given node to the graph, if it is not already present.
  (define (add-node graph node)
    (pair (set-insert node (nodes graph)) (edges graph)))

  ; Removes the given node and all associated edges from the graph.
  (define (remove-node graph node)
    (pair (set-remove node (nodes graph))
          (filter (lambda (edge) (not (edge-includes? edge node))) (edges graph))))

  ; Returns the graph with the given edge inserted.
  ; If the graph does not include the relevant nodes, returns the graph unchanged.
  (define (add-edge graph edge)
    (let ((nodes (nodes graph)))
      (if (and (set-member? (edge-from edge) nodes) (set-member? (edge-to edge) nodes))
          (pair nodes (set-insert edge (edges graph)))
          graph)))

  ; Returns the graph with the given edge removed.
  (define (remove-edge graph edge1)
    (pair (first graph)
          (filter (lambda (edge2) (not (edge-matches? edge1 edge2))) (edges graph))))

  ; Returns true if there is an edge connecting the two nodes in the given graph.
  (define (adjacent? graph node1 node2)
    (some (lambda (edge) (edge-matches? edge (make-edge node1 node2))) (edges graph)))

  ; Given a graph and a specific node, returns the set of nodes that are connected to the given node.
  ; For weighted graphs, nodes are represented as a pair containing the node itself and the weight of the associated edge.
  (define (neighbors graph node)
    (map edge-to-weighted (filter (lambda (edge) (equal? (edge-from edge) node)) (edges graph))))

  (define (dispatch action)
    (cond ((eq? action 'make-empty) make-empty-graph)
          ((eq? action 'nodes) nodes)
          ((eq? action 'edges) edges)
          ((eq? action 'add-node) add-node)
          ((eq? action 'remove-node) remove-node)
          ((eq? action 'add-edge) add-edge)
          ((eq? action 'remove-edge) remove-edge)
          ((eq? action 'adjacent?) adjacent?)
          ((eq? action 'neighbors) neighbors)))

  dispatch)

(define (graph-adjacency-list)
  ; Returns an empty graph.
  (define (make-empty-graph) '())

  ; Returns the set of all nodes in the graph.
  (define (nodes graph)
    (map car graph))

  ; Returns the set of all edges in the graph.
  ; For unweighted graphs, edges are represented as a pair of two nodes, e.g. (a b).
  ; For weighted graphs, edges are represented as the two nodes followed by their weight, e.g. (a b 1).
  (define (edges graph)
    (define (accumulator entry accumulated-edges)
      (let* ((node (first entry))
             (neighbors (second entry))
             (entry-edges (map (lambda (neighbor) (edge-from-weighted node neighbor)) neighbors)))
        (set-union entry-edges accumulated-edges)))
    (accumulate accumulator (set-create-empty) graph))

  ; Adds the given node to the graph, if it is not already present.
  (define (add-node graph node)
    (if (assoc node graph) graph
      (cons (pair node (set-create-empty)) graph)))

  ; Removes the given node and all associated edges from the graph.
  (define (remove-node graph node-to-remove)
    (if (null? graph) '()
        (let* ((entry (car graph))
               (node (first entry))
               (neighbors (second entry)))
          (if (equal? node node-to-remove)
              (remove-node (cdr graph) node-to-remove)
              (cons (pair node (set-remove node-to-remove neighbors)) (remove-node (cdr graph) node-to-remove))))))

  ; Adds the given edge to the graph.
  (define (add-edge graph edge)
    (if (null? graph) '()
        (let* ((entry (car graph))
               (node (first entry))
               (neighbors (second entry)))
          (if (equal? (edge-from edge) node)
              (cons (pair node (set-insert (edge-to-weighted edge) neighbors)) (cdr graph))
              (cons entry (add-edge (cdr graph) edge))))))

  ; Removes the given edge from the graph, if present.
  ; Weights are not necessary (edges are matched based on their start and end node).
  (define (remove-edge graph edge)
    (if (null? graph) '()
        (let* ((entry (car graph))
               (node (first entry))
               (neighbors (second entry)))
          (if (equal? (edge-from edge) node)
              (cons (pair node (set-remove (edge-to edge) neighbors)) (cdr graph))
              (cons entry (remove-edge (cdr graph) edge))))))

  ; Returns true if there is an edge connecting the two nodes in the given graph.
  (define (adjacent? graph node1 node2)
    (some (lambda (edge) (edge-matches? edge (make-edge node1 node2))) (edges graph)))

  ; Given a graph and a specific node, returns the set of nodes that are connected to the given node.
  ; For weighted graphs, nodes are represented as a pair containing the node itself and the weight of the associated edge.
  (define (neighbors graph node)
    (map edge-to-weighted (filter (lambda (edge) (equal? (edge-from edge) node)) (edges graph))))

  (define (dispatch action)
    (cond ((eq? action 'make-empty) make-empty-graph)
          ((eq? action 'nodes) nodes)
          ((eq? action 'edges) edges)
          ((eq? action 'add-node) add-node)
          ((eq? action 'remove-node) remove-node)
          ((eq? action 'add-edge) add-edge)
          ((eq? action 'remove-edge) remove-edge)
          ((eq? action 'adjacent?) adjacent?)
          ((eq? action 'neighbors) neighbors)))

  dispatch)

(define (graph-adjacency-matrix)
  (define alphabet '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
  
  ; Returns an empty graph.
  (define (make-empty-graph) '())

  ; Returns the set of all nodes in the graph.
  (define (nodes graph)
    (define (slice count lst)
      (if (zero? count) '()
          (cons (car lst) (slice (- count 1) (cdr lst)))))
    (slice (length graph) alphabet))

  ; todo: implement the other functions, which will not be easy...

  (define (dispatch action)
    (cond ((eq? action 'make-empty) make-empty-graph)
          ((eq? action 'nodes) nodes)
          ((eq? action 'edges) edges)
          ((eq? action 'add-node) add-node)
          ((eq? action 'remove-node) remove-node)
          ((eq? action 'add-edge) add-edge)
          ((eq? action 'remove-edge) remove-edge)
          ((eq? action 'adjacent?) adjacent?)
          ((eq? action 'neighbors) neighbors)))

  dispatch)
  

(define (make-empty-graph) ((graph-dispatch 'make-empty)))
(define (nodes graph) ((graph-dispatch 'nodes) graph))
(define (edges graph) ((graph-dispatch 'edges) graph))
(define (add-node graph node) ((graph-dispatch 'add-node) graph node))
(define (remove-node graph node) ((graph-dispatch 'remove-node) graph node))
(define (add-edge graph edge) ((graph-dispatch 'add-edge) graph edge))
(define (remove-edge graph edge) ((graph-dispatch 'remove-edge) graph edge))
(define (adjacent? graph node1 node2) ((graph-dispatch 'adjacent?) graph node1 node2))
(define (neighbors graph node) ((graph-dispatch 'neighbors) graph node))

(define graph-dispatch (graph-adjacency-list))

(define g (make-empty-graph))
(define g (add-node g 'a))
(define g (add-node g 'b))
(define g (add-node g 'c))
(define g (add-node g 'd))
(define g (add-edge g (make-edge 'a 'b 1)))
(define g (add-edge g (make-edge 'a 'c 2)))
(define g (add-edge g (make-edge 'b 'd 3)))
(nodes g)
(edges g)
(adjacent? g 'a 'b)
(adjacent? g 'a 'd)
(neighbors g 'a)
(define g (remove-edge g (make-edge 'b 'd)))
(define g (remove-node g 'a))