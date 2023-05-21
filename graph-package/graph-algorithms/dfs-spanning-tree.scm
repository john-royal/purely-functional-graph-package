; Find a spanning tree between two nodes using DFS.

(load "graph-package/basic-data-types/set.scm")
(load "graph-package/basic-data-types/pair.scm")
(load "graph-package/basic-data-types/graph.scm")
(load "graph-package/util.scm")

(define (add-edge-and-nodes graph node1 node2)
  (let ((graph (add-node (add-node graph node1) node2))
          (edge (make-edge node1 node2)))
      (add-edge graph edge)))

(define (dfs-spanning-tree graph start-node)
  (let dfs-visit ((node start-node)
                  (parent #f)
                  (spanning-tree (set-create-empty)))
    (if (set-member? node (nodes spanning-tree))
        spanning-tree
        (accumulate (lambda (neighbor spanning-tree-acc) (dfs-visit neighbor node spanning-tree-acc))
                    (if (not parent) spanning-tree (add-edge-and-nodes spanning-tree parent node))
                    (neighbors graph node)))))
