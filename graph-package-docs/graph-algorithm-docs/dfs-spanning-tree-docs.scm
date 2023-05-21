
; Spanning tree

; Design Plan
; The DFS spanning-tree function starts at a given node in the graph, and tries to visit every node, only traversing edges to nodes that
; haven't been visited. This is does by recursively calling the DFS function on each unvisited neighbor of the current node. It forms
; the spanning tree by adding the edge from the current node to the visited neighbor node in the spanning tree. If all the nodes in the graph
; are visited and are part of the spanning tree, the function returns the spanning tree.

; Proof
; Base Case: If the current node is already in the spanning-tree, the spanning-tree is returned as no further exploration is necessary.
; Inductive Hypothesis (IH): Assumming the dfs-visit function correctly constructs the spanning tree for all nodes that have been explored.
; Inductive Step (IS): Consider an unvisited node n. DFS, recursively explore all of the unvisited neighbors of n.
; According to our IH, the dfs-visit function will correctly construct the spanning tree of these nodes' sub-graphs. The edges between the current
; node and its unvisited neighbors are added to the spanning tree.
; Precondition: The input graph is a valid graph, and start-node is a valid node in the graph.
; Postcondition: The function correctly constructs and returns a spanning tree of the input graph.

