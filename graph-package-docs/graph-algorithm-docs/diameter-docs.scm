; This finds the diameter of the graph, i.e. the maximum shortest path between nodes.

; Overview:
; The function takes a brute-force approach to finding the diameter of the graph.
; First, the set of all possible combinations of nodes in the graph is computed.
; Then, Dijkstra's algorithm is used to compute the length of the shortest path for each combination of nodes.
; Finally, the length of the maximum shortest path is returned.
;
; This assumes the correctness of Dijkstra's algorithm.

; all-paths function:
; This extracts the set of all nodes from the graph and initializes an empty set for paths.

; Proof:
; 1. Base Case: If we have a graph with one node, the diameter is 0 because there is one node and no edges.
;    In this case, the function returns 0 as expected.
; 2. Inductive Hypothesis: The algorithm works correctly for all graphs with fewer than 'n' nodes.
; 3. Inductive Step: To show the algorithm works correctly for all graphs with 'n' nodes, consider a
;    graph 'G' with 'n' nodes. The 'all-paths' function returns the set of all node combinations in the
;    graph, and Dijkstra's algorithm is used to compute the shortest path between all such node combinations.
;    Then, the function takes the 'max' of all such paths, which by definition is the diameter of 'G'.
