
; Check if the graph is connected or not

; Design Idea
; So we want to check if the graph is connected, which means that all the vertices are connected through one parent vertex
; Honestly from all we've done so far this isn't diffuclt, we essentially can just run DFS, and calculate from this
; We'll reimplement DFS, and then a helper function that checks the number of vertices in the graph
; The point of that is to compare the visited elements of DFS with how many vertices there are in the graph
; Then we create a function connect? that essentially started DFS on the first vertex.
; Once that is done, it compares the visted vertices from DFS to the number of vertices in the graph
; If the number of vertices are the same, we have a connected graph!

; Proof
; Base Case: If the current vertex is already in the 'visited' set, the 'visited' set is returned as no further exploration is necessary.
; Inductive Hypothesis (IH): Assumming the dfs-connected function correctly checks if the nodes explored are connected.
; Inductive Step (IS): Consider an unvisited node n. Using DFS, we recursively explore all of the unvisited neighbors of n.
; According to IH, the dfs-connected function will correctly check the connectivity of these nodes' sub-graphs. If all nodes are visited during
; the traversal, the graph is connected.
; Precondition: The input graph is a valid graph.
;  Postcondition: The function correctly identifies whether the graph is connected (#t) or not (#f).
