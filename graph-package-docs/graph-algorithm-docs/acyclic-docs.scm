; DFS acyclic

; Design Plan
; So we want to check if the graph has any cycles or not, for simplicity we went with DFS again
; first we need to identify what a cycle would mean, if the current vertex we have found is also already visited
; That means there's a cycle, unless the current vertex is also it's own parent
; We can create a local function, loop that takes the start of the current vertex, and finds all it's edges in the graph
; First in the loop we check if the current neighbor has any edges, if it doesn't, that means there are no cycles
; else it takes the first element of neighbors and checks if it's not equal to the parent vertex
; if it's not then we recursively call the function switching the current vertex to the next
; and adding the previous current vertex to the visited list
; if in the next recursion, it's true the program returns true, if not, we go through the loop function again, until it goes through all the edges.
; Essentially it returns true if the function has a cycle.

; Proof
; Base Case: If the current vertex is already in the visited set and it is not the parent, that means there is a cycle, and the function returns #t. If
; the current vertex is its own parent, it returns #f, indicating no cycle.
; Inductive Hypothesis (IH): Assuming the dfs-acyclic function correctly identifies if a cycle exists for all nodes that have been explored.
;  Inductive Step (IS): Consider an unvisited node 'n'. Using DFS, we must recursively explore all of the unvisited neighbors of n.
; According to IH, the dfs-acyclic function will identify if a cycle exists in these nodes sub-graphs. If a cycle is
; found in any of these sub-graphs, the function immediately returns #t. If no cycle is found, the function checks the next neighbor. If all
; neighbors are checked and no cycle is found, the function returns #f.
; Precondition: The input graph is a valid graph.
; Postcondition: The function correctly identifies whether the graph is acyclic (#t) or not (#f).
