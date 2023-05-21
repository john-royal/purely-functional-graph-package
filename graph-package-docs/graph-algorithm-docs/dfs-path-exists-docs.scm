
; DFS Checking if there's a path between V1 and V2

;; Now that DFS is applied, let's try to use it in regards to the certain algorithms it can be applied to

; First up is to check if there is a path between the two vertices.
; We can easily apply DFS to that
; So for now I decided to modify the existing DFS for this implementation, however as we neatent up our code further, I will try to erase it

; This code checks if the current node is also the target node, if it is, there is a path
; Else if the current code has been visited, it ouputs the visited set.
; It then goes on the next node and conducts recursion to see if the current vertex goes to the target vertex

; Base case: If the start node is the target node, then it returns #t, indicating that there is a path (the node itself).
; Inductive Hypothesis (IH): The recursive call DFS-recursive correctly checks if there is a path from any given node to the target node.
; Inductive Step (IS): If the current node is not the target and has not been visited yet, it will visit all its neighbors in the graph.
; It does this by recursively calling DFS-recursive on each neighbor while marking the current node as visited. If a path is found from any
; neighbor to the target (or acc (DFS-recursive neighbor (set-insert node visited)) is #t), it stops checking the remaining neighbors and returns #t.
; Precondition: graph: The graph must be a valid data structure where nodes and their neighbors can be fetched. Start: The start node must be a valid node in the graph.
; target: The target node must be a valid node in the graph.

;DFS-path: This function checks if there's a path from start to target in the graph. It initiates a recursive depth-first search from start with an empty set of visited nodes.
; DFS-recursive: This helper function checks if the current node is the target. If it is, it returns #t. If the node has been visited, it returns #f (to avoid cycles). Otherwise,
;it inserts the node into the visited set and recursively applies the function to each of the node's neighbors. If any of these calls return #t, it immediately returns #t.

; Postcondition: The DFS-path function will return #t if there is a path from start to target, and #f otherwise.
