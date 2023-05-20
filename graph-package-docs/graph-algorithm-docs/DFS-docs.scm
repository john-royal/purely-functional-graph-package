; DFS Algorithm Implementations

; DFS

; Firstly we can use stacks for this, one for the current stack and the other for the visited stacks
; First we create a stack with the total number of vertices in the graph
; Then we choose any vertex as the starting point of the transversal, and push it in the stack.
; After that we push, a non-visited vertex (adjacient to the vertex on the top of the stack) to the top of the stack
; We then repeat this prcoess until there are no vertices left to visit.
; If there are no vertices left, we pop a vertex from the stack
; This is done until the stack is empty

; Now let's take a look at our data-types and see how we can implement this.
; Firstly we can assign the stacks as visited and current, for now at least.
; We'll use recursion for this DFS, so let's try with letrec again (local recursive fuctions)
; We'll start from a starting vertex that is the start point for thr DFS.
; From there we can add all the visited vertices to the visited set (or stack)
; We use a helper function foldl to visit the neighbor of the current code.

; Design/Proof

; Precondition: The input graph is a valid graph, and the starting node is current node in the graph.
; Base Case: If the node is already in the 'visited' set, the 'visited' set is returned as no further exploration is necessary.
; Inductive Hypothesis (IH): Assume the DFS-recursive function correctly performs a DFS traversal for all nodes that have been explored.
; Inductive Step (IS): Consider an unvisited node 'n'. we must recursively explore all of the unvisited neighbours of n.
; According to our IH, the DFS-recursive function will correctly explore all these nodes. Once all these recursive calls return, the set of all
; visited nodes is updated. If the node has been visited, it is skipped. If not, it is added to the 'visited' set, and the function is called
; recursively on all its neighbors.

; Postcondition: All nodes reachable from the start node are visited in a depth-first manner, and the set of visited nodes is returned.

; ------------------------------------------------------------------------------------------------------------------------------------------------------------

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

; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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
