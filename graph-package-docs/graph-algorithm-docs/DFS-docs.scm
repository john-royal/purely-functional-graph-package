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
