; Topological Sort

; Design Plan
; This will be a bit tricky, it seems, but essentially this sorting method, just follow a direct line of vertices connected by the edge
; so like this A -> B -> C -> D

; So first a graph can only be topological if it's acyclic, lucky for us we already have a function that does that, so we can check that off.
; Next we can use DFS to help us out with this.
; First we can create an empty stack to hold all the vertices that we sort
; Then create a set to keep track of visited vertices
; We want to go through adjacient vertices. and push the visited ones up to the stack
; It will sort the order from top to bottom, if we can execute this right LOL.

; First let's implement the DFS helper function we need
; We need to slightlty modify the previous ones, not too much work for that
;topological-sort: This function performs a topological sort of the graph. It initializes an empty set of visited nodes and an
; empty list for the order. It then iterates through all the nodes in the graph, and for each node, it calls the dfs-topo-sort
; function to visit the node and all nodes reachable from it.


; Proof
; General Invariant
; At dfs-top-sort, first all vertices in the sorted list have been visited.
; The sorted set holds a partial topological ordfering of the vertices that have been processed, if there is a direct path
; from vertex u to v, and both u an v have been processed, then u comes before v in the current order of sorted.
; For any vertex not in sorted, there isn't a directed edge from a vertex in sorted to v. This means, all incoming
; edges to v from the vertices aren't yet sorted.
; If there are still unvisted nodes, each of the nodes don't have any upcoming edge from another unvisited node, which guarantees that
; at least one of these nodes can be chosen as the next node to visit, while stay acyclic.
; Precondition: The topological-sort function takes a graph as an argument. The graph must be a valid data structure where nodes and
; their neighbors can be fetched, and it must be a DAG (Directed Acyclic Graph), meaning there are no cycles.
; Postcondition: The topological-sort function will return a list of nodes in topological order, meaning that for every directed edge from
; node U to node V, U comes before V in the ordering. If the graph is not a DAG, the result will not be a valid topological order.

