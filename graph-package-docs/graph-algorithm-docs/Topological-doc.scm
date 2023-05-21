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

; Proof
; General Invariant
; At dfs-top-sort, first all vertices in the sorted list have been visited.
; The sorted set holds a partial topological ordfering of the vertices that have been processed, if there is a direct path
; from vertex u to v, and both u an v have been processed, then u comes before v in the current order of sorted.
; For any vertex not in sorted, there isn't a directed edge from a vertex in sorted to v. This means, all incoming
; edges to v from the vertices aren't yet sorted.
; If there are still unvisted nodes, each of the nodes don't have any upcoming edge from another unvisited node, which guarantees that
; at least one of these nodes can be chosen as the next node to visit, while stay acyclic.