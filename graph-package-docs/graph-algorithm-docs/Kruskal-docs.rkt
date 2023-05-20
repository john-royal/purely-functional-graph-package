; Kruskal's Algorithm

; Design Plan
; Rough idea
; First we need to sort all of our edges from the smallest weight to the highest
; Then we start edging edges to a new set for the minimum spanning tree, from the smallest edge, in a way that it doesn't form a cycle
; The nodes need to be connected for this to work.
; We repeat this process until we form a tree with the smallest weight

; First let's extract our edges
; we have a data-type for that
; Now we can sort the edges.
; we can just implement an insertion-sort algorithm
; So we need to implement a find function that finds the parent of the vertex, this means that we have keep asking a
; vertex where it comes from, and goes down until one identifies itself as the parent

; The Kruskal's algorithm constructs a minimum spanning tree by continuously choosing the edge with the least weight that doesn't form a cycle with the edges. To check if a cycle is formed,
; it uses disjoint sets where each set represents a tree.
; it checks if two vertices belong to the same set and merges two sets.

; Proof
; Base Case: If the list of edges is empty, an empty list is returned as the MST.
; Inductive Hypothesis (IH): Assume the kruskal-iter function correctly forms the MST for all previously processed edges.
; Inductive Step (IS): Consider the least-weighted edge not yet processed. If adding this edge doesn't create a cycle with the edges
; in the MST, the two nodes of the edge are not in the same set, then we add it to the MST. If it does form a cycle, then we skip this edge.
; The inductive steps are derived from Kruskal's algorithm. We continuously choose the least-weighted edge that doesn't form a cycle with the
; MST under construction, which is done by using the disjoint set data structure to check if two vertices are in the same set.
; If all edges are processed and we have a connected tree that spans all nodes, we have a MST.
; Precondition: The input graph is a valid weighted graph.
; Postcondition: The function correctly constructs and returns a minimum spanning tree of the input graph.