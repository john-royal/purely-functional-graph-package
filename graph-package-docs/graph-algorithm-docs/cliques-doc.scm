; Cliques Algorithm

; A clique is basically a group of connected edges, and we want to find biggest of that
; How shall we do it?
; Well we can use DFS go through one vertex and find all of the connected vertices with it, and label that
; Then we can keep doing that with different vertices and after compare them all and see which set is the biggest
; Possibly straight forward

; Base case: If the list of nodes is empty, return the current clique, as there are no nodes left to explore.
; Inductive Hypothesis (IH): Assuming the function works for the current clique and remaining nodes,
; the function will find the maximal clique in the graph.
; IS: For each remaining node, it is considered for addition to the current clique. If the addition of the
; node to the clique forms a valid clique and increases its size, the node is added to the clique, and the
; dfs function is called on the rest of the remaining nodes with the new clique. If not, the node is not added
; to the clique, and the dfs function is called twice: once with the original clique and once with the new clique.
; The larger of the two cliques returned by these calls is selected as the result.
; This is used to substitue the max builty-function to find the max clique

;Precondition: The maximal-clique function takes a graph with its nodes and edges defined.


; Design Plan

;all-pairs-adjacent?: This function checks if all pairs of nodes in a given set are adjacent in a graph.
;It returns true if all pairs are adjacent and false otherwise. The adjacency check is performed by taking
;a node from the set and checking if it is adjacent to all other nodes in the set.

;maximal-clique: This function uses DFS to find the maximal clique in a graph.
; It does this by recursively exploring all possible cliques and selecting the
; largest one that meets the adjacency requirement.
; This function uses DFS, it has an empty clique and then the graph of adjacient nodes that haven't been added to the clique
; If there are node nodes left in the remaining, then there is a clique
; If there are nodes remaining, we take the first node, and test if it's a valid clique
; if it is bigger than our current clique, we fun the function all over again.
; if the new clique isn't bigger than the current, we then run DFS two times to compare
; which clique is bigger.
; This processes until all possible cliques are found

 ;Postcondition: The maximal-clique function returns the maximal clique in the graph, which is a
; list of nodes where every two nodes are connected by an edge. If no such clique exists, it returns an empty list.

