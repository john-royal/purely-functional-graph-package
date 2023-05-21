; Dijkstra's algorithm finds the shortest path between two nodes.
; This returns the length of the path if one is found or '#f' otherwise.

; Overview:
; The function iteratively finds the shortest path between nodes.
;
; First, the set of unvisited nodes is initialized as the set of all nodes in the graph,
; and the set of distances is initialized with the distance to 'start' being 0 and
; the distance to all other nodes being '#f' (other implementations use infinity, but Scheme does
; not have infinity and '#f' was the easiest alternative to implement).
; 
; Then, the unvisited node with the shortest recorded distance is selected.
; If this is the end node, then the associated distance is returned.
; Otherwise, the function computes the distances to all neighboring nodes,
; removes the current node from 'unvisited', and continues the iteration.
;
; This should result in a distance being returned. However, if all nodes
; have been visited and no distance was returned, the function returns '#f'
; to indicate that no path was found.

; General Invariants:
; 1. The 'distances' list contains the shortest distance from 'start' to the other nodes
;    that have been visited thus far and is arranged in order of ascending distances.
;    If the path to a node has not been found yet, the distance is recorded as '#f'.
; 2. Each node in 'unvisited' has not been visited yet, which means the shortest possible
;    path may not have been computed yet.
; 3. If a node is not in 'unvisited', the shortest path to the node from 'start' has been found.

; Helper Functions:
; - 'ascending-distances?': Given two node-distance pairs (e.g. (a 1) and (b 2)),
;   returns true if the nodes are arranged in order of ascending distances.
;   There is special behavior if the recorded distance is '#f' because this is analogous to a distance of infinity.
;   (ascending-distances? (a 1) (b 2)) = #t
;   (ascending-distances? (a 1) (b #f)) = #t
; - 'compute-neighbor-distances': Adds the given distance onto each of the given node-distance pairs.
;   (compute-neighbor-distances 1 ((a 1) (b 2) (c 3))) = ((a 2) (b 3) (c 4))
; - 'insert-distances': Inserts the given node-distance pair into a list.
; - 'merge-distances': Merges two lists of node-distance pairs. If a node is present in both lists, the entry
;   with the shorter distance is used. The list is sorted in order of ascending distances using insertion sort.

; Proof:
; 1. Base Case: Before any distances have been computed, the distance from the source node to itself is recorded as 0,
;    and the distance for all other nodes is recorded as '#f' (infinity).
; 2. Inductive Step: Assuming that all are arranged in ascending order, by selecting the first node in the 'distances' list,
;    we know we are on the shortest path available in the graph.
;    If this is the end node, then we know we have found the shortest path to the end node.
;    Otherwise, we know that by finding the shortest paths from the nodes neighboring this one,
;    we will eventually find the shortest path if one exists.