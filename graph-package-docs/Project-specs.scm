; Early Specs


; Project Ideas and Specs
; ------------------------

; Firstly an undirected graph is a type of grpah that has no specific direction when it comes to the edges connecting two vertices.
; In other words (A,B) = (B, A)
;(Directed graphs have specific directions the vertices travel so (A,B) doesn't always equal (B, A)

; General Formula for an Undirected graph is:
; |E| = |v| - 1
; This basically means that for every vertice there should be one less edge, makes sense because if we want to connect two edges, we only need one edge.
; The outlier would be if the graph is acyclic, which means that there are no self-loops so if (A, A)

; For this let's try a basic induction (Will most likely need to be improved on LOL)

; Base case: if |v| = 1, then there should be 0 edges. (This graphs can no cycles)

; Induction step: Let n >= 1 and suppose every graph n vertices has n - 1 edges. So every n + 1 vertices has n edges

; -------------------------------------------------------------------------------------------------------------------------------------

; Regarding finding the shortest path between two vertices, we have a few options to choose:

; First we have Depth-First Search (DFS)

; This is recursion
; So for this we would start at V1 and mark that it is visited and mark it as already visited.
; After this we will be recall DFS again for each unvisited vertex.
; Once V2 is found, the path is stored
; This should be easy enough to implement but the catch is that it isn't reliable in finding the shortest path
; [We can use this to check if there is a path between the vertices]

; Dijkstra Algorithm

; This will make sure we went the shortest path between the two vertices [I'm leaning towards this]

; Assign a distance of 0 to the V1 and the rest of the grpah
; Have all the vertices marked as unvisited
; start with the source V1
; For each unvisited vertix calculate the distance from V1, and if the distance is less than the tenative distance of the neighbor, update the distance and store the path
; These steps is repeated until all the vertices are vistied

; --------------------------------------------------------------------------------------------------------------------------------------------------------------------------

; Is the grpah acyclic?

; Firstly let's identify what this means. We want to check if the graph has a self loop. So if one of the edges were to be [A,A]
; This mean that there is one vertice and one edge as a spec.

; So to implement this, we would need to essentially check if that path of a V1 goes to V1 again.
; Upon some research, we can use DFS, which adjustments to it.

; We can set an empty list (so tail-wind recursion?) this will be used to keep track of of the unvisted edges.
; For the unvisited vertices of the current vertex, mark the neighbor vertex as visited and perfrom DFS recursively with the neighbor as the current vertex.
; During this process if a previously visited neighbor that isn't the parent of the current vertex, there is a cycle in the graph.


; ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

; So we should implore a data type that checks if the graph is connected or not, which means if all the vertices of the graph connect or not
; Maybe using DFS could help solve that issue if we modify the code a bit?
; With this idea in DFS we are essentially checking if the vertices are visited during the transversal. So we can essentially just addif all of the
; vertices are visited then its true, and if not return false.
; We can explore the implementation of this later.

; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

; So next let's look at spanning trees

; A spanning tree was actually described earlier by us, it's essentially having an undirected graph that has no cycles
; easy enough
; The best approach is just using a tie in from the other algorithms and essentially checking if the tree has n - 1 edges.
; So we make sure for every two vertices for example there is one edge.
; To do this we essentially have to check that there is nothing like [A, A] or [B,B] sinc ethat describes a loop,and we don't want that.
; I get this can be helpful in the sense of finding the shortest path of an undirected graph, because neglecting cycles, will erase any unnecessary work in our algorithms.
; We don't need it, but it can help.

; ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

; So let's find the diameter which would actually be very useful
; The diameter basically checked the largest shortest paths between any two vertices in a graph.
; So it tells the greatest nuber of possible edges we need to transverse
; [Let's circle back to this]

; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------