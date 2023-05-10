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
; Upon some research (ChatGBT LOL) We can use DFS, which adjustments to it. This can score us some brownie points as well hopefully.

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

; Let's try starting some of the code.

; First we should consider adding an empty graph function

(define (make-graph)
  '())

; Now we should create a datatype to describe an undirected graph

; We need to add verteices for the grpahs
; Example of an adjacent list
(define ex-graph
  '((A B)
    (B A C)
    (C B)))
; So from some chatgbt questions, we can use a built in function assoc, that will basically return the sublist when presented a key, which is the car of the sublist.
; So for this example (assoc 'B graph) = '(b a c)
; This can be uselful when visting neigbors for vertices.


; Realized I can use this to add vertices.

; So how can we add vertices, bit of learning curve I have realized upon further inspection, but let's see how I do LOL.

; First we want to make sure that the grpah we are adding the vertice into doesn't already exist.
; We can add a conditional to combat that.
; We are working with an empty list, so we can add it to that.

; first we need two parameters one for the new value and the other for the graph, we are adding to

; Let's try the code

; This marks the first part of the project. I doubt we need to make much changes for this as, it's just creating an undirected graph (1.1)

(define (add-vertex graph vertex)
  (if (assoc vertex graph)
      graph
      (cons (cons vertex '()) graph)))
(define graph (make-graph))

(define graph (add-vertex graph 'A))
(define graph (add-vertex graph 'B))
(define graph (add-vertex graph 'C))

; (display graph)

; It seems to work possiblly tweak the code so it doesn't reverse, but that's probably not an issue we have to care about

; Let's begin working on creating a function to add edges to our graph

(define (add-edge graph v1 v2 weight)
  (let ((entry1 (assoc v1 graph))
        (entry2 (assoc v2 graph)))
    (if entry1 (set-cdr! entry1 (cons (cons v2 weight) (cdr entry1))))
    (if entry2 (set-cdr! entry2 (cons (cons v1 weight) (cdr entry2))))
    graph))

;(display graph)

;---------------------------------------------------------------------------------------------------------------------------------------------------------

; Let try do steps for 1.2 (This will most likely be the most bothersome part of the project

; Let's start with seeing if vertice 1 and 2 are connected.
; This actually can be doable without too much issue, but idk if my approach would be good enough or not, let's see

; So from how the undirected graph is created, it's basically a big list with a bunch of sublists that represent the edges, or connect between the vertices.
; So if we want to see if there's a connection between two vertices, we essentially can just spearate the sublists and check if two vertices are in the sublist, that should prove a
; connection or not.

(define (graph-connection v1 v2 graph)
  (cond ((null? graph) #f)
        ((and (member v1 (car graph)) (member v2 (car graph))) #t)
        (else (graph-connection v1 v2 (cdr graph)))))

(define graph (graph-connection 'a 'b graph))

; According to chatgbt, this code wouldn't work --- which is very unfortunate, guess we gotta use DFS

; I used a lot of chatgbt for this, but I got a pretty good understanding for it.

(define (dfs v1 v2 graph visited)
  (if (eq? v1 v2)
      #t
      (let ((neighbors (cdr (assoc v1 graph))))
        (define (dfs-iter nbrs)
          (if (null? nbrs)
              #f
              (let ((nbr (car nbrs)))
                (if (not (member nbr visited))
                    (or (dfs nbr v2 graph (cons v1 visited)) (dfs-iter (cdr nbrs)))
                    (dfs-iter (cdr nbrs))))))
        (dfs-iter neighbors))))

(define (graph-connection v1 v2 graph)
  (dfs v1 v2 graph '()))

; Let's implement Dijsktra algorithm, which will help us find the shortest path between V1 and V2, it's based on the Greedy Method
; If (d[u] + c(u,v) < d[v])
;       d[v] = d[u] + c(u,v)
; This means that if a shorter distance is found than the current distance we will replace it

; We need the starter vertex, V1
; Look for direct edges first, and the ones that arent label them as infitiy
; Find the shortest direct edge distance, and once that is found, conduct the equation I added above
; Keep doing this and modify as it goes

; This is a custom fold program to use
(define (my-foldl func initial-value lst)
  (if (null? lst)
      initial-value
      (my-foldl func (func (car lst) initial-value) (cdr lst))))

(define (find-min-distance-node distances visited)
  (my-foldl
    (lambda (entry min-entry)
      (if (and (not (member (car entry) visited)) (< (cdr entry) (cdr min-entry)))
          entry
          min-entry))
    (cons (car distances) +inf.0)
    distances))

(define (dijkstra graph start end)
  (define (update-distances-iter current-distance neighbors)
    (if (null? neighbors)
        #t
        (let* ((nbr (car neighbors))
               (nbr-node (car nbr))
               (nbr-dist (+ current-distance (cdr nbr)))
               (entry (assoc nbr-node distances)))
          (if (and (< nbr-dist (cdr entry)) (not (member nbr-node visited)))
              (set-cdr! entry nbr-dist)
              #f)
          (update-distances-iter current-distance (cdr neighbors)))))

  (define (dijkstra-helper visited distances)
    (let* ((current-node (car (find-min-distance-node distances visited)))
           (current-distance (cdr (assoc current-node distances))))
      (if (or (null? current-node) (eq? current-node end))
          current-distance
          (begin
            (set! visited (cons current-node visited))
            (update-distances-iter current-distance (cdr (assoc current-node graph)))
            (dijkstra-helper visited distances)))))

  (let ((visited '())
        (distances (map (lambda (v) (cons (car v) (if (eq? (car v) start) 0 +inf.0))) graph)))
    (dijkstra-helper visited distances)))
; Test code
(define graph (make-graph))
(define graph (add-vertex graph 'A))
(define graph (add-vertex graph 'B))
(define graph (add-vertex graph 'C))
(define graph (add-edge graph 'A 'B 1))
(define graph (add-edge graph 'A 'C 3))
(define graph (add-edge graph 'B 'C 2))

;(display (dijkstra graph 'A 'C)) ; Output: 3

; [Side Not John, this doesn't work, it's chatgbt code, I'm sorry I couldn't find the energy to implement it myself, so can you? I cant provide a conceptual view on it for you!]

; ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

; So now let's try to find out if the grpah is acyclic or not
; Just a reminder an acyclic grpah is if there is a self loop so if (A, B), (A, C), which in theory should be not too diffuclt to prove.
; The main concern is of either finding our own algorithm or using an existing algorithm to prove it.
; So a broad design plan would be to recursively go through the graph list and check if we see in a sublist if there is an edge that connected V1 to V1.
; Sounds easy enough or is it? LOL

; Okay let's try modifying the DFS model, so we want to see if there is an edge with one vertex
; We can set parameters graph, and visited. 

(define (dfs-acyclic current_vertex parent graph visited)
  (if (member current_vertex visited)
      (if (eq? parent current_vertex)
          #f
          #t)
      (let loop ((neighbors (cdr (assoc current_vertex graph))))
        (if (null? neighbors)
            #f
            (let ((neighbor (car neighbors)))
              (if (not (eq? parent neighbor))
                  (if (dfs-acyclic neighbor current_vertex graph (cons current_vertex visited))
                      #t
                      (loop (cdr neighbors)))
                  (loop (cdr neighbors))))))))

(define (is-acyclic graph)
  (let ((starting_vertex (caar graph))) (dfs-acyclic starting_vertex #f graph '())))
 ; the parent input is used to identify back edges since an undirected graph doesn't follow a specifc direction
    
; ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

; Now let's create a data type for spanning trees

; Essentially a spanning tree is a graph that has no circuits, or in other words doesn't loop back;
; For this the best coruse is findig a spanning tree inside our unidrected graph. We can do this by having an empty list, which can add a direction, that is a spanning tree
; Obviously this is a very simplistic way to look at it, and will have to add in a lot of knowledge to it

; Oh wait lol, it's easy to define a spanning tree, well I mean if the graph is unidrected, all we need is for it to be connected and doesn't have loops lol

; the function can be to comapre if the graph is connected and doesn't have loops. Let's back track and and create a function to check if the graph is connected

; To start on finding if the graph is connected, I'm gonna implement, I'm going to use a helper function that lets me know how many vertices are in the graph.
(define (number-of-vertices graph)
  (length graph))
; The point of this is to compare the number of vertices in the graph vs the completed vertices of the visted graphs

(define (dfs-connected current visited graph)
  (if (member current visited)
      visited
      (let ((neighbors (cdr (assoc current graph))))
        (define (iter-connect nbrs)
          (if (null? nbrs)
              visited 
              (let ((nbr (car nbrs)))
                (if (not (member nbr visited))
                    (iter-connect (dfs-connected nbr (cons current visited) graph))
                    (iter-connect (cdr nbrs))))))
        (iter-connect neighbors))))


(define (number-of-vertices graph)
  (length graph))

(define (connected? graph)
  (let* ((start-vertex (caar graph))
         (visited (dfs-connected start-vertex '() graph)))
    (= (number-of-vertices graph) (length visited))))

; So i just realized I'm continously creating DFS function, when I don't have to, realized how stupid this is. I can fix it eventually for the final product, so please bear with me
; for now John lol

; Cool, so now we can check if the graph is connected and check if there's a loop
; So now that we have these implemented, we can easily check if there is a spanning tree in the unidrected graph

(define (spanning? graph)
  (and (is-acyclic graph) (connected? graph)))
; The conditional is obviously usueless but, I feel like more now, it can give a more conceptual feel of it

; Now with this placed, we can create a way to find the spanning graph

                    
        
        





