
; Depth-first Search
(load "../basic-data-types/set.scm")
(load "../basic-data-types/pair.scm")

; --------------------------------------------------------------------------------------------------------------------------------------------

; Placeholder for graph stuff

(define (make-empty-graph) '())

(define (add-node graph node)
  (if (assoc node graph) graph
      (cons (pair node (set-create-empty)) graph)))

(define (remove-node graph node)
  (cond ((null? graph) '())
        ((equal? (first (car graph)) node) (cdr graph))
        (else (cons (car graph) (remove-node (cdr graph) node)))))

(define (add-edge graph node1 node2)
  (if (or (null? graph) (not (assoc node1 graph)) (not (assoc node2 graph)))
      graph
      (let* ((entry (car graph))
             (node (first entry))
             (neighbors (second entry)))
        (if (equal? node node1)
            (cons (pair node (set-insert node2 neighbors)) (cdr graph))
            (cons entry (add-edge (cdr graph) node1 node2))))))


(define (remove-edge graph node1 node2)
  (if (null? graph) '()
      (let* ((entry (car graph))
             (node (first entry))
             (neighbors (second entry)))
        (if (equal? node node1)
            (cons (pair node (set-remove node2 neighbors)) (cdr graph))
            (cons entry (remove-edge (cdr graph) node1 node2))))))

(define (neighbors graph node)
  (let ((node-assoc (assoc node graph)))
    (if node-assoc
        (second node-assoc)
        '())))
(define (adjacent? graph node1 node2)
  (if (or (not (assoc node1 graph)) (not (assoc node2 graph)))
      #f
      (let ((n (neighbors graph node1)))
        (set-member? node2 n))))
(define (nodes graph)
  (map car graph))

(define (edges graph)
  (if (null? graph) '()
      (let* ((entry (car graph))
             (node (first entry))
             (connected-nodes (second entry)))
        (set-union (map (lambda (node2) (pair node node2)) connected-nodes) (edges (cdr graph))))))

(define g (make-empty-graph))
(define g (add-node g 'C))
(define g (add-node g 'B))
(define g (add-node g 'A))
(define g (add-edge g 'A 'B))
(define g (add-edge g 'A 'C))
(define g (add-edge g 'B 'A))
(define g (add-edge g 'C 'B))
(display g)

(newline)
(newline)

; --------------------------------------------------------------------------------------------------------------------------------------------
; Firstly we can use stacks for this, one for the current stack and the other for the visited stacks
; First we create a stack with the total number of vertices in the graph
; Then we choose any vertex as the starting point of the transversal, and push it in the stack.
; After that we push, a non-visited vertex (adjacient to the vertex on the top of the stack) to the top of the stack
; We tgeb repeat this prcoess until there are no vertices left to visit.
; If there are no vertices left, we pop a vertex from the stack
; This is done until the stack is empty

; Now let's take a look at our data-types and see how we can implement this.
; Firstly we can assign the stacks as visited and current, for now at least.
; We'll use recursion for this DFS, so let's try with letrec again (local recursive fuctions)
; We'll start from a starting vertex that is the start point for thr DFS.
; From there we can add all the visited vertices to the visited set (or stack)
; We use a helper function foldl to visit the neighbor of the current code.

(define (foldl f initial lst)
  (if (null? lst)
      initial
      (foldl f (f (car lst) initial) (cdr lst))))

(define (DFS graph start)
  (letrec ((DFS-recursive (lambda (node visited)
                            (if (set-member? node visited)
                                visited
                                (foldl (lambda (neighbour acc)
                                         (DFS-recursive neighbour acc))
                                       (set-insert node visited)
                                       (neighbors graph node))))))
    (DFS-recursive start (set-create-empty))))
; Test

;(DFS g 'a)


; ------------------------------------------------------------------------------------------------------------------------------------------

; Now that DFS is applied, let's try to use it in regards to the certain algorithms it can be applied to

; First up is to check if there is a path between the two vertices.
; We can easily apply DFS to that
; So for now I decided to modify the existing DFS for this implementation, however as we neatent up our code further, I will try to erase it

; This code checks if the current node is also the target node, if it is, there is a path
; Else if the current code has been visited, it ouputs a the visited set.
; It then goes on the next node and conducts recursion to see if the current vertex goes to the target vertex

(define (DFS-path graph start target)
  (letrec ((DFS-recursive (lambda (node visited)
                            (if (equal? node target)
                                #t
                                (if (set-member? node visited)
                                    #f
                                    (foldl (lambda (neighbour acc)
                                             (or acc (DFS-recursive neighbour (set-insert node visited))))
                                           #f
                                           (neighbors graph node)))))))
    (DFS-recursive start (set-create-empty))))


; Test
(display (DFS-path g 'A 'C))
(newline)
(newline)
; -------------------------------------------------------------------------------------------------------------------------------------------------------------------

; Now let's use DFS again to implement if the graph is acyclic or not

; Now this was my previous design idea for it

;Design Idea
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

; Let's revamp it for the new graph now
; The main difference we can implement is by using set-memeber, set-insert, set-create-epty, first, nodes, and neighbors. This just makes our function more abstract
; and less tied to the built-in R5RS functions

(define (dfs-acyclic current_vertex parent graph visited)
  (if (set-member? current_vertex visited)
      (if (equal? parent current_vertex)
          #f
          #t)
      (let loop ((neighbors (neighbors graph current_vertex)))
        (if (null? neighbors)
            #f
            (let ((neighbor (car neighbors)))
              (if (not (equal? parent neighbor))
                  (if (dfs-acyclic neighbor current_vertex graph (set-insert current_vertex visited))
                      #t
                      (loop (cdr neighbors)))
                  (loop (cdr neighbors))))))))

(define (is-acyclic graph)
  (let ((starting_vertex (first (nodes graph)))) 
    (dfs-acyclic starting_vertex #f graph (set-create-empty))))


;Test

;(is-acyclic g)

; -------------------------------------------------------------------------------------------------------------------------------

; Now to check if the graph is connected or not, which will also be done through DFS
; Original design plan
; So we want to check if the graph is connected, which means that all the vertices are connected through one parent vertex
; Honestly from all we've done so far this isn't diffuclt, we essentially can just run DFS, and calculate from this
; We'll reimplement DFS, and then a helper function that checks the number of vertices in the graph
; The point of that is to compare the visited elements of DFS with how many vertices there are in the graph
; Then we create a function connect? that essentially started DFS on the first vertex.
; Once that is done, it compares the visted vertices from DFS to the number of vertices in the graph
; If the number of vertices are the same, we have a connected graph!

; I was able to just replace some of the code with the abstract datatypes, which wasn't too hard to do

(define (dfs-connected current visited graph)
  (if (set-member? current visited)
      visited
      (let ((neighbors (neighbors graph current)))
        (define (iter-connect nbrs)
          (if (null? nbrs)
              visited 
              (let ((nbr (first nbrs)))
                (if (not (set-member? nbr visited))
                    (iter-connect (dfs-connected nbr (set-insert current visited) graph))
                    (iter-connect (cdr nbrs))))))
        (iter-connect neighbors))))

(define (number-of-nodes graph)
  (length (nodes graph)))


(define (connected? graph)
  (let* ((start-node (car (nodes graph)))
         (visited (dfs-connected start-node (set-create-empty) graph)))
    (= (number-of-nodes graph) (length visited))))

;Test
;(connected? g)

; For the part that checks if it's an acyclic graph doesn't have to change at all, the data types are already under the hood

(define (spanning? graph)
  (and (is-acyclic graph) (connected? graph)))

; Test
;(spanning? g)

; Lastly we can use DFS to also gives us a spanning tree made from the graph;
; I created this before for the more abstract version, let's try it again;
; We now are using set-member? to check if the current vertex is in the visited set
; We used first to get the first neighbor, without using car
; set-insert is used to insert te current vertex into the visited set
; Pair is used to pair the current vertex into a tree
; Laslty set-empty-set creates an empty set for the tree

(define (dfs-spanning-tree current visited tree graph)
  (if (set-member? current visited)
      tree
      (let ((neighbors (neighbors graph current)))
        (define (iter-tree nbrs)
          (if (null? nbrs)
              tree
              (let ((nbr (first nbrs)))
                (if (not (set-member? nbr visited))
                    (iter-tree (dfs-spanning-tree nbr (set-insert current visited) (set-insert (list current nbr) tree) graph))
                    (iter-tree (cdr nbrs))))))
        (iter-tree neighbors))))

(define (spanning-tree graph)
  (let ((start-vertex (first (first (nodes graph)))))
    (dfs-spanning-tree start-vertex (set-create-empty) (set-create-empty) graph)))

(spanning-tree g)