
; Depth-first Search
(load "../basic-data-types/set.scm")
(load "./dijkstra.scm")


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
; ------------------------------------------------------------------------------------------------------------------------------------------

; Now that DFS is applied, let's try to use it in regards to the certain algorithms it can be applied to

; First up is to check if there is a path between the two vertices.
; We can easily apply DFS to that
; So for now I decided to modify the existing DFS for this implementation, however as we neatent up our code further, I will try to erase it

; This code checks if the current node is also the target node, if it is, there is a path
; Else if the current code has been visited, it ouputs a the visited set.
; It then goes on the next node and conducts recursion to see if the current vertex goes to the target vertex

(define (DFS graph start target)
  (letrec ((DFS-recursive (lambda (node visited)
                            (if (equal? node target)
                                #t
                                (if (set-member? node visited)
                                    visited
                                    (foldl (lambda (neighbour acc)
                                             (or acc (DFS-recursive neighbour (set-insert node visited))))
                                           #f
                                           (neighbors graph node)))))))
    (DFS-recursive start (set-create-empty))))


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




