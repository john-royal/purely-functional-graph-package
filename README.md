# Purely Functional Graph Package Project

## Project Structure

- [ ] `graph-package`
  - [ ] `basic-data-types`
    - [x] `set.scm`
    - [x] `pair.scm`
    - [ ] `stack.scm`
    - [ ] `queue.scm`
    - [ ] `table.scm`
    - [ ] `graph.scm`
    - [ ] ...
  - [ ] `graph-algorithms`
    - [ ] ...
- [ ] `graph-package-docs`
  - [ ] `basic-data-types-docs`
    - [ ] ...
  - [ ] `graph-algorithms-docs`
    - [ ] ...

## Part 1

1. Design an abstract data type for undirected graphs.

   Note: A graph $G$ is a pair $(V, E)$ of a set of vertices and a set of edges. As a result, you’ll need data types for sets, vertices, and edges.

   Resources: A&S Section 2.1, HW 7 Exercise 3, Class Notes → #20

2. To help design the relevant data types, consider the computations that will need to be carried out:

   1. Is there any path in $G$ between vertices $V_1$ and $V_2$?
   2. Find the shortest path in $G$ between vertices $V_1$ and $V_2$.
   3. Is the graph $G$ acyclic?
   4. Is the graph $G$ connected?
   5. Find a spanning tree in $G$ (will it be helpful to have a data type for trees?).
   6. What is the diameter of the graph $G$?
   7. Is the graph $G$ bipartite?
   8. What is a largest clique in the graph $G$?

   Other possibilities will be suggested later.

   Additionally, consider:

   - What modifications are required to implement labeled undirected graphs (i.e. graphs in which each node has a label)? Is it possible to implement depth-first and breadth-first search for such graphs?
   - What modifications are required to implement directed graphs? Is it possible to implement topological sort for such graphs?
   - What modifications are required to support computations on weighted graphs? Would you need any additional data structures to implement an algorithm for finding a minimum spanning tree?

## Part 2

1. Perhaps introducing another data structure, design and implement an adjacency list representation of graphs. Show that the abstract algorithms designed in Part 1.2 work for your representation.
2. Perhaps introducing another data structure, design and implement an adjacency matrix representation of graphs. Show that the abstract algorithms designed in Part 1.2 work for this representation as well.
