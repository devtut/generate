---
metaTitle: "Algorithm - Kruskal's Algorithm"
description: "Optimal, disjoint-set based implementation, Simple, more detailed implementation, Simple, disjoint-set based implementation, Simple, high level implementation"
---

# Kruskal's Algorithm



## Optimal, disjoint-set based implementation


We can do two things to improve the simple and sub-optimal disjoint-set subalgorithms:

<li>
**Path compression heuristic**: `findSet` does not need to ever handle a tree with height bigger than `2`. If it ends up iterating such a tree, it can link the lower nodes directly to the root, optimizing future traversals;

```cpp
subalgo findSet(v: a node):
    if v.parent != v
        v.parent = findSet(v.parent)
    return v.parent

```


</li>
<li>
**Height-based merging heuristic**: for each node, store the height of its subtree. When merging, make the taller tree the parent of the smaller one, thus not increasing anyone's height.

```cpp
subalgo unionSet(u, v: nodes):
    vRoot = findSet(v)
    uRoot = findSet(u)

    if vRoot == uRoot:
        return

    if vRoot.height < uRoot.height:
        vRoot.parent = uRoot
    else if vRoot.height > uRoot.height:
        uRoot.parent = vRoot
    else:
        uRoot.parent = vRoot
        uRoot.height =  uRoot.height + 1

```


</li>

This leads to `O(alpha(n))` time for each operation, where `alpha` is the inverse of the fast-growing Ackermann function, thus it is very slow growing, and can be considered `O(1)` for practical purposes.

This makes the entire Kruskal's algorithm `O(m log m + m) = O(m log m)`, because of the initial sorting.

**Note**

Path compression may reduce the height of the tree, hence comparing heights of the trees during union operation might not be a trivial task. Hence to avoid the complexity of storing and calculating the height of the trees the resulting parent can be picked randomly:

```cpp
subalgo unionSet(u, v: nodes):
    vRoot = findSet(v)
    uRoot = findSet(u)

    if vRoot == uRoot:
        return
    if random() % 2 == 0:
        vRoot.parent = uRoot
    else:
        uRoot.parent = vRoot

```

In practice this randomised algorithm together with path compression for `findSet` operation will result in comparable performance, yet much simpler to implement.



## Simple, more detailed implementation


In order to efficiently handle cycle detection, we consider each node as part of a tree. When adding an edge, we check if its two component nodes are part of distinct trees. Initially, each node makes up a one-node tree.

```cpp
algorithm kruskalMST'(G: a graph)
    sort G's edges by their value
    MST = a forest of trees, initially each tree is a node in the graph
    for each edge e in G:
        if the root of the tree that e.first belongs to is not the same 
        as the root of the tree that e.second belongs to:
            connect one of the roots to the other, thus merging two trees
    
    return MST, which now a single-tree forest

```



## Simple, disjoint-set based implementation


The above forest methodology is actually a disjoint-set data structure, which involves three main operations:

```cpp
subalgo makeSet(v: a node):
    v.parent = v    <- make a new tree rooted at v
    

subalgo findSet(v: a node):
    if v.parent == v:
        return v
    return findSet(v.parent)

subalgo unionSet(v, u: nodes):
    vRoot = findSet(v)
    uRoot = findSet(u)

    uRoot.parent = vRoot

algorithm kruskalMST''(G: a graph):
    sort G's edges by their value
    for each node n in G:
        makeSet(n)
    for each edge e in G:
        if findSet(e.first) != findSet(e.second):
            unionSet(e.first, e.second)

```

This naive implementation leads to `O(n log n)` time for managing the disjoint-set data structure, leading to `O(m*n log n)` time for the entire Kruskal's algorithm.



## Simple, high level implementation


Sort the edges by value and add each one to the MST in sorted order, if it doesn't create a cycle.

```cpp
algorithm kruskalMST(G: a graph)
    sort G's edges by their value
    MST = an empty graph
    for each edge e in G:
        if adding e to MST does not create a cycle:
            add e to MST

    return MST

```



#### Remarks


Kruskal's Algorithm is a **greedy** algorithm used to find **Minimum Spanning Tree (MST)** of a graph. A minimum spanning tree is a tree which connects all the vertices of the graph and has the minimum total edge weight.

Kruskal's algorithm does so by repeatedly picking out edges with **minimum weight** (which are not already in the MST) and add them to the final result if the two vertices connected by that edge are not yet connected in the MST, otherwise it skips that edge. Union - Find data structure can be used to check whether two vertices are already connected in the MST or not. A few properties of MST are as follows:

1. A MST of a graph with `n` vertices will have exactly `n-1` edges.
1. There exists a unique path from each vertex to every other vertex.

