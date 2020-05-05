---
metaTitle: "R - Network analysis with the igraph package"
description: "Simple Directed and Non-directed Network Graphing"
---

# Network analysis with the igraph package



## Simple Directed and Non-directed Network Graphing


The igraph package for R is a wonderful tool that can be used to model networks, both real and virtual, with simplicity. This example is meant to demonstrate how to create two simple network graphs using the igraph package within R v.3.2.3.

**Non-Directed Network**

The network is created with this piece of code:

```r
g<-graph.formula(Node1-Node2, Node1-Node3, Node4-Node1)
plot(g)

```

<img src="http://i.imgur.com/wCJh3xI.png" alt="Valid XHTML" />

**Directed Network**

```r
dg<-graph.formula(Tom-+Mary, Tom-+Bill, Tom-+Sam, Sue+-Mary, Bill-+Sue)
plot(dg)

```

This code will then generate a network with arrows:

<img src="http://i.imgur.com/TdI8Slh.png" alt="Valid XHTML" />

Code example of how to make a double sided arrow:

```r
dg<-graph.formula(Tom-+Mary, Tom-+Bill, Tom-+Sam, Sue+-Mary, Bill++Sue)
plot(dg)

```

<img src="http://i.imgur.com/PEJTMV6.png" alt="Valid XHTML" />

