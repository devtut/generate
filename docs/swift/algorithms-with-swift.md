---
metaTitle: "Swift - Algorithms with Swift"
description: "Sorting, Insertion Sort, Selection sort, Asymptotic analysis, Quick Sort - O(n log n) complexity time, Graph, Trie, Stack"
---

# Algorithms with Swift


Algorithms are a backbone to computing. Making a choice of which algorithm to use in which situation distinguishes an average from good programmer. With that in mind, here are definitions and code examples of some of the basic algorithms out there.



## Sorting


**Bubble Sort**

This is a simple sorting algorithm that repeatedly steps through the list to be sorted, compares each pair of adjacent items and swaps them if they are in the wrong order. The pass through the list is repeated until no swaps are needed.
Although the algorithm is simple, it is too slow and impractical for most problems. It has complexity of O(n2) but it is considered slower than insertion sort.

```swift
extension Array where Element: Comparable {

func bubbleSort() -> Array<Element> {
    
    //check for trivial case
    guard self.count > 1 else {
        return self
    }
    
    //mutated copy
    var output: Array<Element> = self
    
    for primaryIndex in 0..<self.count {
        let passes = (output.count - 1) - primaryIndex
        
        //"half-open" range operator
        for secondaryIndex in 0..<passes {
            let key = output[secondaryIndex]
            
            //compare / swap positions
            if (key > output[secondaryIndex + 1]) {
                swap(&output[secondaryIndex], &output[secondaryIndex + 1])
            }
        }
    }
    
    return output
}

}

```

**Insertion sort**

Insertion sort is one of the more basic algorithms in computer science. The insertion sort ranks elements by iterating through a collection and positions elements based on their value. The set is divided into sorted and unsorted halves and repeats until all elements are sorted.
Insertion sort has complexity of O(n2).
You can put it in an extension, like in an example below, or you can create a method for it.

```swift
extension Array where Element: Comparable {

func insertionSort() -> Array<Element> {
    
    //check for trivial case
    guard self.count > 1 else {
        return self
    }
    
    //mutated copy
    var output: Array<Element> = self
    
    for primaryindex in 0..<output.count {
        
        let key = output[primaryindex]
        var secondaryindex = primaryindex
        
        while secondaryindex > -1 {
            if key < output[secondaryindex] {
                
                //move to correct position
                output.remove(at: secondaryindex + 1)
                output.insert(key, at: secondaryindex)
            }
            secondaryindex -= 1
        }
    }
    
    return output
}
}

```

**Selection sort**

Selection sort is noted for its simplicity. It starts with the first element in the array, saving it's value as a minimum value (or maximum, depending on sorting order). It then itterates through the array, and replaces the min value with any other value lesser then min it finds on the way. That min value is then placed at the leftmost part of the array and the process is repeated, from the next index, until the end of the array. Selection sort has complexity of O(n2) but it is considered slower than it's counterpart - Selection sort.

func selectionSort() -> Array {
//check for trivial case
guard self.count > 1 else {
return self
}

```swift
//mutated copy
var output: Array<Element> = self
 
for primaryindex in 0..<output.count {
    var minimum = primaryindex
    var secondaryindex = primaryindex + 1
     
    while secondaryindex < output.count {
        //store lowest value as minimum
        if output[minimum] > output[secondaryindex] {
            minimum = secondaryindex
        }
        secondaryindex += 1
    }
     
    //swap minimum value with array iteration
    if primaryindex != minimum {
        swap(&output[primaryindex], &output[minimum])
    }
}
 
return output 
}

```

**Quick Sort - O(n log n) complexity time**

Quicksort is one of the advanced algorithms. It features a time complexity of O(n log n) and applies a divide & conquer strategy. This combination results in advanced algorithmic performance. Quicksort first divides a large array into two smaller sub-arrays: the low elements and the high elements. Quicksort can then recursively sort the sub-arrays.

The steps are:

Pick an element, called a pivot, from the array.

Reorder the array so that all elements with values less than the pivot come before the pivot, while all elements with values greater than the pivot come after it (equal values can go either way). After this partitioning, the pivot is in its final position. This is called the partition operation.

Recursively apply the above steps to the sub-array of elements with smaller values and separately to the sub-array of elements with greater values.

mutating func quickSort() -> Array {

```swift
func qSort(start startIndex: Int, _ pivot: Int) {
    
    if (startIndex < pivot) {
        let iPivot = qPartition(start: startIndex, pivot)
        qSort(start: startIndex, iPivot - 1)
        qSort(start: iPivot + 1, pivot)
    }
}
qSort(start: 0, self.endIndex - 1)
return self
}

mutating func qPartition(start startIndex: Int, _ pivot: Int) -> Int {

var wallIndex: Int = startIndex

//compare range with pivot
for currentIndex in wallIndex..<pivot {
    
    if self[currentIndex] <= self[pivot] {
        if wallIndex != currentIndex {
            swap(&self[currentIndex], &self[wallIndex])
        }
        
        //advance wall
        wallIndex += 1
    }
}
    //move pivot to final position
    if wallIndex != pivot {
        swap(&self[wallIndex], &self[pivot])
    }
    return wallIndex
}

```



## Insertion Sort


Insertion sort is one of the more basic algorithms in computer science. The insertion sort ranks elements by iterating through a collection and positions elements based on their value. The set is divided into sorted and unsorted halves and repeats until all elements are sorted.
Insertion sort has complexity of O(n2).
You can put it in an extension, like in an example below, or you can create a method for it.

```swift
extension Array where Element: Comparable {

func insertionSort() -> Array<Element> {
    
    //check for trivial case
    guard self.count > 1 else {
        return self
    }
    
    //mutated copy
    var output: Array<Element> = self
    
    for primaryindex in 0..<output.count {
        
        let key = output[primaryindex]
        var secondaryindex = primaryindex
        
        while secondaryindex > -1 {
            if key < output[secondaryindex] {
                
                //move to correct position
                output.remove(at: secondaryindex + 1)
                output.insert(key, at: secondaryindex)
            }
            secondaryindex -= 1
        }
    }
    
    return output
}
}

```



## Selection sort


Selection sort is noted for its simplicity. It starts with the first element in the array, saving it's value as a minimum value (or maximum, depending on sorting order). It then itterates through the array, and replaces the min value with any other value lesser then min it finds on the way. That min value is then placed at the leftmost part of the array and the process is repeated, from the next index, until the end of the array.
Selection sort has complexity of O(n2) but it is considered slower than it's counterpart - Selection sort.

```swift
func selectionSort() -> Array<Element> {
    //check for trivial case
    guard self.count > 1 else {
        return self
    }
     
    //mutated copy
    var output: Array<Element> = self
     
    for primaryindex in 0..<output.count {
        var minimum = primaryindex
        var secondaryindex = primaryindex + 1
         
        while secondaryindex < output.count {
            //store lowest value as minimum
            if output[minimum] > output[secondaryindex] {
                minimum = secondaryindex
            }
            secondaryindex += 1
        }
         
        //swap minimum value with array iteration
        if primaryindex != minimum {
            swap(&output[primaryindex], &output[minimum])
        }
    }
     
    return output
}

```



## Asymptotic analysis


Since we have many different algorithms to choose from, when we want to sort an array, we need to know which one will do it's job. So we need some method of measuring algoritm's speed and reliability. That's where Asymptotic analysis kicks in.
Asymptotic analysis is the process of describing the efficiency of algorithms as their input size (n) grows. In computer science, asymptotics are usually expressed in a common format known as Big O Notation.

- **Linear time O(n)**: When each item in the array has to be evaluated in order for a function to achieve it's goal, that means that the function becomes less efficent as number of elements is increasing. **A function like this is said to run in linear time because its speed is dependent on its input size.**
- **Polynominal time O(n2)**: If complexity of a function grows exponentialy (meaning that for n elements of an array complexity of a function is n squared) that function operates in polynominal time. These are usually functions with nested loops. Two nested loops result in O(n2) complexity, three nested loops result in O(n3) complexity, and so on...
- **Logarithmic time O(log n):** Logarithmic time functions's complexity is minimized when the size of its inputs (n) grows. These are the type of functions every programmer strives for.



## Quick Sort - O(n log n) complexity time


Quicksort is one of the advanced algorithms. It features a time complexity of O(n log n) and applies a divide & conquer strategy. This combination results in advanced algorithmic performance. Quicksort first divides a large array into two smaller sub-arrays: the low elements and the high elements. Quicksort can then recursively sort the sub-arrays.

The steps are:

<li>
Pick an element, called a pivot, from the array.
</li>
<li>
Reorder the array so that all elements with values less than the pivot come before the pivot, while all elements with values greater than the pivot come after it (equal values can go either way). After this partitioning, the pivot is in its final position. This is called the partition operation.
</li>
<li>
Recursively apply the above steps to the sub-array of elements with smaller values and separately to the sub-array of elements with greater values.

```swift
mutating func quickSort() -> Array<Element> {

func qSort(start startIndex: Int, _ pivot: Int) {
    
    if (startIndex < pivot) {
        let iPivot = qPartition(start: startIndex, pivot)
        qSort(start: startIndex, iPivot - 1)
        qSort(start: iPivot + 1, pivot)
    }
}
qSort(start: 0, self.endIndex - 1)
return self

```


}
mutating func qPartition(start startIndex: Int, _ pivot: Int) -> Int {

```swift
var wallIndex: Int = startIndex

//compare range with pivot
for currentIndex in wallIndex..<pivot {
    
    if self[currentIndex] <= self[pivot] {
        if wallIndex != currentIndex {
            swap(&self[currentIndex], &self[wallIndex])
        }
        
        //advance wall
        wallIndex += 1
    }
}

```


</li>

```

   //move pivot to final position
    if wallIndex != pivot {
        swap(&self[wallIndex], &self[pivot])
    }
    return wallIndex
}

```



## Graph, Trie, Stack


### **Graph**

In computer science, a graph is an abstract data type that is meant to implement the undirected graph and directed graph concepts from mathematics.
A graph data structure consists of a finite (and possibly mutable) set of vertices or nodes or points, together with a set of unordered pairs of these vertices for an undirected graph or a set of ordered pairs for a directed graph. These pairs are known as edges, arcs, or lines for an undirected graph and as arrows, directed edges, directed arcs, or directed lines for a directed graph. The vertices may be part of the graph structure, or may be external entities represented by integer indices or references.
A graph data structure may also associate to each edge some edge value, such as a symbolic label or a numeric attribute (cost, capacity, length, etc.). (Wikipedia, [source](https://en.wikipedia.org/wiki/Graph_(abstract_data_type)))

```swift
//
//  GraphFactory.swift
//  SwiftStructures
//
//  Created by Wayne Bishop on 6/7/14.
//  Copyright (c) 2014 Arbutus Software Inc. All rights reserved.
//
import Foundation


public class SwiftGraph {
   
    
    //declare a default directed graph canvas
    private var canvas: Array<Vertex>
    public var isDirected: Bool
    
    
    init() {
        canvas = Array<Vertex>()
        isDirected = true
    }
    
    
    //create a new vertex
    func addVertex(key: String) -> Vertex {
        
        
        //set the key
        let childVertex: Vertex = Vertex()
        childVertex.key = key
        
        
        //add the vertex to the graph canvas
        canvas.append(childVertex)
        
        
        return childVertex
    }
    
    
    
    //add edge to source vertex
    func addEdge(source: Vertex, neighbor: Vertex, weight: Int) {
        
        
        //create a new edge
        let newEdge = Edge()
        
        
        //establish the default properties
        newEdge.neighbor = neighbor
        newEdge.weight = weight
        source.neighbors.append(newEdge)
        
        
        print("The neighbor of vertex: \(source.key as String!) is \(neighbor.key as String!)..")
        
        
        //check condition for an undirected graph
        if isDirected == false {
            
            
           //create a new reversed edge
           let reverseEdge = Edge()
            
            
           //establish the reversed properties
           reverseEdge.neighbor = source
           reverseEdge.weight = weight
           neighbor.neighbors.append(reverseEdge)
            
           print("The neighbor of vertex: \(neighbor.key as String!) is \(source.key as String!)..")
            
        }
        
        
    }

    
    
    
    
    /* reverse the sequence of paths given the shortest path.
       process analagous to reversing a linked list. */

    func reversePath(_ head: Path!, source: Vertex) -> Path! {
        
        
        guard head != nil else {
            return head
        }
        
        //mutated copy
        var output = head
        
        
        var current: Path! = output
        var prev: Path!
        var next: Path!
        
        
        while(current != nil) {
            next = current.previous
            current.previous = prev
            prev = current
            current = next
        }
        
        
        //append the source path to the sequence
        let sourcePath: Path = Path()
        
        sourcePath.destination = source
        sourcePath.previous = prev
        sourcePath.total = nil
        
        output = sourcePath
        
        
        return output
        
    }

    
    
    
    //process Dijkstra's shortest path algorthim
    func processDijkstra(_ source: Vertex, destination: Vertex) -> Path? {
        
        
        var frontier: Array<Path> = Array<Path>()
        var finalPaths: Array<Path> = Array<Path>()
        
        
        //use source edges to create the frontier
        for e in source.neighbors {
            
            let newPath: Path = Path()
            
            
            newPath.destination = e.neighbor
            newPath.previous = nil
            newPath.total = e.weight
            
            
            //add the new path to the frontier
            frontier.append(newPath)
            
        }
        

        //construct the best path
        var bestPath: Path = Path()
        
        
        while frontier.count != 0 {
            
            //support path changes using the greedy approach
            bestPath = Path()
            var pathIndex: Int = 0

            
            for x in 0..<frontier.count {
               
                let itemPath: Path = frontier[x]
                
                if  (bestPath.total == nil) || (itemPath.total < bestPath.total) {
                    bestPath = itemPath
                    pathIndex = x
                }
                
            }
            
            
            
            //enumerate the bestPath edges
            for e in bestPath.destination.neighbors {
                
                let newPath: Path = Path()
                
                newPath.destination = e.neighbor
                newPath.previous = bestPath
                newPath.total = bestPath.total + e.weight
                
                
                //add the new path to the frontier
                frontier.append(newPath)
                
            }
            
            
            //preserve the bestPath
            finalPaths.append(bestPath)
            
            
            //remove the bestPath from the frontier
            //frontier.removeAtIndex(pathIndex) - Swift2
            frontier.remove(at: pathIndex)
            
            
            
        } //end while
        
        
    
        //establish the shortest path as an optional
        var shortestPath: Path! = Path()
        
        
        for itemPath in finalPaths {
            
            if (itemPath.destination.key == destination.key) {
                
                if  (shortestPath.total == nil) || (itemPath.total < shortestPath.total) {
                    shortestPath = itemPath
                }
                
            }
            
        }
        
        
        return shortestPath
        
    }
    
    
    
    ///an optimized version of Dijkstra's shortest path algorthim
    func processDijkstraWithHeap(_ source: Vertex, destination: Vertex) -> Path! {
        
        
        let frontier: PathHeap = PathHeap()
        let finalPaths: PathHeap = PathHeap()
        
        
        //use source edges to create the frontier
        for e in source.neighbors {
            
            let newPath: Path = Path()
            
            
            newPath.destination = e.neighbor
            newPath.previous = nil
            newPath.total = e.weight
            
            
            //add the new path to the frontier
            frontier.enQueue(newPath)
            
        }
        
        
        //construct the best path
        var bestPath: Path = Path()
        
        
        while frontier.count != 0 {
                        
            //use the greedy approach to obtain the best path
            bestPath = Path()
            bestPath = frontier.peek()
            
            
            //enumerate the bestPath edges
            for e in bestPath.destination.neighbors {
                
                let newPath: Path = Path()
                
                newPath.destination = e.neighbor
                newPath.previous = bestPath
                newPath.total = bestPath.total + e.weight
                
                
                //add the new path to the frontier
                frontier.enQueue(newPath)
                
            }
            
            
            //preserve the bestPaths that match destination
            if (bestPath.destination.key == destination.key) {
                finalPaths.enQueue(bestPath)
            }
            
            
            //remove the bestPath from the frontier
            frontier.deQueue()
            
            
        } //end while
        
        
        
        //obtain the shortest path from the heap
        var shortestPath: Path! = Path()
        shortestPath = finalPaths.peek()
        
        
        return shortestPath
        
    }
    
    
    //MARK: traversal algorithms
    
    
    //bfs traversal with inout closure function
    func traverse(_ startingv: Vertex, formula: (_ node: inout Vertex) -> ()) {

        
        //establish a new queue
        let graphQueue: Queue<Vertex> = Queue<Vertex>()
        
        
        //queue a starting vertex
        graphQueue.enQueue(startingv)
        
        
        while !graphQueue.isEmpty() {
            
            //traverse the next queued vertex
            var vitem: Vertex = graphQueue.deQueue() as Vertex!
            
            
            //add unvisited vertices to the queue
            for e in vitem.neighbors {
                if e.neighbor.visited == false {
                    print("adding vertex: \(e.neighbor.key!) to queue..")
                    graphQueue.enQueue(e.neighbor)
                }
            }
            

            /*
            notes: this demonstrates how to invoke a closure with an inout parameter.
            By passing by reference no return value is required.
            */
            
            //invoke formula
            formula(&vitem)
            
            
        } //end while
        
        
        print("graph traversal complete..")
        
        
    }

    
    
    
    //breadth first search
    func traverse(_ startingv: Vertex) {
        
        
        //establish a new queue
        let graphQueue: Queue<Vertex> = Queue<Vertex>()
        
        
        //queue a starting vertex
        graphQueue.enQueue(startingv)
        
        
        while !graphQueue.isEmpty() {
            
            //traverse the next queued vertex
            let vitem = graphQueue.deQueue() as Vertex!
            
            guard vitem != nil else {
                return
            }
            
            //add unvisited vertices to the queue
            for e in vitem!.neighbors {
                if e.neighbor.visited == false {
                    print("adding vertex: \(e.neighbor.key!) to queue..")
                    graphQueue.enQueue(e.neighbor)
                }
            }
            
            
            vitem!.visited = true
            print("traversed vertex: \(vitem!.key!)..")
            
            
        } //end while
        
        
        print("graph traversal complete..")
        
        
    } //end function
    
    
    
    //use bfs with trailing closure to update all values
    func update(startingv: Vertex, formula:((Vertex) -> Bool)) {
        
        
        //establish a new queue
        let graphQueue: Queue<Vertex> = Queue<Vertex>()
        
        
        //queue a starting vertex
        graphQueue.enQueue(startingv)
        
        
        while !graphQueue.isEmpty() {
            
            //traverse the next queued vertex
            let vitem = graphQueue.deQueue() as Vertex!            
            
            guard vitem != nil else {
                return
            }
            
            //add unvisited vertices to the queue
            for e in vitem!.neighbors {
                if e.neighbor.visited == false {
                    print("adding vertex: \(e.neighbor.key!) to queue..")
                    graphQueue.enQueue(e.neighbor)
                }
            }
            
            
            //apply formula..
            if formula(vitem!) == false {
                print("formula unable to update: \(vitem!.key)")
            }
            else {
                print("traversed vertex: \(vitem!.key!)..")
            }
            
            vitem!.visited = true
            
            
        } //end while
        
        
        print("graph traversal complete..")
        
        
    }

    

    
    
}

```

### **Trie**

In computer science, a trie, also called digital tree and sometimes radix tree or prefix tree (as they can be searched by prefixes), is a kind of search tree—an ordered tree data structure that is used to store a dynamic set or associative array where the keys are usually strings. (Wikipedia, [source](https://en.wikipedia.org/wiki/Trie))

```swift
//
//  Trie.swift
//  SwiftStructures
//
//  Created by Wayne Bishop on 10/14/14.
//  Copyright (c) 2014 Arbutus Software Inc. All rights reserved.
//
import Foundation


public class Trie {
    
    private var root: TrieNode!
    
    
    init(){
        root = TrieNode()
    }
    
    
    
    //builds a tree hierarchy of dictionary content
    func append(word keyword: String) {
        
        
        //trivial case
        guard keyword.length > 0 else {
            return
        }
        
        
        var current: TrieNode = root
        
        
        while keyword.length != current.level {
            
            var childToUse: TrieNode!
            let searchKey = keyword.substring(to: current.level + 1)
            
            
            //print("current has \(current.children.count) children..")
            
            
            //iterate through child nodes
            for child in current.children {
                
                if (child.key == searchKey) {
                    childToUse = child
                    break
                }
                
            }
            
            
            //new node
            if childToUse == nil {
                
                childToUse = TrieNode()
                childToUse.key = searchKey
                childToUse.level = current.level + 1
                current.children.append(childToUse)
            }
            
            
            current = childToUse
            
            
        } //end while
        
        
        //final end of word check
        if (keyword.length == current.level) {
            current.isFinal = true
            print("end of word reached!")
            return
        }
        
        
        
    } //end function
    
    

    
    //find words based on the prefix
    func search(forWord keyword: String) -> Array<String>! {
        
        
        //trivial case
        guard keyword.length > 0 else {
            return nil
        }
        
        
        var current: TrieNode = root
        var wordList = Array<String>()
        
        
        while keyword.length != current.level {
            
            var childToUse: TrieNode!
            let searchKey = keyword.substring(to: current.level + 1)
            

            //print("looking for prefix: \(searchKey)..")
            
            
            //iterate through any child nodes
            for child in current.children {
                
                if (child.key == searchKey) {
                    childToUse = child
                    current = childToUse
                    break
                }
                
            }
            
 
            if childToUse == nil {
               return nil
            }
            
            
        } //end while
        
        
        
        //retrieve the keyword and any descendants
        if ((current.key == keyword) && (current.isFinal)) {
            wordList.append(current.key)
        }

        
        //include only children that are words
        for child in current.children {
            
            if (child.isFinal == true) {
                wordList.append(child.key)
            }
            
        }
        
        
        return wordList

        
    } //end function
    

}

```

(GitHub, [source](https://github.com/waynewbishop/SwiftStructures/blob/master/Source/Factories/Trie.swift))

### **Stack**

In computer science, a stack is an abstract data type that serves as a collection of elements, with two principal operations: push, which adds an element to the collection, and pop, which removes the most recently added element that was not yet removed. The order in which elements come off a stack gives rise to its alternative name, LIFO (for last in, first out). Additionally, a peek operation may give access to the top without modifying the stack.
(Wikipedia, [source](https://en.wikipedia.org/wiki/Stack_(abstract_data_type)))

See license info below and original code source at ([github](https://github.com/waynewbishop/SwiftStructures/blob/master/Source/Factories/Stack.swift))

```swift
//
//  Stack.swift
//  SwiftStructures
//
//  Created by Wayne Bishop on 8/1/14.
//  Copyright (c) 2014 Arbutus Software Inc. All rights reserved.
//
import Foundation


class Stack<T> {
    
    private var top: Node<T>
    
    init() {
        top = Node<T>()
    }
    
    
    //the number of items - O(n)
    var count: Int {
        
        
        //return trivial case
        guard top.key != nil else {
          return 0
        }
                
        
        var current = top
        var x: Int = 1
        
        
        //cycle through list
        while current.next != nil {
            current = current.next!
            x += 1
        }
            
        return x        
        
    }
    
    
    //add item to the stack
    func push(withKey key: T) {
        
        
        //return trivial case
        guard top.key != nil else {
            top.key = key
            return
        }
        
        
        //create new item
        let childToUse = Node<T>()
        childToUse.key = key
            
            
        //set new created item at top
        childToUse.next = top
        top = childToUse        

    }
    

    //remove item from the stack
    func pop() {
        
        if self.count > 1 {
            top = top.next
        }
        else {
            top.key = nil
        }
        
    }
    
    
    //retrieve the top most item
    func peek() -> T! {

        
        //determine instance
        if let topitem = top.key {
            return topitem
        }
            
        else {
            return nil
        }
        
    }
    
    
    
    //check for value
    func isEmpty() -> Bool {
        
        if self.count == 0 {
            return true
        }
        
        else {
            return false
        }
        
    }
    

}

```

> 
The MIT License (MIT)
Copyright (c) 2015, Wayne Bishop & Arbutus Software Inc.
<p>Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:</p>
<p>The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.</p>
<p>THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.</p>


