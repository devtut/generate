# Linked lists


A linked list is a collection of nodes, each made up of a reference and a value. Nodes are strung together into a sequence using their references. Linked lists can be used to implement more complex data structures like lists, stacks, queues, and associative arrays.



## Single linked list example


This example implements a linked list with many of the same methods as that of the built-in list object.

```
class Node:
    def __init__(self, val):
        self.data = val
        self.next = None

    def getData(self):
        return self.data

    def getNext(self):
        return self.next

    def setData(self, val):
        self.data = val

    def setNext(self, val):
        self.next = val

class LinkedList:
    def __init__(self):
        self.head = None

    def isEmpty(self):
        &quot;&quot;&quot;Check if the list is empty&quot;&quot;&quot;
        return self.head is None

    def add(self, item):
        &quot;&quot;&quot;Add the item to the list&quot;&quot;&quot;
        new_node = Node(item)
        new_node.setNext(self.head)
        self.head = new_node

    def size(self):
        &quot;&quot;&quot;Return the length/size of the list&quot;&quot;&quot;
        count = 0
        current = self.head
        while current is not None:
            count += 1
            current = current.getNext()
        return count

    def search(self, item):
        &quot;&quot;&quot;Search for item in list. If found, return True. If not found, return False&quot;&quot;&quot;
        current = self.head
        found = False
        while current is not None and not found:
            if current.getData() is item:
                found = True
            else:
                current = current.getNext()
        return found

    def remove(self, item):
        &quot;&quot;&quot;Remove item from list. If item is not found in list, raise ValueError&quot;&quot;&quot;
        current = self.head
        previous = None
        found = False
        while current is not None and not found:
            if current.getData() is item:
                found = True
            else:
                previous = current
                current = current.getNext()
        if found:
            if previous is None:
                self.head = current.getNext()
            else:
                previous.setNext(current.getNext())
        else:
            raise ValueError
            print 'Value not found.'

    def insert(self, position, item):
        &quot;&quot;&quot;
        Insert item at position specified. If position specified is
        out of bounds, raise IndexError
        &quot;&quot;&quot;
        if position > self.size() - 1:
            raise IndexError
            print &quot;Index out of bounds.&quot;
        current = self.head
        previous = None
        pos = 0
        if position is 0:
            self.add(item)
        else:
            new_node = Node(item)
            while pos < position:
                pos += 1
                previous = current
                current = current.getNext()
            previous.setNext(new_node)
            new_node.setNext(current)

    def index(self, item):
        &quot;&quot;&quot;
        Return the index where item is found.
        If item is not found, return None.
        &quot;&quot;&quot;
        current = self.head
        pos = 0
        found = False
        while current is not None and not found:
            if current.getData() is item:
                found = True
            else:
                current = current.getNext()
                pos += 1
        if found:
            pass
        else:
            pos = None
        return pos

    def pop(self, position = None):
        &quot;&quot;&quot;
        If no argument is provided, return and remove the item at the head. 
        If position is provided, return and remove the item at that position.
        If index is out of bounds, raise IndexError
        &quot;&quot;&quot;
        if position > self.size():
            print 'Index out of bounds'
            raise IndexError
            
        current = self.head
        if position is None:
            ret = current.getData()
            self.head = current.getNext()
        else:
            pos = 0
            previous = None
            while pos < position:
                previous = current
                current = current.getNext()
                pos += 1
                ret = current.getData()
            previous.setNext(current.getNext())
        print ret
        return ret

    def append(self, item):
        &quot;&quot;&quot;Append item to the end of the list&quot;&quot;&quot;
        current = self.head
        previous = None
        pos = 0
        length = self.size()
        while pos < length:
            previous = current
            current = current.getNext()
            pos += 1
        new_node = Node(item)
        if previous is None:
            new_node.setNext(current)
            self.head = new_node
        else:
            previous.setNext(new_node)

    def printList(self):
        &quot;&quot;&quot;Print the list&quot;&quot;&quot;
        current = self.head
        while current is not None:
            print current.getData()
            current = current.getNext()

```

Usage functions much like that of the built-in list.

```
ll = LinkedList()
ll.add('l')
ll.add('H')
ll.insert(1,'e')
ll.append('l')
ll.append('o')
ll.printList()

H
e
l
l
o

```

