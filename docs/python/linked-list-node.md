# Linked List Node



## Write a simple Linked List Node in python


A linked list is either:

- the empty list, represented by None, or
- a node that contains a cargo object and a reference to a linked list.

```
#! /usr/bin/env python

class Node: 
      def __init__(self, cargo=None, next=None): 
          self.car = cargo 
          self.cdr = next    
      def __str__(self): 
          return str(self.car)


      def display(lst):
          if lst:
             w("%s " % lst)
             display(lst.cdr)
          else:
             w("nil\n")

```

