---
metaTitle: "VBA - Data Structures"
description: "Linked List, Binary Tree"
---

# Data Structures


[TODO: This topic should be an example of all the basic CS 101 data structures along with some explanation as an overview of how data structures can be implemented in VBA. This would be a good opportunity to tie in and reinforce concepts introduced in Class-related topics in VBA documentation.]



## Linked List


This linked list example implements [Set abstract data type](https://en.wikipedia.org/wiki/Set_(abstract_data_type)) operations.

**SinglyLinkedNode** class

```vb
Option Explicit

Private Value As Variant
Private NextNode As SinglyLinkedNode '"Next" is a keyword in VBA and therefore is not a valid variable name

```

**LinkedList** class

```vb
Option Explicit

Private head As SinglyLinkedNode

'Set type operations

Public Sub Add(value As Variant)
    Dim node As SinglyLinkedNode
        
    Set node = New SinglyLinkedNode
    node.value = value
    Set node.nextNode = head
    
    Set head = node
End Sub

Public Sub Remove(value As Variant)
    Dim node As SinglyLinkedNode
    Dim prev As SinglyLinkedNode
    
    Set node = head
    
    While Not node Is Nothing
        If node.value = value Then
            'remove node
            If node Is head Then
                Set head = node.nextNode
            Else
                Set prev.nextNode = node.nextNode
            End If
            Exit Sub
        End If
        Set prev = node
        Set node = node.nextNode
    Wend

End Sub

Public Function Exists(value As Variant) As Boolean
    Dim node As SinglyLinkedNode
    
    Set node = head
    While Not node Is Nothing
        If node.value = value Then
            Exists = True
            Exit Function
        End If
        Set node = node.nextNode
    Wend
End Function

Public Function Count() As Long
    Dim node As SinglyLinkedNode
    
    Set node = head
    
    While Not node Is Nothing
        Count = Count + 1
        Set node = node.nextNode
    Wend
    
End Function

```



## Binary Tree


This is an example of an unbalanced [binary search tree](https://en.wikipedia.org/wiki/Binary_search_tree). A binary tree is structured conceptually as a hierarchy of nodes descending downward from a common root, where each node has two children: left and right. For example, suppose the numbers 7, 5, 9, 3, 11, 6, 12, 14 and 15 were inserted into a BinaryTree. The structure would be as below. Note that this binary tree is not [balanced](https://en.wikipedia.org/wiki/Self-balancing_binary_search_tree), which can be a desirable characteristic for guaranteeing the performance of lookups - see [AVL trees](https://en.wikipedia.org/wiki/AVL_tree) for an example of a self-balancing binary search tree.

```

            7
            / \
           5   9
          / \   \
         3   6   11
                   \ 
                    12
                      \
                       14
                        \
                         15

```

**BinaryTreeNode** class

```vb
Option Explicit

Public left As BinaryTreeNode
Public right As BinaryTreeNode
Public key As Variant
Public value As Variant

```

**BinaryTree** class

[TODO]

