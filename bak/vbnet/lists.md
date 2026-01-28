---
metaTitle: "Visual Basic .NET - Lists"
description: "Add items to a List, Create a List, Remove items from a List, Retrieve items from a List, Loop trough items in list, Check if item exists in a List"
---

# Lists



## Add items to a List


```vb
Dim aList as New List(Of Integer)
aList.Add(1)
aList.Add(10)
aList.Add(1001)

```

To add more than one item at a time use **AddRange**. Always adds to the end of the list

```vb
Dim blist as New List(of Integer)
blist.AddRange(alist) 


Dim aList as New List(of String)
alist.AddRange({"one", "two", "three"}) 

```

In order to add items to the middle of the list use **Insert**

**Insert** will place the item at the index, and renumber the remaining items

```vb
Dim aList as New List(Of String)
aList.Add("one")
aList.Add("three")
alist(0) = "one"
alist(1) = "three"
alist.Insert(1,"two")

```

New Output:

```vb
alist(0) = "one"       
alist(1) = "two"
alist(2) = "three"

```



## Create a List


Lists can populated with any data type as necessary, with the format

```vb
Dim aList as New List(Of Type)

```

For example:

Create a new, empty list of Strings

```vb
Dim aList As New List(Of String)

```

Create a new list of strings, and populate with some data

**VB.NET 2005/2008:**

```vb
Dim aList as New List(Of String)(New String() {"one", "two", "three"})

```

**VB.NET 2010:**

```vb
Dim aList as New List(Of String) From {"one", "two", "three"}

```

--

**VB.NET 2015:**

```vb
Dim aList as New List(Of String)(New String() {"one", "two", "three"})

```

**NOTE:**

If you are receiving the following when the code is ran:

> 
Object reference not set to an instance of an object.


Make sure you either declare as `New` i.e. `Dim aList as New List(Of String)` or if declaring without the `New`, make sure you set the list to a new list - `Dim aList as List(Of String) = New List(Of String)`



## Remove items from a List


```vb
Dim aList As New List(Of String)
aList.Add("Hello")
aList.Add("Delete Me!")
aList.Add("World")

'Remove the item from the list at index 1
aList.RemoveAt(1)

'Remove a range of items from a list, starting at index 0, for a count of 1)
'This will remove index 0, and 1!
aList.RemoveRange(0, 1)

'Clear the entire list
alist.Clear()

```



## Retrieve items from a List


```vb
Dim aList as New List(Of String)
aList.Add("Hello, World")
aList.Add("Test")

Dim output As String = aList(0)

```

`output`:

> 
Hello, World


If you do not know the index of the item or only know part of the string then use the **Find** or **FindAll** method

```vb
Dim aList as New List(Of String)
aList.Add("Hello, World")
aList.Add("Test")

Dim output As String = aList.Find(Function(x) x.StartWith("Hello"))

```

`output`:

> 
Hello, World


The **FindAll** method returns a new List (of String)

```vb
Dim aList as New List(Of String)
aList.Add("Hello, Test")
aList.Add("Hello, World")
aList.Add("Test")

Dim output As String = aList.FindAll(Function(x) x.Contains("Test"))

```

> 
output(0) = "Hello, Test"


> 
output(1) = "Test"




## Loop trough items in list


```vb
Dim aList as New List(Of String)
aList.Add("one")
aList.Add("two")
aList.Add("three")

For Each str As String in aList
    System.Console.WriteLine(str)
Next

```

Produces the following output:

```vb
one
two
three

```

Another option, would be to loop through using the index of each element:

```vb
Dim aList as New List(Of String)
aList.Add("one")
aList.Add("two")
aList.Add("three")

For i = 0 to aList.Count - 1 'We use "- 1" because a list uses 0 based indexing.
    System.Console.WriteLine(aList(i))
Next

```



## Check if item exists in a List


```

   Sub Main()
        Dim People = New List(Of String)({"Bob Barker", "Ricky Bobby", "Jeff Bridges"})
        Console.WriteLine(People.Contains("Rick James"))
        Console.WriteLine(People.Contains("Ricky Bobby"))
        Console.WriteLine(People.Contains("Barker"))
        Console.Read 
    End Sub

```

Produces the following output:

```vb
False
True
False

```



#### Syntax


- List.Add(item As Type)
- List.RemoveRange(index As Integer, count As Integer)
- List.Remove(index As Integer)
- List.AddRange(collection)
- List.Find(match as Predicate(of String))
- List.Insert(index as Integer , item as Type)
- List.Contains(item as Type)

