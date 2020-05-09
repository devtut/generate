---
metaTitle: "VBA - Creating a Custom Class"
description: "Adding a Property to a Class, Adding Functionality to a Class, Class module scope, instancing and re-use"
---

# Creating a Custom Class



## Adding a Property to a Class


A `Property` procedure is a series of statement that retrieves or modifies a custom property on a module.

There are three types of property accessors:

1. A `Get` procedure that returns the value of a property.
1. A `Let` procedure that assigns a (non-`Object`) value to an object.
1. A `Set` procedure that assigns an `Object` reference.

Property accessors are often defined in pairs, using both a `Get` and `Let`/`Set` for each property. A property with only a `Get` procedure would be read-only, while a property with only a `Let`/`Set` procedure would be write-only.

In the following example, four property accessors are defined for the `DateRange` class:

1. `StartDate` (**read/write**). Date value representing the earlier date in a range. Each procedure uses the value of the module variable, `mStartDate`.
1. `EndDate` (**read/write**). Date value representing the later date in a range. Each procedure uses the value of the module variable, `mEndDate`.
1. `DaysBetween` (**read-only**). Calculated Integer value representing the number of days between the two dates. Because there is only a `Get` procedure, this property cannot be modified directly.
1. `RangeToCopy` (**write-only**). A `Set` procedure used to copy the values of an existing `DateRange` object.

```vb
Private mStartDate As Date                ' Module variable to hold the starting date
Private mEndDate As Date                  ' Module variable to hold the ending date
  
' Return the current value of the starting date
Public Property Get StartDate() As Date
    StartDate = mStartDate
End Property

' Set the starting date value. Note that two methods have the name StartDate
Public Property Let StartDate(ByVal NewValue As Date)
    mStartDate = NewValue
End Property
  
' Same thing, but for the ending date
Public Property Get EndDate() As Date
    EndDate = mEndDate
End Property
  
Public Property Let EndDate(ByVal NewValue As Date)
    mEndDate = NewValue
End Property

' Read-only property that returns the number of days between the two dates
Public Property Get DaysBetween() As Integer
    DaysBetween = DateDiff("d", mStartDate, mEndDate)
End Function

' Write-only property that passes an object reference of a range to clone
Public Property Set RangeToCopy(ByRef ExistingRange As DateRange)

Me.StartDate = ExistingRange.StartDate
Me.EndDate = ExistingRange.EndDate

End Property

```



## Adding Functionality to a Class


Any public `Sub`, `Function`, or `Property` inside a class module can be called by preceding the call with an object reference:

```vb
Object.Procedure

```

In a `DateRange` class, a `Sub` could be used to add a number of days to the end date:

```vb
Public Sub AddDays(ByVal NoDays As Integer)
    mEndDate = mEndDate + NoDays
End Sub

```

A `Function` could return the last day of the next month-end (note that `GetFirstDayOfMonth` would not be visible outside the class because it is private):

```vb
Public Function GetNextMonthEndDate() As Date
    GetNextMonthEndDate = DateAdd("m", 1, GetFirstDayOfMonth())
End Function

Private Function GetFirstDayOfMonth() As Date
    GetFirstDayOfMonth = DateAdd("d", -DatePart("d", mEndDate), mEndDate)
End Function

```

Procedures can accept arguments of any type, including references to objects of the class being defined.

The following example tests whether the current `DateRange` object has a starting date and ending date that includes the starting and ending date of another `DateRange` object.

```vb
Public Function ContainsRange(ByRef TheRange As DateRange) As Boolean
    ContainsRange = TheRange.StartDate >= Me.StartDate And TheRange.EndDate <= Me.EndDate
End Function

```

Note the use of the `Me` notation as a way to access the value of the object running the code.



## Class module scope, instancing and re-use


By default, a new class module is a Private class, so it is **only** available for instantiation and use within the VBProject in which it is defined. You can declare, instantiate and use the class anywhere in the **same** project:

```vb
'Class List has Instancing set to Private
'In any other module in the SAME project, you can use:

Dim items As List
Set items = New List

```

But often you'll write classes that you'd like to use in other projects **without** copying the module between projects. If you define a class called `List` in `ProjectA`, and want to use that class in `ProjectB`, then you'll need to perform 4 actions:

<li>
Change the instancing property of the `List` class in `ProjectA` in the Properties window, from `Private` to `PublicNotCreatable`
</li>
<li>
Create a public "factory" function in `ProjectA` that creates and returns an instance of a `List` class. Typically the factory function would include arguments for the initialization of the class instance. The factory function is required because the class can be used by `ProjectB` but `ProjectB` cannot directly create an instance of `ProjectA`'s class.

```vb
 Public Function CreateList(ParamArray values() As Variant) As List
     Dim tempList As List
     Dim itemCounter As Long
     Set tempList = New List
     For itemCounter = LBound(values) to UBound(values) 
         tempList.Add values(itemCounter)
     Next itemCounter
     Set CreateList = tempList
 End Function

```


</li>
<li>
In `ProjectB` add a reference to `ProjectA` using the `Tools..References...` menu.
</li>
<li>
In `ProjectB`, declare a variable and assign it an instance of `List` using the factory function from `ProjectA`

```vb
 Dim items As ProjectA.List
 Set items = ProjectA.CreateList("foo","bar")

 'Use the items list methods and properties
 items.Add "fizz"
 Debug.Print items.ToString()
 'Destroy the items object
 Set items = Nothing

```


</li>



#### Remarks


This article will show how to create a complete custom class in VBA. It uses the example of a `DateRange` object, because a starting and ending date are often passed together to functions.

