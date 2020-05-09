---
metaTitle: "VBA - Object-Oriented VBA"
description: "Abstraction, Encapsulation, Polymorphism"
---

# Object-Oriented VBA




## Abstraction


### Abstraction levels help determine when to split things up.

Abstraction is achieved by implementing functionality with increasingly detailed code. The entry point of a macro should be a small procedure with a **high abstraction level** that makes it easy to grasp at a glance what's going on:

```vb
Public Sub DoSomething()
    With New SomeForm
        Set .Model = CreateViewModel
        .Show vbModal            
        If .IsCancelled Then Exit Sub
        ProcessUserData .Model
    End With
End Sub

```

The `DoSomething` procedure has a high **abstraction level**: we can tell that it's displaying a form and creating some model, and passing that object to some `ProcessUserData` procedure that knows what to do with it - how the model is created is the job of another procedure:

```vb
Private Function CreateViewModel() As ISomeModel
    Dim result As ISomeModel
    Set result = SomeModel.Create(Now, Environ$("UserName"))
    result.AvailableItems = GetAvailableItems
    Set CreateViewModel = result
End Function

```

The `CreateViewModel` function is only responsible for creating some `ISomeModel` instance. Part of that responsibility is to acquire an array of **available items** - how these items are acquired is an implementation detail that's abstracted behind the `GetAvailableItems` procedure:

```vb
Private Function GetAvailableItems() As Variant
    GetAvailableItems = DataSheet.Names("AvailableItems").RefersToRange
End Function

```

Here the procedure is reading the available values from a named range on a `DataSheet` worksheet. It could just as well be reading them from a database, or the values could be hard-coded: it's an **implementation detail** that's none of a concern for any of the higher abstraction levels.



## Encapsulation


### Encapsulation hides implementation details from client code.

The [Handling QueryClose](http://stackoverflow.com/documentation/vba/5351/user-forms/19037/handling-queryclose#t=201608091819108848528) example demonstrates encapsulation: the form has a checkbox control, but its client code doesn't work with it directly - the checkbox is an **implementation detail**, what the client code needs to know is whether the setting is enabled or not.

When the checkbox value changes, the handler assigns a private field member:

> 

```vb
Private Type TView
    IsCancelled As Boolean
    SomeOtherSetting As Boolean
    'other properties skipped for brievety
End Type
Private this As TView

'...

Private Sub SomeOtherSettingInput_Change()
    this.SomeOtherSetting = CBool(SomeOtherSettingInput.Value)
End Sub

```




And when the client code wants to read that value, it doesn't need to worry about a checkbox - instead it simply uses the `SomeOtherSetting` property:

> 

```vb
Public Property Get SomeOtherSetting() As Boolean
    SomeOtherSetting = this.SomeOtherSetting
End Property

```




The `SomeOtherSetting` property **encapsulates** the checkbox' state; client code doesn't need to know that there's a checkbox involved, only that there's a setting with a Boolean value. By **encapsulating** the `Boolean` value, we've added an **abstraction layer** around the checkbox.

### Using interfaces to enforce immutability

Let's push that a step further by **encapsulating** the form's **model** in a dedicated class module. But if we made a `Public Property` for the `UserName` and `Timestamp`, we would have to expose `Property Let` accessors, making the properties mutable, and we don't want the client code to have the ability to change these values after they're set.

The `CreateViewModel` function in the **Abstraction** example returns an `ISomeModel` class: that's our **interface**, and it looks something like this:

```vb
Option Explicit

Public Property Get Timestamp() As Date
End Property

Public Property Get UserName() As String
End Property

Public Property Get AvailableItems() As Variant
End Property

Public Property Let AvailableItems(ByRef value As Variant)
End Property

Public Property Get SomeSetting() As String
End Property

Public Property Let SomeSetting(ByVal value As String)
End Property

Public Property Get SomeOtherSetting() As Boolean
End Property

Public Property Let SomeOtherSetting(ByVal value As Boolean)
End Property

```

Notice `Timestamp` and `UserName` properties only expose a `Property Get` accessor. Now the `SomeModel` class can implement that interface:

```vb
Option Explicit
Implements ISomeModel

Private Type TModel
    Timestamp As Date
    UserName As String
    SomeSetting As String
    SomeOtherSetting As Boolean
    AvailableItems As Variant
End Type
Private this As TModel

Private Property Get ISomeModel_Timestamp() As Date
    ISomeModel_Timestamp = this.Timestamp
End Property

Private Property Get ISomeModel_UserName() As String
    ISomeModel_UserName = this.UserName
End Property

Private Property Get ISomeModel_AvailableItems() As Variant
    ISomeModel_AvailableItems = this.AvailableItems
End Property

Private Property Let ISomeModel_AvailableItems(ByRef value As Variant)
    this.AvailableItems = value
End Property

Private Property Get ISomeModel_SomeSetting() As String
    ISomeModel_SomeSetting = this.SomeSetting
End Property

Private Property Let ISomeModel_SomeSetting(ByVal value As String)
    this.SomeSetting = value
End Property

Private Property Get ISomeModel_SomeOtherSetting() As Boolean
    ISomeModel_SomeOtherSetting = this.SomeOtherSetting
End Property

Private Property Let ISomeModel_SomeOtherSetting(ByVal value As Boolean)
    this.SomeOtherSetting = value
End Property

Public Property Get Timestamp() As Date
    Timestamp = this.Timestamp
End Property

Public Property Let Timestamp(ByVal value As Date)
    this.Timestamp = value
End Property

Public Property Get UserName() As String
    UserName = this.UserName
End Property

Public Property Let UserName(ByVal value As String)
    this.UserName = value
End Property

Public Property Get AvailableItems() As Variant
    AvailableItems = this.AvailableItems
End Property

Public Property Let AvailableItems(ByRef value As Variant)
    this.AvailableItems = value
End Property

Public Property Get SomeSetting() As String
    SomeSetting = this.SomeSetting
End Property

Public Property Let SomeSetting(ByVal value As String)
    this.SomeSetting = value
End Property

Public Property Get SomeOtherSetting() As Boolean
    SomeOtherSetting = this.SomeOtherSetting
End Property

Public Property Let SomeOtherSetting(ByVal value As Boolean)
    this.SomeOtherSetting = value
End Property

```

The interface members are all `Private`, and all members of the interface must be implemented for the code to compile. The `Public` members are not part of the interface, and are therefore not exposed to code written against the `ISomeModel` interface.

### Using a **Factory Method** to simulate a constructor

Using a [VB_PredeclaredId](http://stackoverflow.com/documentation/vba/5321/attributes/18932/vb-predeclaredid#t=201608091940164849835) attribute, we can make the `SomeModel` class have a **default instance**, and write a function that works like a type-level (`Shared` in VB.NET, `static` in C#) member that the client code can call without needing to first create an instance, like we did here:

> 

```vb
Private Function CreateViewModel() As ISomeModel
    Dim result As ISomeModel
    Set result = SomeModel.Create(Now, Environ$("UserName"))
    result.AvailableItems = GetAvailableItems
    Set CreateViewModel = result
End Function

```




This **factory method** assigns the property values that are read-only when accessed from the `ISomeModel` interface, here `Timestamp` and `UserName`:

```vb
Public Function Create(ByVal pTimeStamp As Date, ByVal pUserName As String) As ISomeModel
    With New SomeModel
        .Timestamp = pTimeStamp
        .UserName = pUserName
        Set Create = .Self
    End With
End Function

Public Property Get Self() As ISomeModel
    Set Self = Me
End Property

```

And now we can code against the `ISomeModel` interface, which exposes `Timestamp` and `UserName` as read-only properties that can never be reassigned (as long as the code is written against the interface).



## Polymorphism


### Polymorphism is the ability to present the same interface for different underlying implementations.

The ability to implement interfaces allows completely decoupling the application logic from the UI, or from the database, or from this or that worksheet.

Say you have an `ISomeView` interface that the form itself implements:

```vb
Option Explicit

Public Property Get IsCancelled() As Boolean
End Property

Public Property Get Model() As ISomeModel
End Property

Public Property Set Model(ByVal value As ISomeModel)
End Property

Public Sub Show()
End Sub

```

The form's code-behind could look like this:

```vb
Option Explicit 
Implements ISomeView

Private Type TView
    IsCancelled As Boolean
    Model As ISomeModel
End Type
Private this As TView

Private Property Get ISomeView_IsCancelled() As Boolean
    ISomeView_IsCancelled = this.IsCancelled
End Property

Private Property Get ISomeView_Model() As ISomeModel
    Set ISomeView_Model = this.Model
End Property

Private Property Set ISomeView_Model(ByVal value As ISomeModel)
    Set this.Model = value
End Property

Private Sub ISomeView_Show()
    Me.Show vbModal
End Sub

Private Sub SomeOtherSettingInput_Change()
    this.Model.SomeOtherSetting = CBool(SomeOtherSettingInput.Value)
End Sub

'...other event handlers...

Private Sub OkButton_Click()
    Me.Hide
End Sub

Private Sub CancelButton_Click()
    this.IsCancelled = True
    Me.Hide
End Sub

Private Sub UserForm_QueryClose(Cancel As Integer, CloseMode As Integer)
    If CloseMode = VbQueryClose.vbFormControlMenu Then
        Cancel = True
        this.IsCancelled = True
        Me.Hide
    End If
End Sub

```

But then, nothing forbids creating another class module that implements the `ISomeView` interface **without being a user form** - this could be a `SomeViewMock` class:

```vb
Option Explicit
Implements ISomeView

Private Type TView
    IsCancelled As Boolean
    Model As ISomeModel
End Type
Private this As TView

Public Property Get IsCancelled() As Boolean
    IsCancelled = this.IsCancelled
End Property

Public Property Let IsCancelled(ByVal value As Boolean)
    this.IsCancelled = value
End Property

Private Property Get ISomeView_IsCancelled() As Boolean
    ISomeView_IsCancelled = this.IsCancelled
End Property

Private Property Get ISomeView_Model() As ISomeModel
    Set ISomeView_Model = this.Model
End Property

Private Property Set ISomeView_Model(ByVal value As ISomeModel)
    Set this.Model = value
End Property

Private Sub ISomeView_Show()
    'do nothing
End Sub

```

And now we can change the code that works with a `UserForm` and make it work off the `ISomeView` interface, e.g. by giving it the form as a parameter instead of instantiating it:

```vb
Public Sub DoSomething(ByVal view As ISomeView)
    With view
        Set .Model = CreateViewModel
        .Show
        If .IsCancelled Then Exit Sub
        ProcessUserData .Model
    End With
End Sub

```

Because the `DoSomething` method depends on an interface (i.e. an **abstraction**) and not a **concrete class** (e.g. a specific `UserForm`), we can write an automated unit test that ensures that `ProcessUserData` isn't executed when `view.IsCancelled` is `True`, by making our test create a `SomeViewMock` instance, setting its `IsCancelled` property to `True`, and passing it to `DoSomething`.

### Testable code depends on abstractions

Writing unit tests in VBA can be done, there are add-ins out there that even integrate it into the IDE. But when code is **tightly coupled** with a worksheet, a database, a form, or the file system, then the unit test starts requiring an actual worksheet, database, form, or file system - and these **dependencies** are new out-of-control failure points that testable code should isolate, so that unit tests **don't** require an actual worksheet, database, form, or file system.

By writing code against interfaces, in a way that allows test code to **inject** stub/mock implementations (like the above `SomeViewMock` example), you can write tests in a  "controlled environment", and simulate what happens when every single one of the 42 possible permutations of user interactions on the form's data, without even once displaying a form and manually clicking on a form control.

