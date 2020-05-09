---
metaTitle: "Visual Basic .NET - Unit Testing in VB.NET"
description: "Unit Testing for Tax Calculation, Testing Employee Class assigned and derived Properties"
---

# Unit Testing in VB.NET



## Unit Testing for Tax Calculation


This example is divided into two pillars

- **SalaryCalculation Class** : Calculating the net salary after tax deduction
- **SalaryCalculationTests Class** : For testing the method that calculates the net salary

**Step 1:** Create Class Library, name it **WagesLibrary** or any appropriate name. Then rename the class to **SalaryCalculation**

''' 
''' Class for Salary Calculations
''' 
Public Class SalaryCalculation

```

   ''' <summary>
    ''' Employee Salary
    ''' </summary>
    Public Shared Salary As Double

    ''' <summary>
    ''' Tax fraction (0-1)
    ''' </summary>
    Public Shared Tax As Double

    ''' <summary>
    ''' Function to calculate Net Salary
    ''' </summary>
    ''' <returns></returns>
    Public Shared Function CalculateNetSalary()
        Return Salary - Salary * Tax
    End Function
End Class

```

**Step 2** : Create Unit Test Project. Add reference to the created class library and paste the below code

```vb
Imports WagesLibrary 'Class library you want to test

''' <summary>
''' Test class for testing SalaryCalculation
''' </summary>
<TestClass()> Public Class SalaryCalculationTests

    ''' <summary>
    ''' Test case for the method CalculateNetSalary
    ''' </summary>
    <TestMethod()> Public Sub CalculateNetSalaryTest()
        SalaryCalculation.Salary = 100
        SalaryCalculation.Tax = 0.1
        Assert.AreEqual(90.0, SalaryCalculation.CalculateNetSalary(), 0.1)
    End Sub
End Class

```

`Assert.Equal` checks the expected value against the actual calculated value. the value `0.1` is used to allow tolerance or variation between **expected** and **actual** result.

Step 3 : Run the test of the method to see result
[<img src="http://i.stack.imgur.com/CEHyj.png" alt="enter image description here" />](http://i.stack.imgur.com/CEHyj.png)

**Test result**
[<img src="http://i.stack.imgur.com/RRbXy.png" alt="enter image description here" />](http://i.stack.imgur.com/RRbXy.png)



## Testing Employee Class assigned and derived Properties


This example has more tests available in unit testing.

**Employee.vb** (Class Library)

```vb
''' <summary>
''' Employee Class
''' </summary>
Public Class Employee

    ''' <summary>
    ''' First name of employee
    ''' </summary>
    Public Property FirstName As String = ""

    ''' <summary>
    ''' Last name of employee
    ''' </summary>
    Public Property LastName As String = ""

    ''' <summary>
    ''' Full name of employee
    ''' </summary>
    Public ReadOnly Property FullName As String = ""

    ''' <summary>
    ''' Employee's age
    ''' </summary>
    Public Property Age As Byte

    ''' <summary>
    ''' Instantiate new instance of employee
    ''' </summary>
    ''' <param name="firstName">Employee first name</param>
    ''' <param name="lastName">Employee last name</param>
    Public Sub New(firstName As String, lastName As String, dateofbirth As Date)
        Me.FirstName = firstName
        Me.LastName = lastName
        FullName = Me.FirstName + " " + Me.LastName
        Age = Convert.ToByte(Date.Now.Year - dateofbirth.Year)
    End Sub
End Class

```

**EmployeeTest.vb** (Test Project)

```vb
Imports HumanResources

<TestClass()>
Public Class EmployeeTests
    ReadOnly _person1 As New Employee("Waleed", "El-Badry", New DateTime(1980, 8, 22))
    ReadOnly _person2 As New Employee("Waleed", "El-Badry", New DateTime(1980, 8, 22))

    <TestMethod>
    Public Sub TestFirstName()
        Assert.AreEqual("Waleed", _person1.FirstName, "First Name Mismatch")
    End Sub

    <TestMethod>
    Public Sub TestLastName()
        Assert.AreNotEqual("", _person1.LastName, "No Last Name Inserted!")
    End Sub

    <TestMethod>
    Public Sub TestFullName()
        Assert.AreEqual("Waleed El-Badry", _person1.FullName, "Error in concatination of names")
    End Sub

    <TestMethod>
    Public Sub TestAge()
        Assert.Fail("Age is not even tested !") 'Force test to fail !
        Assert.AreEqual(Convert.ToByte(36), _person1.Age)
    End Sub

    <TestMethod>
    Public Sub TestObjectReference()
        Assert.AreSame(_person1.FullName, _person2.FullName, "Different objects with same data")
    End Sub
End Class

```

**Result after running tests**

[<img src="http://i.stack.imgur.com/hhJ92.png" alt="enter image description here" />](http://i.stack.imgur.com/hhJ92.png)

[<img src="http://i.stack.imgur.com/oohnv.png" alt="enter image description here" />](http://i.stack.imgur.com/oohnv.png)



#### Remarks


This is the simplest yet descriptive example for the unit testing procedure. Feel free to add more methods to check against different data types.

