---
metaTitle: "Structural Design Patterns"
description: "Adapter Design Pattern"
---

# Structural Design Patterns


Structural design patterns are patterns that describe how objects and classes can be combined and form a large structure and that ease design by identifying a simple way to realize relationships between entities. There are seven structural patterns described. They are as follows: Adapter, Bridge, Composite, Decorator, Facade, Flyweight and Proxy



## Adapter Design Pattern


> 
<p>[**“Adapter”**](https://en.wikipedia.org/wiki/Adapter_pattern) as the name suggests is the object which lets two mutually
incompatible interfaces communicate with each other.</p>


> 
<p>**For example:** if you buy a Iphone 8 (or any other Apple product) you need alot of
adapters. Because the default interface does not support audio jac or
USB. With these adapters you can use earphones with wires or you can use a normal Ethernet cable. So **"two mutually incompatible interfaces communicate with each other"**.</p>


> 
<p>**So in technical terms this means:** Convert the interface of a
class into another interface that a clients expect. Adapter let
classes work together that couldn't otherwise because of incompatible
interfaces. The classes and objects participating in this pattern
are:</p>


**The adapter pattern exits out 4 elements**

> 
<ol>
- **ITarget:** This is the interface which is used by the client to achieve functionality.
- **Adaptee:** This is the functionality which the client desires but its interface is not compatible with the client.
- **Client:** This is the class which wants to achieve some functionality by using the adaptee’s code.
- **Adapter:** This is the class which would implement ITarget and would call the Adaptee code which the client wants to call.
</ol>


**UML**

[<img src="https://i.stack.imgur.com/oYMFy.gif" alt="enter image description here" />](https://i.stack.imgur.com/oYMFy.gif)

**First code Example (Theoretical example)**.

```cs
public interface ITarget
{
    void MethodA();
}

public class Adaptee
{
    public void MethodB()
    {
        Console.WriteLine("MethodB() is called");
    }
}

public class Client
{
    private ITarget target;

    public Client(ITarget target)
    {
        this.target = target;
    }

    public void MakeRequest()
    {
        target.MethodA();
    }
}  

public class Adapter : Adaptee, ITarget
{
    public void MethodA()
    {
        MethodB();
    }
}

```

**Second code example (Real world imlementation)**

```cs
/// <summary>
///  Interface: This is the interface which is used by the client to achieve functionality.
/// </summary>
public interface ITarget
{
    List<string> GetEmployeeList();
}

/// <summary>
/// Adaptee: This is the functionality which the client desires but its interface is not compatible with the client.
/// </summary>
public class CompanyEmplyees
{
    public string[][] GetEmployees()
    {
        string[][] employees = new string[4][];

        employees[0] = new string[] { "100", "Deepak", "Team Leader" };
        employees[1] = new string[] { "101", "Rohit", "Developer" };
        employees[2] = new string[] { "102", "Gautam", "Developer" };
        employees[3] = new string[] { "103", "Dev", "Tester" };

        return employees;
    }
}

/// <summary>
/// Client: This is the class which wants to achieve some functionality by using the adaptee’s code (list of employees).
/// </summary>
public class ThirdPartyBillingSystem
{
    /* 
     * This class is from a thirt party and you do'n have any control over it. 
     * But it requires a Emplyee list to do its work
     */

    private ITarget employeeSource;

    public ThirdPartyBillingSystem(ITarget employeeSource)
    {
        this.employeeSource = employeeSource;
    }

    public void ShowEmployeeList()
    {
        // call the clietn list in the interface
        List<string> employee = employeeSource.GetEmployeeList();

        Console.WriteLine("######### Employee List ##########");
        foreach (var item in employee)
        {
            Console.Write(item);
        }

    }
}

/// <summary>
/// Adapter: This is the class which would implement ITarget and would call the Adaptee code which the client wants to call.
/// </summary>
public class EmployeeAdapter : CompanyEmplyees, ITarget
{
    public List<string> GetEmployeeList()
    {
        List<string> employeeList = new List<string>();
        string[][] employees = GetEmployees();
        foreach (string[] employee in employees)
        {
            employeeList.Add(employee[0]);
            employeeList.Add(",");
            employeeList.Add(employee[1]);
            employeeList.Add(",");
            employeeList.Add(employee[2]);
            employeeList.Add("\n");
        }

        return employeeList;
    }
}

/// 
/// Demo
/// 
class Programs
{
    static void Main(string[] args)
    {
        ITarget Itarget = new EmployeeAdapter();
        ThirdPartyBillingSystem client = new ThirdPartyBillingSystem(Itarget);
        client.ShowEmployeeList();
        Console.ReadKey();
    }
}

```

**When to use**

<li>Allow a system to use classes of another system that is incompatible
with it.</li>
<li>Allow communication between new and already existing system which are
independent to each other</li>
<li>Ado.Net SqlAdapter, OracleAdapter, MySqlAdapter are best example of
Adapter Pattern.</li>

