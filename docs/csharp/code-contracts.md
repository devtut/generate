---
metaTitle: "C# | Code Contracts"
description: "Postconditions, Invariants, Defining Contracts on Interface, Preconditions"
---

# Code Contracts



## Postconditions


```cs
public double GetPaymentsTotal(string name)
{     
    Contract.Ensures(Contract.Result<double>() >= 0);
 
    double total = 0.0;
 
    foreach (var payment in this._payments) {
        if (string.Equals(payment.Name, name)) {
            total += payment.Amount;
        }
    }
 
    return total;
}

```



## Invariants


```cs
namespace CodeContractsDemo
{
    using System;
    using System.Diagnostics.Contracts;
 
    public class Point
    {
        public int X { get; set; }
        public int Y { get; set; }
 
        public Point()
        {
        }
 
        public Point(int x, int y)
        {
            this.X = x;
            this.Y = y;
        }
 
        public void Set(int x, int y)
        {
            this.X = x;
            this.Y = y;
        }
 
        public void Test(int x, int y)
        {
            for (int dx = -x; dx <= x; dx++) {
                this.X = dx;
                Console.WriteLine("Current X = {0}", this.X);
            }
 
            for (int dy = -y; dy <= y; dy++) {
                this.Y = dy;
                Console.WriteLine("Current Y = {0}", this.Y);
            }
 
            Console.WriteLine("X = {0}", this.X);
            Console.WriteLine("Y = {0}", this.Y);
        }
 
        [ContractInvariantMethod]
        private void ValidateCoordinates()
        {
            Contract.Invariant(this.X >= 0);
            Contract.Invariant(this.Y >= 0);
        }
    }
}

```



## Defining Contracts on Interface


```cs
[ContractClass(typeof(ValidationContract))]
interface IValidation
{
    string CustomerID{get;set;}
    string Password{get;set;}
}
 
[ContractClassFor(typeof(IValidation))]
sealed class ValidationContract:IValidation
{
    string IValidation.CustomerID
    {
        [Pure]
        get
        {
            return Contract.Result<string>();
        }
        set
        {
            Contract.Requires<ArgumentNullException>(!string.IsNullOrEmpty(value), "Customer ID cannot be null!!");
        }
    }
 
    string IValidation.Password
    {
        [Pure]
        get
        {
            return Contract.Result<string>();
        }
        set
        {
            Contract.Requires<ArgumentNullException>(!string.IsNullOrEmpty(value), "Password cannot be null!!");
        }
    }
}
 
class Validation:IValidation
{
    public string GetCustomerPassword(string customerID)
    {
        Contract.Requires(!string.IsNullOrEmpty(customerID),"Customer ID cannot be Null");
        Contract.Requires<ArgumentNullException>(!string.IsNullOrEmpty(customerID), "Exception!!");
        Contract.Ensures(Contract.Result<string>() != null);
        string password="AAA@1234";
        if (customerID!=null)
        {
            return password;    
        }
        else
        {
            return null;
        }
         
    }
 
    private string m_custID, m_PWD;
 
    public string CustomerID
    {
        get
        {
            return m_custID;
        }
        set
        {
            m_custID = value;
        }
    }
 
    public string Password
    {
        get
        {
            return m_PWD;
        }
        set
        {
            m_PWD = value;
        }
    }
}

```

In the above code, we have defined an interface called `IValidation` with an attribute `[ContractClass]`. This attribute takes an address of a class where we have implemented a contract for an Interface. The class `ValidationContract` makes use of properties defined in the interface and checks for the null values using `Contract.Requires<T>`. `T` is an exception class.

We have also marked the get accessor with an attribute `[Pure]`. The pure attribute ensures that the method or a property does not change the instance state of a class in which `IValidation` interface is implemented.



## Preconditions


```cs
namespace CodeContractsDemo
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics.Contracts;
 
    public class PaymentProcessor
    {
        private List<Payment> _payments = new List<Payment>();
 
        public void Add(Payment payment)
        {
            Contract.Requires(payment != null);
            Contract.Requires(!string.IsNullOrEmpty(payment.Name));
            Contract.Requires(payment.Date <= DateTime.Now);
            Contract.Requires(payment.Amount > 0);
 
            this._payments.Add(payment);
        }
    }
}

```



#### Syntax


<li>
Contract.Requires(Condition,userMessage)
Contract.Requires(Condition,userMessage)
Contract.Result<T>
Contract.Ensures()
Contract.Invariants()
</li>



#### Remarks


.NET supports the Design by Contract idea via its Contracts class found in the System.Diagnostics namespace and introduced in .NET 4.0. Code Contracts API includes classes for static and runtime checks of code and allows you to define preconditions, postconditions, and invariants within a method. The preconditions specify the conditions the parameters must fulfill before a method can execute, postconditions that are verified upon completion of a method, and the invariants define the conditions that do not change during the execution of a method.

**Why are Code Contracts needed?**

Tracking issues of an application when your application is running, is one the foremost concerns of all the developers and administrators. Tracking can be performed in many ways. For example -

<li>
You can apply tracing on our application and get the details of an application when the application is running
</li>
<li>
You can use event logging mechanism when you are running the application. The messages can be seen using Event Viewer
</li>
<li>
You can apply Performance Monitoring after a specific time interval and write live data from your application.
</li>

Code Contracts uses a different approach for tracking and managing issues within an application. Instead of validating everything that is returned from a method call, Code Contracts with the help of preconditions, postconditions, and invariants on methods, ensure that everything entering and leaving your methods are correct.

