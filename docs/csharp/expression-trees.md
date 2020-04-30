---
metaTitle: "Expression Trees"
description: "Create Expression Trees with a lambda expression, Creating Expression Trees by Using the API, Compiling Expression Trees, Parsing Expression Trees, Expression Tree Basic, Examining the Structure of an Expression using Visitor, Understanding the expressions API"
---

# Expression Trees




## Create Expression Trees with a lambda expression


Following is most basic expression tree that is created by lambda.

```cs
Expression<Func<int, bool>> lambda = num => num == 42;

```

To create expression trees 'by hand', one should use `Expression` class.

Expression above would be equivalent to:

```cs
ParameterExpression parameter = Expression.Parameter(typeof(int), "num"); // num argument
ConstantExpression constant = Expression.Constant(42, typeof(int)); // 42 constant
BinaryExpression equality = Expression.Equals(parameter, constant); // equality of two expressions (num == 42)
Expression<Func<int, bool>> lambda = Expression.Lambda<Func<int, bool>>(equality, parameter);

```



## Creating Expression Trees by Using the API


```cs
using System.Linq.Expressions;

// Manually build the expression tree for 
// the lambda expression num => num < 5.
ParameterExpression numParam = Expression.Parameter(typeof(int), "num");
ConstantExpression five = Expression.Constant(5, typeof(int));
BinaryExpression numLessThanFive = Expression.LessThan(numParam, five);
Expression<Func<int, bool>> lambda1 =
    Expression.Lambda<Func<int, bool>>(
        numLessThanFive,
        new ParameterExpression[] { numParam });

```



## Compiling Expression Trees


```cs
// Define an expression tree, taking an integer, returning a bool.
Expression<Func<int, bool>> expr = num => num < 5;

// Call the Compile method on the expression tree to return a delegate that can be called.
Func<int, bool> result = expr.Compile();

// Invoke the delegate and write the result to the console.
Console.WriteLine(result(4)); // Prints true

// Prints True.

// You can also combine the compile step with the call/invoke step as below:
Console.WriteLine(expr.Compile()(4));

```



## Parsing Expression Trees


```cs
using System.Linq.Expressions;

// Create an expression tree.
Expression<Func<int, bool>> exprTree = num => num < 5;

// Decompose the expression tree.
ParameterExpression param = (ParameterExpression)exprTree.Parameters[0];
BinaryExpression operation = (BinaryExpression)exprTree.Body;
ParameterExpression left = (ParameterExpression)operation.Left;
ConstantExpression right = (ConstantExpression)operation.Right;

Console.WriteLine("Decomposed expression: {0} => {1} {2} {3}",
                  param.Name, left.Name, operation.NodeType, right.Value);

// Decomposed expression: num => num LessThan 5      

```



## Expression Tree Basic


Expression trees represent code in a tree-like data structure, where each node is an expression

Expression Trees enables dynamic modification of executable code, the execution of LINQ queries in various databases, and the creation of dynamic queries. You can compile and run code represented by expression trees.

These are also used in the dynamic language run-time (DLR) to provide interoperability between dynamic languages and the .NET Framework and to enable compiler writers to emit expression trees instead of Microsoft intermediate language (MSIL).

Expression Trees can be created Via

1. Anonymous lambda expression,
1. Manually by using the System.Linq.Expressions namespace.

**Expression Trees from Lambda Expressions**

When a lambda expression is assigned to Expression type variable , the compiler emits code to build an expression tree that represents the lambda expression.

The following code examples shows how to have the C# compiler create an expression tree that represents the lambda expression num => num < 5.

```cs
Expression<Func<int, bool>> lambda = num => num < 5;

```

**Expression Trees by Using the API**

Expression Trees also created using the **Expression** Class. This class contains static factory methods that create expression tree nodes of specific types.

Below are few type of Tree nodes.

1. ParameterExpression
1. MethodCallExpression

The following code example shows how to create an expression tree that represents the lambda expression num => num < 5 by using the API.

```cs
ParameterExpression numParam = Expression.Parameter(typeof(int), "num");
ConstantExpression five = Expression.Constant(5, typeof(int));
BinaryExpression numLessThanFive = Expression.LessThan(numParam, five);
Expression<Func<int, bool>> lambda1 = Expression.Lambda<Func<int, bool>>(numLessThanFive,new ParameterExpression[] { numParam });

```



## Examining the Structure of an Expression using Visitor


Define a new visitor class by overriding some of the methods of [ExpressionVisitor](https://msdn.microsoft.com/en-us/library/system.linq.expressions.expressionvisitor(v=vs.110).aspx):

```cs
class PrintingVisitor : ExpressionVisitor {
    protected override Expression VisitConstant(ConstantExpression node) {
        Console.WriteLine("Constant: {0}", node);
        return base.VisitConstant(node);
    }
    protected override Expression VisitParameter(ParameterExpression node) {
        Console.WriteLine("Parameter: {0}", node);
        return base.VisitParameter(node);
    }
    protected override Expression VisitBinary(BinaryExpression node) {
        Console.WriteLine("Binary with operator {0}", node.NodeType);
        return base.VisitBinary(node);
    }
}

```

Call `Visit` to use this visitor on an existing expression:

```cs
Expression<Func<int,bool>> isBig = a => a > 1000000;
var visitor = new PrintingVisitor();
visitor.Visit(isBig);

```



## Understanding the expressions API


We're going to use the expression tree API to create a `CalculateSalesTax` tree.  In plain English, here's a summary of the steps it takes to create the tree.

1. Check if the product is taxable
1. If it is, multiply the line total by the applicable tax rate and return that amount
1. Otherwise return 0

```cs
//For reference, we're using the API to build this lambda expression
    orderLine => orderLine.IsTaxable ? orderLine.Total * orderLine.Order.TaxRate : 0;

//The orderLine parameter we pass in to the method.  We specify it's type (OrderLine) and the name of the parameter.
    ParameterExpression orderLine = Expression.Parameter(typeof(OrderLine), "orderLine");

//Check if the parameter is taxable;  First we need to access the is taxable property, then check if it's true
    PropertyInfo isTaxableAccessor = typeof(OrderLine).GetProperty("IsTaxable");
    MemberExpression getIsTaxable = Expression.MakeMemberAccess(orderLine, isTaxableAccessor);
    UnaryExpression isLineTaxable = Expression.IsTrue(getIsTaxable);

//Before creating the if, we need to create the braches
    //If the line is taxable, we'll return the total times the tax rate; get the total and tax rate, then multiply
    //Get the total
    PropertyInfo totalAccessor = typeof(OrderLine).GetProperty("Total");
    MemberExpression getTotal = Expression.MakeMemberAccess(orderLine, totalAccessor);
    
    //Get the order
    PropertyInfo orderAccessor = typeof(OrderLine).GetProperty("Order");
    MemberExpression getOrder = Expression.MakeMemberAccess(orderLine, orderAccessor);
    
    //Get the tax rate - notice that we pass the getOrder expression directly to the member access
    PropertyInfo taxRateAccessor = typeof(Order).GetProperty("TaxRate");
    MemberExpression getTaxRate = Expression.MakeMemberAccess(getOrder, taxRateAccessor);
    
    //Multiply the two - notice we pass the two operand expressions directly to multiply
    BinaryExpression multiplyTotalByRate = Expression.Multiply(getTotal, getTaxRate);
    
//If the line is not taxable, we'll return a constant value - 0.0 (decimal)
    ConstantExpression zero = Expression.Constant(0M);

//Create the actual if check and branches
    ConditionalExpression ifTaxableTernary = Expression.Condition(isLineTaxable, multiplyTotalByRate, zero);
    
//Wrap the whole thing up in a "method" - a LambdaExpression
    Expression<Func<OrderLine, decimal>> method = Expression.Lambda<Func<OrderLine, decimal>>(ifTaxableTernary, orderLine);

```



#### Syntax


- Expression<TDelegate> name = lambdaExpression;



#### Parameters


|Parameter|Details
|------
|TDelegate|The delegate type to be used for the expression
|lambdaExpression|The lambda expression (ex. `num => num < 5`)



#### Remarks


### Intro to Expression Trees

### Where we came from

Expression trees are all about consuming "source code" at runtime.  Consider a method which calculates the sales tax due on a sales order `decimal CalculateTotalTaxDue(SalesOrder order)`.  Using that method in a .NET program is easy — you just call it `decimal taxDue = CalculateTotalTaxDue(order);`.  What if you want to apply it to all the results from a remote query (SQL, XML, a remote server, etc)?  Those remote query sources cannot call the method!  Traditionally, you would have to invert the flow in all these cases.  Make the entire query, store it in memory, then loop through the results and calculate tax for each result.

<a class="remarks-subsection-anchor" name="remarks-how-to-avoid-flow-inversion&#39;s-memory-and-latency-problems-2"></a>
<h3>How to avoid flow inversion's memory and latency problems</h3>

Expression trees are data structures in a format of a tree, where each node holds an expression. They are used to translate the compiled instructions (like methods used to filter data) in expressions which could be used outside of the program environment such as inside a database query.

The problem here is that a remote query **cannot access our method**.  We could avoid this problem if instead, we sent the **instructions** for the method to the remote query.  In our `CalculateTotalTaxDue` example, that means we send this information:

1. Create a variable to store the total tax
1. Loop through all the lines on the order
1. For each line, check if the product is taxable
1. If it is, multiply the line total by the applicable tax rate and add that amount to the total
1. Otherwise do nothing

With those instructions, the remote query can perform the work as it's creating the data.

There are two challenges to implementing this. How do you transform a compiled .NET method into a list of instructions, and how do you format the instructions in a way that they can be consumed by the remote system?

Without expression trees, you could only solve the first problem with MSIL.  (MSIL is the assembler-like code created by the .NET compiler.)  Parsing MSIL is **possible**, but it's not easy.  Even when you do parse it properly, it can be hard to determine what the original programmer's intent was with a particular routine.

<a class="remarks-subsection-anchor" name="remarks-expression-trees-save-the-day-3"></a>
<h3>Expression trees save the day</h3>
Expression trees address these exact issues.  They represent program instructions a tree data structure where each node represents **one instruction** and has references to all the information you need to execute that instruction.  For example, a `MethodCallExpression` has reference to 1) the `MethodInfo` it is going to call, 2) a list of `Expression`s it will pass to that method, 3) for instance methods, the `Expression` you'll call the method on.  You can "walk the tree" and apply the instructions on your remote query.

<a class="remarks-subsection-anchor" name="remarks-creating-expression-trees-4"></a>
<h3>Creating expression trees</h3>
The easiest way to create an expression tree is with a lambda expression.  These expressions look almost the same as normal C# methods.  It's important to realize this is **compiler magic**.  When you first create a lambda expression, the compiler checks what you assign it to.  If it's a `Delegate` type (including `Action` or `Func`), the compiler converts the lambda expression into a delegate.  If it's a `LambdaExpression` (or an `Expression<Action<T>>` or `Expression<Func<T>>` which are strongly typed `LambdaExpression`'s), the compiler transforms it into a `LambdaExpression`.  This is where the magic kicks in.  Behind the scenes, the compiler **uses the expression tree API** to transform your lambda expression into a `LambdaExpression`.

Lambda expressions cannot create every type of expression tree.  In those cases, you can use the Expressions API manually to create the tree you need to.  In the [Understanding the expressions API](//stackoverflow.com/documentation/c%23/75/expression-trees/19200/understanding-the-expressions-api) example, we create the `CalculateTotalSalesTax` expression using the API.

NOTE: The names get a bit confusing here.  A **lambda expression** (two words, lower case) refers to the block of code with a `=>` indicator.  It represents an anonymous method in C# and is converted into either a `Delegate` or `Expression`.  A **`LambdaExpression`** (one word, PascalCase) refers to the node type within the Expression API which represents a method you can execute.

### Expression Trees and LINQ

One of the most common uses of expression trees is with LINQ and database queries.  LINQ pairs an expression tree with a query provider to apply your instructions to the target remote query.  For example, the LINQ to Entity Framework query provider transforms an expression tree into SQL which is executed against the database directly.

Putting all the pieces together, you can see the real power behind LINQ.

1. Write a query using a lambda expression: `products.Where(x => x.Cost > 5)`
1. The compiler transforms that expression into an expression tree with the instructions "check if the Cost property of the parameter is greater than five".
1. The query provider parses the expression tree and produces a valid SQL query `SELECT * FROM products WHERE Cost > 5`
1. The ORM projects all the results into POCOs and you get a list of objects back

### Notes

- Expression trees are immutable. If you want to change an expression tree you need to create a new one, copy the existing one into the new one (to traverse an expression tree you can use the `ExpressionVisitor`) and make the wanted changes.

