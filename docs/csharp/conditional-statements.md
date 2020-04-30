---
metaTitle: "Conditional Statements"
description: "If-Else Statement, If-Else If-Else Statement, If statement conditions are standard boolean expressions and values, Switch statements"
---

# Conditional Statements




## If-Else Statement


Programming in general often requires a `decision` or a `branch` within the code to account for how the code operates under different inputs or conditions. Within the C# programming language (and most programming languages for this matter), the simplest and sometimes the most useful way of creating a branch within your program is through an `If-Else` statement.

Lets assume we have method (a.k.a. a function) which takes an int parameter which will represent a score up to 100, and the method will print out a message saying whether we pass or fail.

```cs
static void PrintPassOrFail(int score)
{
    if (score >= 50) // If score is greater or equal to 50
    {
        Console.WriteLine("Pass!");
    }
    else // If score is not greater or equal to 50
    {
        Console.WriteLine("Fail!");
    }
}

```

When looking at this method, you may notice this line of code (`score >= 50`) inside the `If` statement. This can be seen as a `boolean` condition, where if the condition is evaluated to equal `true`, then the code that is in between the `if` `{ }` is ran.

For example, if this method was called like this:
`PrintPassOrFail(60);`, the output of the method would be a Console Print saying ****Pass!**** since the parameter value of 60 is greater or equal to 50.

However, if the method was called like: `PrintPassOrFail(30);`, the output of the method would print out saying ****Fail!****. This is because the value 30 is not greater or equal to 50, thus the code in between the `else` `{ }` is ran instead of the `If` statement.

In this example, we've said that **score** should go up to 100, which hasn't been accounted for at all. To account for **score** not going past 100 or possibly dropping below 0, see the **If-Else If-Else Statement** example.



## If-Else If-Else Statement


Following on from the **If-Else Statement** example, it is now time to introduce the `Else If` statement. The `Else If` statement follows directly after the `If` statement in the **If-Else If-Else** structure, but intrinsically has has a similar syntax as the `If` statement. It is used to add more branches to the code than what a simple **If-Else** statement can.

In the example from **If-Else Statement**, the example specified that the score goes up to 100; however there were never any checks against this. To fix this, lets modify the method from **If-Else Statement** to look like this:

```cs
static void PrintPassOrFail(int score)
{
    if (score > 100) // If score is greater than 100
    {
        Console.WriteLine("Error: score is greater than 100!");
    }
    else if (score < 0) // Else If score is less than 0
    {
        Console.WriteLine("Error: score is less than 0!");
    }
    else if (score >= 50) // Else if score is greater or equal to 50
    {
        Console.WriteLine("Pass!");
    }
    else // If none above, then score must be between 0 and 49
    {
        Console.WriteLine("Fail!");
    }
}

```

All these statements will run in order from the top all the way to the bottom until a condition has been met. In this new update of the method, we've added two new branches to now accommodate for the score going **out of bounds**.

For example, if we now called the method in our code as `PrintPassOFail(110);`, the output would be a Console Print saying ****Error: score is greater than 100!****; and if we called the method in our code like `PrintPassOrFail(-20);`, the output would say ****Error: score is less than 0!****.



## If statement conditions are standard boolean expressions and values


The following statement

```cs
if (conditionA && conditionB && conditionC) //...

```

is exactly equivalent to

```cs
bool conditions = conditionA && conditionB && conditionC;
if (conditions) // ...

```

in other words, the conditions inside the "if" statement just form an ordinary Boolean expression.

A common mistake when writing conditional statements is to explicitly compare to `true` and `false`:

```cs
if (conditionA == true && conditionB == false && conditionC == true) // ...

```

This can be rewritten as

```cs
if (conditionA && !conditionB && conditionC)

```



## Switch statements


A switch statement allows a variable to be tested for equality against a list of values. Each value is called a case, and the variable being switched on is checked for each switch case.

A `switch` statement is often more concise and understandable than `if...else if... else..` statements when testing multiple possible values for a single variable.

Syntax is as follows

```cs
switch(expression) {
   case constant-expression:
      statement(s);
      break;
   case constant-expression:
      statement(s);
      break;
  
   // you can have any number of case statements
   default : // Optional
      statement(s);
      break;
}

```

there are sevaral things that have to consider while using the switch statement

- The expression used in a switch statement must have an integral or enumerated type, or be of a class type in which the class has a single conversion function to an integral or enumerated type.
- You can have any number of case statements within a switch. Each case is followed by the value to be compared to and a colon. The values to compare to have to be unique within each switch statement.
- A switch statement can have an optional default case. The default case can be used for performing a task when none of the cases is true.
- Each case has to end with a `break` statement unless it is an empty statement. In that case execution will continue at the case below it. The break statement can also be omitted when a `return`, `throw` or `goto case` statement is used.

Example can be given with the grades wise

```cs
char grade = 'B';

switch (grade)
{
    case 'A':
        Console.WriteLine("Excellent!");
        break;
    case 'B':
    case 'C':
        Console.WriteLine("Well done");
        break;
    case 'D':
        Console.WriteLine("You passed");
        break;
    case 'F':
        Console.WriteLine("Better try again");
        break;
    default:
        Console.WriteLine("Invalid grade");
        break;
}

```

