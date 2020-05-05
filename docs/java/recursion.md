---
metaTitle: "Java - Recursion"
description: "Deep recursion is problematic in Java, The basic idea of recursion, Types of Recursion, Computing the Nth Fibonacci Number, Computing the Nth power of a number, Traversing a Tree data structure with recursion, StackOverflowError & recursion to loop, Computing the sum of integers from 1 to N, Reverse a string using Recursion"
---

# Recursion


Recursion occurs when a method calls itself. Such a method is called **recursive**. A recursive method may be more concise than an equivalent non-recursive approach. However, for deep recursion, sometimes an iterative solution can consume less of a thread's finite stack space.

This topic includes examples of recursion in Java.



## Deep recursion is problematic in Java


Consider the following naive method for adding two positive numbers using recursion:

```java
public static int add(int a, int b) {
    if (a == 0) {
        return b;
    } else {
        return add(a - 1, b + 1);  // TAIL CALL
    }
}

```

This is algorithmically correct, but it has a major problem.  If you call `add` with a large `a`, it will crash with a `StackOverflowError`, on any version of Java up to (at least) Java 9.

In a typical functional programming language (and many other languages) the compiler optimizes [tail recursion](http://stackoverflow.com/documentation/java/914/recursion/12829/types-of-recursion#t=201611060141063837492).  The compiler would notice that the call to `add` (at the tagged line) is a [tail call](https://en.wikipedia.org/wiki/Tail_call), and would effectively rewrite the recursion as a loop.  This transformation is called tail-call elimination.

However, current generation Java compilers do not perform tail call elimination.  (This is not a simple oversight.  There are substantial technical reasons for this; see below.)  Instead, each recursive call of `add` causes a new frame to be allocated on the thread's stack.  For example, if you call `add(1000, 1)`, it will take `1000` recursive calls to arrive at the answer `1001`.

The problem is that the size of Java thread stack is fixed when the thread is created.  (This includes the "main" thread in a single-threaded program.)  If too many stack frames are allocated the stack will overflow.  The JVM will detect this and throw a `StackOverflowError`.

One approach to dealing with this is to simply use a bigger stack.  There are JVM options that control the default size of a stack, and you can also specify the stack size as a `Thread` constructor parameter.  Unfortunately, this only "puts off" the stack overflow.  If you need to do a computation that requires an even larger stack, then the `StackOverflowError` comes back.

The real solution is to identify recursive algorithms where deep recursion is likely, and **manually** perform the tail-call optimization at the source code level.   For example, our `add` method can be rewritten as follows:

```java
public static int add(int a, int b) {
    while (a != 0) {
       a = a - 1;
       b = b + 1;
    }
    return b;
}

```

(Obviously, there are better ways to add two integers.  The above is simply to illustrate the effect of manual tail-call elimination.)

### Why tail-call elimination is not implemented in Java (yet)

There are a number of reasons why adding tail call elimination to Java is not easy.  For example:

- Some code could rely on `StackOverflowError` to (for example) place a bound on the size of a computational problem.
- Sandbox security managers often rely on analyzing the call stack when deciding whether to allow non-privileged code to perform a privileged action.

As John Rose explains in ["Tail calls in the VM"](https://blogs.oracle.com/jrose/entry/tail_calls_in_the_vm):

> 
**"The effects of removing the caller’s stack frame are visible to some APIs, notably access control checks and stack tracing. It is as if the caller’s caller had directly called the callee. Any privileges possessed by the caller are discarded after control is transferred to the callee. However, the linkage and accessibility of the callee method are computed before the transfer of control, and take into account the tail-calling caller."**


In other words, tail-call elimination could cause an access control method to mistakenly think that a security sensitive API was was being called by trusted code.



## The basic idea of recursion


**What is recursion:**

In general, recursion is when a function invokes itself, either directly or indirectly.  For example:

```java
// This method calls itself "infinitely"
public void useless() {
    useless();  // method calls itself (directly)
}

```

**Conditions for applying recursion to a problem:**

There are two preconditions for using recursive functions to solving a specific problem:

<li>
There must be a base condition for the problem, which will be the endpoint for the recursion.  When a recursive function reaches the base condition, it makes no further (deeper) recursive calls.
</li>
<li>
Each level of recursion should be attempting a smaller problem.  The recursive function thus divides the problem into smaller and smaller parts.  Assuming that the problem is finite, this will ensure that the recursion terminates.
</li>

In Java there is a third precondition: it should not be necessary to recurse too deeply to solve the problem; see [Deep recursion is problematic in Java](http://stackoverflow.com/documentation/java/914/recursion/15048/deep-recursion-is-problematic-in-java)

**Example**

The following function calculates factorials using recursion. Notice how the method `factorial` calls itself within the function. Each time it calls itself, it reduces the parameter `n` by 1. When `n` reaches 1 (the base condition) the function will recurse no deeper.

```java
public int factorial(int n) {
    if (n <= 1) { // the base condition 
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}

```

<sup>This is not a practical way of computing factorials in Java, since it does not take account of integer overflow, or call stack overflow (i.e. `StackOverflowError` exceptions) for large values of `n`.</sup>



## Types of Recursion


Recursion can be categorized as either **Head Recursion** or **Tail Recursion**, depending on where the recursive method call is placed.

In **head recursion**, the recursive call, when it happens, comes before other processing in the function (think of it happening at the top, or head, of the function).

In **tail recursion**, it’s the opposite—the processing occurs before the recursive call. Choosing between the two recursive styles may seem arbitrary, but the choice can make all the difference.

A function with a path with a single recursive call at the beginning of the path uses what is called head recursion. The factorial function of a previous exhibit uses head recursion. The first thing it does once it determines that recursion is needed is to call itself with the decremented parameter. A function with a single recursive call at the end of a path is using tail recursion.

```java
public void tail(int n)              public void head(int n)
{                                       {
   if(n == 1)                             if(n == 0)
      return;                                return;
   else                                   else
      System.out.println(n);                 head(n-1);

   tail(n-1);                              System.out.println(n);
}                                        }

```

If the recursive call occurs at the end of a method, it is called a `tail recursion`. The tail recursion is `similar to a loop`. The `method executes all the statements before jumping into the next recursive call`.

If the recursive call occurs at the `beginning of a method, it is called a head recursion`. The `method saves the state before jumping into the next recursive call`.

**Reference:** [The difference between head & tail recursion](http://stackoverflow.com/questions/21426688/the-difference-between-head-tail-recursion)



## Computing the Nth Fibonacci Number


The following method computes the Nth Fibonacci number using recursion.

```java
public int fib(final int n) {
    if (n > 2) {
        return fib(n - 2) + fib(n - 1);
    }
    return 1;
}

```

The method implements a base case (n <= 2) and a recursive case (n>2). This illustrates the use of recursion to compute a recursive relation.

However, while this example is illustrative, it is also inefficient: each single instance of the method will call the function itself twice, leading to an exponential growth in the number of times the function is called as N increases. The above function is O(2<sup>N</sup>), but an equivalent iterative solution has complexity O(N).  In addition, there is a "closed form" expression that can be evaluated in O(N) floating-point multiplications.



## Computing the Nth power of a number


The following method computes the value of `num` raised to the power of `exp` using recursion:

```java
public long power(final int num, final int exp) {
    if (exp == 0) {
        return 1;
    }
    if (exp == 1) {
        return num;
    }
    return num * power(num, exp - 1);
}

```

This illustrates the principles mentioned above: the recursive method implements a base case (two cases, n = 0 and n = 1) that terminates the recursion, and a recursive case that calls the method again. This method is O(N) and can be reduced to a simple loop using tail-call optimization.



## Traversing a Tree data structure with recursion


Consider the Node class having 3 members data, left child pointer and right child pointer like below.

```java
public class Node {
    public int data;
    public Node left;
    public Node right;
    
    public Node(int data){
        this.data = data;
    }
}

```

We can traverse the tree constructed by connecting multiple Node class's object like below, the traversal is called in-order traversal of tree.

```java
public static void inOrderTraversal(Node root) {
        if (root != null) {          
            inOrderTraversal(root.left); // traverse left sub tree
            System.out.print(root.data + " "); // traverse current node
            inOrderTraversal(root.right); // traverse right sub tree
        }
    }

```

As demonstrated above, using **recursion** we can traverse the **tree data structure** without using any other data structure which is not possible with the **iterative** approach.



## StackOverflowError & recursion to loop


If a recursive call goes "too deep", this results in a `StackOverflowError`. Java allocates a new frame for every method call on its thread's stack. However, the space of each thread's stack is limited. Too many frames on the stack leads to the Stack Overflow (SO).

### Example

```java
public static void recursion(int depth) {
    if (depth > 0) {
        recursion(depth-1);
    }
}

```

Calling this method with large parameters (e.g. `recursion(50000)` probably will result in a stack overflow. The exact value depends on the thread stack size, which in turn depends on the thread construction, command-line parameters such as `-Xss`, or the default size for the JVM.

### Workaround

A recursion can be converted to a loop by storing the data for each recursive call in a data structure. This data structure can be stored on the heap rather than on the thread stack.

In general the data required to restore the state of a method invocation can be stored in a stack and a while loop can be used to "simulate" the recursive calls. Data that may be required include:

- the object the method was called for (instance methods only)
- the method parameters
- local variables
- the current position in the execution or the method

### Example

The following class allows recursive of a tree structure printing up to a specified depth.

```java
public class Node {

    public int data;
    public Node left;
    public Node right;

    public Node(int data) {
        this(data, null, null);
    }

    public Node(int data, Node left, Node right) {
        this.data = data;
        this.left = left;
        this.right = right;
    }

    public void print(final int maxDepth) {
        if (maxDepth <= 0) {
            System.out.print("(...)");
        } else {
            System.out.print("(");
            if (left != null) {
                left.print(maxDepth-1);
            }
            System.out.print(data);
            if (right != null) {
                right.print(maxDepth-1);
            }
            System.out.print(")");
        }
    }

}

```

e.g.

```java
Node n = new Node(10, new Node(20, new Node(50), new Node(1)), new Node(30, new Node(42), null));
n.print(2);
System.out.println();

```

Prints

```java
(((...)20(...))10((...)30))

```

This could be converted to the following loop:

```java
public class Frame {

    public final Node node;

    // 0: before printing anything
    // 1: before printing data
    // 2: before printing ")"
    public int state = 0;
    public final int maxDepth;

    public Frame(Node node, int maxDepth) {
        this.node = node;
        this.maxDepth = maxDepth;
    }

}

```

```java
List<Frame> stack = new ArrayList<>();
stack.add(new Frame(n, 2)); // first frame = initial call

while (!stack.isEmpty()) {
    // get topmost stack element
    int index = stack.size() - 1;
    Frame frame = stack.get(index); // get topmost frame
    if (frame.maxDepth <= 0) {
        // termial case (too deep)
        System.out.print("(...)");
        stack.remove(index); // drop frame
    } else {
        switch (frame.state) {
            case 0:
                frame.state++;

                // do everything done before the first recursive call
                System.out.print("(");
                if (frame.node.left != null) {
                    // add new frame (recursive call to left and stop)
                    stack.add(new Frame(frame.node.left, frame.maxDepth - 1));
                    break;
                }
            case 1:
                frame.state++;

                // do everything done before the second recursive call
                System.out.print(frame.node.data);
                if (frame.node.right != null) {
                    // add new frame (recursive call to right and stop)
                    stack.add(new Frame(frame.node.right, frame.maxDepth - 1));
                    break;
                }
            case 2:
                // do everything after the second recursive call & drop frame
                System.out.print(")");
                stack.remove(index);
        }
    }
}
System.out.println();

```

**Note:** This is just an example of the general approach. Often you can come up with a much better way to represent a frame and/or store the frame data.



## Computing the sum of integers from 1 to N


The following method computes the sum of integers from 0 to N using recursion.

```java
public int sum(final int n) {
    if (n > 0) {
        return n + sum(n - 1);
    } else {
        return n;
    }
}

```

This method is O(N) and can be reduced to a simple loop using tail-call optimization.  In fact there is a **closed form** expression that computes the sum in `O(1)` operations.



## Reverse a string using Recursion


Below is a recursive code to reverse a string

```java
/**
 * Just a snippet to explain the idea of recursion
 *
 **/

public class Reverse {
    public static void main (String args[]) {
        String string = "hello world";
        System.out.println(reverse(string)); //prints dlrow olleh
    }

    public static String reverse(String s) {
        if (s.length() == 1) {
            return s;
        }
    
        return reverse(s.substring(1)) + s.charAt(0);
    }
}

```



#### Remarks


### Designing a Recursive Method

When designing a recursive method keep in mind that you need:

<li>
**Base Case.** This will define when your recursion will stop and output the result. The base case in the factorial example is:

```java
if (n <= 1) {
    return 1;
}

```


</li>
<li>
**Recursive Call.** In this statement you re-call the method with a changed parameter. The recursive call in the factorial example above is:

```java
else {
    return n * factorial(n - 1);
}

```


</li>

### Output

In this example you compute the n-th factorial number.
The first factorials are:

> 
0! = 1
1! = 1
2! = 1 x 2 = 2
3! = 1 x 2 x 3 = 6
4! = 1 x 2 x 3 x 4 = 24
...


### Java and Tail-call elimination

Current Java compilers (up to and including Java 9) do not perform tail-call elimination. This can impact the performance of recursive algorithms, and if the recursion is deep enough, it can lead to `StackOverflowError` crashes; see [Deep recursion is problematic in Java](http://stackoverflow.com/documentation/java/914/recursion/15048/deep-recursion-is-problematic-in-java#t=201611080249331156811)

