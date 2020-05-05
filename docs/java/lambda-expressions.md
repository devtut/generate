---
metaTitle: "Lambda Expressions"
description: "Introduction to Java lambdas, Using Lambda Expressions to Sort a Collection, Method References, Implementing multiple interfaces, Lambda - Listener Example, Java Closures with lambda expressions., Using lambda expression with your own functional interface, Traditional style to Lambda style, Lambdas and memory utilization, Lambdas and Execute-around Pattern, `return` only returns from the lambda, not the outer method, Using lambda expressions & predicates to get a certain value(s) from a list"
---

# Lambda Expressions


Lambda expressions provide a clear and concise way of implementing a single-method interface using an expression. They allow you to reduce the amount of code you have to create and maintain. While similar to anonymous classes, they have no type information by themselves. Type inference needs to happen.

Method references implement functional interfaces using existing methods rather than expressions. They belong to the lambda family as well.



## Introduction to Java lambdas


### Functional Interfaces

Lambdas can only operate on a functional interface, which is an interface with just one abstract method. Functional interfaces can have any number of [`default`](http://stackoverflow.com/documentation/java/113/default-methods) or `static` methods. (For this reason, they are sometimes referred to as Single Abstract Method Interfaces, or SAM Interfaces).

```java
interface Foo1 {
    void bar();
}

interface Foo2 {
    int bar(boolean baz);
}

interface Foo3 {
    String bar(Object baz, int mink);
}

interface Foo4 {
    default String bar() { // default so not counted
        return "baz";
    }
    void quux();
}

```

When declaring a functional interface the [`@FunctionalInterface`](https://docs.oracle.com/javase/8/docs/api/java/lang/FunctionalInterface.html) annotation can be added. This has no special effect, but a compiler error will be generated if this annotation is applied to an interface which is not functional, thus acting as a reminder that the interface should not be changed.

```java
@FunctionalInterface
interface Foo5 {
    void bar();
}

@FunctionalInterface
interface BlankFoo1 extends Foo3 { // inherits abstract method from Foo3
}

@FunctionalInterface
interface Foo6 {
    void bar();
    boolean equals(Object obj); // overrides one of Object's method so not counted
}

```

Conversely, this is **not** a functional interface, as it has more than **one abstract** method:

```java
interface BadFoo {
    void bar();
    void quux(); // <-- Second method prevents lambda: which one should 
                 // be considered as lambda?
}

```

This is **also not** a functional interface, as it does not have any methods:

```java
interface BlankFoo2 { }

```

Take note of the following.  Suppose you have

`interface Parent { public int parentMethod(); }`

and

`interface Child extends Parent { public int ChildMethod(); }`

Then `Child` **cannot** be a functional interface since it has two
specified methods.

Java 8 also provides a number of generic templated functional interfaces in the package [`java.util.function`](https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html). For example, the built-in interface `Predicate<T>` wraps a single method which inputs a value of type `T` and returns a `boolean`.

### Lambda Expressions

The basic structure of a Lambda expression is:

[<img src="https://i.stack.imgur.com/RRcfc.png" alt="FunctionalInterface fi = () -> System.out.println("Hello");" />](http://stackoverflow.com/documentation/java/113/default-methods)

`fi` will then hold a singleton instance of a class, similar to an anonymous class, which implements `FunctionalInterface` and where the one method's definition is `{ System.out.println("Hello"); }`. In other words, the above is mostly equivalent to:

```java
FunctionalInterface fi = new FunctionalInterface() {
    @Override
    public void theOneMethod() {
        System.out.println("Hello");
    }
};

```

The lambda is only "mostly equivalent" to the anonymous class because in a lambda, the meaning of expressions like `this`, `super` or `toString()` reference the class within which the assignment takes place, not the newly created object.

You cannot specify the name of the method when using a lambda—but you shouldn't need to, because a functional interface must have only one abstract method, so Java overrides that one.

In cases where the type of the lambda is not certain, (e.g. overloaded methods) you can add a cast to the lambda to tell the compiler what its type should be, like so:

```java
Object fooHolder = (Foo1) () -> System.out.println("Hello");
System.out.println(fooHolder instanceof Foo1); // returns true

```

If the functional interface's single method takes parameters, the local formal names of these should appear between the brackets of the lambda. There is no need to declare the type of the parameter or return as these are taken from the interface (although it is not an error to declare the parameter types if you want to). Thus, these two examples are equivalent:

```java
Foo2 longFoo = new Foo2() {
    @Override
    public int bar(boolean baz) {
        return baz ? 1 : 0;
    }
};
Foo2 shortFoo = (x) -> { return x ? 1 : 0; };

```

The parentheses around the argument can be omitted if the function only has one argument:

```java
Foo2 np = x -> { return x ? 1 : 0; }; // okay
Foo3 np2 = x, y -> x.toString() + y // not okay

```

### Implicit Returns

If the code placed inside a lambda is a Java **expression** rather than a **statement**, it is treated as a method which returns the value of the expression. Thus, the following two are equivalent:

```java
IntUnaryOperator addOneShort = (x) -> (x + 1);
IntUnaryOperator addOneLong = (x) -> { return (x + 1); };

```

### Accessing Local Variables (value closures)

Since lambdas are syntactic shorthand for anonymous classes, they follow the same rules for accessing local variables in the enclosing scope; the variables must be treated as `final` and not modified inside the lambda.

```java
IntUnaryOperator makeAdder(int amount) {
    return (x) -> (x + amount); // Legal even though amount will go out of scope
                                // because amount is not modified
}

IntUnaryOperator makeAccumulator(int value) {
    return (x) -> { value += x; return value; }; // Will not compile
}

```

If it is necessary to wrap a changing variable in this way, a regular object that keeps a copy of the variable should be used. Read more in [Java Closures with lambda expressions.](http://stackoverflow.com/documentation/java/91/lambda-expressions/14441/java-closures-with-lambda-expressions)

### Accepting Lambdas

Because a lambda is an implementation of an interface, nothing special needs to be done to make a method accept a lambda: any function which takes a functional interface can also accept a lambda.

```java
public void passMeALambda(Foo1 f) {
    f.bar();
}
passMeALambda(() -> System.out.println("Lambda called"));

```

### The Type of a Lambda Expression

A lambda expression, by itself, does not have a specific type. While it is true that the types and number of parameters, along with the type of a return value can convey some type information, such information will only constrain what types it can be assigned to. The lambda receives a type when it is assigned to a functional interface type in one of the following ways:

- Direct assignment to a functional type, e.g. `myPredicate = s -> s.isEmpty()`
- Passing it as a parameter that has a functional type, e.g. `stream.filter(s -> s.isEmpty())`
- Returning it from a function that returns a functional type, e.g. `return s -> s.isEmpty()`
- Casting it to a functional type, e.g. `(Predicate<String>) s -> s.isEmpty()`

Until any such assignment to a functional type is made, the lambda does not have a definite type.  To illustrate, consider the lambda expression `o -> o.isEmpty()`. The same lambda expression can be assigned to many different functional types:

```java
Predicate<String> javaStringPred = o -> o.isEmpty();
Function<String, Boolean> javaFunc = o -> o.isEmpty();
Predicate<List> javaListPred = o -> o.isEmpty();
Consumer<String> javaStringConsumer = o -> o.isEmpty(); // return value is ignored!
com.google.common.base.Predicate<String> guavaPredicate = o -> o.isEmpty();

```

Now that they are assigned, the examples shown are of completely different types even though the lambda expressions looked the same, and they cannot be assigned to each other.



## Using Lambda Expressions to Sort a Collection


### Sorting lists

Prior to Java 8, it was necessary to implement the [`java.util.Comparator`](https://docs.oracle.com/javase/8/docs/api/java/util/Comparator.html) interface with an anonymous (or named) class when sorting a list<sup>1</sup>:

```java
List<Person> people = ...
Collections.sort(
    people,
    new Comparator<Person>() {
        public int compare(Person p1, Person p2){
            return p1.getFirstName().compareTo(p2.getFirstName());
        }
    }
);

```

Starting with Java 8, the anonymous class can be replaced with a lambda expression. Note that the types for the parameters `p1` and `p2` can be left out, as the compiler will infer them automatically:

```java
Collections.sort(
    people, 
    (p1, p2) -> p1.getFirstName().compareTo(p2.getFirstName())
);

```

The example can be simplified by using [`Comparator.comparing`](https://docs.oracle.com/javase/8/docs/api/java/util/Comparator.html#comparing-java.util.function.Function-) and [method references](http://stackoverflow.com/documentation/java/91/lambda-expressions/5080/method-references) expressed using the `::`  (double colon) symbol.

```java
Collections.sort(
    people,
    Comparator.comparing(Person::getFirstName)
);

```

A static import allows us to express this more concisely, but it is debatable whether this improves overall readability:

```java
import static java.util.Collections.sort;
import static java.util.Comparator.comparing;
//...
sort(people, comparing(Person::getFirstName));

```

Comparators built this way can also be chained together. For example, after comparing people by their first name, if there are people with the same first name, the `thenComparing` method with also compare by last name:

```java
sort(people, comparing(Person::getFirstName).thenComparing(Person::getLastName));

```

<sup>1 - Note that Collections.sort(...) only works on collections that are subtypes of `List`.  The `Set` and `Collection` APIs do not imply any ordering of the elements.</sup>

### Sorting maps

You can sort the entries of a `HashMap` by value in a similar fashion. (Note that a `LinkedHashMap` must be used as the target.  The keys in an ordinary `HashMap` are unordered.)

```java
Map<String, Integer> map = new HashMap();  // ... or any other Map class
// populate the map
map = map.entrySet()
    .stream()
    .sorted(Map.Entry.<String, Integer>comparingByValue())
    .collect(Collectors.toMap(k -> k.getKey(), v -> v.getValue(),
                              (k, v) -> k, LinkedHashMap::new));

```



## Method References


Method references allow predefined static or instance methods that adhere to a compatible functional interface to be passed as arguments instead of an anonymous lambda expression.

Assume that we have a model:

```java
class Person {
    private final String name;
    private final String surname;

    public Person(String name, String surname){
        this.name = name;
        this.surname = surname;
    }

    public String getName(){ return name; }
    public String getSurname(){ return surname; }
}

List<Person> people = getSomePeople();

```

### Instance method reference (to an arbitrary instance)

```java
people.stream().map(Person::getName)

```

The equivalent lambda:

```java
people.stream().map(person -> person.getName())

```

In this example, a method reference to the instance method `getName()` of type `Person`, is being passed.  Since it's known to be of the collection type, the method on the instance (known later) will be invoked.

### Instance method reference (to a specific instance)

```java
people.forEach(System.out::println);

```

Since `System.out` is an instance of `PrintStream`, a method reference to this specific instance is being passed as an argument.

The equivalent lambda:

```java
people.forEach(person -> System.out.println(person));

```

### Static method reference

Also for transforming streams we can apply references to static methods:

```java
List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5, 6);
numbers.stream().map(String::valueOf)

```

This example passes a reference to the static `valueOf()` method on the `String` type.  Therefore, the instance object in the collection is passed as an argument to `valueOf()`.

The equivalent lambda:

```

numbers.stream().map(num -> String.valueOf(num))

```

### Reference to a constructor

```java
List<String> strings = Arrays.asList("1", "2", "3");
strings.stream().map(Integer::new)

```

Read [Collect Elements of a Stream into a Collection](http://stackoverflow.com/documentation/java/88/streams/384/collect-elements-of-a-stream-into-a-collection#t=20160916125357953683) to see how to collect elements to collection.

The single String argument constructor of the `Integer` type is being used here, to construct an integer given the string provided as the argument. In this case, as long as the string represents a number, the stream will be mapped to Integers.
The equivalent lambda:

```java
strings.stream().map(s -> new Integer(s));

```

### Cheat-Sheet

|Method Reference Format|Code|Equivalent Lambda
|---|---|---|---|---|---|---|---|---|---
|Static method|`TypeName::method`|`(args) -> TypeName.method(args)`
|Non-static method (on instance<sup>*</sup>)|`instance::method`|`(args) -> instance.method(args)`
|Non-static method (no instance)|`TypeName::method`|`(instance, args) -> instance.method(args)`
|Constructor<sup>**</sup>|`TypeName::new`|`(args) -> new TypeName(args)`
|Array constructor|`TypeName[]::new`|`(int size) -> new TypeName[size]`

<sup>*</sup> `instance` can be any expression that evaluates to a reference to an instance, e.g. `getInstance()::method`, `this::method`

<sup>**</sup> If `TypeName` is a non-static inner class, constructor reference is only valid within the scope of an outer class instance



## Implementing multiple interfaces


Sometimes you may want to have a lambda expression implementing more than one interface. This is mostly useful with marker interfaces (such as [java.io.Serializable](https://docs.oracle.com/javase/8/docs/api/java/io/Serializable.html)) since they don't add abstract methods.

For example, you want to create a [`TreeSet`](https://docs.oracle.com/javase/7/docs/api/java/util/TreeSet.html) with a custom `Comparator` and then serialize it and send it over the network. The trivial approach:

```java
TreeSet<Long> ts = new TreeSet<>((x, y) -> Long.compare(y, x));

```

doesn't work since the lambda for the comparator does not implement `Serializable`. You can fix this by using intersection types and explicitly specifying that this lambda needs to be serializable:

```java
TreeSet<Long> ts = new TreeSet<>(
    (Comparator<Long> & Serializable) (x, y) -> Long.compare(y, x));

```

If you're frequently using intersection types (for example, if you're using a framework such as [Apache Spark](http://spark.apache.org/) where almost everything has to be serializable), you can create empty interfaces and use them in your code instead:

```java
public interface SerializableComparator extends Comparator<Long>, Serializable {}

public class CustomTreeSet {
  public CustomTreeSet(SerializableComparator comparator) {}
}

```

This way you're guaranteed that the passed comparator will be serializable.



## Lambda - Listener Example


**Anonymous class listener**

Before Java 8, it’s very common that an anonymous class is used to handle click event of a JButton, as shown in the following code. This example shows how to implement an anonymous listener within the scope of `btn.addActionListener`.

```java
JButton btn = new JButton("My Button");
btn.addActionListener(new ActionListener() {
    @Override
    public void actionPerformed(ActionEvent e) {
        System.out.println("Button was pressed");
    }
});

```

**Lambda listener**

Because the `ActionListener` interface defines only one method `actionPerformed()`, it is a functional interface which means there’s a place to use Lambda expressions to replace the boilerplate code. The above example can be re-written using Lambda expressions as follows:

```java
JButton btn = new JButton("My Button");
btn.addActionListener(e -> {
    System.out.println("Button was pressed");
});

```



## Java Closures with lambda expressions.


A lambda closure is created when a lambda expression references the variables of an enclosing scope (global or local). The rules for doing this are the same as those for inline methods and anonymous classes.

**Local variables** from an enclosing scope that are used within a lambda have to be `final`. With Java 8 (the earliest version that supports lambdas), they don't need to be **declared** `final` in the outside context, but must be treated that way.  For example:

```java
int n = 0; // With Java 8 there is no need to explicit final
Runnable r = () -> { // Using lambda
    int i = n;
    // do something
};

```

This is legal as long as the value of the `n` variable is not changed.  If you try to change the variable, inside or outside the lambda, you will get the following compilation error:

> 
"local variables referenced from a lambda expression must be **final** or **effectively final**".


For example:

```java
int n = 0;
Runnable r = () -> { // Using lambda
    int i = n;
    // do something
};
n++; // Will generate an error.

```

If it is necessary to use a changing variable within a lambda, the normal approach is to declare a `final` copy of the variable and use the copy.  For example

```java
int n = 0;
final int k = n; // With Java 8 there is no need to explicit final
Runnable r = () -> { // Using lambda
    int i = k;
    // do something
};
n++;      // Now will not generate an error
r.run();  // Will run with i = 0 because k was 0 when the lambda was created

```

Naturally, the body of the lambda does not see the changes to the original variable.

Note that Java does not support true closures. A Java lambda cannot be created in a way that allows it to see changes in the environment in which it was instantiated. If you want to implement a closure that observes or makes changes to its environment, you should simulate it using a regular class. For example:

```java
// Does not compile ...
public IntUnaryOperator createAccumulator() {
    int value = 0;
    IntUnaryOperator accumulate = (x) -> { value += x; return value; };
    return accumulate;
}

```

The above example will not compile for reasons discussed previously. We can work around the compilation error as follows:

```java
// Compiles, but is incorrect ...
public class AccumulatorGenerator {
    private int value = 0;

    public IntUnaryOperator createAccumulator() {
        IntUnaryOperator accumulate = (x) -> { value += x; return value; };
        return accumulate;
    }
}

```

The problem is that this breaks the design contract for the `IntUnaryOperator` interface which states that instances should be functional and stateless.  If such a closure is passed to built-in functions that accept functional objects, it is liable to cause crashes or erroneous behavior.  Closures that encapsulate mutable state should be implemented as regular classes.  For example.

```java
// Correct ...
public class Accumulator {
   private int value = 0;

   public int accumulate(int x) {
      value += x;
      return value;
   }
}

```



## Using lambda expression with your own functional interface


Lambdas are meant to provide inline implementation code for single method interfaces and the ability to pass them around as we have been doing with normal variables. We call them Functional Interface.

For example, writing a Runnable in anonymous class and starting a Thread looks like:

```java
//Old way
new Thread(
        new Runnable(){
            public void run(){
                System.out.println("run logic...");
            }
        }
).start();

//lambdas, from Java 8
new Thread(
        ()-> System.out.println("run logic...")
).start();

```

Now, in line with above, lets say you have some custom interface:

```java
interface TwoArgInterface {
    int operate(int a, int b);
}

```

How do you use lambda to give implementation of this interface in your code? Same as Runnable example shown above. See the driver program below:

```java
public class CustomLambda {
    public static void main(String[] args) {

        TwoArgInterface plusOperation = (a, b) -> a + b;
        TwoArgInterface divideOperation = (a,b)->{
            if (b==0) throw new IllegalArgumentException("Divisor can not be 0");
            return a/b;
        };

        System.out.println("Plus operation of 3 and 5 is: " + plusOperation.operate(3, 5));
        System.out.println("Divide operation 50 by 25 is: " + divideOperation.operate(50, 25));

    }
}

```



## Traditional style to Lambda style


**Traditional way**

```java
interface MathOperation{
    boolean unaryOperation(int num);
}

public class LambdaTry {
    public static void main(String[] args) {
        MathOperation isEven = new MathOperation() {
            @Override
            public boolean unaryOperation(int num) {
                return num%2 == 0;
            }
        };
        
        System.out.println(isEven.unaryOperation(25));
        System.out.println(isEven.unaryOperation(20));
    }
}

```

**Lambda style**

1. Remove class name and functional interface body.

```java
public class LambdaTry {
    public static void main(String[] args) {
        MathOperation isEven = (int num) -> {
            return num%2 == 0;
        };
        
        System.out.println(isEven.unaryOperation(25));
        System.out.println(isEven.unaryOperation(20));
    }
}

```


1. Optional type declaration

```java
MathOperation isEven = (num) -> {
    return num%2 == 0;
};

```


1. Optional parenthesis around parameter, if it is single parameter

```java
MathOperation isEven = num -> {
    return num%2 == 0;
};

```


1. Optional curly braces, if there is only one line in function body
1. Optional return keyword, if there is only one line in function body

```java
MathOperation isEven = num -> num%2 == 0;

```



## Lambdas and memory utilization


Since Java lambdas are closures, they can "capture" the values of variables in the enclosing lexical scope.  While not all lambdas capture anything -- simple lambdas like `s -> s.length()` capture nothing and are called **stateless** -- capturing lambdas require a temporary object to hold the captured variables.  In this code snippet, the lambda `() -> j` is a capturing lambda, and may cause an object to be allocated when it is evaluated:

```java
public static void main(String[] args) throws Exception {
    for (int i = 0; i < 1000000000; i++) {
        int j = i;
        doSomethingWithLambda(() -> j);
    }
}

```

Although it might not be immediately obvious since the `new` keyword doesn't appear anywhere in the snippet, this code is liable to create 1,000,000,000 separate objects to represent the instances of the `() -> j` lambda expression.  However, it should also be noted that future versions of Java<sup>1</sup> may be able to optimize this so that **at runtime** the lambda instances were reused, or were represented in some other way.

<sup>1 - For instance, Java 9 introduces an optional "link" phase to the Java build sequence which will provide the opportunity for doing global optimizations like this.</sup>



## Lambdas and Execute-around Pattern


There are several good examples of using lambdas as a FunctionalInterface in simple scenarios. A fairly common use case that can be improved by lambdas is what is called the Execute-Around pattern. In this pattern, you have a set of standard setup/teardown code that is needed for multiple scenarios surrounding use case specific code. A few common example of this are file io, database io, try/catch blocks.

```java
interface DataProcessor {
    void process( Connection connection ) throws SQLException;;
}

public void doProcessing( DataProcessor processor ) throws SQLException{
    try (Connection connection = DBUtil.getDatabaseConnection();) {
        processor.process(connection);
        connection.commit();
    } 
}

```

Then to call this method with a lambda it might look like:

```java
public static void updateMyDAO(MyVO vo) throws DatabaseException {
    doProcessing((Connection conn) -> MyDAO.update(conn, ObjectMapper.map(vo)));
}

```

This is not limited to I/O operations. It can apply to any scenario where similar setup/tear down tasks are applicable with minor variations. The main benefit of this Pattern is code re-use and enforcing DRY (Don't Repeat Yourself).



## `return` only returns from the lambda, not the outer method


The `return` method only returns from the lambda, not the outer method.

Beware that this is **different** from Scala and Kotlin!

```java
void threeTimes(IntConsumer r) {
  for (int i = 0; i < 3; i++) {
    r.accept(i);
  }
}

void demo() {
  threeTimes(i -> {
    System.out.println(i);
    return; // Return from lambda to threeTimes only!
  });
}

```

This can lead to unexpected behavior when attempting to write own language constructs, as in builtin constructs such as `for` loops `return` behaves differently:

```java
void demo2() {
  for (int i = 0; i < 3; i++) {
    System.out.println(i);
    return; // Return from 'demo2' entirely
  }
}

```

In Scala and Kotlin, `demo` and `demo2` would both only print `0`. But this is **not** more consistent. The Java approach is consistent with refactoring and the use of classes - the `return` in the code at the top, and the code below behaves the same:

```java
void demo3() {
  threeTimes(new MyIntConsumer());
}

class MyIntConsumer implements IntConsumer {
  public void accept(int i) {
    System.out.println(i);
    return;
  }
}

```

Therefore, the Java `return` is more consistent with class methods and refactoring, but less with the `for` and `while` builtins, these remain special.

Because of this, the following two are equivalent in Java:

```java
IntStream.range(1, 4)
    .map(x -> x * x)
    .forEach(System.out::println);
IntStream.range(1, 4)
    .map(x -> { return x * x; })
    .forEach(System.out::println);

```

Furthermore, the use of try-with-resources is safe in Java:

```java
class Resource implements AutoCloseable {
  public void close() { System.out.println("close()"); }
}

void executeAround(Consumer<Resource> f) {
  try (Resource r = new Resource()) {
    System.out.print("before ");
    f.accept(r);
    System.out.print("after ");
  }
}

void demo4() {
  executeAround(r -> {
    System.out.print("accept() ");
    return; // Does not return from demo4, but frees the resource.
  });
}

```

will print `before accept() after close()`. In the Scala and Kotlin semantics, the try-with-resources would not be closed, but it would print `before accept()` only.



## Using lambda expressions & predicates to get a certain value(s) from a list


Starting with Java 8, you can use lambda expressions & predicates.

Example: Use a lambda expressions & a predicate to get a certain value from a list. In this example every person will be printed out with the fact if they are 18 and older or not.

Person Class:

```java
public class Person {
    private String name;
    private int age;

    public Person(String name, int age) {
        this.name = name;
        this.age = age;
    }

    public int getAge() { return age; }
    public String getName() { return name; }
}

```

The built-in interface Predicate  from the java.util.function.Predicate packages is a functional interface with a `boolean test(T t)` method.

Example Usage:

```java
import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;

public class LambdaExample {
    public static void main(String[] args) {
        List<Person> personList = new ArrayList<Person>();
        personList.add(new Person("Jeroen", 20));
        personList.add(new Person("Jack", 5));
        personList.add(new Person("Lisa", 19));

        print(personList, p -> p.getAge() >= 18);
    }

    private static void print(List<Person> personList, Predicate<Person> checker) {
        for (Person person : personList) {
            if (checker.test(person)) {
                System.out.print(person + " matches your expression.");
            } else {
                System.out.println(person  + " doesn't match your expression.");
            }
        }
    }
}

```

The `print(personList, p -> p.getAge() >= 18);` method takes a lambda expression (because  the Predicate is used a parameter) where you can define the expression that is needed. The checker's test method checks if this expression is correct or not: `checker.test(person)`.

You can easily change this to something else, for example to `print(personList, p -> p.getName().startsWith("J"));`. This will check if the person's name starts with a "J".



#### Syntax


- () -> { return expression; } // Zero-arity with function body to return a value.
- () -> expression // Shorthand for the above declaration; there is no semicolon for expressions.
- () -> { function-body } // Side-effect in the lambda expression to perform operations.
- parameterName -> expression // One-arity lambda expression. In lambda expressions with only one argument, the parenthesis can be removed.
- (Type parameterName, Type secondParameterName, ...) -> expression // lambda evaluating an expression with parameters listed to the left
- (parameterName, secondParameterName, ...) -> expression // Shorthand that removes the parameter types for the parameter names. Can only be used in contexts that can be inferred by the compiler where the given parameter list size matches one (and only one) of the size of the functional interfaces expected.

