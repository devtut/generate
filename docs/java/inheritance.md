---
metaTitle: "Java - Inheritance"
description: "Abstract Classes, Using 'final' to restrict inheritance and overriding, Inheritance, The Liskov Substitution Principle, Static Inheritance, Abstract class and Interface usage: Is-a relation vs Has-a capability, Inheritance and Static Methods, Variable shadowing, Narrowing and Widening of object references, Programming to an interface, Overriding in Inheritance"
---

# Inheritance


Inheritance is a basic object oriented feature in which one class acquires and extends upon the properties of another class, using the keyword `extends`. For Interfaces and the keyword `implements`, see [interfaces](http://stackoverflow.com/documentation/java/102/interfaces#t=201701031900066194592).



## Abstract Classes


An abstract class is a class marked with the `abstract` keyword. It, contrary to non-abstract class, may contain abstract - implementation-less - methods. It is, however, valid to create an abstract class without abstract methods.

An abstract class cannot be instantiated. It can be sub-classed (extended) as long as the sub-class is either also abstract, or implements all methods marked as abstract by super classes.

An example of an abstract class:

```java
public abstract class Component {
    private int x, y;
    
    public setPosition(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public abstract void render();
}

```

The class must be marked abstract, when it has at least one abstract method. An abstract method is a method that has no implementation. Other methods can be declared within an abstract class that have implementation in order to provide common code for any sub-classes.

Attempting to instantiate this class will provide a compile error:

```java
//error: Component is abstract; cannot be instantiated   
Component myComponent = new Component();

```

However a class that extends `Component`, and provides an implementation for all of its abstract methods and can be instantiated.

```java
public class Button extends Component {

    @Override
    public void render() {
        //render a button
    }
}

public class TextBox extends Component {

    @Override
    public void render() {
        //render a textbox
    }
}

```

Instances of inheriting classes also can be cast as the parent class (normal inheritance) and they provide a polymorphic effect when the abstract method is called.

```java
Component myButton = new Button();
Component myTextBox = new TextBox();

myButton.render(); //renders a button
myTextBox.render(); //renders a text box

```

**Abstract classes vs Interfaces**

Abstract classes and interfaces both provide a way to define method signatures while requiring the extending/implementing class to provide the implementation.

There are two key differences between abstract classes and interfaces:

- A class may only extend a single class, but may implement many interfaces.
- An abstract class can contain instance (non-`static`) fields, but interfaces may only contain `static` fields.

Methods declared in interfaces could not contain implementations, so abstract classes were used when it was useful to provide additional methods which implementations called the abstract methods.

Java 8 allows interfaces to contain [default methods](https://stackoverflow.com/documentation/java/113/default-methods#t=201607191734172336331), usually [implemented using the other methods of the interface](https://stackoverflow.com/documentation/java/113/default-methods/456/accessing-other-interface-methods-within-default-method#t=201607191734172336331), making interfaces and abstract classes equally powerful in this regard.

**Anonymous subclasses of Abstract Classes**

As a convenience java allows for instantiation of anonymous instances of subclasses of abstract classes, which provide implementations for the abstract methods upon creating the new object. Using the above example this could look like this:

```java
Component myAnonymousComponent = new Component() {
    @Override
    public void render() {
        // render a quick 1-time use component
    }
}

```



## Using 'final' to restrict inheritance and overriding


### Final classes

When used in a `class` declaration, the `final` modifier prevents other classes from being declared that `extend` the class.  A `final` class is a "leaf" class in the inheritance class hierarchy.

```java
// This declares a final class
final class MyFinalClass {
    /* some code */
}

// Compilation error: cannot inherit from final MyFinalClass
class MySubClass extends MyFinalClass {
    /* more code */
}

```

### Use-cases for final classes

Final classes can be combined with a `private` constructor to control or prevent the instantiation of a class.  This can be used to create a so-called "utility class" that only defines static members; i.e. constants and static methods.

```java
public final class UtilityClass {

    // Private constructor to replace the default visible constructor
    private UtilityClass() {}

    // Static members can still be used as usual
    public static int doSomethingCool() {
        return 123;
    }

}

```

Immutable classes should also be declared as `final`.  (An immutable class is one whose instances cannot be changed after they have been created; see the I[mmutable Objects](http://stackoverflow.com/documentation/java/2807/immutable-objects) topic. ) By doing this, you make it impossible to create a mutable subclass of an immutable class. That would violate the [Liskov Substitution Principle](http://stackoverflow.com/documentation/java/87/inheritance/3106/the-liskov-substitution-principle) which requires that a subtype should obey the "behavioral contract" of its supertypes.

From a practical perspective, declaring an immutable class to be `final` makes it easier to reason about program behavior.  It also addresses security concerns in the scenario where untrusted code is executed in a security sandbox.  (For instance, since `String` is declared as `final`, a trusted class does not need to worry that it might be tricked into accepting mutable subclass, which the untrusted caller could then surreptitiously change.)

One disadvantage of `final` classes is that they do not work with some mocking frameworks such as Mockito. Update: Mockito version 2 now support mocking of final classes.

### Final methods

The `final` modifier can also be applied to methods to prevent them being overridden in sub-classes:

```java
public class MyClassWithFinalMethod {

    public final void someMethod() {
    }
}

public class MySubClass extends MyClassWithFinalMethod {

    @Override
    public void someMethod() { // Compiler error (overridden method is final)
    }
}

```

Final methods are typically used when you want to restrict what a subclass can change in a class without forbidding subclasses entirely.

The `final` modifier can also be applied to variables, but the meaning of `final` for variables is unrelated to inheritance.



## Inheritance


With the use of the `extends` keyword among classes, all the properties of the superclass (also known as the **Parent Class** or **Base Class**) are present in the subclass (also known as the **Child Class** or **Derived Class**)

```java
public class BaseClass {

    public void baseMethod(){
        System.out.println("Doing base class stuff");
    }
}

public class SubClass extends BaseClass {

}

```

Instances of `SubClass` have **inherited** the method `baseMethod()`:

```java
SubClass s = new SubClass();
s.baseMethod();  //Valid, prints "Doing base class stuff"

```

Additional content can be added to a subclass. Doing so allows for additional functionality in the subclass without any change to the base class or any other subclasses from that same base class:

```java
public class Subclass2 extends BaseClass {

    public void anotherMethod() {
        System.out.println("Doing subclass2 stuff");
    }
}

Subclass2 s2 = new Subclass2();
s2.baseMethod(); //Still valid , prints "Doing base class stuff"
s2.anotherMethod(); //Also valid, prints "Doing subclass2 stuff" 

```

Fields are also inherited:

```java
public class BaseClassWithField {

    public int x;

}

public class SubClassWithField extends BaseClassWithField {

    public SubClassWithField(int x) {
        this.x = x; //Can access fields
    }
}

```

`private` fields and methods still exist within the subclass, but are not accessible:

```java
public class BaseClassWithPrivateField {

    private int x = 5;

    public int getX() {
        return x;
    }
}

public class SubClassInheritsPrivateField extends BaseClassWithPrivateField {

    public void printX() {
        System.out.println(x); //Illegal, can't access private field x
        System.out.println(getX()); //Legal, prints 5
    }
}

SubClassInheritsPrivateField s = new SubClassInheritsPrivateField();
int x = s.getX(); //x will have a value of 5.

```

In Java, each class may extend at most one other class.

```java
public class A{}
public class B{}
public class ExtendsTwoClasses extends A, B {} //Illegal

```

This is known as multiple inheritance, and while it is legal in some languages, Java does not permit it with classes.

As a result of this, every class has an unbranching ancestral chain of classes leading to `Object`, from which all classes descend.



## The Liskov Substitution Principle


Substitutability is a principle in object-oriented programming introduced by Barbara Liskov in a 1987 conference keynote stating that, if class `B` is a subclass of class `A`, then wherever `A` is expected, `B` can be used instead:

```java
class A {...}
class B extends A {...}

public void method(A obj) {...}

A a = new B(); // Assignment OK
method(new B()); // Passing as parameter OK

```

This also applies when the type is an interface, where there doesn't need to any hierarchical relationship between the objects:

```java
interface Foo {
    void bar();
}

class A implements Foo {
    void bar() {...}
}

class B implements Foo {
    void bar() {...}
}

List<Foo> foos = new ArrayList<>();
foos.add(new A()); // OK
foos.add(new B()); // OK

```

Now the list contains objects that are not from the same class hierarchy.



## Static Inheritance


Static method can be inherited similar to normal methods, however unlike normal methods it is impossible to create "[abstract](http://stackoverflow.com/documentation/java/87/inheritance/397/abstract-classes)" methods in order to force static method overriding. Writing a method with the same signature as a static method in a super class appears to be a form of overriding, but really this simply creates a new function hides the other.

```java
public class BaseClass {
    
    public static int num = 5;

    public static void sayHello() {
        System.out.println("Hello");
    }

    public static void main(String[] args) {
        BaseClass.sayHello();
        System.out.println("BaseClass's num: " + BaseClass.num);
            
        SubClass.sayHello();
        //This will be different than the above statement's output, since it runs
        //A different method
        SubClass.sayHello(true);
        
        StaticOverride.sayHello();
        System.out.println("StaticOverride's num: " + StaticOverride.num);
    }
}

public  class SubClass extends BaseClass {
    
    //Inherits the sayHello function, but does not override it   
    public static void sayHello(boolean test) {
        System.out.println("Hey");
    }
}

public static class StaticOverride extends BaseClass {

    //Hides the num field from BaseClass
    //You can even change the type, since this doesn't affect the signature
    public static String num = "test";
        
    //Cannot use @Override annotation, since this is static
    //This overrides the sayHello method from BaseClass
    public static void sayHello() {
        System.out.println("Static says Hi");
    }

}

```

Running any of these classes produces the output:

```java
Hello
BaseClass's num: 5
Hello
Hey
Static says Hi
StaticOverride's num: test

```

Note that unlike normal inheritance, in static inheritance methods are not hidden. You can always call the base `sayHello` method by using `BaseClass.sayHello()`. But classes do inherit static methods **if** no methods with the same signature are found in the subclass. If two method's signatures vary, both methods can be run from the subclass, even if the name is the same.

Static fields hide each other in a similar way.



## Abstract class and Interface usage: "Is-a" relation vs "Has-a" capability


When to use abstract classes:  To implement the same or different behaviour among multiple related objects

When to use interfaces: to implement a contract by multiple unrelated objects

**Abstract classes create "is a" relations while interfaces provide "has a" capability.**

This can be seen in the code below:

```java
public class InterfaceAndAbstractClassDemo{
    public static void main(String args[]){
        
        Dog dog = new Dog("Jack",16);
        Cat cat = new Cat("Joe",20);
            
        System.out.println("Dog:"+dog);
        System.out.println("Cat:"+cat);
        
        dog.remember();
        dog.protectOwner();
        Learn dl = dog;
        dl.learn();
                
        cat.remember();
        cat.protectOwner();
        
        Climb c = cat;
        c.climb();
        
        Man man = new Man("Ravindra",40);
        System.out.println(man);
        
        Climb cm = man;
        cm.climb();
        Think t = man;
        t.think();
        Learn l = man;
        l.learn();
        Apply a = man;
        a.apply();
    }
}

abstract class Animal{
    String name;
    int lifeExpentency;
    public Animal(String name,int lifeExpentency ){
        this.name = name;
        this.lifeExpentency=lifeExpentency;
    }
    public abstract void remember();
    public abstract void protectOwner();
    
    public String toString(){
        return this.getClass().getSimpleName()+":"+name+":"+lifeExpentency;
    }
}
class Dog extends Animal implements Learn{
    
    public Dog(String name,int age){
        super(name,age);
    }
    public void remember(){
        System.out.println(this.getClass().getSimpleName()+" can remember for 5 minutes");
    }
    public void protectOwner(){
        System.out.println(this.getClass().getSimpleName()+ " will protect owner");
    }
    public void learn(){
        System.out.println(this.getClass().getSimpleName()+ " can learn:");
    }
}
class Cat extends Animal implements Climb {
    public Cat(String name,int age){
        super(name,age);
    }
    public void remember(){
        System.out.println(this.getClass().getSimpleName() + " can remember for 16 hours");
    }
    public void protectOwner(){
        System.out.println(this.getClass().getSimpleName()+ " won't protect owner");
    }
    public void climb(){
        System.out.println(this.getClass().getSimpleName()+ " can climb");
    }
}
interface Climb{
    void climb();
}
interface Think {
    void think();
}

interface Learn {
    void learn();
}
interface Apply{
    void apply();
}

class Man implements Think,Learn,Apply,Climb{
    String name;
    int age;

    public Man(String name,int age){
        this.name = name;
        this.age = age;
    }
    public void think(){
        System.out.println("I can think:"+this.getClass().getSimpleName());
    }
    public void learn(){
        System.out.println("I can learn:"+this.getClass().getSimpleName());
    }
    public void apply(){
        System.out.println("I can apply:"+this.getClass().getSimpleName());
    }
    public void climb(){
        System.out.println("I can climb:"+this.getClass().getSimpleName());
    }
    public String toString(){
        return "Man :"+name+":Age:"+age;
    }
}

```

output:

```java
Dog:Dog:Jack:16
Cat:Cat:Joe:20
Dog can remember for 5 minutes
Dog will protect owner
Dog can learn:
Cat can remember for 16 hours
Cat won't protect owner
Cat can climb
Man :Ravindra:Age:40
I can climb:Man
I can think:Man
I can learn:Man
I can apply:Man

```

Key notes:

<li>
`Animal` is an abstract class with shared attributes: `name` and `lifeExpectancy` and abstract methods: `remember()` and `protectOwner()`. `Dog` and `Cat` are `Animals` that have implemented the `remember()` and `protectOwner()` methods.
</li>
<li>
`Cat` can `climb()` but `Dog` cannot. `Dog` can `think()` but `Cat` cannot. These specific capabilities are added to `Cat` and `Dog` by implementation.
</li>
<li>
`Man` is not an `Animal` but he can `Think` , `Learn`, `Apply`, and `Climb`.
</li>
<li>
`Cat` is not a `Man` but it can `Climb`.
</li>
<li>
`Dog` is not a `Man` but it can `Learn`
</li>
<li>
`Man` is neither a `Cat` nor a `Dog` but can have some of the capabilities of the latter two without extending `Animal`, `Cat`, or `Dog`. This is done with Interfaces.
</li>
<li>
Even though `Animal` is an abstract class, it has a constructor, unlike an interface.
</li>

TL;DR:

**Unrelated classes can have capabilities through interfaces, but related classes change the behaviour through extension of base classes.**

Refer to the Java documentation [page](https://docs.oracle.com/javase/tutorial/java/IandI/abstract.html) to understand which one to use in a specific use case.

**Consider using abstract classes** if...

1. You want to share code among several closely related classes.
1. You expect that classes that extend your abstract class have many common methods or fields, or require access modifiers other than public (such as protected and private).
1. You want to declare non-static or non-final fields.

**Consider using interfaces** if...

1. You expect that unrelated classes would implement your interface. For example, many unrelated objects can implement the `Serializable` interface.
1. You want to specify the behaviour of a particular data type but are not concerned about who implements its behaviour.
1. You want to take advantage of multiple inheritance of type.



## Inheritance and Static Methods


In Java, parent and child class both can have static methods with the same name. But in such cases implementation of static method in child is [hiding](https://docs.oracle.com/javase/specs/jls/se8/html/jls-8.html#jls-8.4.8.2) parent class' implementation, it's not method overriding. For example:

```java
class StaticMethodTest {

  // static method and inheritance
  public static void main(String[] args) {
    Parent p = new Child();
    p.staticMethod(); // prints Inside Parent
    ((Child) p).staticMethod(); // prints Inside Child
  }

  static class Parent {
    public static void staticMethod() {
      System.out.println("Inside Parent");
    }
  }

  static class Child extends Parent {
    public static void staticMethod() {
      System.out.println("Inside Child");
    }
  }
}

```

Static methods are bind to a class not to an instance and this method binding happens at compile time. Since in the first call to `staticMethod()`, parent class reference `p` was used, `Parent`'s version of `staticMethod()` is invoked. In second case, we did cast `p` into `Child` class, `Child`'s `staticMethod()` executed.



## Variable shadowing


Variables are SHADOWED and methods are OVERRIDDEN.
Which variable will be used depends on the class that the variable is declared of.
Which method will be used depends on the actual class of the object that is referenced by the variable.

```java
class Car {
    public int gearRatio = 8;

    public String accelerate() {
        return "Accelerate : Car";
    }
}

class SportsCar extends Car {
    public int gearRatio = 9;

    public String accelerate() {
        return "Accelerate : SportsCar";
    }

    public void test() {

    }


    public static void main(String[] args) {

        Car car = new SportsCar();
        System.out.println(car.gearRatio + "  " + car.accelerate());
        // will print out 8  Accelerate : SportsCar
    }
}

```



## Narrowing and Widening of object references


Casting an instance of a base class to a subclass as in : `b = (B) a;` is called **narrowing** (as you are trying to narrow the base class object to a more specific class object) and needs an explicit type-cast.

Casting an instance of a subclass to a base class as in: `A a = b;` is called **widening** and does not need a type-cast.

To illustrate, consider the following class declarations, and test code:

```java
class Vehicle {
}

class Car extends Vehicle {
}

class Truck extends Vehicle {
}

class MotorCycle extends Vehicle {
}

class Test {

    public static void main(String[] args) {
    
        Vehicle vehicle = new Car();
        Car car = new Car();        
    
        vehicle = car; // is valid, no cast needed

        Car c = vehicle // not valid
        Car c = (Car) vehicle; //valid
    }
}

```

The statement `Vehicle vehicle = new Car();` is a valid Java statement.   Every instance of `Car` is also a `Vehicle`.  Therefore, the assignment is legal without the need for an explicit type-cast.

On the other hand, `Car c = vehicle;` is not valid.  The static type of the `vehicle` variable is `Vehicle` which means that it could refer to an instance of `Car`, Truck`,`MotorCycle`, or any other current or future subclass of`Vehicle`. (Or indeed, an instance of`Vehicle`itself, since we did not declare it as an`abstract`class.) The assignment cannot be allowed, since that might lead to`car`referring to a`Truck` instance.

To prevent this situation, we need to add an explicit type-cast:

```java
Car c = (Car) vehicle;

```

The type-cast tells the compiler that we **expect** the value of `vehicle` to be a `Car` or a subclass of `Car`.  If necessary, compiler will insert code to perform a run-time type check.  If the check fails, then a `ClassCastException` will be thrown when the code is executed.

Note that not all type-casts are valid.  For example:

```java
String s = (String) vehicle;  // not valid

```

The Java compiler knows that an instance that is type compatible with `Vehicle` **cannot ever be** type compatible with `String`.  The type-cast could never succeed, and the JLS mandates that this gives in a compilation error.



## Programming to an interface


The idea behind programming to an interface is to base the code primarily on interfaces and only use concrete classes at the time of instantiation. In this context, good code dealing with e.g. Java collections will look something like this (not that the method itself is of any use at all, just illustration):

```java
public <T> Set<T> toSet(Collection<T> collection) {
  return Sets.newHashSet(collection);
}

```

while bad code might look like this:

```java
public <T> HashSet<T> toSet(ArrayList<T> collection) {
  return Sets.newHashSet(collection);
}

```

Not only the former can be applied to a wider choice of arguments, its results will be more compatible with code provided by other developers that generally adhere to the concept of programming to an interface. However, the most important reasons to use the former are:

- most of the time the context, in which the result is used, does not and should not need that many details as the concrete implementation provides;
- adhering to an interface forces cleaner code and less hacks such as yet another public method gets added to a class serving some specific scenario;
- the code is more testable as interfaces are easily mockable;
- finally, the concept helps even if only one implementation is expected (at least for testability).

So how can one easily apply the concept of programming to an interface when writing new code having in mind one particular implementation? One option that we commonly use is a combination of the following patterns:

- programming to an interface
- factory
- builder

The following example based on these principles is a simplified and truncated version of an RPC implementation written for a number of different protocols:

```java
public interface RemoteInvoker {
  <RQ, RS> CompletableFuture<RS> invoke(RQ request, Class<RS> responseClass);
}

```

The above interface is not supposed to be instantiated directly via a factory, instead we derive further more concrete interfaces, one for HTTP invocation and one for AMQP, each then having a factory and a builder to construct instances, which in turn are also instances of the above interface:

```java
public interface AmqpInvoker extends RemoteInvoker {
  static AmqpInvokerBuilder with(String instanceId, ConnectionFactory factory) {
    return new AmqpInvokerBuilder(instanceId, factory);
  }
}

```

Instances of `RemoteInvoker` for the use with AMQP can now be constructed as easy as (or more involved depending on the builder):

```java
RemoteInvoker invoker = AmqpInvoker.with(instanceId, factory)
  .requestRouter(router)
  .build();

```

And an invocation of a request is as easy as:

```java
Response res = invoker.invoke(new Request(data), Response.class).get();

```

Due to Java 8 permitting placing of static methods directly into interfaces, the intermediate factory has become implicit in the above code replaced with `AmqpInvoker.with()`. In Java prior to version 8, the same effect can be achieved with an inner `Factory` class:

```java
public interface AmqpInvoker extends RemoteInvoker {
  class Factory {
    public static AmqpInvokerBuilder with(String instanceId, ConnectionFactory factory) {
      return new AmqpInvokerBuilder(instanceId, factory);
    }
  }
}

```

The corresponding instantiation would then turn into:

```java
RemoteInvoker invoker = AmqpInvoker.Factory.with(instanceId, factory)
  .requestRouter(router)
  .build();

```

The builder used above could look like this (although this is a simplification as the actual one permits defining of up to 15 parameters deviating from defaults). Note that the construct is not public, so it is specifically usable only from the above `AmqpInvoker` interface:

```java
public class AmqpInvokerBuilder {
  ...
  AmqpInvokerBuilder(String instanceId, ConnectionFactory factory) {
    this.instanceId = instanceId;
    this.factory = factory;
  }

  public AmqpInvokerBuilder requestRouter(RequestRouter requestRouter) {
    this.requestRouter = requestRouter;
    return this;
  }

  public AmqpInvoker build() throws TimeoutException, IOException {
    return new AmqpInvokerImpl(instanceId, factory, requestRouter);
  }
}

```

Generally, a builder can also be generated using a tool like FreeBuilder.

Finally, the standard (and the only expected) implementation of this interface is defined as a package-local class to enforce the use of the interface, the factory and the builder:

```java
class AmqpInvokerImpl implements AmqpInvoker {
  AmqpInvokerImpl(String instanceId, ConnectionFactory factory, RequestRouter requestRouter) {
    ...
  }

  @Override
  public <RQ, RS> CompletableFuture<RS> invoke(final RQ request, final Class<RS> respClass) {
    ...
  }
}

```

Meanwhile, this pattern proved to be very efficient in developing all our new code not matter how simple or complex the functionality is.



## Overriding in Inheritance


Overriding in Inheritance is used when you use a already defined method from a super class in a sub class, but in a different way than how the method was originally designed in the super class. Overriding allows the user to reuse code by using existing material and modifying it to suit the user's needs better.

The following example demonstrates how `ClassB` overrides the functionality of `ClassA` by changing what gets sent out through the printing method:

**Example:**

```java
public static void main(String[] args) {
    ClassA a = new ClassA();
    ClassA b = new ClassB();
    a.printing();
    b.printing();
}

class ClassA {
    public void printing() {        
        System.out.println("A");
    }
}

class ClassB extends ClassA {
    public void printing() {
         System.out.println("B");
    }
}

```

**Output:**

> 
A


> 
B




#### Syntax


- class ClassB extends ClassA {...}
- class ClassB implements InterfaceA {...}
- interface InterfaceB extends InterfaceA {...}
- class ClassB extends ClassA implements InterfaceC, InterfaceD {...}
- abstract class AbstractClassB extends ClassA {...}
- abstract class AbstractClassB extends AbstractClassA {...}
- abstract class AbstractClassB extends ClassA implements InterfaceC, InterfaceD {...}



#### Remarks


Inheritance is often combined with generics so that the base class has one or more type parameters. See [Creating a Generic Class](http://stackoverflow.com/documentation/java/92/generics/388/creating-a-generic-class).

