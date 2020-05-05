---
metaTitle: "Java - Classes and Objects"
description: "Overloading Methods, Explaining what is method overloading and overriding., Simplest Possible Class, Object Member vs Static Member, Basic Object Construction and Use, Constructors, Initializing static final fields using a static initializer"
---

# Classes and Objects


Objects have states and behaviors. Example: A dog has states - color, name, breed as well as behaviors – wagging the tail, barking, eating. An object is an instance of a class.

Class − A class can be defined as a template/blueprint that describes the behavior/state that the object of its type support.



## Overloading Methods


Sometimes the same functionality has to be written for different kinds of inputs. At that time, one can use the same method name with a different set of parameters. Each different set of parameters is known as a method signature. As seen per the example, a single method can have multiple signatures.

```java
public class Displayer {

    public void displayName(String firstName) {
        System.out.println("Name is: " + firstName);
    }

    public void displayName(String firstName, String lastName) {
        System.out.println("Name is: " + firstName + " " + lastName);
    }

    public static void main(String[] args) {
        Displayer displayer = new Displayer();
        displayer.displayName("Ram");          //prints "Name is: Ram"
        displayer.displayName("Jon", "Skeet"); //prints "Name is: Jon Skeet"
    }
}

```

The advantage is that the same functionality is called with two different numbers of inputs. While invoking the method according to the input we are passing, (In this case either one string value or two string values) the corresponding method is executed.

**Methods can be overloaded:**

<li>
Based on the **number of parameters** passed.
Example: `method(String s)` and `method(String s1, String s2)`.
</li>

<li>
Based on the **order of parameters**.
Example: `method(int i, float f)` and `method(float f, int i))`.
</li>

****Note:** Methods cannot be overloaded by changing <em>just** the return type (`int method()` is considered the same as `String method()` and will throw a `RuntimeException` if attempted). If you change the return type you must also change the parameters in order to overload.</em>



## Explaining what is method overloading and overriding.


Method Overriding and Overloading are two forms of polymorphism supported by Java.

**Method Overloading**

Method overloading (also known as static Polymorphism) is a way you can have two (or more) methods (functions) with same name in a single class. Yes its as simple as that.

```java
public class Shape{
    //It could be a circle or rectangle or square
    private String type;
    
    //To calculate area of rectangle
    public Double area(Long length, Long breadth){
        return (Double) length * breadth;
    }
    
     //To calculate area of a circle
     public Double area(Long radius){
        return (Double) 3.14 * r * r;
    }
}

```

This way user can call the same method for area depending on the type of shape it has.

But the real question now is, how will java compiler will distinguish which method body is to be executed?

Well Java have made it clear that even though the **method names** (`area()` in our case) **can be same but the arguments method is taking should be different.**

> 
Overloaded methods must have different arguments list (quantity and types).


That being said we cannot add another method to calculate area of a square like this : `public Double area(Long side)` because in this case, it will conflict with area method of circle and will cause **ambiguity** for java compiler.

Thank god, there are some relaxations while writing overloaded methods like

> 
May have different return types.


> 
May have different access modifiers.


> 
May throw different exceptions.


**Why is this called static polymorphism?**

Well that's because which overloaded methods is to be invoked is decided at compile time, based on the actual number of arguments and the compile-time types of the arguments.

> 
One of common reasons of using method overloading is the simplicity of code it provides. For example remember `String.valueOf()` which takes almost any type of argument? What is written behind the scene is probably something like this :-


```java
static String valueOf(boolean b) 
static String valueOf(char c) 
static String valueOf(char[] data) 
static String valueOf(char[] data, int offset, int count) 
static String valueOf(double d) 
static String valueOf(float f) 
static String valueOf(int i) 
static String valueOf(long l) 
static String valueOf(Object obj) 

```

**Method Overriding**

Well, method overriding (yes you guess it right, it is also known as dynamic polymorphism) is somewhat more interesting and complex topic.

In method overriding we overwrite the method body provided by the parent class. Got it? No? Let's go through an example.

```java
public abstract class Shape{
    
    public abstract Double area(){
        return 0.0;
    }
}

```

So we have a class called Shape and it has method called area which will probably return the area of the shape.

Let's say now we have two classes called Circle and Rectangle.

```java
public class Circle extends Shape {
    private Double radius = 5.0;

    // See this annotation @Override, it is telling that this method is from parent
    // class Shape and is overridden here
    @Override
    public Double area(){
        return 3.14 * radius * radius;
    }
}

```

Similarly rectangle class:

```

public class Rectangle extends Shape {
    private Double length = 5.0;
    private Double breadth= 10.0;


    // See this annotation @Override, it is telling that this method is from parent
    // class Shape and is overridden here
    @Override
    public Double area(){
        return length * breadth;
    }
}

```

So, now both of your children classes have updated method body provided by the parent (`Shape`) class. Now question is how to see the result? Well lets do it the old `psvm` way.

```java
public class AreaFinder{
    
    public static void main(String[] args){

        //This will create an object of circle class
        Shape circle = new Circle();
        //This will create an object of Rectangle class
        Shape rectangle = new Rectangle();
        
        // Drumbeats ......
        //This should print 78.5
        System.out.println("Shape of circle : "+circle.area());

        //This should print 50.0
        System.out.println("Shape of rectangle: "+rectangle.area());            
        
    }
}

```

Wow! isn't it great? Two objects of same type calling same methods and returning different values. My friend, that's the power of dynamic polymorphism.

Here's a chart to better compare the differences between these two:-

|Method Overloading|Method Overriding
|---|---|---|---|---|---|---|---|---|---
|Method overloading is used to increase the readability of the program.|Method overriding is used to provide the specific implementation of the method that is already provided by its super class.
|Method overloading is performed within class.|Method overriding occurs in two classes that have IS-A (inheritance) relationship.
|In case of method overloading, parameter must be different.|In case of method overriding, parameter must be same.
|Method overloading is the example of compile time polymorphism.|Method overriding is the example of run time polymorphism.
|In java, method overloading can't be performed by changing return type of the method only. Return type can be same or different in method overloading. But you must have to change the parameter.|Return type must be same or covariant in method overriding.



## Simplest Possible Class


```java
class TrivialClass {}

```

A class consists at a minimum of the `class` keyword, a name, and a body, which might be empty.

You instantiate a class with the `new` operator.

```java
TrivialClass tc = new TrivialClass();

```



## Object Member vs Static Member


With this class:

```java
class ObjectMemberVsStaticMember {

    static int staticCounter = 0;
    int memberCounter = 0;

    void increment() {
        staticCounter ++;
        memberCounter++;
    }
}

```

the following code snippet:

```java
final ObjectMemberVsStaticMember o1 = new ObjectMemberVsStaticMember();
final ObjectMemberVsStaticMember o2 = new ObjectMemberVsStaticMember();

o1.increment();

o2.increment();
o2.increment();

System.out.println("o1 static counter " + o1.staticCounter);
System.out.println("o1 member counter " + o1.memberCounter);
System.out.println();

System.out.println("o2 static counter " + o2.staticCounter);
System.out.println("o2 member counter " + o2.memberCounter);
System.out.println();

System.out.println("ObjectMemberVsStaticMember.staticCounter = " + ObjectMemberVsStaticMember.staticCounter);

// the following line does not compile. You need an object
// to access its members
//System.out.println("ObjectMemberVsStaticMember.staticCounter = " + ObjectMemberVsStaticMember.memberCounter);

```

produces this output:

```java
o1 static counter 3
o1 member counter 1

o2 static counter 3
o2 member counter 2

ObjectMemberVsStaticMember.staticCounter = 3

```

**Note:** You should not call `static` members on objects, but on classes. While it does not make a difference for the JVM, human readers will appreciate it.

`static` members are part of the class and exists only once per class. Non-`static` members exist on instances, there is an independent copy for each instance. This also means that you need access to an object of that class to access its members.



## Basic Object Construction and Use


Objects come in their own class, so a simple example would be a car (detailed explanations below):

```java
public class Car {
    
    //Variables describing the characteristics of an individual car, varies per  object
   private int milesPerGallon;
   private String name;
   private String color;
   public int numGallonsInTank; 
    
    public Car(){
        milesPerGallon = 0;
        name = "";
        color = "";
        numGallonsInTank = 0;
    }
    
    //this is where an individual object is created
    public Car(int mpg, int, gallonsInTank, String carName, String carColor){
        milesPerGallon = mpg;
        name = carName;
        color = carColor;
        numGallonsInTank = gallonsInTank;
    }

    //methods to make the object more usable

    //Cars need to drive
    public void drive(int distanceInMiles){
        //get miles left in car
        int miles = numGallonsInTank * milesPerGallon;
        
        //check that car has enough gas to drive distanceInMiles
        if (miles <= distanceInMiles){
            numGallonsInTank = numGallonsInTank - (distanceInMiles / milesPerGallon)
            System.out.println("Drove " + numGallonsInTank + " miles!");
        } else {
            System.out.println("Could not drive!");
        }
    }

    public void paintCar(String newColor){
        color = newColor;
    }
        //set new Miles Per Gallon
    public void setMPG(int newMPG){
        milesPerGallon = newMPG;
    }

       //set new number of Gallon In Tank
    public void setGallonsInTank(int numGallons){
        numGallonsInTank = numGallons;
    }
    
    public void nameCar(String newName){
        name = newName;
    }

    //Get the Car color
    public String getColor(){
        return color;
    }

    //Get the Car name
    public String getName(){
        return name;
    }

    //Get the number of Gallons
    public String getGallons(){
        return numGallonsInTank;
    }
    
}  

```

Objects are **instances of** their class. So, the way you would **create an object** would be by calling the Car class in **one of two ways** in your main class (main method in Java or onCreate in Android).

**Option 1**

```java
`Car newCar = new Car(30, 10, "Ferrari", "Red");

```

Option 1 is where you essentially tell the program everything about the Car upon creation of the object. Changing any property of the car would require calling one of the methods such as the `repaintCar` method. Example:

```

newCar.repaintCar("Blue");

```

**Note:** Make sure you pass the correct data type to the method. In the example above, you may also pass a variable to the `repaintCar` method **as long as the data type is correct`**.

That was an example of changing properties of an object, receiving properties of an object would require using a method from the Car class that has a return value (meaning a method that is not `void`).  Example:

```java
String myCarName = newCar.getName();  //returns string "Ferrari"

```

Option 1 is the **best** option when you have **all the object's data** at the time of creation.

**Option 2**

```java
`Car newCar = new Car();

```

Option 2 gets the same effect but required more work to create an object correctly. I want to recall this Constructor in the Car class:

```java
public void Car(){
        milesPerGallon = 0;
        name = "";
        color = "";
        numGallonsInTank = 0;
    }

```

Notice that you do not have to actually pass any parameters into the object to create it. This is very useful for when you do not have all the aspects of the object but you need to use the parts that you do have. This sets generic data into each of the instance variables of the object so that, if you call for a piece of data that does not exist, no errors are thrown.

**Note:** Do not forget that you have to set the parts of the object later that you did not initialize it with. For example,

```java
Car myCar = new Car();
String color = Car.getColor(); //returns empty string

```

This is a common mistake amongst objects that are not initialized with all their data. Errors were avoided because there is a Constructor that allows an empty Car object to be created with **stand-in variables** (`public Car(){}`), but no part of the myCar was actually customized. **Correct example of creating Car Object:**

```java
Car myCar = new Car();
myCar.nameCar("Ferrari");
myCar.paintCar("Purple");
myCar.setGallonsInTank(10);
myCar.setMPG(30);

```

And, as a reminder, get an object's properties by calling a method in your main class. Example:

```java
String myCarName = myCar.getName(); //returns string "Ferrari"

```



## Constructors


Constructors are special methods named after the class and without a return type, and are used to construct objects. Constructors, like methods, can take input parameters. Constructors are used to initialize objects. Abstract classes can have constructors also.

```java
public class Hello{
    // constructor
    public Hello(String wordToPrint){
        printHello(wordToPrint);
    }
    public void printHello(String word){
        System.out.println(word);
    }
}
// instantiates the object during creating and prints out the content
// of wordToPrint

```

It is important to understand that constructors are different from methods in several ways:

<li>
Constructors can only take the modifiers `public`, `private`, and `protected`, and cannot be declared `abstract`, `final`, `static`, or `synchronized`.
</li>
<li>
Constructors do not have a return type.
</li>
<li>
Constructors MUST be named the same as the class name. In the `Hello` example, the `Hello` object's constructor name is the same as the class name.
</li>
<li>
The `this` keyword has an additional usage inside constructors. `this.method(...)` calls a method on the current instance, while `this(...)` refers to another constructor in the current class with different signatures.
</li>

Constructors also can be called through inheritance using the keyword `super`.

```java
public class SuperManClass{

    public SuperManClass(){
        // some implementation
    }
    
    // ... methods
}


public class BatmanClass extends SupermanClass{
    public BatmanClass(){
        super();
    }
    //... methods...
}

```

See [Java Language Specification #8.8](http://docs.oracle.com/javase/specs/jls/se8/html/jls-8.html#jls-8.8) and [#15.9](http://docs.oracle.com/javase/specs/jls/se8/html/jls-15.html#jls-15.9)



## Initializing static final fields using a static initializer


To initialize a `static final` fields that require using more than a single expression, a `static` initializer can be used to assign the value. The following example initializes a unmodifiable set of `String`s:

```java
public class MyClass {

    public static final Set<String> WORDS;
    
    static {
        Set<String> set = new HashSet<>();
        set.add("Hello");
        set.add("World");
        set.add("foo");
        set.add("bar");
        set.add("42");
        WORDS = Collections.unmodifiableSet(set);
    }
}

```



#### Syntax


- class Example {} //class keyword, name, body

