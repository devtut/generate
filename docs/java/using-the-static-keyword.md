---
metaTitle: "Java - Using the static keyword"
description: "Reference to non-static member from static context, Using static to declare constants, Using static with this"
---

# Using the static keyword




## Reference to non-static member from static context


Static variables and methods are not part of an instance, There will always be a single copy of that variable no matter how many objects you create of a particular class.

For example you might want to have an immutable list of constants, it would be a good idea to keep it static and initialize it just once inside a static method. This would give you a significant performance gain if you are creating several instances of a particular class on a regular basis.

Furthermore you can also have a static block in a class as well. You can use it to assign a default value to a static variable. They are executed only once when the class is loaded into memory.

Instance variable as the name suggest are dependent on an instance of a particular object, they live to serve the whims of it. You can play around with them during a particular life cycle of an object.

All the fields and methods of a class used inside a static method of that class must be static or local. If you try to use instance (non-static) variables or methods,  your code will not compile.

```java
public class Week {
    static int daysOfTheWeek = 7; // static variable
    int dayOfTheWeek; // instance variable
    
    public static int getDaysLeftInWeek(){
        return Week.daysOfTheWeek-dayOfTheWeek; // this will cause errors
    }

    public int getDaysLeftInWeek(){
        return Week.daysOfTheWeek-dayOfTheWeek; // this is valid
    }

    public static int getDaysLeftInTheWeek(int today){
        return Week.daysOfTheWeek-today; // this is valid
    }
    
}

```



## Using static to declare constants


As the `static` keyword is used for accessing fields and methods without an instantiated class, it can be used to declare constants for use in other classes. These variables will remain constant across every instantiation of the class. By convention, `static` variables are always `ALL_CAPS` and use underscores rather than camel case. ex:

```

static E STATIC_VARIABLE_NAME

```

As constants cannot change, `static` can also be used with the `final` modifier:

For example, to define the mathematical constant of pi:

```java
public class MathUtilities {
    
    static final double PI = 3.14159265358

}

```

Which can be used in any class as a constant, for example:

```java
public class MathCalculations {
   
    //Calculates the circumference of a circle
    public double calculateCircumference(double radius) {
        return (2 * radius * MathUtilities.PI);
    }

}

```



## Using static with this


Static gives a method or variable storage that is **not** allocated for each instance of the class. Rather, the static variable is shared among all class members. Incidentally, trying to treat the static variable like a member of the class instance will result in a warning:

```java
public class Apple {
    public static int test;
    public int test2;
}

Apple a = new Apple();
a.test = 1; // Warning
Apple.test = 1; // OK
Apple.test2 = 1; // Illegal: test2 is not static
a.test2 = 1; // OK

```

Methods that are declared static behave in much the same way, but with an additional restriction:

> 
You can't use the `this` keyword in them!


```java
public class Pineapple {

    private static int numberOfSpikes;   
    private int age;

    public static getNumberOfSpikes() {
        return this.numberOfSpikes; // This doesn't compile
    }


    public static getNumberOfSpikes() {
        return numberOfSpikes; // This compiles
    }

}

```

In general, it's best to declare generic methods that apply to different instances of a class (such as clone methods) `static`, while keeping methods like `equals()` as non-static. The `main` method of a Java program is always static, which means that the keyword `this` cannot be used inside `main()`.



#### Syntax


- public static int myVariable; //Declaring a static variable
- public static myMethod() { } //Declaring a static method
- public static final double MY_CONSTANT; //Declaring a constant variable that is shared among all instances of the class
- public final double MY_CONSTANT; // Declaring a constant variable specific to this instance of the class (best used in a constructor that generates a different constant for each instance)

