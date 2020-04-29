---
metaTitle: "Encapsulation"
description: "Encapsulation to maintain invariants, Encapsulation to reduce coupling"
---

# Encapsulation


Imagine you had a class with some pretty important variables and they were set (by other programmers from their code) to unacceptable values.Their code brought errors in your code. As a solution, In OOP, you allow the state of an object (stored in its variables) to be modified only through methods. Hiding the state of an object and providing all interaction through an objects methods is known as Data Encapsulation.



## Encapsulation to maintain invariants


There are two parts of a class: the interface and the implementation.

The interface is the exposed functionality of the class. Its public methods and variables are part of the interface.

The implementation is the internal workings of a class. Other classes shouldn't need to know about the implementation of a class.

Encapsulation refers to the practice of hiding the implementation of a class from any users of that class. This allows the class to make assumptions about its internal state.

For example, take this class representing an Angle:

```java
public class Angle {
    
    private double angleInDegrees;
    private double angleInRadians;
    
    public static Angle angleFromDegrees(double degrees){
        Angle a = new Angle();
        a.angleInDegrees = degrees;
        a.angleInRadians = Math.PI*degrees/180;
        return a;
    }
    
    public static Angle angleFromRadians(double radians){
        Angle a = new Angle();
        a.angleInRadians = radians;
        a.angleInDegrees = radians*180/Math.PI;
        return a;
    }
    
    public double getDegrees(){
        return angleInDegrees;
    }
    
    public double getRadians(){
        return angleInRadians;
    }
    
    public void setDegrees(double degrees){
        this.angleInDegrees = degrees;
        this.angleInRadians = Math.PI*degrees/180;
    }
    
    public void setRadians(double radians){
        this.angleInRadians = radians;
        this.angleInDegrees = radians*180/Math.PI;
    }
    private Angle(){}
}

```

This class relies on a basic assumption (or **invariant**): **angleInDegrees and angleInRadians are always in sync**. If the class members were public, there would be no guarantees that the two representations of angles are correlated.



## Encapsulation to reduce coupling


Encapsulation allows you to make internal changes to a class without affecting any code that calls the class. This reduces **coupling**, or how much any given class relies on the implementation of another class.

For example, let's change the implementation of the Angle class from the previous example:

```java
public class Angle {
    
    private double angleInDegrees;
    
    public static Angle angleFromDegrees(double degrees){
        Angle a = new Angle();
        a.angleInDegrees = degrees;
        return a;
    }
    
    public static Angle angleFromRadians(double radians){
        Angle a = new Angle();
        a.angleInDegrees = radians*180/Math.PI;
        return a;
    }
    
    public double getDegrees(){
        return angleInDegrees;
    }
    
    public double getRadians(){
        return angleInDegrees*Math.PI / 180;
    }
    
    public void setDegrees(double degrees){
        this.angleInDegrees = degrees;
    }
    
    public void setRadians(double radians){
        this.angleInDegrees = radians*180/Math.PI;
    }

    private Angle(){}
}

```

The implementation of this class has changed so that it only stores one representation of the angle and calculates the other angle when needed.

However, **the implementation changed, but the interface didn't**. If a calling class relied on accessing the angleInRadians method, it would need to be changed to use the new version of `Angle`. Calling classes shouldn't care about the internal representation of a class.



#### Remarks


It is much easier to start with marking a variable `private` and expose it if necessary than to hide an already `public` variable.

There is one exception where encapsulation may not be beneficial: "dumb" data structures (classes whose sole purpose is to hold variables).

```java
public class DumbData {
    public String name;
    public int timeStamp;
    public int value;
}

```

In this case, the interface of the class **is** the data that it holds.

Note that variables marked `final` can be marked `public` without violating encapsulation because they can't be changed after being set.

