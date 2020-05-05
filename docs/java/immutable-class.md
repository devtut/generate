---
metaTitle: "Java - Immutable Class"
description: "Example without mutable refs, What is the advantage of immutability?, Rules to define immutable classes, Example with mutable refs"
---

# Immutable Class


Immutable objects are instances whose state doesn’t change after it has been initialized. For example, String is an immutable class and once instantiated its value never changes.



## Example without mutable refs


```java
public final class Color {
    final private int red;
    final private int green;
    final private int blue;

    private void check(int red, int green, int blue) {
        if (red < 0 || red > 255 || green < 0 || green > 255 || blue < 0 || blue > 255) {
            throw new IllegalArgumentException();
        }
    }

    public Color(int red, int green, int blue) {
        check(red, green, blue);
        this.red = red;
        this.green = green;
        this.blue = blue;
    }

    public Color invert() {
        return new Color(255 - red, 255 - green, 255 - blue);
    }
}

```



## What is the advantage of immutability?


The advantage of immutability comes with concurrency. It is difficult to maintain correctness in mutable objects, as multiple threads could be trying to change the state of the same object, leading to some threads seeing a different state of the same object, depending on the timing of the reads and writes to the said object.

By having an immutable object, one can ensure that all threads that are looking at the object will be seeing the same state, as the state of an immutable object will not change.



## Rules to define immutable classes


The following rules define a simple strategy for creating immutable objects.

1. Don't provide "setter" methods - methods that modify fields or objects referred to by fields.
1. Make all fields final and private.
1. Don't allow subclasses to override methods. The simplest way to do this is to declare the class as final. A more sophisticated approach is to make the constructor private and construct instances in factory methods.
1. If the instance fields include references to mutable objects, don't allow those objects to be changed:
1. Don't provide methods that modify the mutable objects.
1. Don't share references to the mutable objects. Never store references to external, mutable objects passed to the constructor; if necessary, create copies, and store references to the copies. Similarly, create copies of your internal mutable objects when necessary to avoid returning the originals in your methods.



## Example with mutable refs


In this case class Point is mutable and some user can modify state of object of this class.

```java
class Point {
    private int x, y;

    public Point(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public int getX() {
        return x;
    }
    
    public void setX(int x) {
        this.x = x;
    }
    
    public int getY() {
        return y;
    }

    public void setY(int y) {
        this.y = y;
    }
}

//...

public final class ImmutableCircle {
    private final Point center;
    private final double radius;

    public ImmutableCircle(Point center, double radius) {
        // we create new object here because it shouldn't be changed
        this.center = new Point(center.getX(), center.getY());
        this.radius = radius;
    }

```



#### Remarks


Some immutable classes in Java:

1. java.lang.String
1. The wrapper classes for the primitive types: java.lang.Integer, java.lang.Byte, java.lang.Character, java.lang.Short, java.lang.Boolean, java.lang.Long, java.lang.Double, java.lang.Float
1. Most enum classes are immutable, but this in fact depends on the concrete case.
1. java.math.BigInteger and java.math.BigDecimal (at least objects of those classes themselves)
1. java.io.File. Note that this represents an object external to the VM (a file on the local system), which may or may not exist, and has some methods modifying and querying the state of this external object. But the File object itself stays immutable.

