---
metaTitle: "Java - Object Cloning"
description: "Cloning performing a deep copy, Cloning using a copy constructor, Cloning by implementing Clonable interface, Cloning performing a shallow copy, Cloning using a copy factory"
---

# Object Cloning




## Cloning performing a deep copy


To copy nested objects, a [deep copy](https://en.wikipedia.org/wiki/Object_copying#Deep_copy) must be performed, as shown in this example.

```java
import java.util.ArrayList;
import java.util.List;

public class Sheep implements Cloneable {

    private String name;

    private int weight;

    private List<Sheep> children;

    public Sheep(String name, int weight) {
        this.name = name;
        this.weight = weight;
    }

    @Override
    public Object clone() throws CloneNotSupportedException {
        Sheep clone = (Sheep) super.clone();
        if (children != null) {
            // make a deep copy of the children
            List<Sheep> cloneChildren = new ArrayList<>(children.size());
            for (Sheep child : children) {
                cloneChildren.add((Sheep) child.clone());
            }
            clone.setChildren(cloneChildren);
        }
        return clone;
    }

    public List<Sheep> getChildren() {
        return children;
    }

    public void setChildren(List<Sheep> children) {
        this.children = children;
    }

}

import java.util.Arrays;
import java.util.List;

// create a sheep
Sheep sheep = new Sheep("Dolly", 20);

// create children
Sheep child1 = new Sheep("Child1", 4);
Sheep child2 = new Sheep("Child2", 5);

sheep.setChildren(Arrays.asList(child1, child2));

// clone the sheep
Sheep dolly =  (Sheep) sheep.clone();
List<Sheep> sheepChildren = sheep.getChildren();
List<Sheep> dollysChildren = dolly.getChildren();
for (int i = 0; i < sheepChildren.size(); i++) {
    // prints false, both arrays contain copies of the objects inside
    System.out.println(sheepChildren.get(i) == dollysChildren.get(i));
}

```



## Cloning using a copy constructor


An easy way to clone an object is by implementing a copy constructor.

```java
public class Sheep {

    private String name;

    private int weight;

    public Sheep(String name, int weight) {
        this.name = name;
        this.weight = weight;
    }

    // copy constructor
    // copies the fields of other into the new object
    public Sheep(Sheep other) {
        this.name = other.name;
        this.weight = other.weight;
    }

}

// create a sheep
Sheep sheep = new Sheep("Dolly", 20);
// clone the sheep
Sheep dolly = new Sheep(sheep); // dolly.name is "Dolly" and dolly.weight is 20

```



## Cloning by implementing Clonable interface


Cloning an object by implementing the [Cloneable](https://docs.oracle.com/javase/8/docs/api/java/lang/Cloneable.html) interface.

```java
public class Sheep implements Cloneable {

    private String name;

    private int weight;

    public Sheep(String name, int weight) {
        this.name = name;
        this.weight = weight;
    }

    @Override
    public Object clone() throws CloneNotSupportedException {
        return super.clone();
    }

}

// create a sheep
Sheep sheep = new Sheep("Dolly", 20);
// clone the sheep
Sheep dolly =  (Sheep) sheep.clone(); // dolly.name is "Dolly" and dolly.weight is 20

```



## Cloning performing a shallow copy


Default behavior when cloning an object is to perform a [shallow copy](https://en.wikipedia.org/wiki/Object_copying#Shallow_copy) of the object's fields. In that case, both the original object and the cloned object, hold references to the same objects.

This example shows that behavior.

```java
import java.util.List;

public class Sheep implements Cloneable {

    private String name;

    private int weight;

    private List<Sheep> children;

    public Sheep(String name, int weight) {
        this.name = name;
        this.weight = weight;
    }

    @Override
    public Object clone() throws CloneNotSupportedException {
        return super.clone();
    }

    public List<Sheep> getChildren() {
        return children;
    }

    public void setChildren(List<Sheep> children) {
        this.children = children;
    }

}

import java.util.Arrays;
import java.util.List;

// create a sheep
Sheep sheep = new Sheep("Dolly", 20);

// create children
Sheep child1 = new Sheep("Child1", 4);
Sheep child2 = new Sheep("Child2", 5);

sheep.setChildren(Arrays.asList(child1, child2));

// clone the sheep
Sheep dolly =  (Sheep) sheep.clone();
List<Sheep> sheepChildren = sheep.getChildren();
List<Sheep> dollysChildren = dolly.getChildren();
for (int i = 0; i < sheepChildren.size(); i++) {
    // prints true, both arrays contain the same objects
    System.out.println(sheepChildren.get(i) == dollysChildren.get(i));
}

```



## Cloning using a copy factory


```java
public class Sheep {

    private String name;
    
    private int weight;
    
    public Sheep(String name, int weight) {
        this.name = name;
        this.weight = weight;
    }
    
    public static Sheep newInstance(Sheep other);
        return new Sheep(other.name, other.weight)
    }

}

```



#### Remarks


Cloning can be tricky, especially when the object's fields hold other objects. There are situations where you want to perform a [deep copy](https://en.wikipedia.org/wiki/Object_copying#Deep_copy), instead of only copying the field values (i.e. references to the other objects).

The bottom line is [clone is broken](http://www.artima.com/intv/bloch13.html), and you should think twice before implementing the `Cloneable` interface and overriding the `clone` method. The `clone` method is declared in the `Object` class and not in the `Cloneable` interface, so `Cloneable` fails to function as an interface because it lacks a public `clone` method. The result is the contract for using `clone` is thinly documented and weakly enforced. For example, a class that overrides `clone` sometimes relies on all its parent classes also overriding `clone`. They are not enforced to do so, and if they do not your code may throw exceptions.

A much better solution for providing cloning functionality is to provide a **copy constructor** or **copy factory**. Refer to [Joshua Bloch's Effective Java](https://amzn.com/B00B8V09HY) Item 11: Override clone judiciously.

