---
metaTitle: "Java - Immutable Objects"
description: "Creating an immutable version of a type using defensive copying., The recipe for an immutable class, Typical design flaws which prevent a class from being immutable"
---

# Immutable Objects



## Creating an immutable version of a type using defensive copying.


Some basic types and classes in Java are fundamentally mutable.  For example, all array types are mutable, and so are classes like `java.util.Data`.  This can be awkward in situations where an immutable type is mandated.

One way to deal with this is to create an immutable wrapper for the mutable type.  Here is a simple wrapper for an array of integers

```java
public class ImmutableIntArray {
    private final int[] array;
    
    public ImmutableIntArray(int[] array) {
        this.array = array.clone();
    }

    public int[] getValue() {
        return this.clone();
    }
}

```

This class works by using **defensive copying** to isolate the mutable state (the `int[]`) from any code that might mutate it:

<li>
The constructor uses `clone()` to create a distinct copy of the parameter array.  If the caller of the constructor subsequent changed the parameter array, it would not affect the state of the `ImmutableIntArray`.
</li>
<li>
The `getValue()` method also uses `clone()` to create the array that is returned.  If the caller were to change the result array, it would not affect the state of the `ImmutableIntArray`.
</li>

We could also add methods to `ImmutableIntArray` to perform read-only operations on the wrapped array; e.g. get its length, get the value at a particular index, and so on.

Note that an immutable wrapper type implemented this way is not type compatible with the original type.  You cannot simply substitute the former for the latter.



## The recipe for an immutable class


An immutable object is an object whose state cannot be changed.  An immutable class is a class whose instances are immutable by design, and implementation.  The Java class which is most commonly presented as an example of immutability is [java.lang.String](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html).

The following is a stereotypical example:

```java
public final class Person {
    private final String name;
    private final String ssn;     // (SSN == social security number)

    public Person(String name, String ssn) {
        this.name = name;
        this.ssn = ssn;
    }

    public String getName() {
        return name;
    }
   
    public String getSSN() {
        return ssn;
    }
}

```

A variation on this is to declare the constructor as `private` and provide a `public static` factory method instead.

The  **standard recipe** for an immutable class is as follows:

- All properties must be set in the constructor(s) or factory method(s).
- There should be no setters.
- If it is necessary to include setters for interface compatibility reasons, they should either do nothing or throw an exception.
- All properties should be declared as `private` and `final`.
<li>For all properties that are references to mutable types:
<ul>
- the property should be initialized with a deep copy of the value passed via the constructor, and
- the property's getter should return a deep copy of the property value.

A couple of other things to note:

- Immutability does not prevent object from being nullable; e.g. `null` can be assigned to a `String` variable.
- If an immutable classes properties are declared as `final`, instances are inherently thread-safe.  This makes immutable classes a good building block for implementing multi-threaded applications.



## Typical design flaws which prevent a class from being immutable


**Using some setters, without setting all needed properties in the constructor(s)**

```java
public final class Person { // example of a bad immutability
    private final String name;
    private final String surname;
    public Person(String name) {
        this.name = name;
      }
    public String getName() { return name;}
    public String getSurname() { return surname;}
    public void setSurname(String surname) { this.surname = surname); }
}

```

It’s easy to show that `Person` class is not immutable:

```java
Person person = new Person("Joe");
person.setSurname("Average"); // NOT OK, change surname field after creation

```

To fix it, simply delete `setSurname()` and refactor the constructor as follows:

```java
public Person(String name, String surname) {
    this.name = name;
    this.surname = surname;
  }

```

**Not marking instance variables as private and final**

Take a look at the following class:

```java
public final class Person {
    public String name;
    public Person(String name) {
        this.name = name;
     }
    public String getName() {
        return name;
    }
    
}

```

The following snippet shows that the above class is not immutable:

```java
Person person = new Person("Average Joe");
person.name = "Magic Mike"; // not OK, new name for person after creation

```

To fix it, simply mark name property as `private` and `final`.

**Exposing a mutable object of the class in a getter**

Take a look at the following class:

```java
import java.util.List;
import java.util.ArrayList;
public final class Names {
    private final List<String> names;
    public Names(List<String> names) {
        this.names = new ArrayList<String>(names);
    }
    public List<String> getNames() {
        return names;
    }
    public int size() {
        return names.size();
    }
}

```

`Names` class seems immutable at the first sight, but it is not as the following code shows:

```java
List<String> namesList = new ArrayList<String>();
namesList.add("Average Joe");
Names names = new Names(namesList);
System.out.println(names.size()); // 1, only containing "Average Joe"
namesList = names.getNames();
namesList.add("Magic Mike");
System.out.println(names.size()); // 2, NOT OK, now names also contains "Magic Mike"

```

This happened because a change to the reference List returned by `getNames()` can modify the actual list of `Names`.

To fix this, simply avoid returning references that reference class's mutable objects **either** by making defensive copies, as follows:

```java
public List<String> getNames() {
   return new ArrayList<String>(this.names); // copies elements
}

```

**or** by designing getters in way that only other **immutable objects** and **primitives** are returned, as follows:

```java
public String getName(int index) {
    return names.get(index);
}
public int size() {
    return names.size();
}

```

**Injecting constructor with object(s) that can be modified outside the immutable class**

This is a variation of the previous flaw. Take a look at the following class:

```java
import java.util.List;
public final class NewNames {
    private final List<String> names;
    public Names(List<String> names) {
        this.names = names;
    }
    public String getName(int index) {
        return names.get(index);
    }
    public int size() {
        return names.size();
    }
}

```

As `Names` class before, also `NewNames` class seems immutable at the first sight, but it is not, in fact the following snippet proves the contrary:

```java
List<String> namesList = new ArrayList<String>();
namesList.add("Average Joe");
NewNames names = new NewNames(namesList);
System.out.println(names.size()); // 1, only containing "Average Joe"
namesList.add("Magic Mike");
System.out.println(names.size()); // 2, NOT OK, now names also contains "Magic Mike"

```

To fix this, as in the previous flaw, simply make defensive copies of the object without assigning it directly to the immutable class, i.e. constructor can be changed as follows:

```

   public Names(List<String> names) {
        this.names = new ArrayList<String>(names);
    }

```

**Letting the methods of the class being overridden**

Take a look at the following class:

```java
public class Person {
    private final String name;
    public Person(String name) {
        this.name = name;
      }
    public String getName() { return name;}
}

```

`Person` class seems immutable at the first sight, but suppose a new subclass of `Person` is defined:

```java
public class MutablePerson extends Person {
    private String newName;
    public MutablePerson(String name) {
        super(name);            
    }
    @Override
    public String getName() {
        return newName;
    }
    public void setName(String name) {
        newName = name;
    }
}

```

now `Person` (im)mutability can be exploited through polymorphism by using the new subclass:

```java
Person person = new MutablePerson("Average Joe");
System.out.println(person.getName()); prints Average Joe
person.setName("Magic Mike"); // NOT OK, person has now a new name!
System.out.println(person.getName()); // prints Magic Mike    

```

To fix this, **either** mark the class as `final` so it cannot be extended **or** declare all of its constructor(s) as `private`.



#### Remarks


Immutable objects have fixed state (no setters), so all state must be known at object creation time.

Although not technically required, it is best practice to make all fields `final`. This will make the immutable class thread-safe (cf. Java Concurrency in Practice, 3.4.1).

The examples show several patterns that can assist with achieving this.

