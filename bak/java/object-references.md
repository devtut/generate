---
metaTitle: "Java - Object References"
description: "Object References as method parameters"
---

# Object References



## Object References as method parameters


This topic explains the concept of an **object reference**; it is targeted at people who are new to programming in Java. You should already be familiar with some terms and meanings: class definition, main method, object instance, and the calling of methods "on" an object, and passing parameters to methods.

```java
public class Person {

  private String name;

  public void setName(String name) { this.name = name; }

  public String getName() { return name; }

  public static void main(String [] arguments) {
    Person person = new Person();
    person.setName("Bob");

    int i = 5;
    setPersonName(person, i);

    System.out.println(person.getName() + " " + i);
  }

  private static void setPersonName(Person person, int num) {
    person.setName("Linda");
    num = 99;
  }
}

```

To be fully competent in Java programming, you should be able to explain this example to someone else off the top of your head. Its concepts are fundamental to understanding how Java works.

As you can see, we have a `main` that instantiates an object to the variable `person`, and calls a method to set the `name` field in that object to `"Bob"`. Then it calls another method, and passes `person` as one of two parameters; the other parameter is an integer variable, set to 5.

The method called sets the `name` value on the passed object to "Linda', and sets the integer variable passed to 99, then returns.

So what would get printed?

```java
Linda 5

```

So why does the change made to `person` take effect in `main`, but the change made to the integer does not?

When the call is made, the main method passes an **object reference** for `person` to the `setPersonName` method; any change that `setAnotherName` makes to that object is part of that object, and so those changes are still part of that object when the method returns.

Another way of saying the same thing: `person` points to an object (stored on the heap, if you're interested). Any change the method makes to that object are made "on that object", and are not affected by whether the method making the change is still active or has returned. When the method returns, any changes made to the object are still stored on that object.

Contrast this with the integer that is passed. Since this is a [**primitive**](http://stackoverflow.com/documentation/java/148/primitive-data-types#t=201609241023157023623) int (and not an Integer object instance), it is passed "by value", meaning its value is provided to the method, not a pointer to the original integer passed in. The method can change it for the method's own purposes, but that does not affect the variable used when the method call is made.

In Java, all primitives are passed by value. Objects are passed by reference, which means that a pointer to the object is passed as the parameter to any methods that take them.

One less-obvious thing this means: it is not possible for a called method to create a **new** object and return it as one of the parameters. The only way for a method to return an object that is created, directly or indirectly, by the method call, is as a return value from the method. Let's first see how that would not work, and then how it would work.

Let's add another method to our little example here:

```java
private static void getAnotherObjectNot(Person person) {
  person = new Person();
  person.setName("George");
}

```

And, back in the `main`, below the call to `setAnotherName`, let's put a call to this method and another println call:

```java
getAnotherObjectNot(person);
System.out.println(person.getName());

```

Now the program would print out:

```java
Linda 5
Linda

```

What happened to the object that had George? Well, the parameter that was passed in was a pointer to Linda; when the `getAnotherObjectNot` method created a new object, it replaced the reference to the Linda object with a reference to the George object. The Linda object still exists (on the heap), the `main` method can still access it, but the `getAnotherObjectNot` method wouldn't be able to do anything with it after that, because it has no reference to it. It would appear that the writer of the code intended for the method to create a new object and pass it back, but if so, it didn't work.

If that is what the writer wanted to do, he would need to return the newly created object from the method, something like this:

```java
private static Person getAnotherObject() {
  Person person = new Person();
  person.setName("Mary");
  return person;
}

```

Then call it like this:

```java
Person mary;
mary = getAnotherObject();
System.out.println(mary.getName());

```

And the entire program output would now be:

```java
Linda 5
Linda
Mary

```

Here is the entire program, with both additions:

```java
public class Person {
  private String name;

  public void setName(String name) { this.name = name; }
  public String getName() { return name; }

  public static void main(String [] arguments) {
    Person person = new Person();
    person.setName("Bob");

    int i = 5;
    setPersonName(person, i);
    System.out.println(person.getName() + " " + i);
    
    getAnotherObjectNot(person);
    System.out.println(person.getName());
    
    Person person;
    person = getAnotherObject();
    System.out.println(person.getName());
  }
  
  private static void setPersonName(Person person, int num) {
    person.setName("Linda");
    num = 99;
  }
  
  private static void getAnotherObjectNot(Person person) {
    person = new Person();
    person.setMyName("George");
  }
  
  private static person getAnotherObject() {
    Person person = new Person();
    person.setMyName("Mary");
    return person;
  }
}

```



#### Remarks


This should help you understand a "Null Pointer Exception" -- one gets one of those because an object reference is null, but the program code expects the program to use something in that object reference. However, that deserves its own topic...

