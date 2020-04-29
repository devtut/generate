---
metaTitle: "Fluent Interface"
description: "Fluent programming style, Truth - Fluent Testing Framework"
---

# Fluent Interface



## Fluent programming style


In fluent programming style you return `this` from fluent (setter) methods that would return nothing in non-fluent programming style.

This allows you to chain the different method calls which makes your code shorter and easier to handle for the developers.

Consider this non-fluent code:

```java
public class Person {
  private  String firstName;
  private String lastName;

  public String getFirstName() {
    return firstName;
  }

  public void setFirstName(String firstName) {
    this.firstName = firstName;
  }

  public String getLastName() {
    return lastName;
  }

  public void setLastName(String lastName) {
    this.lastName = lastName;
  }

  public String whoAreYou() {
    return "I am " + firstName + " " + lastName;
  }

  public static void main(String[] args) {
    Person person = new Person();
    person.setFirstName("John");
    person.setLastName("Doe");
    System.out.println(person.whoAreYou());
  }
}

```

As the setter methods don't return anything, we need 4 instructions in the `main`method to instantiate a `Person` with some data and print it. With a fluent style this code can be changed to:

```java
public class Person {
  private  String firstName;
  private String lastName;

  public String getFirstName() {
    return firstName;
  }

  public Person withFirstName(String firstName) {
    this.firstName = firstName;
    return this;
  }

  public String getLastName() {
    return lastName;
  }

  public Person withLastName(String lastName) {
    this.lastName = lastName;
    return this;
  }

  public String whoAreYou() {
    return "I am " + firstName + " " + lastName;
  }

  public static void main(String[] args) {
    System.out.println(new Person().withFirstName("John")
      .withLastName("Doe").whoAreYou());
  }
}

```

The idea is to always return some object to enable building of a method call chain and to use method names which reflect natural speaking.
This fluent style makes the code more readable.



## Truth - Fluent Testing Framework


From "How to use Truth" [http://google.github.io/truth/](http://google.github.io/truth/)

```java
String string = "awesome";
assertThat(string).startsWith("awe");
assertWithMessage("Without me, it's just aweso").that(string).contains("me");

Iterable<Color> googleColors = googleLogo.getColors();
assertThat(googleColors)
    .containsExactly(BLUE, RED, YELLOW, BLUE, GREEN, RED)
    .inOrder();

```



#### Remarks


**Goals**

The primary goal of a Fluent Interface is increased readability.

When used for constructing objects, the choices available to the caller can be made clearly and enforced via compile-time checks. For example, consider the following tree of options representing steps along the path to construct some complex object:

```java
A -> B 
  -> C -> D -> Done
       -> E -> Done
       -> F -> Done.
       -> G -> H -> I -> Done.

```

A builder using a fluent interface would allow the caller to easily see what options are available at each step.  For example, **A -> B** is possible, but **A -> C** is not and would result in a compile-time error.

