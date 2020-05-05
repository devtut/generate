---
metaTitle: "Java - Comparable and Comparator"
description: "Sorting a List using Comparable<T> or a Comparator<T>, The compareTo and compare Methods, Natural (comparable) vs explicit (comparator) sorting, Creating a Comparator using comparing method, Sorting Map entries"
---

# Comparable and Comparator




## Sorting a List using Comparable<T> or a Comparator<T>


Say we are working on a class representing a Person by their first and last names. We have created a basic class to do this and implemented proper `equals` and `hashCode` methods.

```java
public class Person {

    private final String lastName; //invariant - nonnull
    private final String firstName; //invariant - nonnull

    public Person(String firstName, String lastName){
        this.firstName = firstName != null ? firstName : "";
        this.lastName = lastName != null ? lastName : "";
    }

    public String getFirstName() {
        return firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public String toString() {
        return lastName + ", " + firstName;
    }

    @Override
    public boolean equals(Object o) {
        if (! (o instanceof Person)) return false;
        Person p = (Person)o;
        return firstName.equals(p.firstName) && lastName.equals(p.lastName);
    }

    @Override
    public int hashCode() {
        return Objects.hash(firstName, lastName);
    }
}

```

Now we would like to sort a list of `Person` objects by their name, such as in the following scenario:

```java
public static void main(String[] args) {
    List<Person> people = Arrays.asList(new Person("John", "Doe"),
                                     new Person("Bob", "Dole"),
                                     new Person("Ronald", "McDonald"),
                                     new Person("Alice", "McDonald"),
                                     new Person("Jill", "Doe"));
    Collections.sort(people); //This currently won't work.
}

```

Unfortunately, as marked, the above currently won't compile. `Collections.sort(..)` only knows how to sort a list if the elements in that list are comparable, or a custom method of comparison is given.

If you were asked to sort the following list : `1,3,5,4,2`, you'd have no problem saying the answer is `1,2,3,4,5`. This is because Integers (both in Java and mathematically) have a **natural ordering**, a standard, default comparison base ordering. To give our Person class a natural ordering, we implement `Comparable<Person>`, which requires implementing the method `compareTo(Person p):`

```java
public class Person implements Comparable<Person> {

    private final String lastName; //invariant - nonnull
    private final String firstName; //invariant - nonnull

    public Person(String firstName, String lastName) {
        this.firstName = firstName != null ? firstName : "";
        this.lastName = lastName != null ? lastName : "";
    }

    public String getFirstName() {
        return firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public String toString() {
        return lastName + ", " + firstName;
    }

    @Override
    public boolean equals(Object o) {
        if (! (o instanceof Person)) return false;
        Person p = (Person)o;
        return firstName.equals(p.firstName) && lastName.equals(p.lastName);
    }

    @Override
    public int hashCode() {
        return Objects.hash(firstName, lastName);
    }

    @Override
    public int compareTo(Person other) {
        // If this' lastName and other's lastName are not comparably equivalent,
        // Compare this to other by comparing their last names.
        // Otherwise, compare this to other by comparing their first names
        int lastNameCompare = lastName.compareTo(other.lastName);
        if (lastNameCompare != 0) {
            return lastNameCompare;
        } else {
            return firstName.compareTo(other.firstName);
        }
    }
}

```

Now, the main method given will function correctly

```java
public static void main(String[] args) {
    List<Person> people = Arrays.asList(new Person("John", "Doe"),
                                     new Person("Bob", "Dole"),
                                     new Person("Ronald", "McDonald"),
                                     new Person("Alice", "McDonald"),
                                     new Person("Jill", "Doe"));
    Collections.sort(people); //Now functions correctly

    //people is now sorted by last name, then first name:
    // --> Jill Doe, John Doe, Bob Dole, Alice McDonald, Ronald McDonald
}

```

If, however, you either do not want or are unable to modify class `Person`, you can provide a custom `Comparator<T>` that handles the comparison of any two `Person` objects. If you were asked to sort the following list: `circle, square, rectangle, triangle, hexagon` you could not, but if you were asked to sort that list **based on the number of corners**, you could. Just so, providing a comparator instructs Java how to compare two normally not comparable objects.

```java
public class PersonComparator implements Comparator<Person> {

    public int compare(Person p1, Person p2) {
        // If p1's lastName and p2's lastName are not comparably equivalent,
        // Compare p1 to p2 by comparing their last names.
        // Otherwise, compare p1 to p2 by comparing their first names
        if (p1.getLastName().compareTo(p2.getLastName()) != 0) {
            return p1.getLastName().compareTo(p2.getLastName());
        } else {
            return p1.getFirstName().compareTo(p2.getFirstName());
        }
    }
}

//Assume the first version of Person (that does not implement Comparable) is used here
public static void main(String[] args) {
    List<Person> people = Arrays.asList(new Person("John", "Doe"),
                                     new Person("Bob", "Dole"),
                                     new Person("Ronald", "McDonald"),
                                     new Person("Alice", "McDonald"),
                                     new Person("Jill", "Doe"));
    Collections.sort(people); //Illegal, Person doesn't implement Comparable.
    Collections.sort(people, new PersonComparator()); //Legal

    //people is now sorted by last name, then first name:
    // --> Jill Doe, John Doe, Bob Dole, Alice McDonald, Ronald McDonald
}

```

Comparators can also be created/used as an anonymous inner class

```java
//Assume the first version of Person (that does not implement Comparable) is used here
public static void main(String[] args) {
    List<Person> people = Arrays.asList(new Person("John", "Doe"),
                                     new Person("Bob", "Dole"),
                                     new Person("Ronald", "McDonald"),
                                     new Person("Alice", "McDonald"),
                                     new Person("Jill", "Doe"));
    Collections.sort(people); //Illegal, Person doesn't implement Comparable.

    Collections.sort(people, new PersonComparator()); //Legal

    //people is now sorted by last name, then first name:
    // --> Jill Doe, John Doe, Bob Dole, Alice McDonald, Ronald McDonald

    //Anonymous Class
    Collections.sort(people, new Comparator<Person>() { //Legal
        public int compare(Person p1, Person p2) {
            //Method code...
        }
    });
}

```

### Lambda expression based comparators

As of Java 8, comparators can also be expressed as lambda expressions

```

   //Lambda
    Collections.sort(people, (p1, p2) -> { //Legal
        //Method code....
    });

```

### Comparator default methods

Furthermore, there are interesting default methods on the Comparator interface for building comparators : the following builds a comparator comparing by `lastName` and then `firstName`.

```java
Collections.sort(people, Comparator.comparing(Person::getLastName)
                                .thenComparing(Person::getFirstName));

```

### Inversing the order of a comparator

Any comparator can also easily be reversed using the `reversedMethod` which will change ascending order to descending.



## The compareTo and compare Methods


The `Comparable<T>` interface requires one method:

```java
public interface Comparable<T> {

    public int compareTo(T other);

}

```

And the `Comparator<T>` interface requires one method:

```java
public interface Comparator<T> {

    public int compare(T t1, T t2);

}

```

These two methods do essentially the same thing, with one minor difference: `compareTo` compares `this` to `other`, whereas `compare` compares `t1` to `t2`, not caring at all about `this`.

Aside from that difference, the two methods have similar requirements. Specifically (for compareTo),
[Compares this object with the specified object for order. Returns a negative integer, zero, or a positive integer as this object is less than, equal to, or greater than the specified object.](http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/8u40-b25/java/lang/Comparable.java#Comparable) Thus, for the comparison of `a` and `b`:

- If `a < b`, `a.compareTo(b)` and `compare(a,b)` should return a negative integer, and `b.compareTo(a)` and `compare(b,a)` should return a positive integer
- If `a > b`, `a.compareTo(b)` and `compare(a,b)` should return a positive integer, and `b.compareTo(a)` and `compare(b,a)` should return a negative integer
- If `a` equals `b` for comparison, all comparisons should return `0`.



## Natural (comparable) vs explicit (comparator) sorting


There are two `Collections.sort()` methods:

<li>One that takes a `List<T>` as a parameter where `T` must implement
Comparable and override the `compareTo()` method that determines
sort order.</li>
<li>One that takes a List and a Comparator as the arguments, where the
Comparator determines the sort order.</li>

First, here is a Person class that implements Comparable:

```java
public class Person implements Comparable<Person> {         
    private String name;  
    private int age;

    public String getName() {
        return name;
    }
    public void setName(String name) {
        this.name = name;
    }
    public int getAge() {
        return age;
    }
    public void setAge(int age) {
        this.age = age;
    }         

    @Override
    public int compareTo(Person o) {
        return this.getAge() - o.getAge();
    }
    @Override
    public String toString() {
        return this.getAge()+"-"+this.getName();
    }

}

```

Here is how you would use the above class to sort a List in the natural ordering of its elements, defined by the `compareTo()` method override:

```java
//-- usage
List<Person> pList = new ArrayList<Person>();
            Person p = new Person();
            p.setName("A");
            p.setAge(10);
            pList.add(p);
            p = new Person();
            p.setName("Z");
            p.setAge(20);
            pList.add(p);
            p = new Person();
            p.setName("D");
            p.setAge(30);
            pList.add(p);
            
            //-- natural sorting i.e comes with object implementation, by age
            Collections.sort(pList);

            System.out.println(pList);

```

Here is how you would use an anonymous inline Comparator to sort a List that does not implement Comparable, or in this case, to sort a List in an order other than the natural ordering:

```

           //-- explicit sorting, define sort on another property here goes with name
            Collections.sort(pList, new Comparator<Person>() {

                @Override
                public int compare(Person o1, Person o2) {
                    return o1.getName().compareTo(o2.getName());
                }
            });            
            System.out.println(pList);

```



## Creating a Comparator using comparing method


```java
Comparator.comparing(Person::getName)

```

This creates a comparator for the class `Person` that uses this person name as the comparison source.
Also it is possible to use method version to compare long, int and double. For example:

```java
Comparator.comparingInt(Person::getAge)

```

**Reversed order**

To create a comparator that imposes the reverse ordering use `reversed()` method:

```java
Comparator.comparing(Person::getName).reversed()

```

**Chain of comparators**

```java
Comparator.comparing(Person::getLastName).thenComparing(Person::getFirstName)

```

This will create a comparator that firs compares with last name then compares with first name. You can chain as many comparators as you want.



## Sorting Map entries


As of Java 8, there are default methods on the `Map.Entry` interface to allow sorting of map iterations.

```java
Map<String, Integer> numberOfEmployees = new HashMap<>();
numberOfEmployees.put("executives", 10);
numberOfEmployees.put("human ressources", 32);
numberOfEmployees.put("accounting", 12);
numberOfEmployees.put("IT", 100);

// Output the smallest departement in terms of number of employees
numberOfEmployees.entrySet().stream()
    .sorted(Map.Entry.comparingByValue())
    .limit(1)
    .forEach(System.out::println);   // outputs : executives=10

```

Of course, these can also be used outside of the stream api :

```java
List<Map.Entry<String, Integer>> entries = new ArrayList<>(numberOfEmployees.entrySet());
Collections.sort(entries, Map.Entry.comparingByValue());

```



#### Syntax


- public class MyClass implements Comparable`<MyClass`>
- public class MyComparator implements Comparator`<SomeOtherClass`>
- public int compareTo(MyClass other)
- public int compare(SomeOtherClass o1, SomeOtherClass o2)



#### Remarks


When implementing a `compareTo(..)` method which depends upon a `double`, **do not** do the following:

```java
public int comareTo(MyClass other) {
    return (int)(doubleField - other.doubleField); //THIS IS BAD
}

```

The truncation caused by the `(int)` cast will cause the method to sometimes incorrectly return `0` instead of a positive or negative number, and can thus lead to comparison and sorting bugs.

Instead, the simplest correct implementation is to use [Double.compare](http://docs.oracle.com/javase/8/docs/api/java/lang/Double.html#compare-double-double-), as such:

```java
public int comareTo(MyClass other) {
    return Double.compare(doubleField,other.doubleField); //THIS IS GOOD
} 

```

A non-generic version of `Comparable<T>`, simply `Comparable`, [has existed since Java 1.2](http://docs.oracle.com/javame/config/cdc/ref-impl/pp1.1.2/jsr216/java/lang/Comparable.html#skip-navbar_top). Other than for interfacing with legacy code, it's always better to implement the generic version `Comparable<T>`, as it doesn't require casting upon comparison.

It is very standard for a class to be comparable to itself, as in:

```java
public class A implements Comparable<A>

```

While it is possible to break from this paradigm, be cautious when doing so.

A `Comparator<T>` can still be used on instances of a class if that class implements `Comparable<T>`. In this case, the `Comparator`'s logic will be used; the natural ordering specified by the `Comparable` implementation will be ignored.

