---
metaTitle: "Enums"
description: "Declaring and using a basic enum, Enums with constructors, Enums with Abstract Methods, Implements Interface, Implement Singleton pattern with a single-element enum, Using methods and static blocks, Documenting enums, Enum as a bounded type parameter, Enum constant specific body, Zero instance enum, Getting the values of an enum, Enum Polymorphism Pattern, Compare and Contains for Enum values, Get enum constant by name, Enum with properties (fields), Convert enum to String, Enums with static fields"
---

# Enums




## Declaring and using a basic enum


[Enum](https://docs.oracle.com/javase/tutorial/java/javaOO/enum.html) can be considered to be syntax sugar for a sealed class that is instantiated only a number of times known at compile-time to define a set of constants.

A simple enum to list the different seasons would be declared as follows:

```java
public enum Season {
    WINTER,
    SPRING,
    SUMMER,
    FALL
}

```

While the enum constants don't necessarily need to be in all-caps, it is Java convention that names of constants are entirely uppercase, with words separated by underscores.

You can declare an Enum in its own file:

```java
/**
 * This enum is declared in the Season.java file.
*/
public enum Season {
    WINTER,
    SPRING,
    SUMMER,
    FALL
}

```

But you can also declare it inside another class:

```

public class Day {

    private Season season;

    public String getSeason() {
        return season.name();
    }

    public void setSeason(String season) {
        this.season = Season.valueOf(season);
    }

    /**
     * This enum is declared inside the Day.java file and 
     * cannot be accessed outside because it's declared as private.
     */
    private enum Season {
        WINTER,
        SPRING,
        SUMMER,
        FALL
    }

}

```

Finally, you cannot declare an Enum inside a method body or constructor:

```java
public class Day {

    /**
     * Constructor
    */
    public Day() {
        // Illegal. Compilation error
        enum Season {
            WINTER,
            SPRING,
            SUMMER,
            FALL
        }
    }

    public void aSimpleMethod() {
        // Legal. You can declare a primitive (or an Object) inside a method. Compile!
        int primitiveInt = 42;

        // Illegal. Compilation error.
        enum Season {
            WINTER,
            SPRING,
            SUMMER,
            FALL
        }

        Season season = Season.SPRING;
    }
    
}

```

Duplicate enum constants are not allowed:

```java
public enum Season {
    WINTER,
    WINTER, //Compile Time Error : Duplicate Constants
    SPRING,
    SUMMER,
    FALL
} 

```

Every constant of enum is **`public`, `static`** and **`final`** by default. As every constant is `static`, they can be accessed directly using the enum name.

Enum constants can be passed around as method parameters:

```java
public static void display(Season s) {
    System.out.println(s.name());  // name() is a built-in method that gets the exact name of the enum constant
}

display(Season.WINTER);  // Prints out "WINTER"

```

You can get an array of the enum constants using the `values()` method. The values are guaranteed to be in declaration order in the returned array:

```java
Season[] seasons = Season.values();

```

**Note: this method allocates a new array of values each time it is called.**

To iterate over the enum constants:

```java
public static void enumIterate() {
    for (Season s : Season.values()) {
        System.out.println(s.name());
    }
}

```

You can use enums in a `switch` statement:

```java
public static void enumSwitchExample(Season s) {
    switch(s) {
        case WINTER:
            System.out.println("It's pretty cold");
            break;
        case SPRING:
            System.out.println("It's warming up");
            break;
        case SUMMER:
            System.out.println("It's pretty hot");
            break;
        case FALL:
            System.out.println("It's cooling down");
            break;
    }
}

```

You can also compare enum constants using `==`:

```java
Season.FALL == Season.WINTER    // false
Season.SPRING == Season.SPRING  // true

```

Another way to compare enum constants is by using `equals()` as below, which is considered bad practice as you can easily fall into pitfalls as follows:

```java
Season.FALL.equals(Season.FALL); // true
Season.FALL.equals(Season.WINTER); // false
Season.FALL.equals("FALL"); // false and no compiler error

```

Furthermore, although the set of instances in the `enum` cannot be changed at run-time, the instances themselves are not inherently immutable because like any other class, an `enum` can contain mutable fields as is demonstrated below.

```java
public enum MutableExample {
    A,
    B;

    private int count = 0;

    public void increment() {
        count++;
    }

    public void print() {
        System.out.println("The count of " + name() + " is " + count);
    }
}

// Usage:
MutableExample.A.print();       // Outputs 0
MutableExample.A.increment();
MutableExample.A.print();       // Outputs 1 -- we've changed a field   
MutableExample.B.print();       // Outputs 0 -- another instance remains unchanged

```

However, a good practice is to make `enum` instances immutable, i.e. when they either don't have any additional fields or all such fields are marked as `final` and are immutable themselves. This will ensure that for a lifetime of the application an `enum` won't leak any memory and that it is safe to use its instances across all threads.

Enums implicitly implement `Serializable` and `Comparable` because the `Enum` class does:

```java
public abstract class Enum<E extends Enum<E>>
extends Object
implements Comparable<E>, Serializable

```



## Enums with constructors


An `enum` cannot have a public constructor; however, private constructors are acceptable (constructors for enums are [package-private](http://stackoverflow.com/documentation/java/134/visibility-controlling-access-to-members-of-a-class/520/package-visibility#t=201609151811172109684) by default):

```java
public enum Coin {
    PENNY(1), NICKEL(5), DIME(10), QUARTER(25); // usual names for US coins
    // note that the above parentheses and the constructor arguments match
    private int value;

    Coin(int value) { 
        this.value = value;
    }

    public int getValue() {
        return value;
    }
}

int p = Coin.NICKEL.getValue(); // the int value will be 5

```

It is recommended that you keep all fields private and provide getter methods, as there are a finite number of instances for an enum.

If you were to implement an `Enum` as a `class` instead, it would look like this:

```java
public class Coin<T extends Coin<T>> implements Comparable<T>, Serializable{
    public static final Coin PENNY = new Coin(1);
    public static final Coin NICKEL = new Coin(5);
    public static final Coin DIME = new Coin(10);
    public static final Coin QUARTER = new Coin(25);

    private int value;

    private Coin(int value){
        this.value = value;
    }

    public int getValue() {
        return value;
    }
}

int p = Coin.NICKEL.getValue(); // the int value will be 5

```

Enum constants are technically mutable, so a setter could be added to change the internal structure of an enum constant. However, this is considered very bad practice and should be avoided.

Best practice is to make Enum fields immutable, with `final`:

```java
public enum Coin {
    PENNY(1), NICKEL(5), DIME(10), QUARTER(25);

    private final int value;

    Coin(int value){ 
        this.value = value;
    }

    ...

}

```

You may define multiple constructors in the same enum. When you do, the arguments you pass in your enum declaration decide which constructor is called:

```java
public enum Coin {
    PENNY(1, true), NICKEL(5, false), DIME(10), QUARTER(25);

    private final int value;
    private final boolean isCopperColored;

    Coin(int value){
        this(value, false);
    }

    Coin(int value, boolean isCopperColored){ 
        this.value = value;
        this.isCopperColored = isCopperColored;
    }

    ...

}

```

Note: All non-primitive enum fields should implement [`Serializable`](https://docs.oracle.com/javase/7/docs/api/java/io/Serializable.html) because the [`Enum`](https://docs.oracle.com/javase/7/docs/api/java/lang/Enum.html) class does.



## Enums with Abstract Methods


Enums can define abstract methods, which each `enum` member is required to implement.

```java
enum Action {
    DODGE {
        public boolean execute(Player player) {
            return player.isAttacking();
        }
    },
    ATTACK {
        public boolean execute(Player player) {
            return player.hasWeapon();
        }
    },
    JUMP {
        public boolean execute(Player player) {
            return player.getCoordinates().equals(new Coordinates(0, 0));
        }
    };

    public abstract boolean execute(Player player);
}

```

This allows for each enum member to define its own behaviour for a given operation, without having to switch on types in a method in the top-level definition.

Note that this pattern is a short form of what is typically achieved using polymorphism and/or implementing interfaces.



## Implements Interface


This is an `enum` that is also a callable function that tests `String` inputs against precompiled regular expression patterns.

```java
import java.util.function.Predicate;
import java.util.regex.Pattern;

enum RegEx implements Predicate<String> {
    UPPER("[A-Z]+"), LOWER("[a-z]+"), NUMERIC("[+-]?[0-9]+");

    private final Pattern pattern;

    private RegEx(final String pattern) {
        this.pattern = Pattern.compile(pattern);
    }

    @Override 
    public boolean test(final String input) {
        return this.pattern.matcher(input).matches();
    }
}

public class Main {
    public static void main(String[] args) {
        System.out.println(RegEx.UPPER.test("ABC"));
        System.out.println(RegEx.LOWER.test("abc"));
        System.out.println(RegEx.NUMERIC.test("+111"));
    }
}

```

Each member of the enum can also implement the method:

```java
import java.util.function.Predicate;

enum Acceptor implements Predicate<String> {
    NULL {
        @Override
        public boolean test(String s) { return s == null; }
    },
    EMPTY {
        @Override
        public boolean test(String s) { return s.equals(""); }
    },
    NULL_OR_EMPTY {
        @Override
        public boolean test(String s) { return NULL.test(s) || EMPTY.test(s); }
    };
}

public class Main {
    public static void main(String[] args) {
        System.out.println(Acceptor.NULL.test(null));  // true
        System.out.println(Acceptor.EMPTY.test(""));   // true
        System.out.println(Acceptor.NULL_OR_EMPTY.test(" ")); // false
    }
}

```



## Implement Singleton pattern with a single-element enum


Enum constants are instantiated when an enum is referenced for the first time. Therefore, that allows to implement [Singleton](http://stackoverflow.com/documentation/java/130/singletons) software design pattern with a single-element enum.

```java
public enum Attendant {

    INSTANCE;

    private Attendant() {
        // perform some initialization routine
    }

    public void sayHello() {
        System.out.println("Hello!");
    }
}


public class Main {

    public static void main(String... args) {
        Attendant.INSTANCE.sayHello();// instantiated at this point
    }
}

```

According to "Effective Java" book by Joshua Bloch, a single-element enum is the best way to implement a singleton. This approach has following advantages:

- thread safety
- guarantee of single instantiation
- out-of-the-box serialization

And as shown in the section [implements interface](http://stackoverflow.com/documentation/java/155/enums/1809/implements-interface) this singleton might also implement one or more interfaces.



## Using methods and static blocks


An enum can contain a method, just like any class. To see how this works, we'll declare an enum like this:

```java
public enum Direction {
    NORTH, SOUTH, EAST, WEST;
}

```

Let's have a method that returns the enum in the opposite direction:

```java
public enum Direction {
    NORTH, SOUTH, EAST, WEST;

    public Direction getOpposite(){
        switch (this){
            case NORTH:
                return SOUTH;               
            case SOUTH:
                return NORTH;                
            case WEST:
                return EAST; 
            case EAST:
                return WEST;  
            default: //This will never happen
                return null;
        }
    }
}

```

This can be improved further through the use of fields and static initializer blocks:

```java
public enum Direction {
    NORTH, SOUTH, EAST, WEST;
    
    private Direction opposite;
    
    public Direction getOpposite(){
        return opposite;
    }
    
    static {
        NORTH.opposite = SOUTH;
        SOUTH.opposite = NORTH;
        WEST.opposite = EAST;
        EAST.opposite = WEST;
    }
}

```

In this example, the opposite direction is stored in a private instance field `opposite`, which is statically initialized the first time a `Direction` is used.
In this particular case (because `NORTH` references `SOUTH` and conversely), we cannot use [Enums with constructors](http://stackoverflow.com/documentation/java/155/enums/602/enums-with-constructors#t=201607220740460629821) here (Constructors `NORTH(SOUTH), SOUTH(NORTH), EAST(WEST), WEST(EAST)` would be more elegant and would allow `opposite` to be declared `final`, but would be self-referential and therefore are not allowed).



## Documenting enums


Not always the `enum` name is clear enough to be understood. To document an `enum`, use standard javadoc:

```java
/**
 * United States coins
 */
public enum Coins {
    
    /**
     * One-cent coin, commonly known as a penny, 
     * is a unit of currency equaling one-hundredth 
     * of a United States dollar
     */
    PENNY(1),

    /**
     * A nickel is a five-cent coin equaling 
     * five-hundredth of a United States dollar
     */        
    NICKEL(5),

    /**
     * The dime is a ten-cent coin refers to 
     * one tenth of a United States dollar
     */        
    DIME(10),

    /**
     * The quarter is a US coin worth 25 cents, 
     * one-fourth of a United States dollar
     */        
    QUARTER(25);

    private int value;

    Coins(int value){ 
        this.value = value;
    }

    public int getValue(){
        return value;
    }
}

```



## Enum as a bounded type parameter


When writing a class with generics in java, it is possible to ensure that the type parameter is an enum. Since all enums extend the `Enum` class, the following syntax may be used.

```java
public class Holder<T extends Enum<T>> {
    public final T value;

    public Holder(T init) {
        this.value = init;
    }
}

```

In this example, the type `T` **must** be an enum.



## Enum constant specific body


In an `enum` it is possible to define a specific behavior for a particular constant of the `enum` which overrides the default behavior of the `enum`, this technique is known as **constant specific body**.

Suppose three piano students - John, Ben and Luke - are defined in an `enum` named `PianoClass`, as follows:

```

   enum PianoClass {
    JOHN, BEN, LUKE;
        public String getSex() {
            return "Male";
        }
        public String getLevel() {
            return "Beginner";
        }
    }

```

And one day two other students arrive - Rita and Tom - with a sex (Female) and level (Intermediate) that do not match the previous ones:

```

   enum PianoClass2 {
    JOHN, BEN, LUKE, RITA, TOM;
        public String getSex() {
            return "Male"; // issue, Rita is a female
        }
        public String getLevel() {
            return "Beginner"; // issue, Tom is an intermediate student
        }
    }

```

so that simply adding the new students to the constant declaration, as follows, is not correct:

```java
PianoClass2 tom = PianoClass2.TOM;
PianoClass2 rita = PianoClass2.RITA;
System.out.println(tom.getLevel()); // prints Beginner -> wrong Tom's not a beginner
System.out.println(rita.getSex()); // prints Male -> wrong Rita's not a male

```

It's possible to define a specific behavior for each of the constant, Rita and Tom, which overrides the `PianoClass2` default behavior as follows:

```java
enum PianoClass3 {
    JOHN, BEN, LUKE,
    RITA {
        @Override
        public String getSex() {
            return "Female";
        }
    },
    TOM {
        @Override
        public String getLevel() {
            return "Intermediate";
        }
    };
    public String getSex() {
        return "Male";
    }
    public String getLevel() {
        return "Beginner";
    }
}

```

and now Tom's level and Rita's sex are as they should be:

```java
PianoClass3 tom = PianoClass3.TOM;
PianoClass3 rita = PianoClass3.RITA;
System.out.println(tom.getLevel()); // prints Intermediate
System.out.println(rita.getSex()); // prints Female

```

Another way to define content specific body is by using constructor, for instance:

```java
enum Friend {
    MAT("Male"),
    JOHN("Male"),
    JANE("Female");
    
    private String gender;

    Friend(String gender) {
        this.gender = gender;
    }

    public String getGender() {
        return this.gender;
    }
}

```

and usage:

```java
Friend mat = Friend.MAT;
Friend john = Friend.JOHN;
Friend jane = Friend.JANE;
System.out.println(mat.getGender());     // Male
System.out.println(john.getGender());    // Male
System.out.println(jane.getGender());    // Female

```



## Zero instance enum


```java
enum Util {
    /* No instances */;

    public static int clamp(int min, int max, int i) {
        return Math.min(Math.max(i, min), max);
    }

    // other utility methods...
}

```

Just as `enum` [can be used for singletons](http://stackoverflow.com/documentation/java/155/enums/5741/implement-singleton-pattern-with-a-single-element-enum) (1 instance classes), it can be used for utility classes (0 instance classes). Just make sure to terminate the (empty) list of enum constants with a `;`.

See the question [Zero instance enum vs private constructors for preventing instantiation](http://stackoverflow.com/questions/25137490/zero-instance-enum-vs-private-constructors-for-preventing-instantiation) for a discussion on pro's and con's compared to private constructors.



## Getting the values of an enum


Each enum class contains an implicit static method named `values()`. This method returns an array containing all values of that enum. You can use this method to iterate over the values. It is important to note however that this method returns a **new** array every time it is called.

```java
public enum Day {
    MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY, SUNDAY;
    
    /**
    * Print out all the values in this enum.
    */
    public static void printAllDays() {
        for(Day day : Day.values()) {
            System.out.println(day.name());
        }
    }
}

```

If you need a `Set` you can use `EnumSet.allOf(Day.class)` as well.



## Enum Polymorphism Pattern


When a method need to accept an "extensible" set of `enum` values, the programmer can apply polymorphism like on a normal `class` by creating an interface which will be used anywere where the `enum`s shall be used:

```java
public interface ExtensibleEnum {
    String name();
}

```

This way, any `enum` tagged by (implementing) the interface can be used as a parameter, allowing the programmer to create a variable amount of `enum`s that will be accepted by the method. This can be useful, for example, in APIs where there is a default (unmodifiable) `enum` and the user of these APIs want to "extend" the `enum` with more values.

A set of default enum values can be defined as follows:

```java
public enum DefaultValues implements ExtensibleEnum {
    VALUE_ONE, VALUE_TWO;
}

```

Additional values can then be defined like this:

```java
public enum ExtendedValues implements ExtensibleEnum {
    VALUE_THREE, VALUE_FOUR;
}

```

Sample which shows how to use the enums - note how `printEnum()` accepts values from both `enum` types:

```java
private void printEnum(ExtensibleEnum val) {
    System.out.println(val.name());
}  

printEnum(DefaultValues.VALUE_ONE);    // VALUE_ONE
printEnum(DefaultValues.VALUE_TWO);    // VALUE_TWO
printEnum(ExtendedValues.VALUE_THREE); // VALUE_THREE
printEnum(ExtendedValues.VALUE_FOUR);  // VALUE_FOUR

```

Note: This pattern does not prevent you from redefining enum values, which are already defined in one enum, in another enum. These enum values would be different instances then.
Also, it is not possible to use switch-on-enum since all we have is the interface, not the real `enum`.



## Compare and Contains for Enum values


Enums contains only constants and can be compared directly with `==`. So, only reference check is needed, no need to use `.equals` method. Moreover, if `.equals` used incorrectly, may raise the `NullPointerException` while that's not the case with `==` check.

```java
enum Day {
    GOOD, AVERAGE, WORST;
}

public class Test {

    public static void main(String[] args) {
        Day day = null;

        if (day.equals(Day.GOOD)) {//NullPointerException!
            System.out.println("Good Day!");
        }

        if (day == Day.GOOD) {//Always use == to compare enum
            System.out.println("Good Day!");
        }

    }
}

```

To group, complement, range the enum values we have [`EnumSet`](https://docs.oracle.com/javase/8/docs/api/java/util/EnumSet.html) class which contains different methods.

<li>
`EnumSet#range`         : To get subset of enum by range defined by two endpoints
</li>
<li>
`EnumSet#of`             : Set of specific enums without any range. Multiple overloaded `of` methods are there.
</li>
<li>
`EnumSet#complementOf`     : Set of enum which is complement of enum values provided in method parameter

```java
enum Page {
   A1, A2, A3, A4, A5, A6, A7, A8, A9, A10
}

public class Test {

  public static void main(String[] args) {
      EnumSet<Page> range = EnumSet.range(Page.A1, Page.A5);

      if (range.contains(Page.A4)) {
          System.out.println("Range contains A4");
      }

      EnumSet<Page> of = EnumSet.of(Page.A1, Page.A5, Page.A3);

      if (of.contains(Page.A1)) {
          System.out.println("Of contains A1");
      }
  }
}

```


</li>



## Get enum constant by name


Say we have an enum `DayOfWeek`:

```java
enum DayOfWeek {
    SUNDAY, MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY;
}

```

An enum is compiled with a built-in static `valueOf()` method which can be used to lookup a constant by its name:

```java
String dayName = DayOfWeek.SUNDAY.name();
assert dayName.equals("SUNDAY");

DayOfWeek day = DayOfWeek.valueOf(dayName);
assert day == DayOfWeek.SUNDAY;

```

This is also possible using a dynamic enum type:

```java
Class<DayOfWeek> enumType = DayOfWeek.class;
DayOfWeek day = Enum.valueOf(enumType, "SUNDAY");
assert day == DayOfWeek.SUNDAY;

```

Both of these `valueOf()` methods will throw an `IllegalArgumentException` if the specified enum does not have a constant with a matching name.

The Guava library provides a helper method [`Enums.getIfPresent()`](https://google.github.io/guava/releases/18.0/api/docs/com/google/common/base/Enums.html#getIfPresent(java.lang.Class,%20java.lang.String)) that returns a Guava [`Optional`](https://google.github.io/guava/releases/18.0/api/docs/com/google/common/base/Optional.html) to eliminate explicit exception handling:

```java
DayOfWeek defaultDay = DayOfWeek.SUNDAY;
DayOfWeek day = Enums.valueOf(DayOfWeek.class, "INVALID").or(defaultDay);
assert day == DayOfWeek.SUNDAY;

```



## Enum with properties (fields)


In case we want to use `enum` with more information and not just as constant values, and we want to be able to compare two enums.

Consider the following example:

```java
public enum Coin {
    PENNY(1), NICKEL(5), DIME(10), QUARTER(25);

    private final int value;

    Coin(int value){
        this.value = value;
    }

    public boolean isGreaterThan(Coin other){
        return this.value > other.value;
    }

}

```

Here we defined an `Enum` called `Coin` which represent its value. With the method `isGreaterThan` we can compare two `enum`s:

```java
Coin penny = Coin.PENNY;
Coin dime = Coin.DIME;

System.out.println(penny.isGreaterThan(dime)); // prints: false
System.out.println(dime.isGreaterThan(penny)); // prints: true

```



## Convert enum to String


Sometimes you want to convert your enum to a String, there are two ways to do that.

Assume we have:

```java
public enum Fruit {
    APPLE, ORANGE, STRAWBERRY, BANANA, LEMON, GRAPE_FRUIT;
}

```

So how do we convert something like `Fruit.APPLE` to `"APPLE"`?

### Convert using `name()`

`name()` is an internal method in `enum` that returns the `String` representation of the enum, the return `String` represents ****exactly**** how the enum value was defined.

For example:

```java
System.out.println(Fruit.BANANA.name());      // "BANANA"
System.out.println(Fruit.GRAPE_FRUIT.name()); // "GRAPE_FRUIT"

```

### Convert using `toString()`

`toString()` is, **by default**, overridden to have the same behavior as `name()`

However, `toString()` is likely overridden by **developers** to make it print a more user friendly `String`

> 
<p>Don't use `toString()` if you want to do checking in your code, `name()` is much more stable for that.
Only use `toString()` when you are going to output the value to logs or stdout or something</p>


### By default:

```java
System.out.println(Fruit.BANANA.toString());      // "BANANA"
System.out.println(Fruit.GRAPE_FRUIT.toString()); // "GRAPE_FRUIT"

```

### Example of being overridden

```java
System.out.println(Fruit.BANANA.toString());      // "Banana"
System.out.println(Fruit.GRAPE_FRUIT.toString()); // "Grape Fruit"

```



## Enums with static fields


If your enum class is required to have static fields, keep in mind they are created **after** the enum values themselves. That means, the following code will result in a `NullPointerException`:

```java
enum Example {
    ONE(1), TWO(2);

    static Map<String, Integer> integers = new HashMap<>();

    private Example(int value) {
        integers.put(this.name(), value);
    }
}

```

A possible way to fix this:

```java
enum Example {
    ONE(1), TWO(2);

    static Map<String, Integer> integers;

    private Example(int value) {
        putValue(this.name(), value);
    }

    private static void putValue(String name, int value) {
        if (integers == null)
            integers = new HashMap<>();
        integers.put(name, value);
    }
}

```

Do not initialize the static field:

```java
enum Example {
    ONE(1), TWO(2);

    // after initialisisation integers is null!!
    static Map<String, Integer> integers = null;

    private Example(int value) {
        putValue(this.name(), value);
    }

    private static void putValue(String name, int value) {
        if (integers == null)
            integers = new HashMap<>();
        integers.put(name, value);
    }
    // !!this may lead to null poiner exception!!
    public int getValue(){
        return (Example.integers.get(this.name()));
    }
}

```

initialisisation:

<li>create the enum values
<ul>
- as side effect     putValue() called that initializes integers

- integers = null; // is executed after the enums so the content of integers is lost



#### Syntax


- [public/protected/private] enum Enum_name { // Declare a new enum.
- ENUM_CONSTANT_1[, ENUM_CONSTANT_2...]; // Declare the enum constants. This must be the first line inside of the enum, and should be separated by commas, with a semicolon at the end.
- ENUM_CONSTANT_1(param)[, ENUM_CONSTANT_2(param)...]; // Declare enum constants with parameters. The parameter types must match the constructor.
- ENUM_CONSTANT_1 {...}[, ENUM_CONSTANT_2 {...}...]; // Declare enum constants with overridden methods. This must be done if the enum contains abstract methods; all such methods must be implemented.
- ENUM_CONSTANT.name() // Returns a String with the name of the enum constant.
- ENUM_CONSTANT.ordinal() // Returns the ordinal of this enumeration constant, its position in its enum declaration, where the initial constant is assigned an ordinal of zero.
- Enum_name.values() // Returns a new array (of type Enum_name[]) containing every constant of that enum everytime it is called.
- Enum_name.valueOf("ENUM_CONSTANT") // The inverse of ENUM_CONSTANT.name() -- returns the enum constant with the given name.
- Enum.valueOf(Enum_name.class, "ENUM_CONSTANT") // A synonym of the previous one: The inverse of ENUM_CONSTANT.name() -- returns the enum constant with the given name.



#### Remarks


### Restrictions

Enums always extend [`java.lang.Enum`](https://docs.oracle.com/javase/7/docs/api/java/lang/Enum.html), so it is impossible for an enum to extend a class. However, they can implement many interfaces.

### Tips & Tricks

Because of their specialized representation, there are more efficient [maps](https://docs.oracle.com/javase/7/docs/api/java/util/EnumMap.html) and [sets](https://docs.oracle.com/javase/7/docs/api/java/util/EnumSet.html) that can be used with enums as their keys. These will often run quicker than their non-specialized counterparts.

