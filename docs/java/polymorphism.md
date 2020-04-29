---
metaTitle: "Polymorphism"
description: "Method Overriding, Method Overloading, Polymorphism and different types of overriding, Adding behaviour by adding classes without touching existing code, Virtual functions"
---

# Polymorphism


Polymorphism is one of main OOP(object oriented programming) concepts. Polymorphism word was derived from the greek words "poly" and "morphs". Poly means "many" and morphs means "forms" (many forms).

There are two ways to perform polymorphism.
**Method Overloading** and  **Method Overriding**.



## Method Overriding


Method overriding is the ability of subtypes to redefine (override) the behavior of their supertypes.

In Java, this translates to subclasses overriding the methods defined in the super class. In Java, all non-primitive variables are actually `references`, which are akin to pointers to the location of the actual object in memory. The `references` only have one type, which is the type they were declared with. However, they can point to an object of either their declared type or any of its subtypes.

When a method is called on a `reference`, the corresponding **method of the actual object being pointed to is invoked**.

```java
class SuperType {
    public void sayHello(){
        System.out.println("Hello from SuperType");
    }

    public void sayBye(){
        System.out.println("Bye from SuperType");
    }
}

class SubType extends SuperType {
    // override the superclass method
    public void sayHello(){
        System.out.println("Hello from SubType");
    }
}

class Test {
    public static void main(String... args){
        SuperType superType = new SuperType();
        superType.sayHello(); // -> Hello from SuperType

        // make the reference point to an object of the subclass
        superType = new SubType();
        // behaviour is governed by the object, not by the reference
        superType.sayHello(); // -> Hello from SubType

        // non-overridden method is simply inherited
        superType.sayBye(); // -> Bye from SuperType
    }
}

```

**Rules to keep in mind**

To override a method in the subclass, the overriding method (i.e. the one in the subclass) **MUST HAVE**:

- same name
- same return type in case of primitives (a subclass is allowed for classes, this is also known as covariant return types).
- same type and order of parameters
<li>it may throw only those exceptions that are declared in the throws clause of the superclass's method or exceptions that are subclasses of the declared exceptions. It may also choose NOT to throw any exception.
The names of the parameter types do not matter. For example, void methodX(int i) is same as void methodX(int k)</li>
- We are unable to Override final or Static methods. Only thing that we can do change only method body.



## Method Overloading


**Method overloading**, also known as **function overloading**, is the ability of a class to have multiple methods with the same name, granted that they differ in either number or type of arguments.

Compiler checks ****method signature**** for method overloading.

Method signature consists of three things -

1. Method name
1. Number of parameters
1. Types of parameters

If these three are same for any two methods in a class, then compiler throws **duplicate method error**.

This type of polymorphism is called **static** or **compile time** polymorphism because the appropriate method to be called is decided by the compiler during the compile time based on the argument list.

```java
class Polymorph {

    public int add(int a, int b){
        return a + b;
    }
    
    public int add(int a, int b, int c){
        return a + b + c;
    }

    public float add(float a, float b){
        return a + b;
    }

    public static void main(String... args){
        Polymorph poly = new Polymorph();
        int a = 1, b = 2, c = 3;
        float d = 1.5, e = 2.5;

        System.out.println(poly.add(a, b));
        System.out.println(poly.add(a, b, c));
        System.out.println(poly.add(d, e));
    }

}

```

This will result in:

```java
2
6
4.000000

```

Overloaded methods may be static or non-static. This also does not effect method overloading.

```java
public class Polymorph {

    private static void methodOverloaded()
    {
        //No argument, private static method
    }
 
    private int methodOverloaded(int i)
    {
        //One argument private non-static method
        return i;
    }
 
    static int methodOverloaded(double d)
    {
        //static Method
        return 0;
    }
 
    public void methodOverloaded(int i, double d)
    {
        //Public non-static Method
    }
}

```

Also if you change the return type of method, we are unable to get it as method overloading.

```java
public class Polymorph {  

void methodOverloaded(){
    //No argument and No return type
}

int methodOverloaded(){
    //No argument and int return type 
    return 0;
}

```



## Polymorphism and different types of overriding


From java [tutorial](https://docs.oracle.com/javase/tutorial/java/IandI/polymorphism.html)

> 
The dictionary definition of polymorphism refers to a principle in biology in which an organism or species can have many different forms or stages. This principle can also be applied to object-oriented programming and languages like the Java language. ****Subclasses of a class can define their own unique behaviors and yet share some of the same functionality of the parent class.****


Have a look at this example to understand different types of overriding.

1. Base class provides no implementation and sub-class has to override complete method - (abstract)
1. Base class provides default implementation and sub-class can change the behaviour
1. Sub-class adds extension to base class implementation by calling `super.methodName()` as first statement
1. Base class defines structure of the algorithm (Template method) and sub-class will override a part of algorithm

code snippet:

```java
import java.util.HashMap;

abstract class Game implements Runnable{

    protected boolean runGame = true;
    protected Player player1 = null;
    protected Player player2 = null;
    protected Player currentPlayer = null;
    
    public Game(){
        player1 = new Player("Player 1");
        player2 = new Player("Player 2");
        currentPlayer = player1;
        initializeGame();
    }

    /* Type 1: Let subclass define own implementation. Base class defines abstract method to force
        sub-classes to define implementation    
    */
    
    protected abstract void initializeGame();
    
    /* Type 2: Sub-class can change the behaviour. If not, base class behaviour is applicable */
    protected void logTimeBetweenMoves(Player player){
        System.out.println("Base class: Move Duration: player.PlayerActTime - player.MoveShownTime");
    }
    
    /* Type 3: Base class provides implementation. Sub-class can enhance base class implementation by calling
        super.methodName() in first line of the child class method and specific implementation later */
    protected void logGameStatistics(){
        System.out.println("Base class: logGameStatistics:");
    }
    /* Type 4: Template method: Structure of base class can't be changed but sub-class can some part of behaviour */
    protected void runGame() throws Exception{
        System.out.println("Base class: Defining the flow for Game:");    
        while (runGame) {
            /*
            1. Set current player
            2. Get Player Move
            */
            validatePlayerMove(currentPlayer);    
            logTimeBetweenMoves(currentPlayer);
            Thread.sleep(500);
            setNextPlayer();
        }
        logGameStatistics();
    }
    /* sub-part of the template method, which define child class behaviour */
    protected abstract void validatePlayerMove(Player p);
    
    protected void setRunGame(boolean status){
        this.runGame = status;
    }
    public void setCurrentPlayer(Player p){
        this.currentPlayer = p;
    }
    public void setNextPlayer(){
        if (currentPlayer == player1) {
            currentPlayer = player2;
        }else{
            currentPlayer = player1;
        }
    }
    public void run(){
        try{
            runGame();
        }catch(Exception err){
            err.printStackTrace();
        }
    }
}

class Player{
    String name;
    Player(String name){
        this.name = name;
    }
    public String getName(){
        return name;
    }
}

/* Concrete Game implementation  */
class Chess extends Game{
    public Chess(){
        super();
    }
    public void initializeGame(){
        System.out.println("Child class: Initialized Chess game");
    }
    protected void validatePlayerMove(Player p){
        System.out.println("Child class: Validate Chess move:" + p.getName());
    }
    protected void logGameStatistics(){
        super.logGameStatistics();
        System.out.println("Child class: Add Chess specific logGameStatistics:");
    }
}
class TicTacToe extends Game{
    public TicTacToe(){
        super();
    }
    public void initializeGame(){
        System.out.println("Child class: Initialized TicTacToe game");
    }
    protected void validatePlayerMove(Player p){
        System.out.println("Child class: Validate TicTacToe move:" + p.getName());
    }
}

public class Polymorphism{
    public static void main(String args[]){
        try{
        
            Game game = new Chess();
            Thread t1 = new Thread(game);
            t1.start();
            Thread.sleep(1000);
            game.setRunGame(false);
            Thread.sleep(1000);
                        
            game = new TicTacToe();
            Thread t2 = new Thread(game);
            t2.start();
            Thread.sleep(1000);
            game.setRunGame(false);
        
        }catch(Exception err){
            err.printStackTrace();
        }        
    }
}

```

output:

```java
Child class: Initialized Chess game
Base class: Defining the flow for Game:
Child class: Validate Chess move:Player 1
Base class: Move Duration: player.PlayerActTime - player.MoveShownTime
Child class: Validate Chess move:Player 2
Base class: Move Duration: player.PlayerActTime - player.MoveShownTime
Base class: logGameStatistics:
Child class: Add Chess specific logGameStatistics:

Child class: Initialized TicTacToe game
Base class: Defining the flow for Game:
Child class: Validate TicTacToe move:Player 1
Base class: Move Duration: player.PlayerActTime - player.MoveShownTime
Child class: Validate TicTacToe move:Player 2
Base class: Move Duration: player.PlayerActTime - player.MoveShownTime
Base class: logGameStatistics:

```



## Adding behaviour by adding classes without touching existing code


```java
import java.util.ArrayList;
import java.util.List;

import static java.lang.System.out;

public class PolymorphismDemo {

    public static void main(String[] args) {
        List<FlyingMachine> machines = new ArrayList<FlyingMachine>();
        machines.add(new FlyingMachine());
        machines.add(new Jet());
        machines.add(new Helicopter());
        machines.add(new Jet());

        new MakeThingsFly().letTheMachinesFly(machines);
    }
}

class MakeThingsFly {
    public void letTheMachinesFly(List<FlyingMachine> flyingMachines) {
        for (FlyingMachine flyingMachine : flyingMachines) {
            flyingMachine.fly();
        }
    }
}

class FlyingMachine {
    public void fly() {
        out.println("No implementation");
    }
}

class Jet extends FlyingMachine {
    @Override
    public void fly() {
        out.println("Start, taxi, fly");
    }

    public void bombardment() {
        out.println("Fire missile");
    }
}

class Helicopter extends FlyingMachine {
    @Override
    public void fly() {
        out.println("Start vertically, hover, fly");
    }
}

```

**Explanation**

a) The `MakeThingsFly` class can work with everything that is of type `FlyingMachine`.

b) The method `letTheMachinesFly` also works without any change (!) when you add a new class, for example `PropellerPlane`:

```java
public void letTheMachinesFly(List<FlyingMachine> flyingMachines) {
        for (FlyingMachine flyingMachine : flyingMachines) {
            flyingMachine.fly();
        }
    }
}

```

That's the power of polymorphism. You can implement the [open-closed-principle](https://en.wikipedia.org/wiki/Open/closed_principle#Polymorphic_open.2Fclosed_principle) with it.



## Virtual functions


Virtual Methods are methods in Java that are non-static and without the keyword Final in front. All methods by default are virtual in Java. Virtual Methods play important roles in Polymorphism because children classes in Java can override their parent classes' methods if the function being overriden is non-static and has the same method signature.

There are, however, some methods that are not virtual. For example, if the method is declared private or with the keyword final, then the method is not Virtual.

Consider the following modified example of inheritance with Virtual Methods from this StackOverflow post [How do virtual functions work in C# and Java?](http://stackoverflow.com/questions/460446/how-do-virtual-functions-work-in-c-sharp-and-java)) :

```java
public class A{
    public void hello(){
        System.out.println("Hello");
    }
    
    public void boo(){
        System.out.println("Say boo");

    }
}

public class B extends A{
     public void hello(){
        System.out.println("No");
     }
    
    public void boo(){
        System.out.println("Say haha");

    }
}

```

If we invoke class B and call hello() and boo(), we would get "No" and "Say haha" as the resulting output because B overrides the same methods from A. Even though the example above is almost exactly the same as method overriding, it is important to understand that the methods in class A are all, by default, Virtual.

Additionally, we can implement Virtual methods using the abstract keyword. Methods declared with the keyword "abstract" does not have a method definition, meaning the method's body is not yet implemented. Consider the example from above again, except the boo() method is declared abstract:

```java
public class A{
   public void hello(){
        System.out.println("Hello");
    }
    
    abstract void boo();
}

public class B extends A{
     public void hello(){
        System.out.println("No");
     }
    
    public void boo(){
        System.out.println("Say haha");

    }
}

```

If we invoke boo() from B, the output will still be "Say haha" since B inherits the abstract method boo() and makes boo () output "Say haha".

Sources used and further readings:

[How do virtual functions work in C# and Java?](http://stackoverflow.com/questions/460446/how-do-virtual-functions-work-in-c-sharp-and-java)

Check out this great answer that gives a much more complete information about Virtual functions:

[Can you write virtual functions / methods in Java?](http://stackoverflow.com/questions/4547453/can-you-write-virtual-functions-methods-in-java)



#### Remarks


[`Interfaces`](https://docs.oracle.com/javase/tutorial/java/concepts/interface.html) are another way to achieve polymorphism in Java, apart from class based inheritance. Interfaces define a list of methods which form the API of the program. Classes must `implement` an `interface` by overriding all its methods.

