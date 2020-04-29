---
metaTitle: "super keyword"
description: "Super keyword use with examples"
---

# super keyword



## Super keyword use with examples


super keyword performs important role in three places

1. Constructor Level
1. Method Level
1. Variable Level

### Constructor Level

`super` keyword is used to call parent class constructor. This constructor can be default constructor or parameterized constructor.

<li>
Default constructor : `super();`
</li>
<li>
Parameterized constructor : `super(int no, double amount, String name);`

```java
 class Parentclass
 {
    Parentclass(){
       System.out.println("Constructor of Superclass");
    }
 }
 class Subclass extends Parentclass
 {
    Subclass(){
     /* Compile adds super() here at the first line
      * of this constructor implicitly
      */
     System.out.println("Constructor of Subclass");
    }
    Subclass(int n1){
     /* Compile adds super() here at the first line
      * of this constructor implicitly
      */
     System.out.println("Constructor with arg");
    }
    void display(){
     System.out.println("Hello");
    }
    public static void main(String args[]){
     // Creating object using default constructor
     Subclass obj= new Subclass();
     //Calling sub class method 
        obj.display();
        //Creating object 2 using arg constructor
        Subclass obj2= new Subclass(10);
        obj2.display();
   }
 }

```


</li>

**Note**:  `super()` must be the first statement in constructor otherwise we will get the compilation error message.

### Method Level

`super` keyword can also be used in case of method overriding. `super` keyword can be used to invoke or call parent class method.

```java
class Parentclass
{
   //Overridden method
   void display(){
    System.out.println("Parent class method");
   }
}
class Subclass extends Parentclass
{
   //Overriding method
   void display(){
    System.out.println("Child class method");
   }
   void printMsg(){
    //This would call Overriding method
    display();
    //This would call Overridden method
    super.display();
   }
   public static void main(String args[]){        
    Subclass obj= new Subclass();
    obj.printMsg(); 
   }
}

```

**Note**:If there is not method overriding then we do not need to use `super` keyword to call parent class method.

### Variable Level

`super` is used to refer immediate parent class instance variable. In case of inheritance, there may be possibility of base class and derived class may have similar data members.In order to differentiate between the data member of base/parent class and derived/child class, in the context of derived class the base class data members must be preceded by `super` keyword.

```java
//Parent class or Superclass
class Parentclass
{
    int num=100;
}
//Child class or subclass
class Subclass extends Parentclass
{
    /* I am declaring the same variable 
     * num in child class too.
     */
    int num=110;
    void printNumber(){
     System.out.println(num); //It will print value 110
     System.out.println(super.num); //It will print value 100
    }
    public static void main(String args[]){
       Subclass obj= new Subclass();
       obj.printNumber();    
    }
}

```

**Note**: If we are not writing `super` keyword before the base class data member name then it will be referred as current class data member and base class data member are hidden in the context of derived class.

