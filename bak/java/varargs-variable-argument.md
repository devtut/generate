---
metaTitle: "Java - Varargs  (Variable Argument)"
description: "Specifying a varargs parameter, Working with Varargs parameters"
---

# Varargs  (Variable Argument)



## Specifying a varargs parameter


```java
void doSomething(String... strings) {
    for (String s : strings) {
        System.out.println(s);
    }
}

```

The three periods after the final parameter's type indicate that the final argument may be passed as an array or as a sequence of arguments. Varargs can be used only in the final argument position.



## Working with Varargs parameters


Using varargs as a parameter for a method definition, it is possible to pass either an array or a sequence of arguments. If a sequence of arguments are passed, they are converted into an array automatically.

This example shows both an array and a sequence of arguments being passed into the `printVarArgArray()` method, and how they are treated identically in the code inside the method:

```java
public class VarArgs {
    
    // this method will print the entire contents of the parameter passed in
    
    void printVarArgArray(int... x) {
        for (int i = 0; i < x.length; i++) {
            System.out.print(x[i] + ",");
        }
    }
    
    public static void main(String args[]) {
        VarArgs obj = new VarArgs();
        
        //Using an array:
        int[] testArray = new int[]{10, 20};
        obj.printVarArgArray(testArray); 
       
        System.out.println(" ");
        
        //Using a sequence of arguments
        obj.printVarArgArray(5, 6, 5, 8, 6, 31);
    }
}

```

Output:

```java
10,20, 
5,6,5,8,6,31

```

****If you define the method like this, it will give compile-time errors.****

```java
void method(String... a, int... b , int c){} //Compile time error (multiple varargs )

void method(int... a, String b){} //Compile time error (varargs must be the last argument 

```



#### Remarks


A “varargs” method argument allows callers of that method to specify multiple arguments of the designated type, each as a separate argument.  It is specified in the method declaration by three ASCII periods (`...`) after the base type.

The method itself receives those arguments as a single array, whose element type is the type of the varargs argument.  The array is created automatically (though callers are still permitted to pass an explicit array instead of passing multiple values as separate method arguments).

**Rules for varargs:**

1. Varargs must be the last argument.
1. There can be only one Varargs in the method.

You must follow above rules otherwise program will give compile error.

