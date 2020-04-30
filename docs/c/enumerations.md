---
metaTitle: "Enumerations"
description: "Enumeration with duplicate value, enumeration constant without typename, Simple Enumeration, Typedef enum"
---

# Enumerations



## Enumeration with duplicate value


An enumerations value in no way needs to be unique:

```c
#include <stdlib.h> /* for EXIT_SUCCESS */
#include <stdio.h> /* for printf() */


enum Dupes
{
   Base, /* Takes 0 */
   One, /* Takes Base + 1 */
   Two, /* Takes One + 1 */
   Negative = -1,
   AnotherZero /* Takes Negative + 1 == 0, sigh */
};

int main(void)
{
  printf("Base = %d\n", Base);
  printf("One = %d\n", One);
  printf("Two = %d\n", Two);
  printf("Negative = %d\n", Negative);
  printf("AnotherZero = %d\n", AnotherZero);

  return EXIT_SUCCESS;
}

```

The sample prints:

```c
Base = 0
One = 1
Two = 2
Negative = -1
AnotherZero = 0

```



## enumeration constant without typename


Enumeration types can also be declared without giving them a name:

```

 enum { buffersize = 256, };
  static unsigned char buffer [buffersize] = { 0 };

```

This enables us to define compile time constants of type `int` that can as in this example be used as array length.



## Simple Enumeration


An enumeration is a user-defined data type consists of integral constants and each integral constant is given a name. Keyword `enum` is used to define enumerated data type.

If you use `enum` instead of `int` or `string/ char*`, you increase compile-time checking and avoid errors from passing in invalid constants, and you document which values are legal to use.

### Example 1

```c
enum color{ RED, GREEN, BLUE };

void printColor(enum color chosenColor)
{
    const char *color_name = "Invalid color";
    switch (chosenColor)
    {
       case RED:
         color_name = "RED";
         break;
       
       case GREEN:
        color_name = "GREEN";
        break;    

       case BLUE:
        color_name = "BLUE";
        break;
    }
    printf("%s\n", color_name);
}

```

With a main function defined as follows (for example):

```c
int main(){
    enum color chosenColor;
    printf("Enter a number between 0 and 2");
    scanf("%d", (int*)&chosenColor);
    printColor(chosenColor);
    return 0;
}

```

### Example 2

<sup>(This example uses designated initializers which are standardized since C99.)</sup>

```c
enum week{ MON, TUE, WED, THU, FRI, SAT, SUN };
     
static const char* const dow[] = { 
  [MON] = "Mon", [TUE] = "Tue", [WED] = "Wed", 
  [THU] = "Thu", [FRI] = "Fri", [SAT] = "Sat", [SUN] = "Sun" };
    
void printDayOfWeek(enum week day) 
{ 
   printf("%s\n", dow[day]);
}

```

The same example using range checking:

```c
enum week{ DOW_INVALID = -1, 
  MON, TUE, WED, THU, FRI, SAT, SUN, 
  DOW_MAX };
     
static const char* const dow[] = { 
  [MON] = "Mon", [TUE] = "Tue", [WED] = "Wed", 
  [THU] = "Thu", [FRI] = "Fri", [SAT] = "Sat", [SUN] = "Sun" };
    
void printDayOfWeek(enum week day) 
{ 
   assert(day > DOW_INVALID && day < DOW_MAX);
   printf("%s\n", dow[day]);
}

```



## Typedef enum


There are several possibilities and conventions to name an enumeration. The first is to use a **tag name** just after the `enum` keyword.

```c
enum color
{ 
    RED, 
    GREEN, 
    BLUE 
};

```

This enumeration must then always be used with the keyword **and** the tag like this:

```c
enum color chosenColor = RED;

```

If we use `typedef` directly when declaring the `enum`, we can omit the tag name and then use the type without the `enum` keyword:

```c
typedef enum 
{ 
    RED, 
    GREEN, 
    BLUE 
} color;

color chosenColor = RED;

```

But in this latter case we cannot use it as `enum color`, because we didn't use the tag name in the definition. One common convention is to use both, such that the same name can be used with or without `enum` keyword. This has the particular advantage of being compatible with [C++](https://stackoverflow.com/documentation/c%2b%2b/topics)

```c
enum color                /* as in the first example */
{ 
    RED, 
    GREEN, 
    BLUE 
};
typedef enum color color; /* also a typedef of same identifier */

color chosenColor  = RED;
enum color defaultColor = BLUE;

```

Function:

```c
void printColor()
{
    if (chosenColor == RED)
    {
        printf("RED\n");
    }
    else if (chosenColor == GREEN)
    {
        printf("GREEN\n");    
    }
    else if (chosenColor == BLUE)
    {
        printf("BLUE\n");
    }
}

```

For more on `typedef` see [Typedef](https://stackoverflow.com/documentation/c/2681/typedef)



#### Remarks


Enumerations consist of the `enum` keyword and an optional **identifier** followed by an **enumerator-list** enclosed by braces.

An **identifier** is of  type `int`.

The **enumerator-list** has at least one **enumerator** element.

An **enumerator** may optionally be "assigned" a constant expression of type `int`.

An **enumerator** is constant and is compatible to either a `char`, a signed integer or an unsigned integer. Which ever is used is [implementation-defined](http://stackoverflow.com/documentation/c/4832/implementation-defined-behaviour#t=201608141558080440014). In any case the type used should be able to represent all values defined for the enumeration in question.

If no constant expression is "assigned" to an **enumerator** and it is the 1<sup>st</sup> entry in an **enumerator-list** it takes value of `0`, else it get takes the value of the previous entry in the **enumerator-list** plus 1.

Using multiple "assignments" can lead to different **enumerators** of the same enumeration carry the same values.

