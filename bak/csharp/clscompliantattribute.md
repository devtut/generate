---
metaTitle: "C# | CLSCompliantAttribute"
description: "Access Modifier to which CLS rules apply, Violation of CLS rule: Unsigned types / sbyte, Violation of CLS rule: Same naming, Violation of CLS rule: Identifier _, Violation of CLS rule: Inherit from non CLSComplaint class"
---

# CLSCompliantAttribute



## Access Modifier to which CLS rules apply


```cs
using System;

[assembly:CLSCompliant(true)]
namespace CLSDoc
{
   
    public class Cat
    {
        internal UInt16 _age = 0;
        private UInt16 _daysTillVacination = 0;

        //Warning CS3003  Type of 'Cat.DaysTillVacination' is not CLS-compliant
        protected UInt16 DaysTillVacination
        {
            get { return _daysTillVacination; }
        }

        //Warning    CS3003    Type of 'Cat.Age' is not CLS-compliant
        public UInt16 Age
        { get { return _age; } }

        //valid behaviour by CLS-compliant rules
        public int IncreaseAge()
        {
            int increasedAge = (int)_age + 1;
           
            return increasedAge;
        }

    }
}

```

The rules for CLS compliance apply only to a public/protected components.



## Violation of CLS rule: Unsigned types / sbyte


```cs
using System;

[assembly:CLSCompliant(true)]
namespace CLSDoc
{
   
    public class Car
    {
        internal UInt16 _yearOfCreation = 0;

        //Warning CS3008  Identifier '_numberOfDoors' is not CLS-compliant 
        //Warning CS3003  Type of 'Car._numberOfDoors' is not CLS-compliant 
        public UInt32 _numberOfDoors = 0;

        //Warning    CS3003    Type of 'Car.YearOfCreation' is not CLS-compliant
        public UInt16 YearOfCreation
        {
            get { return _yearOfCreation; }
        }


        //Warning CS3002  Return type of 'Car.CalculateDistance()' is not CLS-compliant
        public UInt64 CalculateDistance()
        {
            return 0;
        }

        
        //Warning CS3002  Return type of 'Car.TestDummyUnsignedPointerMethod()' is not CLS-compliant 
        public UIntPtr TestDummyUnsignedPointerMethod()
        {
            int[] arr = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
            UIntPtr ptr = (UIntPtr)arr[0];

            
            return ptr;
        }

        //Warning CS3003  Type of 'Car.age' is not CLS-compliant 
        public sbyte age = 120;


    }
}

```



## Violation of CLS rule: Same naming


```cs
using System;

[assembly:CLSCompliant(true)]
namespace CLSDoc
{
   
    public class Car
    {
        //Warning    CS3005    Identifier 'Car.CALCULATEAge()' differing only in case is not CLS-compliant
        public int CalculateAge()
        {
            return 0;
        }

        public int CALCULATEAge()
        {
            return 0;
        }

    }
}

```

Visual Basic is not case sensitive



## Violation of CLS rule: Identifier _


```cs
using System;

[assembly:CLSCompliant(true)]
namespace CLSDoc
{
   
    public class Car
    {
        //Warning CS3008  Identifier '_age' is not CLS-complian    
        public int _age = 0;    
    }

}

```

You can not start variable with _



## Violation of CLS rule: Inherit from non CLSComplaint class


```cs
using System;

[assembly:CLSCompliant(true)]
namespace CLSDoc
{

    [CLSCompliant(false)]
    public class Animal
    {
        public int age = 0;
    }
  
    //Warning    CS3009    'Dog': base type 'Animal' is not CLS-compliant
    public class Dog : Animal
    {
    }

}

```



#### Syntax


1. [assembly:CLSCompliant(true)]
1. [CLSCompliant(true)]



#### Parameters


|Constructor|Parameter
|---|---|---|---|---|---|---|---|---|---
|CLSCompliantAttribute(Boolean)|Initializes an instance of the CLSCompliantAttribute class with a Boolean value indicating whether the indicated program element is CLS-compliant.



#### Remarks


The Common Language Specification (CLS) is a set of base rules to which any language targeting the CLI(language which confirms the Common Language Infrastructure specifications) should confirm in order to interoperate with other CLS-compliant languages.

[List of CLI languages](https://en.wikipedia.org/wiki/List_of_CLI_languages)

You should mark your assembly as CLSCompliant in most cases when you are distributing libraries. This attribute will guarantee you that your code will be usable by all CLS-compliant languages. This means that your code can be consumed by any language that can be compiled and run on CLR([Common Language Runtime](https://en.wikipedia.org/wiki/List_of_CLI_languages))

When your assembly is marked with `CLSCompliantAttribute`, the compiler will check if your code violates any of CLS rules and return **warning** if it is needed.

