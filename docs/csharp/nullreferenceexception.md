---
metaTitle: "NullReferenceException"
description: "NullReferenceException explained"
---

# NullReferenceException



## NullReferenceException explained


A `NullReferenceException` is thrown when you try to access a non-static member (property, method, field or event) of a reference object but it is null.

```cs
Car myFirstCar = new Car();
Car mySecondCar = null;
Color myFirstColor = myFirstCar.Color; // No problem as myFirstCar exists / is not null
Color mySecondColor = mySecondCar.Color; // Throws a NullReferenceException 
// as mySecondCar is null and yet we try to access its color.

```

To debug such an exception, it's quite easy: on the line where the exception is thrown, you just have to look before every '`.`' or '`[`', or on rare occasions '`(`'.

```cs
myGarage.CarCollection[currentIndex.Value].Color = theCarInTheStreet.Color;

```

Where does my exception come from?
Either:

- `myGarage` is `null`
- `myGarage.CarCollection` is `null`
- `currentIndex` is `null`
- `myGarage.CarCollection[currentIndex.Value]` is `null`
- `theCarInTheStreet` is `null`

In debug mode, you only have to put your mouse cursor on every of these elements and you will find your null reference. Then, what you have to do is understand why it doesn't have a value. The correction totally depends on the goal of your method.

Have you forgotten to instantiate/initialize it?

```cs
myGarage.CarCollection = new Car[10];

```

Are you supposed to do something different if the object is null?

```cs
if (myGarage == null)
{
    Console.WriteLine("Maybe you should buy a garage first!");
}

```

Or maybe someone gave you a null argument, and was not supposed to:

```cs
if (theCarInTheStreet == null)
{
    throw new ArgumentNullException("theCarInTheStreet");
}

```

In any case, remember that a method should never throw a NullReferenceException. If it does, that means you have forgotten to check something.

