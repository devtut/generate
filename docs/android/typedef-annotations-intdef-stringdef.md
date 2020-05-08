---
metaTitle: "Android - Typedef Annotations: @IntDef, @StringDef"
description: "IntDef Annotations, Combining constants with flags"
---

# Typedef Annotations: @IntDef, @StringDef



## IntDef Annotations


This [annotation](https://developer.android.com/reference/android/support/annotation/IntDef.html) ensures that only the valid integer constants that you expect are used.<br />
The following example illustrates the steps to create an annotation:

```java
import android.support.annotation.IntDef;

public abstract class Car {

    //Define the list of accepted constants
    @IntDef({MICROCAR, CONVERTIBLE, SUPERCAR, MINIVAN, SUV})

    //Tell the compiler not to store annotation data in the .class file
    @Retention(RetentionPolicy.SOURCE)
    //Declare the CarType annotation
    public @interface CarType {}

    //Declare the constants
    public static final int MICROCAR = 0;
    public static final int CONVERTIBLE = 1;
    public static final int SUPERCAR = 2;
    public static final int MINIVAN = 3;
    public static final int SUV = 4;

    @CarType
    private int mType;

    @CarType
    public int getCarType(){
        return mType;
    };

    public void setCarType(@CarType int type){
        mType = type;
    }
}

```

They also enable code completion to automatically offer the allowed constants.<br />
When you build this code, a warning is generated if the type parameter does not reference one of the defined constants.



## Combining constants with flags


Using the `IntDef#flag()` attribute set to `true`, multiple constants can be combined.

Using the [same example](http://stackoverflow.com/documentation/android/4505/typedef-annotations-intdef-stringdef/15758/intdef-annotations#t=201609280657212936347) in this topic:

```java
public abstract class Car {

    //Define the list of accepted constants
    @IntDef(flag=true, value={MICROCAR, CONVERTIBLE, SUPERCAR, MINIVAN, SUV})

    //Tell the compiler not to store annotation data in the .class file
    @Retention(RetentionPolicy.SOURCE)

    .....

}

```

Users can combine the allowed constants with a flag (such as `|`, `&`, `^` ).



#### Remarks


The annotations package includes a number of useful metadata annotations you can decorate your own code with, to help catch bugs.

Just add the dependency in the `build.gradle` file.

```java
dependencies {
    compile 'com.android.support:support-annotations:25.3.1'
}

```

