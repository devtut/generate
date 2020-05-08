---
metaTitle: "Android - Parcelable"
description: "Making a custom object Parcelable., Parcelable object containing another Parcelable object, Using Enums with Parcelable"
---

# Parcelable


Parcelable is an Android specific interface where you implement the serialization yourself. It was created to be far more efficient that Serializable, and to get around some problems with the default Java serialization scheme.



## Making a custom object Parcelable.


```java
/**
 * Created by Alex Sullivan on 7/21/16.
 */
public class Foo implements Parcelable
{
    private final int myFirstVariable;
    private final String mySecondVariable;
    private final long myThirdVariable;

    public Foo(int myFirstVariable, String mySecondVariable, long myThirdVariable)
    {
        this.myFirstVariable = myFirstVariable;
        this.mySecondVariable = mySecondVariable;
        this.myThirdVariable = myThirdVariable;
    }
    
    // Note that you MUST read values from the parcel IN THE SAME ORDER that
    // values were WRITTEN to the parcel! This method is our own custom method
    // to instantiate our object from a Parcel. It is used in the Parcelable.Creator variable we declare below.
    public Foo(Parcel in)
    {
        this.myFirstVariable = in.readInt();
        this.mySecondVariable = in.readString();
        this.myThirdVariable = in.readLong();
    }
    
    // The describe contents method can normally return 0. It's used when
    // the parceled object includes a file descriptor.
    @Override
    public int describeContents()
    {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags)
    {
        dest.writeInt(myFirstVariable);
        dest.writeString(mySecondVariable);
        dest.writeLong(myThirdVariable);
    }
    
    // Note that this seemingly random field IS NOT OPTIONAL. The system will
    // look for this variable using reflection in order to instantiate your
    // parceled object when read from an Intent.
    public static final Parcelable.Creator<Foo> CREATOR = new Parcelable.Creator<Foo>()
    {
        // This method is used to actually instantiate our custom object
        // from the Parcel. Convention dictates we make a new constructor that
        // takes the parcel in as its only argument.
        public Foo createFromParcel(Parcel in)
        {
            return new Foo(in);
        }
        
        // This method is used to make an array of your custom object.
        // Declaring a new array with the provided size is usually enough.
        public Foo[] newArray(int size)
        {
            return new Foo[size];
        }
    };
}

```



## Parcelable object containing another Parcelable object


An example of a class that contains a parcelable class inside:

```java
public class Repository implements Parcelable {
    private String name;
    private Owner owner;
    private boolean isPrivate;
 
    public Repository(String name, Owner owner, boolean isPrivate) {
        this.name = name;      
        this.owner = owner;
        this.isPrivate = isPrivate;
    }
 
    protected Repository(Parcel in) {      
        name = in.readString();
        owner = in.readParcelable(Owner.class.getClassLoader());
        isPrivate = in.readByte() != 0;
    }
 
    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(name);
        dest.writeParcelable(owner, flags);
        dest.writeByte((byte) (isPrivate ? 1 : 0));
    }
 
    @Override
    public int describeContents() {
        return 0;
    }
 
    public static final Creator<Repository> CREATOR = new Creator<Repository>() {
        @Override
        public Repository createFromParcel(Parcel in) {
            return new Repository(in);
        }
 
        @Override
        public Repository[] newArray(int size) {
            return new Repository[size];
        }
    };
 
    //getters and setters
 
    public String getName() {
        return name;
    }
 
    public void setName(String name) {
        this.name = name;
    }
 
    public Owner getOwner() {
        return owner;
    }
 
    public void setOwner(Owner owner) {
        this.owner = owner;
    }
   
     public boolean isPrivate() {
        return isPrivate;
    }
 
    public void setPrivate(boolean isPrivate) {
        this.isPrivate = isPrivate;
    }
}

```

Owner is just a normal parcelable class.



## Using Enums with Parcelable


```java
/**
 * Created by Nick Cardoso on 03/08/16.
 * This is not a complete parcelable implementation, it only highlights the easiest 
 * way to read and write your Enum values to your parcel
 */
public class Foo implements Parcelable {

    private final MyEnum myEnumVariable;
    private final MyEnum mySaferEnumVariableExample;

    public Foo(Parcel in) {

        //the simplest way
        myEnumVariable = MyEnum.valueOf( in.readString() );

        //with some error checking
        try {
            mySaferEnumVariableExample= MyEnum.valueOf( in.readString() );
        } catch (IllegalArgumentException e) { //bad string or null value
            mySaferEnumVariableExample= MyEnum.DEFAULT;
        }

    }
    
    ...

    @Override
    public void writeToParcel(Parcel dest, int flags) {

        //the simple way
        dest.writeString(myEnumVariable.name()); 

        //avoiding NPEs with some error checking
        dest.writeString(mySaferEnumVariableExample == null? null : mySaferEnumVariableExample.name());

    }
    
}

public enum MyEnum {
    VALUE_1,
    VALUE_2,
    DEFAULT
}

```

This is preferable to (for example) using an ordinal, because inserting new values into your enum will not affect previously stored values



#### Remarks


It is important to remember that the order in which you write fields into a Parcel MUST BE THE SAME ORDER that you read them out from the parcel when constructing your custom object.

The parcelable interface has a strict 1 MB size limit. That means that any object, or combinations of objects, you put into a parcel that take up over 1MB of space will become corrupted on the other side. This can be hard to discover, so keep in mind what kind of objects you plan to make parcelable. If they have large dependency trees, consider another way in which to pass data around.

