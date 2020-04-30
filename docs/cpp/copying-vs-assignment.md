---
metaTitle: "Copying vs Assignment"
description: "Assignment Operator, Copy Constructor, Copy Constructor Vs Assignment Constructor"
---

# Copying vs Assignment



## Assignment Operator


The Assignment Operator is when you replace the data with an already existing(previously initialized) object with some other object's data. Lets take this as an example:

```cpp
// Assignment Operator
#include <iostream>
#include <string>

using std::cout;
using std::endl;

class Foo
{
  public:
    Foo(int data)
    {
        this->data = data;    
    }
    ~Foo(){};
    Foo& operator=(const Foo& rhs)
    {
            data = rhs.data; 
            return *this;
    }

    int data;
};

int main()
{
   Foo foo(2); //Foo(int data) called
   Foo foo2(42);
   foo = foo2; // Assignment Operator Called
   cout << foo.data << endl; //Prints 42
}

```

You can see here I call the assignment operator when I already initialized the `foo` object. Then later I assign `foo2` to `foo` . All the changes to appear when you call that equal sign operator is defined in your `operator=` function. You can see a runnable output here: [http://cpp.sh/3qtbm](http://cpp.sh/3qtbm)



## Copy Constructor


Copy constructor on the other hand , is the complete opposite of the Assignment Constructor. This time, it is used to initialize an already nonexistent(or non-previously initialized) object. This means it copies all the data from the object you are assigning it to , without actually initializing the object that is being copied onto. Now Let's take a look at the same code as before but modify the assignment constructor to be a copy constructor :

```cpp
// Copy Constructor
#include <iostream>
#include <string>

using std::cout;
using std::endl;

class Foo
{
  public:
    Foo(int data)
    {
        this->data = data;    
    }
    ~Foo(){};
    Foo(const Foo& rhs)
    {
            data = rhs.data; 
    }

    int data;
};

int main()
{
   Foo foo(2); //Foo(int data) called
   Foo foo2 = foo; // Copy Constructor called
   cout << foo2.data << endl;
}

```

You can see here `Foo foo2 = foo;` in the main function I immediately assign the object before actually initializing it, which as said before means it's a copy constructor. And notice that I didn't need to pass the parameter int for the `foo2` object since I automatically pulled the previous data from the object foo. Here is an example output : [http://cpp.sh/5iu7](http://cpp.sh/5iu7)



## Copy Constructor Vs Assignment Constructor


Ok we have briefly looked over what the copy constructor and assignment constructor are above and gave examples of each now let's see both of them in the same code. This code will be similar as above two. Let's take this :

```cpp
// Copy vs Assignment Constructor
#include <iostream>
#include <string>

using std::cout;
using std::endl;

class Foo
{
  public:
    Foo(int data)
    {
        this->data = data;    
    }
    ~Foo(){};
    Foo(const Foo& rhs)
    {
            data = rhs.data; 
    }

    Foo& operator=(const Foo& rhs)
    {
        data = rhs.data; 
        return *this;
    }

    int data;
};

int main()
{
   Foo foo(2); //Foo(int data) / Normal Constructor called
   Foo foo2 = foo; //Copy Constructor Called
   cout << foo2.data << endl;

   Foo foo3(42);
   foo3=foo; //Assignment Constructor Called
   cout << foo3.data << endl;
}

```

Output:

```cpp
2
2

```

Here you can see we first call the copy constructor by executing the line `Foo foo2 = foo;` . Since we didn't initialize it previously. And then next we call the assignment operator on foo3 since it was already initialized `foo3=foo`;



#### Syntax


- **Copy Constructor**
- MyClass( const MyClass& other );
- MyClass( MyClass& other );
- MyClass( volatile const MyClass& other );
- MyClass( volatile MyClass& other );
- **Assignment Constructor**
- MyClass& operator=( const MyClass& rhs );
- MyClass& operator=( MyClass& rhs );
- MyClass& operator=( MyClass rhs );
- const MyClass& operator=( const MyClass& rhs );
- const MyClass& operator=( MyClass& rhs );
- const MyClass& operator=( MyClass rhs );
- MyClass operator=( const MyClass& rhs );
- MyClass operator=( MyClass& rhs );
- MyClass operator=( MyClass rhs );



#### Parameters


|rhs|Right Hand Side of the equality for both copy and assignment constructors. For example the assignment constructor : MyClass operator=( MyClass& rhs );
|------
|Placeholder|Placeholder



#### Remarks


Other Good Resources for further research :

[What&#39;s the difference between assignment operator and copy constructor?](http://stackoverflow.com/questions/11706040/whats-the-difference-between-assignment-operator-and-copy-constructor)

[assignment operator vs. copy constructor C++](http://stackoverflow.com/questions/18969083/assignment-operator-vs-copy-constructor-c)

[GeeksForGeeks](http://www.geeksforgeeks.org/copy-constructor-vs-assignment-operator-in-c/)

[C++ Articles](http://www.cplusplus.com/articles/y8hv0pDG/)

