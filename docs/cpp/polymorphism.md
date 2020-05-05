---
metaTitle: "C++ | Polymorphism"
description: "Define polymorphic classes, Safe downcasting, Polymorphism & Destructors"
---

# Polymorphism



## Define polymorphic classes


The typical example is an abstract shape class, that can then be derived into squares, circles, and other concrete shapes.

**The parent class:**

Let's start with the polymorphic class:

```cpp
class Shape {
public:
    virtual ~Shape() = default;
    virtual double get_surface() const = 0;
    virtual void describe_object() const { std::cout << "this is a shape" << std::endl; }  

    double get_doubled_surface() const { return 2 * get_surface(); } 
};

```

How to read this definition ?

<li>
You can define polymorphic behavior by introduced member functions with the keyword `virtual`.  Here `get_surface()` and `describe_object()` will obviously be implemented differently for a square than for a circle.  When the function is invoked on an object, function corresponding to the real class of the object will be determined at runtime.
</li>
<li>
It makes no sense to define `get_surface()` for an abstract shape. This is why the function is followed by `= 0`. This means that the function is **pure virtual function**.
</li>
<li>
A polymorphic class should always define a virtual destructor.
</li>
<li>
You may define non virtual member functions.  When these function will be invoked for an object, the function will be chosen depending on the class used at compile-time. Here `get_double_surface()` is defined in this way.
</li>
<li>
A class that contains at least one pure virtual function is an abstract class. Abstract classes cannot be instantiated. You may only have pointers or references of an abstract class type.
</li>

**Derived classes**

Once a polymorphic base class is defined you can derive it.  For example:

```cpp
class Square : public Shape {
    Point top_left;
    double side_length;
public: 
    Square (const Point& top_left, double side)
       : top_left(top_left), side_length(side_length) {}

    double get_surface() override { return side_length * side_length; }   
    void describe_object() override { 
        std::cout << "this is a square starting at " << top_left.x << ", " << top_left.y
                  << " with a length of " << side_length << std::endl; 
    }  
};

```

Some explanations:

- You can define or override any of the virtual functions of the parent class.  The fact that a function was virtual in the parent class makes it virtual in the derived class.  No need to tell the compiler the keyword `virtual` again.  But it's recommended to add the keyword `override` at the end of the function declaration, in order to prevent subtle bugs caused by unnoticed variations in the function signature.
- If all the pure virtual functions of the parent class are defined you can instantiate objects for this class, else it will also become an abstract class.
- You are not obliged to override all the virtual functions.  You can keep the version of the parent if it suits your need.

**Example of instantiation**

```cpp
int main() {

    Square square(Point(10.0, 0.0), 6); // we know it's a square, the compiler also
    square.describe_object(); 
    std::cout << "Surface: " << square.get_surface() << std::endl; 

    Circle circle(Point(0.0, 0.0), 5);

    Shape *ps = nullptr;  // we don't know yet the real type of the object
    ps = &circle;         // it's a circle, but it could as well be a square
    ps->describe_object(); 
    std::cout << "Surface: " << ps->get_surface() << std::endl;
}

```



## Safe downcasting


Suppose that you have a pointer to an object of a polymorphic class:

```cpp
Shape *ps;                       // see example on defining a polymorphic class
ps =  get_a_new_random_shape();  // if you don't have such a function yet, you 
                                 // could just write ps = new Square(0.0,0.0, 5);

```

a downcast would be to cast from a general polymorphic `Shape` down to one of its derived and more specific shape like `Square` or `Circle`.

**Why to downcast ?**

Most of the time, you would not need to know which is the real type of the object, as the virtual functions allow you to manipulate your object independently of its type:

```cpp
std::cout << "Surface: " << ps->get_surface() << std::endl; 

```

If you don't need any downcast, your design would be perfect.

However, you may need sometimes to downcast.  A typical example is when you want to invoke a non virtual function that exist only for the child class.

Consider for example circles.  Only circles have a diameter. So the class would be defined as :

```cpp
class Circle: public Shape { // for Shape, see example on defining a polymorphic class
    Point center;
    double radius;
public: 
    Circle (const Point& center, double radius)
       : center(center), radius(radius) {}

    double get_surface() const override { return r * r * M_PI; }   

    // this is only for circles. Makes no sense for other shapes 
    double get_diameter() const { return 2 * r; }
};

```

The `get_diameter()` member function only exist for circles. It was not defined for a `Shape` object:

```cpp
Shape* ps = get_any_shape();
ps->get_diameter(); // OUCH !!! Compilation error 

```

**How to downcast ?**

If you'd know for sure that `ps` points to a circle you could opt for a `static_cast`:

```cpp
std::cout << "Diameter: " << static_cast<Circle*>(ps)->get_diameter() << std::endl;

```

This will do the trick. But it's very risky:  if `ps` appears to by anything else than a `Circle` the behavior of your code will be undefined.

So rather than playing Russian roulette, you should safely use a `dynamic_cast`.  This is specifically for polymorphic classes  :

```cpp
int main() {
    Circle circle(Point(0.0, 0.0), 10);
    Shape &shape = circle;

    std::cout << "The shape has a surface of " << shape.get_surface() << std::endl;

    //shape.get_diameter();   // OUCH !!! Compilation error 

    Circle *pc = dynamic_cast<Circle*>(&shape); // will be nullptr if ps wasn't a circle 
    if (pc) 
        std::cout << "The shape is a circle of diameter " << pc->get_diameter() << std::endl;
    else
        std::cout << "The shape isn't a circle !" << std::endl; 
}        

```

Note that `dynamic_cast` is not possible on a class that is not polymorphic.  You'd need at least one virtual function in the class or its parents to be able to use it.



## Polymorphism & Destructors


If a class is intended to be used polymorphically, with derived instances being stored as base pointers/references, its base class' destructor should be either `virtual` or `protected`.  In the former case, this will cause object destruction to check the `vtable`, automatically calling the correct destructor based on the dynamic type.  In the latter case, destroying the object through a base class pointer/reference is disabled, and the object can only be deleted when explicitly treated as its actual type.

```cpp
struct VirtualDestructor {
    virtual ~VirtualDestructor() = default;
};

struct VirtualDerived : VirtualDestructor {};

struct ProtectedDestructor {
  protected:
    ~ProtectedDestructor() = default;
};

struct ProtectedDerived : ProtectedDestructor {
    ~ProtectedDerived() = default;
};

// ...

VirtualDestructor* vd = new VirtualDerived;
delete vd; // Looks up VirtualDestructor::~VirtualDestructor() in vtable, sees it's
           // VirtualDerived::~VirtualDerived(), calls that.

ProtectedDestructor* pd = new ProtectedDerived;
delete pd; // Error: ProtectedDestructor::~ProtectedDestructor() is protected.
delete static_cast<ProtectedDerived*>(pd); // Good.

```

Both of these practices guarantee that the derived class' destructor will always be called on derived class instances, preventing memory leaks.

