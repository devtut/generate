---
metaTitle: "Design pattern implementation in C++"
description: "Adapter Pattern, Observer pattern, Factory Pattern, Builder Pattern with Fluent API"
---

# Design pattern implementation in C++


On this page, you can find examples of how design patterns are implemented in C++. For the details on these patterns, you can check out [the design patterns documentation](http://stackoverflow.com/documentation/design-patterns/topics).



## Adapter Pattern


Convert the interface of a class into another interface clients expect. Adapter (or Wrapper) lets classes work together that couldn't otherwise because of incompatible interfaces. Adapter pattern's motivation is that we can reuse existing software if we can modify the interface.

<li>
Adapter pattern relies on object composition.
</li>
<li>
Client calls operation on Adapter object.
</li>
<li>
Adapter calls Adaptee to carry out the operation.
</li>
<li>
<p>In STL, stack adapted from vector:
When stack executes push(), underlying vector does vector::push_back().</p>
</li>

**Example:**

```cpp
#include <iostream>

// Desired interface (Target)
class Rectangle 
{
  public:
    virtual void draw() = 0;
};

// Legacy component (Adaptee)
class LegacyRectangle 
{
  public:
    LegacyRectangle(int x1, int y1, int x2, int y2) {
        x1_ = x1;
        y1_ = y1;
        x2_ = x2;
        y2_ = y2;
        std::cout << "LegacyRectangle(x1,y1,x2,y2)\n";
    }
    void oldDraw() {
        std::cout << "LegacyRectangle:  oldDraw(). \n";
    }
  private:
    int x1_;
    int y1_;
    int x2_;
    int y2_;
};

// Adapter wrapper
class RectangleAdapter: public Rectangle, private LegacyRectangle 
{
  public:
    RectangleAdapter(int x, int y, int w, int h):
      LegacyRectangle(x, y, x + w, y + h) {
         std::cout << "RectangleAdapter(x,y,x+w,x+h)\n";
      }

    void draw() {
        std::cout << "RectangleAdapter: draw().\n"; 
        oldDraw();
    }
};

int main()
{
  int x = 20, y = 50, w = 300, h = 200;
  Rectangle *r = new RectangleAdapter(x,y,w,h);
  r->draw();
}

//Output:
//LegacyRectangle(x1,y1,x2,y2)
//RectangleAdapter(x,y,x+w,x+h)

```

**Summary of the code:**

<li>
The client thinks he is talking to a `Rectangle`
</li>
<li>
The target is the `Rectangle` class. This is what the client invokes method on.

```cpp
 Rectangle *r = new RectangleAdapter(x,y,w,h);
 r->draw();

```


</li>
<li>
Note that the adapter class uses multiple inheritance.

```cpp
 class RectangleAdapter: public Rectangle, private LegacyRectangle {
     ...
 }

```


</li>
<li>
The Adapter `RectangleAdapter` lets the `LegacyRectangle` responds to request (`draw()` on a `Rectangle`) by inheriting BOTH classes.
</li>
<li>
<p>The `LegacyRectangle` class does not have the same methods (`draw()`) as `Rectangle`,
but the `Adapter(RectangleAdapter)` can take the `Rectangle` method calls and turn around and invoke method on the `LegacyRectangle`, `oldDraw()`.</p>

```cpp
 class RectangleAdapter: public Rectangle, private LegacyRectangle {
   public:
     RectangleAdapter(int x, int y, int w, int h):
       LegacyRectangle(x, y, x + w, y + h) {
         std::cout << "RectangleAdapter(x,y,x+w,x+h)\n";
       }

     void draw() {
         std::cout << "RectangleAdapter: draw().\n"; 
         oldDraw();
     }
 };

```


</li>

**Adapter** design pattern translates the interface for one class into a compatible but different interface. So, this is similar to the **proxy** pattern in that it's a single-component wrapper. But the interface for the adapter class and the original class may be different.

As we've seen in the example above, this **adapter** pattern is useful to expose a different interface for an existing API to allow it to work with other code. Also, by using adapter pattern, we can take heterogeneous interfaces, and transform them to provide consistent API.

**Bridge pattern** has a structure similar to an object adapter, but Bridge has a different intent: It is meant to **separate** an interface from its implementation so that they can be varied easily and independently. An **adapter** is meant to **change the interface** of an **existing** object.



## Observer pattern


Observer Pattern's intent is to define a one-to-many dependency between objects so that when one object changes state, all its dependents are notified and updated automatically.

The subject and observers define the one-to-many relationship. The observers are dependent on the subject such that when the subject's state changes, the observers get notified. Depending on the notification, the observers may also be updated with new values.

Here is the example from the book "Design Patterns" by Gamma.

```cpp
#include <iostream>
#include <vector>

class Subject; 

class Observer 
{ 
public:
    virtual ~Observer() = default;
    virtual void Update(Subject&) = 0;
};

class Subject 
{ 
public: 
     virtual ~Subject() = default;
     void Attach(Observer& o) { observers.push_back(&o); }
     void Detach(Observer& o)
     {
         observers.erase(std::remove(observers.begin(), observers.end(), &o));
     }
     void Notify()
     {
         for (auto* o : observers) {
             o->Update(*this);
         }
     }
private:
     std::vector<Observer*> observers; 
};

class ClockTimer : public Subject 
{ 
public:

    void SetTime(int hour, int minute, int second)
    {
        this->hour = hour; 
        this->minute = minute;
        this->second = second;

        Notify(); 
    }

    int GetHour() const { return hour; }
    int GetMinute() const { return minute; }
    int GetSecond() const { return second; }

private: 
    int hour;
    int minute;
    int second;
}; 

class DigitalClock: public Observer 
{ 
public: 
     explicit DigitalClock(ClockTimer& s) : subject(s) { subject.Attach(*this); }
     ~DigitalClock() { subject.Detach(*this); }
     void Update(Subject& theChangedSubject) override
     {
         if (&theChangedSubject == &subject) {
             Draw();
         }
     }

     void Draw()
     {
         int hour = subject.GetHour(); 
         int minute = subject.GetMinute(); 
         int second = subject.GetSecond(); 

         std::cout << "Digital time is " << hour << ":" 
                   << minute << ":" 
                   << second << std::endl;           
     }

private:
     ClockTimer& subject;
};

class AnalogClock: public Observer 
{ 
public: 
     explicit AnalogClock(ClockTimer& s) : subject(s) { subject.Attach(*this); }
     ~AnalogClock() { subject.Detach(*this); }
     void Update(Subject& theChangedSubject) override
     {
         if (&theChangedSubject == &subject) {
             Draw();
         }
     }
     void Draw()
     {
         int hour = subject.GetHour(); 
         int minute = subject.GetMinute(); 
         int second = subject.GetSecond(); 

         std::cout << "Analog time is " << hour << ":" 
                   << minute << ":" 
                   << second << std::endl; 
     }
private:
     ClockTimer& subject;
};

int main()
{ 
    ClockTimer timer; 

    DigitalClock digitalClock(timer); 
    AnalogClock analogClock(timer); 

    timer.SetTime(14, 41, 36);
}

```

Output:

```cpp
Digital time is 14:41:36
Analog time is 14:41:36

```

Here are the summary of the pattern:

<li>
Objects (`DigitalClock` or `AnalogClock` object) use the Subject interfaces (`Attach()` or `Detach()`) either to subscribe (register) as observers or unsubscribe (remove) themselves from being observers (`subject.Attach(*this);` , `subject.Detach(*this);`.
</li>
<li>
Each subject can have many observers( `vector<Observer*> observers;`).
</li>
<li>
All observers need to implement the Observer interface. This interface just has one method, `Update()`, that gets called when the Subject's state changes (`Update(Subject &)`)
</li>
<li>
In addition to the `Attach()` and `Detach()` methods, the concrete subject implements a `Notify()` method that is used to update all the current observers whenever state changes. But in this case, all of them are done in the parent class, `Subject` (`Subject::Attach (Observer&)`, `void Subject::Detach(Observer&)` and `void Subject::Notify()` .
</li>
<li>
The Concrete object may also have methods for setting and getting its state.
</li>
<li>
Concrete observers can be any class that implements the Observer interface. Each observer subscribe (register) with a concrete subject to receive update (`subject.Attach(*this);` ).
</li>
<li>
The two objects of Observer Pattern are **loosely coupled**, they can interact but with little knowledge of each other.
</li>

**Variation:**

Signal and Slots

Signals and slots is a language construct introduced in Qt, which makes it easy to implement the Observer pattern while avoiding boilerplate code. The concept is that controls (also known as widgets) can send signals containing event information which can be received by other controls using special functions known as slots. The slot in Qt must be a class member declared as such.
The signal/slot system fits well with the way Graphical User Interfaces are designed. Similarly, the signal/slot system can be used for asynchronous I/O (including sockets, pipes, serial devices, etc.) event notification or to associate timeout events with appropriate object instances and methods or functions. No registration/deregistration/invocation code need be written, because Qt's Meta Object Compiler (MOC) automatically generates the needed infrastructure.

The C# language also supports a similar construct although with a different terminology and syntax: events play the role of signals, and delegates are the slots. Additionally, a delegate can be a local variable, much like a function pointer, while a slot in Qt must be a class member declared as such.



## Factory Pattern


Factory pattern decouples object creation and allows creation by name using a common interface:

```cpp
class Animal{
public:
    virtual std::shared_ptr<Animal> clone() const = 0;
    virtual std::string  getname() const = 0;
};

class Bear: public Animal{
public:
    virtual std::shared_ptr<Animal> clone() const override
    {
        return std::make_shared<Bear>(*this);
    }
    virtual std::string getname() const override
    {
        return "bear";
    }
};


class Cat: public Animal{
public:
    virtual std::shared_ptr<Animal> clone() const override
    {
        return std::make_shared<Cat>(*this);
    }
    virtual std::string  getname() const override
    {
        return "cat";
    }
};

class AnimalFactory{
public:
    static std::shared_ptr<Animal> getAnimal( const std::string&   name )
    {
      if ( name == "bear" )
        return std::make_shared<Bear>();
      if ( name == "cat" )
        return std::shared_ptr<Cat>();
     
    return nullptr;
    }


};

```



## Builder Pattern with Fluent API


The Builder Pattern decouples the creation of the object from the object itself. The main idea behind is that **an object does not have to be responsible for its own creation**. The correct and valid assembly of a complex object may be a complicated task in itself, so this task can be delegated to another class.

Inspired by the [Email Builder in C#](http://stackoverflow.com/documentation/design-patterns/1811/builder-pattern/5912/builder-pattern-c-sharp-fluent-interrface#t=201704231441269920397), I've decided to make a C++ version here. An Email object is not necessarily a **very complex object**, but it can demonstrate the pattern.

```cpp
#include <iostream>
#include <sstream>
#include <string>

using namespace std;

// Forward declaring the builder
class EmailBuilder;

class Email
{
  public:
    friend class EmailBuilder;  // the builder can access Email's privates
    
    static EmailBuilder make();
    
    string to_string() const {
        stringstream stream;
        stream << "from: " << m_from
               << "\nto: " << m_to
               << "\nsubject: " << m_subject
               << "\nbody: " << m_body;
        return stream.str();
    }
    
  private:
    Email() = default; // restrict construction to builder
    
    string m_from;
    string m_to;
    string m_subject;
    string m_body;
};

class EmailBuilder
{
  public:
    EmailBuilder& from(const string &from) {
        m_email.m_from = from;
        return *this;
    }
    
    EmailBuilder& to(const string &to) {
        m_email.m_to = to;
        return *this;
    }
    
    EmailBuilder& subject(const string &subject) {
        m_email.m_subject = subject;
        return *this;
    }
    
    EmailBuilder& body(const string &body) {
        m_email.m_body = body;
        return *this;
    }
    
    operator Email&&() {
        return std::move(m_email); // notice the move
    }
    
  private:
    Email m_email;
};

EmailBuilder Email::make()
{
    return EmailBuilder();
}

// Bonus example!
std::ostream& operator <<(std::ostream& stream, const Email& email)
{
    stream << email.to_string();
    return stream;
}


int main()
{
    Email mail = Email::make().from("me@mail.com")
                              .to("you@mail.com")
                              .subject("C++ builders")
                              .body("I like this API, don't you?");
                              
    cout << mail << endl;
}

```

For older versions of C++, one may just ignore the `std::move` operation and remove the && from the conversion operator (although this will create a temporary copy).

The builder finishes its work when it releases the built email by the `operator Email&&()`. In this example, the builder is a temporary object and returns the email before being destroyed. You could also use an explicit operation like `Email EmailBuilder::build() {...}` instead of the conversion operator.

### Pass the builder around

A great feature the Builder Pattern provides is the ability to **use several actors to build an object together.** This is done by passing the builder to the other actors that will each one give some more information to the built object. This is specially powerful when you are building some sort of query, adding filters and other specifications.

```cpp
void add_addresses(EmailBuilder& builder)
{
    builder.from("me@mail.com")
           .to("you@mail.com");
}

void compose_mail(EmailBuilder& builder)
{
    builder.subject("I know the subject")
           .body("And the body. Someone else knows the addresses.");
}

int main()
{
    EmailBuilder builder;
    add_addresses(builder);
    compose_mail(builder);
    
    Email mail = builder;
    cout << mail << endl;
}

```

### Design variant : Mutable object

You can change the design of this pattern to fit your needs. I'll give one variant.

In the given example the Email object is immutable, i.e., it's properties can't be modified because there is no access to them. This was a desired feature. If you need to modify the object after its creation you have to provide some setters to it. Since those setters would be duplicated in the builder, you may consider to do it all in one class (no builder class needed anymore). Nevertheless, I would consider the need to make the built object mutable in the first place.



#### Remarks


A design pattern is a general reusable solution to a commonly occurring problem within a given context in software design.

