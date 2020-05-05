---
metaTitle: "C++ | Singleton Design Pattern"
description: "Lazy Initialization, Static deinitialization-safe singleton., Subclasses, Thread-safe Singeton"
---

# Singleton Design Pattern



## Lazy Initialization


This example has been lifted from the `Q & A` section here:[http://stackoverflow.com/a/1008289/3807729](http://stackoverflow.com/a/1008289/3807729)

See this article for a simple design for a lazy evaluated with guaranteed destruction singleton:<br />
[Can any one provide me a sample of Singleton in c++?](http://stackoverflow.com/questions/270947/can-any-one-provide-me-a-sample-of-singleton-in-c/271104#271104)

**The classic lazy evaluated and correctly destroyed singleton.**

```cpp
class S
{
    public:
        static S& getInstance()
        {
            static S    instance; // Guaranteed to be destroyed.
                                  // Instantiated on first use.
            return instance;
        }
    private:
        S() {};                   // Constructor? (the {} brackets) are needed here.

        // C++ 03
        // ========
        // Dont forget to declare these two. You want to make sure they
        // are unacceptable otherwise you may accidentally get copies of
        // your singleton appearing.
        S(S const&);              // Don't Implement
        void operator=(S const&); // Don't implement

        // C++ 11
        // =======
        // We can use the better technique of deleting the methods
        // we don't want.
    public:
        S(S const&)               = delete;
        void operator=(S const&)  = delete;

        // Note: Scott Meyers mentions in his Effective Modern
        //       C++ book, that deleted functions should generally
        //       be public as it results in better error messages
        //       due to the compilers behavior to check accessibility
        //       before deleted status
};

```

See this article about when to use a singleton: (not often)<br />
[Singleton: How should it be used](http://stackoverflow.com/questions/86582/singleton-how-should-it-be-used)

See this two article about initialization order and how to cope:<br />
[Static variables initialisation order](http://stackoverflow.com/questions/211237/c-static-variables-initialisation-order/211307#211307)<br />
[Finding C++ static initialization order problems](http://stackoverflow.com/questions/335369/finding-c-static-initialization-order-problems/335746#335746)

See this article describing lifetimes:<br />
[What is the lifetime of a static variable in a C++ function?](http://stackoverflow.com/questions/246564/what-is-the-lifetime-of-a-static-variable-in-a-c-function)

See this article that discusses some threading implications to singletons:<br />
[Singleton instance declared as static variable of GetInstance method](http://stackoverflow.com/questions/449436/singleton-instance-declared-as-static-variable-of-getinstance-method/449823#449823)

See this article that explains why double checked locking will not work on C++:<br />
[What are all the common undefined behaviours that a C++ programmer should know about?](http://stackoverflow.com/questions/367633/what-are-all-the-common-undefined-behaviour-that-c-programmer-should-know-about/367690#367690)



## Static deinitialization-safe singleton.


There are times with multiple static objects where you need to be able to guarantee that the **singleton** will not be destroyed until all the static objects that use the **singleton** no longer need it.

In this case `std::shared_ptr` can be used to keep the **singleton** alive for all users even when the static destructors are being called at the end of the program:

```cpp
class Singleton
{
public:
    Singleton(Singleton const&) = delete;
    Singleton& operator=(Singleton const&) = delete;

    static std::shared_ptr<Singleton> instance()
    {
        static std::shared_ptr<Singleton> s{new Singleton};
        return s;
    }

private:
    Singleton() {}
};

```

NOTE: [This example appears as an answer in the Q&A section here.](http://stackoverflow.com/a/40337728/3807729)



## Subclasses


```cpp
class API
{
public:
    static API& instance();
    
    virtual ~API() {}
    
    virtual const char* func1() = 0;
    virtual void func2() = 0;
    
protected:
    API() {}
    API(const API&) = delete;
    API& operator=(const API&) = delete;
};

class WindowsAPI : public API
{
public:
    virtual const char* func1()  override { /* Windows code */ }
    virtual void func2() override { /* Windows code */ }    
};

class LinuxAPI : public API
{
public:
    virtual const char* func1() override { /* Linux code */ }
    virtual void func2() override { /* Linux code */ }    
};

API& API::instance() {
#if PLATFORM == WIN32
    static WindowsAPI instance;
#elif PLATFORM = LINUX
    static LinuxAPI instance;
#endif
    return instance;
}

```

In this example, a simple compiler switch binds the `API` class to the appropriate subclass. In this way, `API` can be accessed without being coupled to platform-specific code.



## Thread-safe Singeton


The C++11 standards guarantees that the initialization of function scope objects are initialized in a synchronized manner. This can be used to implement a thread-safe singleton with [lazy initialization](http://stackoverflow.com/documentation/c%2B%2B/2713/singleton-design-pattern/9075/lazy-initialization).

```cpp
class Foo
{
public:
    static Foo& instance()
    {
        static Foo inst;
        return inst;
    }        
private:
    Foo() {}
    Foo(const Foo&) = delete;
    Foo& operator =(const Foo&) = delete;
};

```



#### Remarks


A **Singleton** is designed to ensure a class only has one instance **and** provides a global point of access to it. If you only require one instance **or** a convenient global point of access, but not both, consider other options before turning to the singleton.

Global variables **can** make it harder to reason about code. For example, if one of the calling functions isn't happy with the data it's receiving from a Singleton, you now have to track down what is first giving the singleton bad data in the first place.

Singletons also encourage [coupling](https://en.wikipedia.org/wiki/Coupling_(computer_programming)), a term used to describe two components of code that are joined together thus reducing each components own measure of self-containment.

Singletons aren't concurrency friendly. When a class has a global point of access, every thread has the ability to access it which can lead to deadlocks and race conditions.

Lastly, lazy initialization can cause performance issues if initialized at the wrong time. Removing lazy initialization also removes some of the features that do make Singleton's interesting in the first place, such as polymorphism (see Subclasses).

Sources: [Game Programming Patterns](http://gameprogrammingpatterns.com/singleton.html) by [Robert Nystrom](https://twitter.com/munificentbob)

