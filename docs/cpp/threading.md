---
metaTitle: "Threading"
description: "Creating a std::thread, Passing a reference to a thread, Using std::async instead of std::thread, Ensuring a thread is always joined, Basic Synchronization, Create a simple thread pool, Operations on the current thread, Using Condition Variables, Thread operations, Thread-local storage, Reassigning thread objects"
---

# Threading




## Creating a std::thread


In C++, threads are created using the std::thread class. A thread is a separate flow of execution; it is analogous to having a helper perform one task while you simultaneously perform another. When all the code in the thread is executed, it **terminates**.  When creating a thread, you need to pass something to be executed on it. A few things that you can pass to a thread:

- Free functions
- Member functions
- Functor objects
- Lambda expressions

Free function example - executes a function on a separate thread ([Live Example](http://ideone.com/hX1Ygn)):

```cpp
#include <iostream>
#include <thread>
 
void foo(int a)
{
    std::cout << a << '\n';
}
 
int main()
{
    // Create and execute the thread
    std::thread thread(foo, 10); // foo is the function to execute, 10 is the
                                 // argument to pass to it
 
    // Keep going; the thread is executed separately
 
    // Wait for the thread to finish; we stay here until it is done
    thread.join();
 
    return 0;
}

```

Member function example - executes a member function on a separate thread ([Live Example](http://ideone.com/4QeG4E)):

```cpp
#include <iostream>
#include <thread>
 
class Bar
{
public:
    void foo(int a)
    {
        std::cout << a << '\n';
    }
};
 
int main()
{
    Bar bar;
    
    // Create and execute the thread
    std::thread thread(&Bar::foo, &bar, 10); // Pass 10 to member function
 
    // The member function will be executed in a separate thread
 
    // Wait for the thread to finish, this is a blocking operation
    thread.join();
 
    return 0;
}

```

Functor object example ([Live Example](http://ideone.com/h2EepE)):

```cpp
#include <iostream>
#include <thread>
 
class Bar
{
public:
    void operator()(int a)
    {
        std::cout << a << '\n';
    }
};
 
int main()
{
    Bar bar;
    
    // Create and execute the thread
    std::thread thread(bar, 10); // Pass 10 to functor object
 
    // The functor object will be executed in a separate thread
 
    // Wait for the thread to finish, this is a blocking operation
    thread.join();
 
    return 0;
}

```

Lambda expression example ([Live Example](http://ideone.com/UacLRf)):

```cpp
#include <iostream>
#include <thread>
 
int main()
{
    auto lambda = [](int a) { std::cout << a << '\n'; };

    // Create and execute the thread
    std::thread thread(lambda, 10); // Pass 10 to the lambda expression
 
    // The lambda expression will be executed in a separate thread
 
    // Wait for the thread to finish, this is a blocking operation
    thread.join();
 
    return 0;
}

```



## Passing a reference to a thread


You cannot pass a reference (or `const` reference) directly to a thread because `std::thread` will copy/move them. Instead, use `std::reference_wrapper`:

```cpp
void foo(int& b)
{
    b = 10;
}

int a = 1;
std::thread thread{ foo, std::ref(a) }; //'a' is now really passed as reference

thread.join();
std::cout << a << '\n'; //Outputs 10

```

```cpp
void bar(const ComplexObject& co)
{
    co.doCalculations();
}

ComplexObject object;
std::thread thread{ bar, std::cref(object) }; //'object' is passed as const&

thread.join();
std::cout << object.getResult() << '\n'; //Outputs the result

```



## Using std::async instead of std::thread


`std::async` is also able to make threads. Compared to `std::thread` it is considered less powerful but easier to use when you just want to run a function asynchronously.

### Asynchronously calling a function

```cpp
#include <future>
#include <iostream>

unsigned int square(unsigned int i){
    return i*i;
}

int main() {
    auto f = std::async(std::launch::async, square, 8);
    std::cout << "square currently running\n"; //do something while square is running
    std::cout << "result is " << f.get() << '\n'; //getting the result from square
}

```

### Common Pitfalls

<li>
`std::async` returns a `std::future` that holds the return value that will be calculated by the function. When that `future` gets destroyed it waits until the thread completes, making your code effectively single threaded. This is easily overlooked when you don't need the return value:

```cpp
std::async(std::launch::async, square, 5);
//thread already completed at this point, because the returning future got destroyed

```


</li>
<li>
`std::async` works without a launch policy, so `std::async(square, 5);` compiles. When you do that the system gets to decide if it wants to create a thread or not. The idea was that the system chooses to make a thread unless it is already running more threads than it can run efficiently. Unfortunately implementations commonly just choose not to create a thread in that situation, ever, so you need to override that behavior with `std::launch::async` which forces the system to create a thread.
</li>
<li>
Beware of race conditions.
</li>

More on async on [Futures and Promises](http://stackoverflow.com/documentation/c%2B%2B/9840/futures-and-promises#t=201704281532312052997)



## Ensuring a thread is always joined


When the destructor for `std::thread` is invoked, a call to either `join()` or `detach()` **must** have been made. If a thread has not been joined or detached, then by default `std::terminate` will be called. Using [RAII](http://en.cppreference.com/w/cpp/language/raii), this is generally simple enough to accomplish:

```cpp
class thread_joiner
{
public:

    thread_joiner(std::thread t)
        : t_(std::move(t))
    { }

    ~thread_joiner()
    {
        if(t_.joinable()) {
            t_.join();
        }
    }

private:

    std::thread t_;
}

```

This is then used like so:

```

void perform_work()
 {
     // Perform some work
 }

 void t()
 {
     thread_joiner j{std::thread(perform_work)};
     // Do some other calculations while thread is running
 } // Thread is automatically joined here

```

This also provides exception safety; if we had created our thread normally and the work done in `t()` performing other calculations had thrown an exception, `join()` would never have been called on our thread and our process would have been terminated.



## Basic Synchronization


Thread synchronization can be accomplished using mutexes, among other synchronization primitives. There are several mutex types provided by the standard library, but the simplest is `std::mutex`. To lock a mutex, you construct a lock on it. The simplest lock type is `std::lock_guard`:

```cpp
std::mutex m;
void worker() {
    std::lock_guard<std::mutex> guard(m); // Acquires a lock on the mutex
    // Synchronized code here
} // the mutex is automatically released when guard goes out of scope

```

With `std::lock_guard` the mutex is locked for the whole lifetime of the lock object. In cases where you need to manually control the regions for locking, use `std::unique_lock` instead:

```cpp
std::mutex m;
void worker() {
    // by default, constructing a unique_lock from a mutex will lock the mutex
    // by passing the std::defer_lock as a second argument, we
    // can construct the guard in an unlocked state instead and
    // manually lock later.
    std::unique_lock<std::mutex> guard(m, std::defer_lock);
    // the mutex is not locked yet!
    guard.lock();
    // critical section
    guard.unlock();
    // mutex is again released
}

```

More [Thread synchronization structures](http://stackoverflow.com/documentation/c%2B%2B/9794/thread-synchronization-structures#t=201704240523319231466)



## Create a simple thread pool


C++11 threading primitives are still relatively low level.  They can be used to write a higher level construct, like a thread pool:

```cpp
struct tasks {
  // the mutex, condition variable and deque form a single
  // thread-safe triggered queue of tasks:
  std::mutex m;
  std::condition_variable v;
  // note that a packaged_task<void> can store a packaged_task<R>:
  std::deque<std::packaged_task<void()>> work;

  // this holds futures representing the worker threads being done:
  std::vector<std::future<void>> finished;

  // queue( lambda ) will enqueue the lambda into the tasks for the threads
  // to use.  A future of the type the lambda returns is given to let you get
  // the result out.
  template<class F, class R=std::result_of_t<F&()>>
  std::future<R> queue(F&& f) {
    // wrap the function object into a packaged task, splitting
    // execution from the return value:
    std::packaged_task<R()> p(std::forward<F>(f));

    auto r=p.get_future(); // get the return value before we hand off the task
    {
      std::unique_lock<std::mutex> l(m);
      work.emplace_back(std::move(p)); // store the task<R()> as a task<void()>
    }
    v.notify_one(); // wake a thread to work on the task

    return r; // return the future result of the task
  }

  // start N threads in the thread pool.
  void start(std::size_t N=1){
    for (std::size_t i = 0; i < N; ++i)
    {
      // each thread is a std::async running this->thread_task():
      finished.push_back(
        std::async(
          std::launch::async,
          [this]{ thread_task(); }
        )
      );
    }
  }
  // abort() cancels all non-started tasks, and tells every working thread
  // stop running, and waits for them to finish up.
  void abort() {
    cancel_pending();
    finish();
  }
  // cancel_pending() merely cancels all non-started tasks:
  void cancel_pending() {
    std::unique_lock<std::mutex> l(m);
    work.clear();
  }
  // finish enques a "stop the thread" message for every thread, then waits for them:
  void finish() {
    {
      std::unique_lock<std::mutex> l(m);
      for(auto&&unused:finished){
        work.push_back({});
      }
    }
    v.notify_all();
    finished.clear();
  }
  ~tasks() {
    finish();
  }
private:
  // the work that a worker thread does:
  void thread_task() {
    while(true){
      // pop a task off the queue:
      std::packaged_task<void()> f;
      {
        // usual thread-safe queue code:
        std::unique_lock<std::mutex> l(m);
        if (work.empty()){
          v.wait(l,[&]{return !work.empty();});
        }
        f = std::move(work.front());
        work.pop_front();
      }
      // if the task is invalid, it means we are asked to abort:
      if (!f.valid()) return;
      // otherwise, run the task:
      f();
    }
  }
};

```

`tasks.queue( []{ return "hello world"s; } )` returns a `std::future<std::string>`, which when the tasks object gets around to running it is populated with `hello world`.

You create threads by running `tasks.start(10)` (which starts 10 threads).

The use of `packaged_task<void()>` is merely because there is no type-erased `std::function` equivalent that stores move-only types.  Writing a custom one of those would probably be faster than using `packaged_task<void()>`.

[Live example](http://coliru.stacked-crooked.com/).

In C++11, replace `result_of_t<blah>` with `typename result_of<blah>::type`.

More on [Mutexes](http://stackoverflow.com/documentation/c%2B%2B/9895/mutexes#t=201705101230556157064).



## Operations on the current thread


`std::this_thread` is a `namespace` which has functions to do interesting things on the current thread from function it is called from.

|Function|Description
|---|---|---|---|---|---|---|---|---|---
|`get_id`|Returns the id of the thread
|`sleep_for`|Sleeps for a specified amount of time
|`sleep_until`|Sleeps until a specific time
|`yield`|Reschedule running threads, giving other threads priority

Getting the current threads id using `std::this_thread::get_id`:

```cpp
void foo()
{
    //Print this threads id
    std::cout << std::this_thread::get_id() << '\n';
}

std::thread thread{ foo };
thread.join(); //'threads' id has now been printed, should be something like 12556

foo(); //The id of the main thread is printed, should be something like 2420

```

Sleeping for 3 seconds using `std::this_thread::sleep_for`:

```cpp
void foo()
{
    std::this_thread::sleep_for(std::chrono::seconds(3));
}

std::thread thread{ foo };
foo.join();

std::cout << "Waited for 3 seconds!\n";

```

Sleeping until 3 hours in the future using  `std::this_thread::sleep_until`:

```cpp
void foo()
{
    std::this_thread::sleep_until(std::chrono::system_clock::now() + std::chrono::hours(3));
}

std::thread thread{ foo };
thread.join();

std::cout << "We are now located 3 hours after the thread has been called\n";

```

Letting other threads take priority using `std::this_thread::yield`:

```cpp
void foo(int a)
{
    for (int i = 0; i < al ++i)
        std::this_thread::yield(); //Now other threads take priority, because this thread
                                   //isn't doing anything important

    std::cout << "Hello World!\n";
}

std::thread thread{ foo, 10 };
thread.join();

```



## Using Condition Variables


A condition variable is a primitive used in conjunction with a mutex to orchestrate communication between threads. While it is neither the exclusive or most efficient way to accomplish this, it can be among the simplest to those familiar with the pattern.

One waits on a `std::condition_variable` with a `std::unique_lock<std::mutex>`. This allows the code to safely examine shared state before deciding whether or not to proceed with acquisition.

Below is a producer-consumer sketch that uses `std::thread`, `std::condition_variable`, `std::mutex`, and a few others to make things interesting.

```cpp
#include <condition_variable>
#include <cstddef>
#include <iostream>
#include <mutex>
#include <queue>
#include <random>
#include <thread>


int main()
{
    std::condition_variable cond;
    std::mutex mtx;
    std::queue<int> intq;
    bool stopped = false;

    std::thread producer{[&]()
    {
        // Prepare a random number generator.
        // Our producer will simply push random numbers to intq.
        //
        std::default_random_engine gen{};
        std::uniform_int_distribution<int> dist{};

        std::size_t count = 4006;    
        while(count--)
        {    
            // Always lock before changing
            // state guarded by a mutex and
            // condition_variable (a.k.a. "condvar").
            std::lock_guard<std::mutex> L{mtx};

            // Push a random int into the queue
            intq.push(dist(gen));

            // Tell the consumer it has an int
            cond.notify_one();
        }

        // All done.
        // Acquire the lock, set the stopped flag,
        // then inform the consumer.
        std::lock_guard<std::mutex> L{mtx};

        std::cout << "Producer is done!" << std::endl;

        stopped = true;
        cond.notify_one();
    }};

    std::thread consumer{[&]()
    {
        do{
            std::unique_lock<std::mutex> L{mtx};
            cond.wait(L,[&]()
            {
                // Acquire the lock only if
                // we've stopped or the queue
                // isn't empty
                return stopped || ! intq.empty();
            });

            // We own the mutex here; pop the queue
            // until it empties out.

            while( ! intq.empty())
            {
                const auto val = intq.front();
                intq.pop();

                std::cout << "Consumer popped: " << val << std::endl;
            }

            if(stopped){
                // producer has signaled a stop
                std::cout << "Consumer is done!" << std::endl;
                break;
            }

        }while(true);
    }};

    consumer.join();
    producer.join();
    
    std::cout << "Example Completed!" << std::endl;

    return 0;
}

```



## Thread operations


When you start a thread, it will execute until it is finished.

Often, at some point, you need to (possibly - the thread may already be done) wait for the thread to finish, because you want to use the result for example.

```cpp
int n;
std::thread thread{ calculateSomething, std::ref(n) };

//Doing some other stuff

//We need 'n' now!
//Wait for the thread to finish - if it is not already done
thread.join();

//Now 'n' has the result of the calculation done in the seperate thread
std::cout << n << '\n';

```

You can also `detach` the thread, letting it execute freely:

```cpp
std::thread thread{ doSomething };

//Detaching the thread, we don't need it anymore (for whatever reason)
thread.detach();

//The thread will terminate when it is done, or when the main thread returns

```



## Thread-local storage


Thread-local storage can be created using the `thread_local` [keyword](http://stackoverflow.com/documentation/c%2b%2b/4891/keywords). A variable declared with the `thread_local` specifier is said to have **thread storage duration.**

- Each thread in a program has its own copy of each thread-local variable.
- A thread-local variable with function (local) scope will be initialized the first time control passes through its definition. Such a variable is implicitly static, unless declared `extern`.
- A thread-local variable with namespace or class (non-local) scope will be initialized as part of thread startup.
- Thread-local variables are destroyed upon thread termination.
- A member of a class can only be thread-local if it is static. There will therefore be one copy of that variable per thread, rather than one copy per (thread, instance) pair.

Example:

```cpp
void debug_counter() {
    thread_local int count = 0;
    Logger::log("This function has been called %d times by this thread", ++count);
}

```



## Reassigning thread objects


We can create empty thread objects and assign work to them later.

If we assign a thread object to another active, `joinable` thread,  `std::terminate` will automatically be called before the thread is replaced.

```cpp
#include <thread>

void foo()
{
    std::this_thread::sleep_for(std::chrono::seconds(3));
}
//create 100 thread objects that do nothing
std::thread executors[100];

// Some code

// I want to create some threads now

for (int i = 0;i < 100;i++)
{
    // If this object doesn't have a thread assigned
    if (!executors[i].joinable())
         executors[i] = std::thread(foo);
}

```



#### Syntax


- thread()
- thread(thread&& other)
- explicit thread(Function&& func, Args&&... args)



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|`other`|Takes ownership of `other`, `other` doesn't own the thread anymore
|`func`|Function to call in a seperate thread
|`args`|Arguments for `func`



#### Remarks


Some notes:

- Two `std::thread` objects can **never** represent the same thread.
- A `std::thread` object can be in a state where it doesn't represent **any** thread (i.e. after a move, after calling `join`, etc.).

