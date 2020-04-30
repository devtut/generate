---
metaTitle: "Futures and Promises"
description: "std::future and std::promise, Deferred async example, std::packaged_task and std::future, std::future_error and std::future_errc, std::future and std::async, Async operation classes"
---

# Futures and Promises


Promises and Futures are used to ferry a single object from one thread to another.

A `std::promise` object is set by the thread which generates the result.

A `std::future` object can be used to retrieve a value, to test to see if a value is available, or to halt execution until the value is available.



## std::future and std::promise


The following example sets a promise to be consumed by another thread:

```

   {
        auto promise = std::promise<std::string>();
        
        auto producer = std::thread([&]
        {
            promise.set_value("Hello World");
        });
        
        auto future = promise.get_future();
        
        auto consumer = std::thread([&]
        {
            std::cout << future.get();
        });
        
        producer.join();
        consumer.join();
}

```



## Deferred async example


This code implements a version of `std::async`, but it behaves as if `async` were always called with the `deferred` launch policy. This function also does not have `async`'s special `future` behavior; the returned `future` can be destroyed without ever acquiring its value.

```cpp
template<typename F>
auto async_deferred(F&& func) -> std::future<decltype(func())>
{
    using result_type = decltype(func());

    auto promise = std::promise<result_type>();
    auto future  = promise.get_future();

    std::thread(std::bind([=](std::promise<result_type>& promise)
    {
        try
        {
            promise.set_value(func()); 
            // Note: Will not work with std::promise<void>. Needs some meta-template programming which is out of scope for this example.
        }
        catch(...)
        {
            promise.set_exception(std::current_exception());
        }
    }, std::move(promise))).detach();

    return future;
}

```



## std::packaged_task and std::future


`std::packaged_task` bundles a function and the associated promise for its return type:

```cpp
template<typename F>
auto async_deferred(F&& func) -> std::future<decltype(func())>
{
    auto task   = std::packaged_task<decltype(func())()>(std::forward<F>(func));
    auto future = task.get_future();

    std::thread(std::move(task)).detach();

    return std::move(future);
}

```

The thread starts running immediately. We can either detach it, or have join it at the end of the scope. When the function call to std::thread finishes, the result is ready.

Note that this is slightly different from `std::async` where the returned `std::future` when destructed will actually **block** until the thread is finished.



## std::future_error and std::future_errc


If constraints for std::promise and std::future are not met an exception of type
std::future_error is thrown.

The error code member in the exception is of type std::future_errc and values are as below, along with some test cases:

```cpp
enum class future_errc {
    broken_promise             = /* the task is no longer shared */,
    future_already_retrieved   = /* the answer was already retrieved */,
    promise_already_satisfied  = /* the answer was stored already */,
    no_state                   = /* access to a promise in non-shared state */
};

```

**Inactive promise:**

```cpp
int test()
{
    std::promise<int> pr;
    return 0; // returns ok
}

```

**Active promise, unused:**

```

 int test()
    {
        std::promise<int> pr;
        auto fut = pr.get_future(); //blocks indefinitely!
        return 0; 
    }

```

**Double retrieval:**

```cpp
int test()
{
    std::promise<int> pr;
    auto fut1 = pr.get_future();

    try{
        auto fut2 = pr.get_future();    //   second attempt to get future
        return 0;
    }
    catch(const std::future_error& e)
    {
        cout << e.what() << endl;       //   Error: "The future has already been retrieved from the promise or packaged_task."
        return -1;
    }
    return fut2.get();
}

```

**Setting std::promise value twice:**

```cpp
int test()
{
    std::promise<int> pr;
    auto fut = pr.get_future();
    try{
        std::promise<int> pr2(std::move(pr));
        pr2.set_value(10);
        pr2.set_value(10);  // second attempt to set promise throws exception
    }
    catch(const std::future_error& e)
    {
        cout << e.what() << endl;       //   Error: "The state of the promise has already been set."
        return -1;
    }
    return fut.get();
}

```



## std::future and std::async


In the following naive parallel merge sort example, `std::async` is used to launch multiple parallel merge_sort tasks. `std::future` is used to wait for the results and synchronize them:

```cpp
#include <iostream>
using namespace std;


void merge(int low,int mid,int high, vector<int>&num)
{
    vector<int> copy(num.size());
    int h,i,j,k;
    h=low;
    i=low;
    j=mid+1;
    
    while((h<=mid)&&(j<=high))
    {
        if(num[h]<=num[j])
        {
            copy[i]=num[h];
            h++;
        }
        else
        {
            copy[i]=num[j];
            j++;
        }
        i++;
    }
    if(h>mid)
    {
        for(k=j;k<=high;k++)
        {
            copy[i]=num[k];
            i++;
        }
    }
    else
    {
        for(k=h;k<=mid;k++)
        {
            copy[i]=num[k];
            i++;
        }
    }
    for(k=low;k<=high;k++)
        swap(num[k],copy[k]);
}


void merge_sort(int low,int high,vector<int>& num)
{
    int mid;
    if(low<high)
    {
        mid = low + (high-low)/2;
        auto future1    =  std::async(std::launch::deferred,[&]()
                                      {
                                        merge_sort(low,mid,num);
                                      });
        auto future2    =  std::async(std::launch::deferred, [&]()
                                       {
                                          merge_sort(mid+1,high,num) ;
                                       });
        
        future1.get();
        future2.get();
        merge(low,mid,high,num);
    }
}

```

Note: In the example `std::async` is launched with policy `std::launch_deferred`. This is to avoid a new thread being created in every call. In the case of our example, the calls to `std::async` are made out of order, the they synchronize at the calls for `std::future::get()`.

`std::launch_async` forces a new thread to be created in every call.

The default policy is `std::launch::deferred| std::launch::async`, meaning the implementation determines  the policy for creating new threads.



## Async operation classes


- std::async: performs an asynchronous operation.
- std::future: provides access to the result of an asynchronous operation.
- std::promise: packages the result of an asynchronous operation.
- std::packaged_task: bundles a function and the associated promise for its return type.

