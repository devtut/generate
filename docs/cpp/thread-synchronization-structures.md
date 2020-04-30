---
metaTitle: "Thread synchronization structures"
description: "std::shared_lock, std::call_once, std::once_flag, Object locking for efficient access., std::condition_variable_any, std::cv_status"
---

# Thread synchronization structures


Working with [threads](http://stackoverflow.com/documentation/c%2b%2b/699/threading) might need some synchronization techniques if the threads interact. In this topic, you can find the different structures which are provided by the standard library to solve these issues.



## std::shared_lock


A shared_lock can be used in conjunction with a unique lock to allow multiple readers and exclusive writers.

```cpp
#include <unordered_map>
#include <mutex>
#include <shared_mutex>
#include <thread>
#include <string>
#include <iostream>
    
class PhoneBook {
    public:
        string getPhoneNo( const std::string & name )
        {
            shared_lock<shared_timed_mutex> r(_protect);
            auto it =  _phonebook.find( name );
            if ( it == _phonebook.end() )
                return (*it).second;
            return "";
        }
        void addPhoneNo ( const std::string & name, const std::string & phone )
        {
            unique_lock<shared_timed_mutex> w(_protect);
            _phonebook[name] = phone;
        }
        
        shared_timed_mutex _protect;
        unordered_map<string,string>  _phonebook;
    };

```



## std::call_once, std::once_flag


`std::call_once` ensures execution of a function exactly once by competing threads. It throws `std::system_error` in case it cannot complete its task.

Used in conjunction with s`td::once_flag`.

```cpp
#include <mutex>
#include <iostream>

std::once_flag flag;
void do_something(){
      std::call_once(flag, [](){std::cout << "Happens once" << std::endl;});
    
      std::cout << "Happens every time" << std::endl;
}

```



## Object locking for efficient access.


Often you want to lock the entire object while you perform multiple operations on it. For example, if you need to examine or modify the object using **iterators**. Whenever you need to call multiple member functions it is generally more efficient to lock the whole object rather than individual member functions.

For example:

```cpp
class text_buffer
{
    // for readability/maintainability
    using mutex_type = std::shared_timed_mutex;
    using reading_lock = std::shared_lock<mutex_type>;
    using updates_lock = std::unique_lock<mutex_type>;

public:
    // This returns a scoped lock that can be shared by multiple
    // readers at the same time while excluding any writers
    [[nodiscard]]
    reading_lock lock_for_reading() const { return reading_lock(mtx); }

    // This returns a scoped lock that is exclusing to one
    // writer preventing any readers
    [[nodiscard]]
    updates_lock lock_for_updates() { return updates_lock(mtx); }

    char* data() { return buf; }
    char const* data() const { return buf; }

    char* begin() { return buf; }
    char const* begin() const { return buf; }

    char* end() { return buf + sizeof(buf); }
    char const* end() const { return buf + sizeof(buf); }

    std::size_t size() const { return sizeof(buf); }

private:
    char buf[1024];
    mutable mutex_type mtx; // mutable allows const objects to be locked
};

```

When calculating a checksum the object is locked for reading, allowing other threads that want to read from the object at the same time to do so.

```cpp
std::size_t checksum(text_buffer const& buf)
{
    std::size_t sum = 0xA44944A4;

    // lock the object for reading
    auto lock = buf.lock_for_reading();

    for(auto c: buf)
        sum = (sum << 8) | (((unsigned char) ((sum & 0xFF000000) >> 24)) ^ c);

    return sum;
}

```

Clearing the object updates its internal data so it must be done using an exclusing lock.

```cpp
void clear(text_buffer& buf)
{
    auto lock = buf.lock_for_updates(); // exclusive lock
    std::fill(std::begin(buf), std::end(buf), '\0');
}

```

When obtaining more than one lock care should be taken to always acquire the locks in the same order for all threads.

```cpp
void transfer(text_buffer const& input, text_buffer& output)
{
    auto lock1 = input.lock_for_reading();
    auto lock2 = output.lock_for_updates();

    std::copy(std::begin(input), std::end(input), std::begin(output));
}

```

**note:** This is best done using [std::deferred::lock](http://en.cppreference.com/w/cpp/thread/lock_tag) and calling [std::lock](http://en.cppreference.com/w/cpp/thread/lock)



## std::condition_variable_any, std::cv_status


A generalization of `std::condition_variable`, `std::condition_variable_any` works with any type of BasicLockable structure.

`std::cv_status` as a return status for a condition variable has two possible return codes:

- std::cv_status::no_timeout: There was no timeout, condition variable was notified
- std::cv_status::no_timeout: Condition variable timed out

