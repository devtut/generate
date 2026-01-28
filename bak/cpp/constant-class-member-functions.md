---
metaTitle: "C++ | Constant class member functions"
description: "constant member function"
---

# Constant class member functions



## constant member function


```cpp
#include <iostream>
#include <map>
#include <string>

using namespace std;

class A {
public:
    map<string, string> * mapOfStrings;
public:
    A() {
        mapOfStrings = new map<string, string>();
    }

    void insertEntry(string const & key, string const & value) const {
        (*mapOfStrings)[key] = value;             // This works? Yes it does. 
        delete mapOfStrings;                      // This also works
        mapOfStrings = new map<string, string>(); // This * does * not work
    }

    void refresh() {
        delete mapOfStrings;
        mapOfStrings = new map<string, string>(); // Works as refresh is non const function
    }

    void getEntry(string const & key) const {
        cout << mapOfStrings->at(key);
    }
};

int main(int argc, char* argv[]) {

    A var;
    var.insertEntry("abc", "abcValue");
    var.getEntry("abc");
    getchar();
    return 0;
}

```



#### Remarks


What does 'const member functions' of a class really means. The simple definition seems to be that, a const member function cannot change the object. But what does 'can not change' really means here. It simply means that you cannot do an assignment for class data members.

However, you can do other indirect operations like inserting an entry into a map as shown in the example. Allowing this might look like this const function is modifying the object (yes, it does in one sense), but it is allowed.

So, the real meaning is that a const member function cannot do an assignment for the class data variables. But it can do other stuff like explained in the example.

