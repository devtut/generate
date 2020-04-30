---
metaTitle: "References"
description: "Defining a reference, C++ References are Alias of existing variables"
---

# References



## Defining a reference


References behaves similarly, but not entirely like const pointers. A reference is defined by suffixing an ampersand `&` to a type name.

```cpp
int i = 10;
int &refi = i;

```

Here, `refi` is a reference bound to `i`.<br />
References abstracts the semantics of pointers, acting like an alias to the underlying object:

```cpp
refi = 20; // i = 20;

```

You can also define multiple references in a single definition:

```cpp
int i = 10, j = 20;
int &refi = i, &refj = j;

// Common pitfall :
// int& refi = i, k = j;
// refi will be of type int&.
// though, k will be of type int, not int&!

```

References **must** be initialized correctly at the time of definition, and cannot be modified afterwards. The following piece of codes causes a compile error:

```cpp
int &i; // error: declaration of reference variable 'i' requires an initializer

```

You also cannot bind directly a reference to `nullptr`, unlike pointers:

```cpp
int *const ptri = nullptr;
int &refi = nullptr; // error: non-const lvalue reference to type 'int' cannot bind to a temporary of type 'nullptr_t'

```



## C++ References are Alias of existing variables


A Reference in C++ is just an `Alias` or another name of a variable. Just like most of us can be referred using our passport name and nick name.

References doesn't exist literally and they don't occupy any memory. If we print the address of reference variable it will print the same address as that of the variable its referring to.

```cpp
int main() {
    int i = 10;
    int &j = i;
    
    cout<<&i<<endl;
    cout<<&b<<endl;
    return 0;
}

```

In the above example, both `cout` will print the same address. The situation will be same if we take a variable as a reference in a function

```cpp
void func (int &fParam ) {
   cout<<"Address inside function => "<<fParam<<endl;
}

int main() {
    int i = 10;
    cout<<"Address inside Main => "<<&i<<endl;    

    func(i);

    return 0;
}


```

In this example also, both `cout` will print the same address.

As we know by now that `C++ References` are just alias, and for an alias to be created, we need to have something which the Alias can refer to.

That's the precise reason why the statement like this will throw a compiler error

```cpp
int &i;


```

Because, the alias is not referring to anything.

