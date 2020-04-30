---
metaTitle: "Const Correctness"
description: "The Basics, Const Correct Class Design, Const Correct Function Parameters, Const Correctness as Documentation"
---

# Const Correctness



## The Basics


**`const` correctness** is the practice of designing code so that only code that **needs** to modify an instance is **able** to modify an instance (i.e. has write access), and conversely, that any code that doesn't need to modify an instance is unable to do so (i.e. only has read access).  This prevents the instance from being modified unintentionally, making code less errorprone, and documents whether the code is intended to change the instance's state or not.  It also allows instances to be treated as `const` whenever they don't need to be modified, or defined as `const` if they don't need to be changed after initialisation, without losing any functionality.

This is done by giving member functions [`const` CV-qualifiers](http://stackoverflow.com/documentation/c%2B%2B/7146/the-this-pointer/24492/this-pointer-cv-qualifiers#t=201610191445574282562), and by making pointer/reference parameters `const`, except in the case that they need write access.

```cpp
class ConstCorrectClass {
    int x;

  public:
    int getX() const { return x; } // Function is const: Doesn't modify instance.
    void setX(int i) { x = i; }    // Not const: Modifies instance.
};

// Parameter is const: Doesn't modify parameter.
int const_correct_reader(const ConstCorrectClass& c) {
    return c.getX();
}

// Parameter isn't const: Modifies parameter.
void const_correct_writer(ConstCorrectClass& c) {
    c.setX(42);
}

const ConstCorrectClass invariant; // Instance is const: Can't be modified.
ConstCorrectClass         variant; // Instance isn't const: Can be modified.

// ...

const_correct_reader(invariant); // Good.   Calling non-modifying function on const instance.
const_correct_reader(variant);   // Good.   Calling non-modifying function on modifiable instance.

const_correct_writer(variant);   // Good.   Calling modifying function on modifiable instance.
const_correct_writer(invariant); // Error.  Calling modifying function on const instance.

```

Due to the nature of const correctness, this starts with the class' member functions, and works its way outwards; if you try to call a non-`const` member function from a `const` instance, or from a non-`const` instance being treated as `const`, the compiler will give you an error about it losing cv-qualifiers.



## Const Correct Class Design


In a `const`-correct class, all member functions which don't change logical state have `this` cv-qualified as `const`, indicating that they don't modify the object (apart from any [`mutable`](http://stackoverflow.com/documentation/c%2B%2B/2705/mutable-keyword/9058/non-static-class-member-modifier#t=201610041643529340028) fields, which can freely be modified even in `const` instances); if a `const` cv-qualified function returns a reference, that reference should also be `const`.  This allows them to be called on both constant and non-cv-qualified instances, as a `const T*` is capable of binding to either a `T*` or a `const T*`.  This, in turn, allows functions to declare their passed-by-reference parameters as `const` when they don't need to be modified, without losing any functionality.

Furthermore, in a `const` correct class, all passed-by-reference function parameters will be `const` correct, as discussed in `Const Correct Function Parameters`, so that they can only be modified when the function explicitly **needs** to modify them.

First, let's look at `this` cv-qualifiers:

```cpp
// Assume class Field, with member function "void insert_value(int);".

class ConstIncorrect {
    Field fld;

  public:
    ConstIncorrect(Field& f); // Modifies.

    Field& getField();        // Might modify.  Also exposes member as non-const reference,
                              //  allowing indirect modification.
    void setField(Field& f);  // Modifies.

    void doSomething(int i);  // Might modify.
    void doNothing();         // Might modify.
};

ConstIncorrect::ConstIncorrect(Field& f) : fld(f) {} // Modifies.
Field& ConstIncorrect::getField() { return fld; }    // Doesn't modify.
void ConstIncorrect::setField(Field& f) { fld = f; } // Modifies.
void ConstIncorrect::doSomething(int i) {            // Modifies.
    fld.insert_value(i);
}
void ConstIncorrect::doNothing() {}                  // Doesn't modify.


class ConstCorrectCVQ {
    Field fld;

  public:
    ConstCorrectCVQ(Field& f);     // Modifies.

    const Field& getField() const; // Doesn't modify.  Exposes member as const reference,
                                   //  preventing indirect modification.
    void setField(Field& f);       // Modifies.

    void doSomething(int i);       // Modifies.
    void doNothing() const;        // Doesn't modify.
};

ConstCorrectCVQ::ConstCorrectCVQ(Field& f) : fld(f) {}
Field& ConstCorrectCVQ::getField() const { return fld; }
void ConstCorrectCVQ::setField(Field& f) { fld = f; }
void ConstCorrectCVQ::doSomething(int i) {
    fld.insert_value(i);
}
void ConstCorrectCVQ::doNothing() const  {}

// This won't work.
// No member functions can be called on const ConstIncorrect instances.
void const_correct_func(const ConstIncorrect& c) {
    Field f = c.getField();
    c.do_nothing();
}

// But this will.
// getField() and doNothing() can be called on const ConstCorrectCVQ instances.
void const_correct_func(const ConstCorrectCVQ& c) {
    Field f = c.getField();
    c.do_nothing();
}

```

We can then combine this with `Const Correct Function Parameters`, causing the class to be fully `const`-correct.

```cpp
class ConstCorrect {
    Field fld;

  public:
    ConstCorrect(const Field& f);  // Modifies instance.  Doesn't modify parameter.

    const Field& getField() const; // Doesn't modify.  Exposes member as const reference,
                                   //  preventing indirect modification.
    void setField(const Field& f); // Modifies instance.  Doesn't modify parameter.

    void doSomething(int i);       // Modifies.  Doesn't modify parameter (passed by value).
    void doNothing() const;        // Doesn't modify.
};

ConstCorrect::ConstCorrect(const Field& f) : fld(f) {}
Field& ConstCorrect::getField() const { return fld; }
void ConstCorrect::setField(const Field& f) { fld = f; }
void ConstCorrect::doSomething(int i) {
    fld.insert_value(i);
}
void ConstCorrect::doNothing() const {}

```

This can also be combined with overloading based on `const`ness, in the case that we want one behaviour if the instance is `const`, and a different behaviour if it isn't; a common use for this is constainers providing accessors that only allow modification if the container itself is non-`const`.

```cpp
class ConstCorrectContainer {
    int arr[5];

  public:
    // Subscript operator provides read access if instance is const, or read/write access
    // otherwise.    
          int& operator[](size_t index)       { return arr[index]; }
    const int& operator[](size_t index) const { return arr[index]; }

    // ...
};

```

This is commonly used in the standard library, with most containers providing overloads to take `const`ness into account.



## Const Correct Function Parameters


In a `const`-correct function, all passed-by-reference parameters are marked as `const` unless the function directly or indirectly modifies them, preventing the programmer from inadvertently changing something they didn't mean to change.  This allows the function to take both `const` and non-cv-qualified instances, and in turn, causes the instance's `this` to be of type `const T*` when a member function is called, where `T` is the class' type.

```cpp
struct Example {
    void func()       { std::cout << 3 << std::endl; }
    void func() const { std::cout << 5 << std::endl; }
};

void const_incorrect_function(Example& one, Example* two) {
    one.func();
    two->func();
}

void const_correct_function(const Example& one, const Example* two) {
    one.func();
    two->func();
}

int main() {
    Example a, b;
    const_incorrect_function(a, &b);
    const_correct_function(a, &b);
}

// Output:
3
3
5
5

```

While the effects of this are less immediately visible than those of `const` correct class design (in that `const`-correct functions and `const`-incorrect classes will cause compilation errors, while `const`-correct classes and `const`-incorrect functions will compile properly), `const` correct functions will catch a lot of errors that `const` incorrect functions would let slip through, such as the one below.  [Note, however, that a `const`-incorrect function **will** cause compilation errors if passed a `const` instance when it expected a non-`const` one.]

```cpp
// Read value from vector, then compute & return a value.
// Caches return values for speed.
template<typename T>
const T& bad_func(std::vector<T>& v, Helper<T>& h) {
    // Cache values, for future use.
    // Once a return value has been calculated, it's cached & its index is registered.
    static std::vector<T> vals = {};

    int v_ind = h.get_index();               // Current working index for v.
    int vals_ind = h.get_cache_index(v_ind); // Will be -1 if cache index isn't registered.

    if (vals.size() && (vals_ind != -1) && (vals_ind < vals.size()) && !(h.needs_recalc())) {
        return vals[h.get_cache_index(v_ind)];
    }

    T temp = v[v_ind];

    temp -= h.poll_device();
    temp *= h.obtain_random();
    temp += h.do_tedious_calculation(temp, v[h.get_last_handled_index()]);

    // We're feeling tired all of a sudden, and this happens.
    if (vals_ind != -1) {
        vals[vals_ind] = temp;
    } else {
        v.push_back(temp);  // Oops.  Should've been accessing vals.
        vals_ind = vals.size() - 1;
        h.register_index(v_ind, vals_ind);
    }

    return vals[vals_ind];
}

// Const correct version.  Is identical to above version, so most of it shall be skipped.
template<typename T>
const T& good_func(const std::vector<T>& v, Helper<T>& h) {
    // ...

    // We're feeling tired all of a sudden, and this happens.
    if (vals_ind != -1) {
        vals[vals_ind] = temp;
    } else {
        v.push_back(temp);  // Error: discards qualifiers.
        vals_ind = vals.size() - 1;
        h.register_index(v_ind, vals_ind);
    }

    return vals[vals_ind];
}

```



## Const Correctness as Documentation


One of the more useful things about `const` correctness is that it serves as a way of documenting code, providing certain guarantees to the programmer and other users.  These guarantees are enforced by the compiler due to `const`ness, with a lack of `const`ness in turn indicating that code doesn't provide them.

### `const` CV-Qualified Member Functions:

<li>Any member function which is `const` can be assumed to have intent to read the instance, and:
<ul>
- Shall not modify the logical state of the instance they are called on.  Therefore, they shall not modify any member variables of the instance they are called on, except `mutable` variables.
- Shall not call any **other** functions that would modify any member variables of the instance, except `mutable` variables.

- May or may not modify logical state.
- May or may not call other functions which modify logical state.

This can be used to make assumptions about the state of the object after any given member function is called, even without seeing the definition of that function:

```cpp
// ConstMemberFunctions.h

class ConstMemberFunctions {
    int val;
    mutable int cache;
    mutable bool state_changed;

  public:
    // Constructor clearly changes logical state.  No assumptions necessary.
    ConstMemberFunctions(int v = 0);

    // We can assume this function doesn't change logical state, and doesn't call
    //  set_val().  It may or may not call squared_calc() or bad_func().
    int calc() const;

    // We can assume this function doesn't change logical state, and doesn't call
    //  set_val().  It may or may not call calc() or bad_func().
    int squared_calc() const;

    // We can assume this function doesn't change logical state, and doesn't call
    //  set_val().  It may or may not call calc() or squared_calc().
    void bad_func() const;

    // We can assume this function changes logical state, and may or may not call
    //  calc(), squared_calc(), or bad_func().
    void set_val(int v);
};

```

Due to `const` rules, these assumptions will in fact be enforced by the compiler.

```cpp
// ConstMemberFunctions.cpp

ConstMemberFunctions::ConstMemberFunctions(int v /* = 0*/)
  : cache(0), val(v), state_changed(true) {}

// Our assumption was correct.
int ConstMemberFunctions::calc() const {
    if (state_changed) {
        cache = 3 * val;
        state_changed = false;
    }

    return cache;
}

// Our assumption was correct.
int ConstMemberFunctions::squared_calc() const {
    return calc() * calc();
}

// Our assumption was incorrect.
// Function fails to compile, due to `this` losing qualifiers.
void ConstMemberFunctions::bad_func() const {
    set_val(863);
}

// Our assumption was correct.
void ConstMemberFunctions::set_val(int v) {
    if (v != val) {
        val = v;
        state_changed = true;
    }
}

```

### `const` Function Parameters:

<li>Any function with one or more parameters which are `const` can be assumed to have intent to read those parameters, and:
<ul>
- Shall not modify those parameters, or call any member functions that would modify them.
- Shall not pass those parameters to any **other** function which would modify them and/or call any member functions that would modify them.

- May or may not modify those parameters, or call any member functions which whould modify them.
- May or may not pass those parameters to other functions which would modify them and/or call any member functions that would modify them.

This can be used to make assumptions about the state of the parameters after being passed to any given function, even without seeing the definition of that function.

```cpp
// function_parameter.h

// We can assume that c isn't modified (and c.set_val() isn't called), and isn't passed
//  to non_qualified_function_parameter().  If passed to one_const_one_not(), it is the first
//  parameter.
void const_function_parameter(const ConstMemberFunctions& c);

// We can assume that c is modified and/or c.set_val() is called, and may or may not be passed
//  to any of these functions.  If passed to one_const_one_not, it may be either parameter.
void non_qualified_function_parameter(ConstMemberFunctions& c);

// We can assume that:
  // l is not modified, and l.set_val() won't be called.
  // l may or may not be passed to const_function_parameter().
  // r is modified, and/or r.set_val() may be called.
  // r may or may not be passed to either of the preceding functions.
void one_const_one_not(const ConstMemberFunctions& l, ConstMemberFunctions& r);

// We can assume that c isn't modified (and c.set_val() isn't called), and isn't passed
//  to non_qualified_function_parameter().  If passed to one_const_one_not(), it is the first
//  parameter.
void bad_parameter(const ConstMemberFunctions& c);

```

Due to `const` rules, these assumptions will in fact be enforced by the compiler.

```cpp
// function_parameter.cpp

// Our assumption was correct.
void const_function_parameter(const ConstMemberFunctions& c) {
    std::cout << "With the current value, the output is: " << c.calc() << '\n'
              << "If squared, it's: " << c.squared_calc()
              << std::endl;
}

// Our assumption was correct.
void non_qualified_function_parameter(ConstMemberFunctions& c) {
    c.set_val(42);
    std::cout << "For the value 42, the output is: " << c.calc() << '\n'
              << "If squared, it's: " << c.squared_calc()
              << std::endl;
}

// Our assumption was correct, in the ugliest possible way.
// Note that const correctness doesn't prevent encapsulation from intentionally being broken,
//  it merely prevents code from having write access when it doesn't need it.
void one_const_one_not(const ConstMemberFunctions& l, ConstMemberFunctions& r) {
    // Let's just punch access modifiers and common sense in the face here.
    struct Machiavelli {
        int val;
        int unimportant;
        bool state_changed;
    };
    reinterpret_cast<Machiavelli&>(r).val = l.calc();
    reinterpret_cast<Machiavelli&>(r).state_changed = true;

    const_function_parameter(l);
    const_function_parameter(r);
}

// Our assumption was incorrect.
// Function fails to compile, due to `this` losing qualifiers in c.set_val().
void bad_parameter(const ConstMemberFunctions& c) {
    c.set_val(18);
}

```

While it **is** possible to [circumvent `const` correctness](http://stackoverflow.com/documentation/c%2B%2B/4891/keywords/18511/const-cast#t=201610041626457103824), and by extension break these guarantees, this must be done intentionally by the programmer (just like breaking encapsulation with `Machiavelli`, above), and is likely to cause undefined behaviour.

```cpp
class DealBreaker : public ConstMemberFunctions {
  public:
    DealBreaker(int v = 0);

    // A foreboding name, but it's const...
    void no_guarantees() const;
}

DealBreaker::DealBreaker(int v /* = 0 */) : ConstMemberFunctions(v) {}

// Our assumption was incorrect.
// const_cast removes const-ness, making the compiler think we know what we're doing.
void DealBreaker::no_guarantees() const {
    const_cast<DealBreaker*>(this)->set_val(823);
}

// ...

const DealBreaker d(50);
d.no_guarantees(); // Undefined behaviour: d really IS const, it may or may not be modified.

```

However, due to this requiring the programmer to very specifically **tell** the compiler that they intend to ignore `const`ness, and being inconsistent across compilers, it is generally safe to assume that `const` correct code will refrain from doing so unless otherwise specified.



#### Syntax


- class ClassOne { public: bool non_modifying_member_function() const { /* ... */ } };
- int ClassTwo::non_modifying_member_function() const { /* ... */ }
- void ClassTwo::modifying_member_function() { /* ... */ }
- char non_param_modding_func(const ClassOne& one, const ClassTwo* two) { /* ... */ }
- float parameter_modifying_function(ClassTwo& one, ClassOne* two) { /* ... */ }
- short ClassThree::non_modding_non_param_modding_f(const ClassOne&) const { /* ... */ }



#### Remarks


`const` correctness is a very useful troubleshooting tool, as it allows the programmer to quickly determine which functions might be inadvertently modifying code.  It also prevents unintentional errors, such as the one shown in `Const Correct Function Parameters`, from compiling properly and going unnoticed.

It is much easier to design a class for `const` correctness, than it is to later add `const` correctness to a pre-existing class.  If possible, design any class that **can** be `const` correct so that it **is** `const` correct, to save yourself and others the hassle of later modifying it.

Note that this can also be applied to `volatile` correctness if necessary, with the same rules as for `const` correctness, but this is used much less often.

Refrences :

[ISO_CPP](https://isocpp.org/wiki/faq/const-correctness)

[Sell me on const correctness](http://stackoverflow.com/questions/136880/sell-me-on-const-correctness)

[C++ Tutorial](http://www.cprogramming.com/tutorial/const_correctness.html)

