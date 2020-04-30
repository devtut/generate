---
metaTitle: "Expression templates"
description: "A basic example illustrating expression templates, Basic expression templates on element-wise algebraic expressions"
---

# Expression templates



## A basic example illustrating expression templates


An expression template is a compile-time optimization technique used mostly in scientific computing. It's main purpose is to avoid unnecessary temporaries and optimize loop calculations using a single pass (typically when performing operations on numerical aggregates). Expression templates were initially devised in order to circumvent the inefficiencies of naïve operator overloading when implementing numerical `Array` or `Matrix` types. An equivalent terminology for expression templates has been introduced by Bjarne Stroustrup, who calls them "fused operations" in the latest version of his book,  "The C++ Programming Language".

Before actually diving into expression templates, you should understand why you need them in the first place. To illustrate this, consider the very simple Matrix class given below:

```cpp
template <typename T, std::size_t COL, std::size_t ROW>
class Matrix {
public:
    using value_type = T;

    Matrix() : values(COL * ROW) {}

    static size_t cols() { return COL; }
    static size_t rows() { return ROW; }

    const T& operator()(size_t x, size_t y) const { return values[y * COL + x]; }
    T& operator()(size_t x, size_t y) { return values[y * COL + x]; }

private:
    std::vector<T> values;
};

template <typename T, std::size_t COL, std::size_t ROW>
Matrix<T, COL, ROW>
operator+(const Matrix<T, COL, ROW>& lhs, const Matrix<T, COL, ROW>& rhs)
{
    Matrix<T, COL, ROW> result;

    for (size_t y = 0; y != lhs.rows(); ++y) {
        for (size_t x = 0; x != lhs.cols(); ++x) {
            result(x, y) = lhs(x, y) + rhs(x, y);
        }
    }
    return result;
}

```

Given the previous class definition, you can now write Matrix expressions such as:

```cpp
const std::size_t cols = 2000;
const std::size_t rows = 1000;

Matrix<double, cols, rows> a, b, c;

// initialize a, b & c
for (std::size_t y = 0; y != rows; ++y) {
    for (std::size_t x = 0; x != cols; ++x) {
        a(x, y) = 1.0;
        b(x, y) = 2.0;
        c(x, y) = 3.0;
    }
}  

Matrix<double, cols, rows> d = a + b + c;  // d(x, y) = 6 

```

As illustrated above, being able to overload `operator+()` provides you with a notation which mimics the natural mathematical notation for matrices.

Unfortunately, the previous implementation is also highly inefficient compared to an equivalent "hand-crafted" version.

To understand why, you have to consider what happens when you write an expression such as `Matrix d = a + b + c`. This in fact expands to `((a + b) + c)` or `operator+(operator+(a, b), c)`. In other words, the loop inside `operator+()` is executed twice, whereas it could have been easily performed in a single pass. This also results in 2 temporaries being created, which further degrades performance. In essence, by adding the flexibility to use a notation close to its mathematical counterpart, you have also made the `Matrix` class highly inefficient.

For example, without operator overloading, you could implement a far more efficient Matrix summation using a single pass:

```cpp
template<typename T, std::size_t COL, std::size_t ROW>
Matrix<T, COL, ROW> add3(const Matrix<T, COL, ROW>& a,
                         const Matrix<T, COL, ROW>& b,
                         const Matrix<T, COL, ROW>& c)
{
    Matrix<T, COL, ROW> result;
    for (size_t y = 0; y != ROW; ++y) {
        for (size_t x = 0; x != COL; ++x) {
            result(x, y) = a(x, y) + b(x, y) + c(x, y);
        }
    }
    return result;
}

```

The previous example however has its own disadvantages because it creates a far more convoluted interface for the Matrix class (you would have to consider methods such as `Matrix::add2()`, `Matrix::AddMultiply()` and so on).

Instead let us take a step back and see how we can adapt operator overloading to perform in a more efficient way

The problem stems from the fact that the expression `Matrix d = a + b + c` is evaluated too "eagerly" before you have had an opportunity to build the entire expression tree. In other words, what you really want to achieve is to evaluate `a + b + c` in one pass and only once you actually need to assign the resulting expressing to `d`.

This is the core idea behind expression templates: instead of having `operator+()` evaluate immediately the result of adding two Matrix instances, it will return an **"expression template"** for future evaluation once the entire expression tree has been built.

For example, here is a possible implementation for an expression template corresponding to the summation of 2 types:

```cpp
template <typename LHS, typename RHS>
class MatrixSum
{
public:
    using value_type = typename LHS::value_type;

    MatrixSum(const LHS& lhs, const RHS& rhs) : rhs(rhs), lhs(lhs) {}
    
    value_type operator() (int x, int y) const  {
        return lhs(x, y) + rhs(x, y);
    }
private:
    const LHS& lhs;
    const RHS& rhs;
};

```

And here is the updated version of `operator+()`

```cpp
template <typename LHS, typename RHS>
MatrixSum<LHS, RHS> operator+(const LHS& lhs, const LHS& rhs) {
    return MatrixSum<LHS, RHS>(lhs, rhs);
}

```

As you can see, `operator+()` no longer returns an "eager evaluation" of the result of adding 2 Matrix instances (which would be another Matrix instance), but instead an expression template representing the addition operation. The most important point to keep in mind is that the expression has not been evaluated yet. It merely holds references to its operands.

In fact, nothing stops you from instantiating the `MatrixSum<>` expression template as follows:

```cpp
MatrixSum<Matrix<double>, Matrix<double> > SumAB(a, b);

```

You can however at a later stage, when you actually need the result of the summation, evaluate the expression `d = a + b` as follows:

```cpp
for (std::size_t y = 0; y != a.rows(); ++y) {
    for (std::size_t x = 0; x != a.cols(); ++x) {
        d(x, y) = SumAB(x, y);
    }
}

```

As you can see, another benefit of using an expression template, is that you have basically managed to evaluate the sum of `a` and `b` and assign it to `d` in a single pass.

Also, nothing stops you from combining multiple expression templates. For example,  `a + b + c` would result in the following expression template:

```cpp
MatrixSum<MatrixSum<Matrix<double>, Matrix<double> >, Matrix<double> > SumABC(SumAB, c);

```

And here again you can evaluate the final result using a single pass:

```cpp
for (std::size_t y = 0; y != a.rows(); ++y) {
    for (std::size_t x = 0; x != a.cols(); ++x) {
        d(x, y) = SumABC(x, y);
    }
}

```

Finally, the last piece of the puzzle is to actually plug your expression template into the `Matrix` class. This is essentially achieved by providing an implementation for  `Matrix::operator=()`, which takes the expression template as an argument and evaluates it in one pass, as you did "manually" before:

```cpp
template <typename T, std::size_t COL, std::size_t ROW>
class Matrix {
public:
    using value_type = T;

    Matrix() : values(COL * ROW) {}

    static size_t cols() { return COL; }
    static size_t rows() { return ROW; }

    const T& operator()(size_t x, size_t y) const { return values[y * COL + x]; }
    T& operator()(size_t x, size_t y) { return values[y * COL + x]; }

    template <typename E>
    Matrix<T, COL, ROW>& operator=(const E& expression) {
        for (std::size_t y = 0; y != rows(); ++y) {
            for (std::size_t x = 0; x != cols(); ++x) {
                values[y * COL + x] = expression(x, y);
            }
        }
        return *this;
    }

private:
    std::vector<T> values;
};

```



## Basic expression templates on element-wise algebraic expressions


**Introduction and motivation**

**Expression templates** (denoted as **ETs** in the following) are a powerful template meta-programming technique, used to speed-up calculations of sometimes quite expensive expressions. It is widely used in different domains, for example in implementation of linear algebra libraries.

For this example, consider the context of linear algebraic computations. More specifically, computations involving only **element-wise operations**. This kind of computations are the most basic applications of **ETs**, and they serve as a good introduction to how **ETs** work internally.

Let's look at a motivating example. Consider the computation of the expression:

```cpp
Vector vec_1, vec_2, vec_3;

// Initializing vec_1, vec_2 and vec_3.

Vector result = vec_1 + vec_2*vec_3;

```

Here for the sake of simplicity, I'll assume that the class `Vector` and operation + (vector plus: element-wise plus operation) and operation * (here means vector inner product: also element-wise operation) are both correctly implemented, as how they should be, mathematically.

In a conventional implementation without using **ETs** (or other similar techniques), **at least five** constructions of `Vector` instances take place in order to obtain the final `result`:

1. Three instances corresponding to `vec_1`, `vec_2` and `vec_3`.
1. A temporary `Vector` instance `_tmp`, representing the result of `_tmp = vec_2*vec_3;`.
1. Finally with proper use of **return value optimization**, the construction of final `result` in `result = vec_1 + _tmp;`.

Implementation using **ETs** can **eliminate the creation of temporary** `Vector _tmp` in 2, thus leaving only **four** constructions of `Vector` instances. More interestingly, consider the following expression which is more complex:

```cpp
Vector result = vec_1 + (vec_2*vec3 + vec_1)*(vec_2 + vec_3*vec_1);

```

There will also be four constructions of `Vector` instances in total: `vec_1, vec_2, vec_3` and `result`. In other words, in this example, **where only element-wise operations are involved**, it is guaranteed that **no temporary objects will be created from intermediate calculations**.

**How do ETs work**

Basically speaking, **ETs for any algebraic computations** consist of two building blocks:

1. **Pure algebraic expressions** (**PAE**): they are proxies / abstractions of algebraic expressions. A pure algebraic does not do actual computations, they are merely abstractions / modeling of the computation work-flow. A PAE can be a model of **either the input or the output of any algebraic expressions**. Instances of **PAE**s are often considered cheap to copy.
1. **Lazy evaluations**: which are implementation of real computations. In the following example, we will see that for expressions involving only element-wise operations, lazy evaluations can implement actual computations inside the indexed-access operation on the final result, thus creating a scheme of evaluation on-demand: a computation is not performed only if the final result is accessed / asked for.

So, specifically how do we implement **ETs** in this example? Let's walk through it now.

Consider always the following code snippet:

```cpp
Vector vec_1, vec_2, vec_3;

// Initializing vec_1, vec_2 and vec_3.

Vector result = vec_1 + vec_2*vec_3;

```

The expression to compute result can be decomposed further into two sub-expressions:

1. A vector plus expression (denoted as **plus_expr**)
1. A vector inner product expression (denoted as **innerprod_expr**).

What **ETs** do is the following:

<li>
Instead of compute right away each sub-expression, **ETs** first model the whole expression using a graphical structure. Each node in the graph represents a **PAE**. The edge connection of the nodes represent the actual computation flow. So for the above expression, we obtain the following graph:

```cpp
       result = plus_expr( vec_1, innerprod_expr(vec_2, vec_3) )
          /   \
         /     \
        /       \
       /   innerprod_expr( vec_2, vec_3 )
      /         /  \
     /         /    \
    /         /      \
 vec_1     vec_2    vec_3

```


</li>

<li>
The final computation is implemented by **looking through the graph hierarchy**: since here we are dealing with **only element-wise** operations, the computation of each indexed value in `result` **can be done independently**: the final evaluation of `result` can be lazily postponed to a **element-wise evaluation** of each element of `result`. In other words, since the computation of an element of `result`, `elem_res`, can be expressed using corresponding elements in `vec_1` (`elem_1`), `vec_2` (`elem_2`) and `vec_3` (`elem_3`) as:

```cpp
elem_res = elem_1 + elem_2*elem_3;

```


</li>

there is therefore no need to create a temporary `Vector` to store the result of intermediate inner product: **the whole computation for one element can be done altogether, and be encoded inside the indexed-access operation**.

**Here are the example codes in action.**

### File vec.hh : wrapper for std::vector, used to show log when a construction is called.

```cpp
#ifndef EXPR_VEC
# define EXPR_VEC

# include <vector>
# include <cassert>
# include <utility>
# include <iostream>
# include <algorithm>
# include <functional>

///
/// This is a wrapper for std::vector. It's only purpose is to print out a log when a
/// vector constructions in called.
/// It wraps the indexed access operator [] and the size() method, which are 
/// important for later ETs implementation.
///

// std::vector wrapper.
template<typename ScalarType> class Vector
{
public:
  explicit Vector() { std::cout << "ctor called.\n"; };
  explicit Vector(int size): _vec(size) { std::cout << "ctor called.\n"; };
  explicit Vector(const std::vector<ScalarType> &vec): _vec(vec)
  { std::cout << "ctor called.\n"; };
  
  Vector(const Vector<ScalarType> & vec): _vec{vec()}
  { std::cout << "copy ctor called.\n"; };
  Vector(Vector<ScalarType> && vec): _vec(std::move(vec()))
  { std::cout << "move ctor called.\n"; };

  Vector<ScalarType> & operator=(const Vector<ScalarType> &) = default;
  Vector<ScalarType> & operator=(Vector<ScalarType> &&) = default;

  decltype(auto) operator[](int indx) { return _vec[indx]; }
  decltype(auto) operator[](int indx) const { return _vec[indx]; }

  decltype(auto) operator()() & { return (_vec); };        
  decltype(auto) operator()() const & { return (_vec); };  
  Vector<ScalarType> && operator()() && { return std::move(*this); }

  int size() const { return _vec.size(); }
  
private:
  std::vector<ScalarType> _vec;
};

///
/// These are conventional overloads of operator + (the vector plus operation)
/// and operator * (the vector inner product operation) without using the expression
/// templates. They are later used for bench-marking purpose.
///

// + (vector plus) operator.
template<typename ScalarType>
auto operator+(const Vector<ScalarType> &lhs, const Vector<ScalarType> &rhs)
{
  assert(lhs().size() == rhs().size() &&
         "error: ops plus -> lhs and rhs size mismatch.");
  
  std::vector<ScalarType> _vec;
  _vec.resize(lhs().size());
  std::transform(std::cbegin(lhs()), std::cend(lhs()),
                 std::cbegin(rhs()), std::begin(_vec),
                 std::plus<>());
  return Vector<ScalarType>(std::move(_vec));
}

// * (vector inner product) operator.
template<typename ScalarType>
auto operator*(const Vector<ScalarType> &lhs, const Vector<ScalarType> &rhs)
{
  assert(lhs().size() == rhs().size() &&
         "error: ops multiplies -> lhs and rhs size mismatch.");
  
  std::vector<ScalarType> _vec;
  _vec.resize(lhs().size());
  std::transform(std::cbegin(lhs()), std::cend(lhs()),
                 std::cbegin(rhs()), std::begin(_vec),
                 std::multiplies<>());
  return Vector<ScalarType>(std::move(_vec));
}

#endif //!EXPR_VEC

```

### File expr.hh : implementation of expression templates for element-wise operations (vector plus and vector inner product)

Let's break it down to sections.

1. Section 1 implements a base class for all expressions. It employs the **Curiously Recurring Template Pattern** ([CRTP](http://stackoverflow.com/documentation/c%2B%2B/709/curiously-recurring-template-pattern-crtp#t=201607241604559383674)).
1. Section 2 implements the first **PAE**: a **terminal**, which is just a wrapper (const reference) of an input data structure containing real input value for computation.
1. Section 3 implements the second **PAE**: **binary_operation**, which is a class template later used for vector_plus and vector_innerprod. It's parametrized by the **type of operation**, **the left-hand-side PAE** and **the right-hand-side PAE**. The actual computation is encoded in the indexed-access operator.
1. Section 4 defines vector_plus and vector_innerprod operations as **element-wise operation**. It also overload operator + and * for **PAE**s: such that these two operations also return **PAE**.

```cpp
#ifndef EXPR_EXPR
# define EXPR_EXPR
      

/// Fwd declaration.
template<typename> class Vector;

namespace expr
{


/// -----------------------------------------
///
/// Section 1.
///
/// The first section is a base class template for all kinds of expression. It         
/// employs the Curiously Recurring Template Pattern, which enables its instantiation 
/// to any kind of expression structure inheriting from it.
///
/// -----------------------------------------


  /// Base class for all expressions.
  template<typename Expr> class expr_base
  {
  public:
    const Expr& self() const { return static_cast<const Expr&>(*this); }
    Expr& self() { return static_cast<Expr&>(*this); }

  protected:
    explicit expr_base() {};
    int size() const { return self().size_impl(); }
    auto operator[](int indx) const { return self().at_impl(indx); }
    auto operator()() const { return self()(); };
  };
  

/// -----------------------------------------
///
/// The following section 2 & 3 are abstractions of pure algebraic expressions (PAE).
/// Any PAE can be converted to a real object instance using operator(): it is in 
/// this conversion process, where the real computations are done.

///
/// Section 2. Terminal
///
/// A terminal is an abstraction wrapping a const reference to the Vector data 
/// structure. It inherits from expr_base, therefore providing a unified interface
/// wrapping a Vector into a PAE.
///
/// It provides the size() method, indexed access through at_impl() and a conversion
/// to referenced object through () operator.
/// 
/// It might no be necessary for user defined data structures to have a terminal 
/// wrapper, since user defined structure can inherit expr_base, therefore eliminates
/// the need to provide such terminal wrapper. 
///
/// -----------------------------------------


  /// Generic wrapper for underlying data structure.
  template<typename DataType> class terminal: expr_base<terminal<DataType>>
  {
  public:
    using base_type = expr_base<terminal<DataType>>;
    using base_type::size;
    using base_type::operator[];
    friend base_type;
    
    explicit terminal(const DataType &val): _val(val) {}
    int size_impl() const { return _val.size(); };
    auto at_impl(int indx) const { return _val[indx]; };
    decltype(auto) operator()() const { return (_val); }
    
  private:
    const DataType &_val;
  };


/// -----------------------------------------
///
/// Section 3. Binary operation expression.
///
/// This is a PAE abstraction of any binary expression. Similarly it inherits from 
/// expr_base.
///
/// It provides the size() method, indexed access through at_impl() and a conversion
/// to referenced object through () operator. Each call to the at_impl() method is
/// a element wise computation.
/// 
/// -----------------------------------------


  /// Generic wrapper for binary operations (that are element-wise).
  template<typename Ops, typename lExpr, typename rExpr>
  class binary_ops: public expr_base<binary_ops<Ops,lExpr,rExpr>>
  {
  public:
    using base_type = expr_base<binary_ops<Ops,lExpr,rExpr>>;
    using base_type::size;
    using base_type::operator[];
    friend base_type;
    
    explicit binary_ops(const Ops &ops, const lExpr &lxpr, const rExpr &rxpr)
      : _ops(ops), _lxpr(lxpr), _rxpr(rxpr) {};
    int size_impl() const { return _lxpr.size(); };

    /// This does the element-wise computation for index indx.
    auto at_impl(int indx) const { return _ops(_lxpr[indx], _rxpr[indx]); };

    /// Conversion from arbitrary expr to concrete data type. It evaluates
    /// element-wise computations for all indices.
    template<typename DataType> operator DataType()
    {
      DataType _vec(size());
      for(int _ind = 0; _ind < _vec.size(); ++_ind)
        _vec[_ind] = (*this)[_ind];
      return _vec;
    }
    
  private: /// Ops and expr are assumed cheap to copy.
    Ops   _ops;
    lExpr _lxpr;
    rExpr _rxpr;
  };


/// -----------------------------------------
/// Section 4.
///
/// The following two structs defines algebraic operations on PAEs: here only vector 
/// plus and vector inner product are implemented. 
///
/// First, some element-wise operations are defined : in other words, vec_plus and 
/// vec_prod acts on elements in Vectors, but not whole Vectors. 
///
/// Then, operator + & * are overloaded on PAEs, such that: + & * operations on PAEs         
/// also return PAEs.
///
/// -----------------------------------------


  /// Element-wise plus operation.
  struct vec_plus_t
  {
    constexpr explicit vec_plus_t() = default; 
    template<typename LType, typename RType>
    auto operator()(const LType &lhs, const RType &rhs) const
    { return lhs+rhs; }
  };
  
  /// Element-wise inner product operation.
  struct vec_prod_t
  {
    constexpr explicit vec_prod_t() = default; 
    template<typename LType, typename RType>
    auto operator()(const LType &lhs, const RType &rhs) const
    { return lhs*rhs; }
  };
  
  /// Constant plus and inner product operator objects.
  constexpr vec_plus_t vec_plus{};
  constexpr vec_prod_t vec_prod{};
  
  /// Plus operator overload on expressions: return binary expression.
  template<typename lExpr, typename rExpr>
  auto operator+(const lExpr &lhs, const rExpr &rhs)
  { return binary_ops<vec_plus_t,lExpr,rExpr>(vec_plus,lhs,rhs); }
  
  /// Inner prod operator overload on expressions: return binary expression.
  template<typename lExpr, typename rExpr>
  auto operator*(const lExpr &lhs, const rExpr &rhs)
  { return binary_ops<vec_prod_t,lExpr,rExpr>(vec_prod,lhs,rhs); }
  
} //!expr


#endif //!EXPR_EXPR

```

### File main.cc : test src file

```cpp
# include <chrono>
# include <iomanip>
# include <iostream>
# include "vec.hh"
# include "expr.hh"
# include "boost/core/demangle.hpp"


int main()
{
  using dtype = float;
  constexpr int size = 5e7;
  
  std::vector<dtype> _vec1(size);
  std::vector<dtype> _vec2(size);
  std::vector<dtype> _vec3(size);

  // ... Initialize vectors' contents.

  Vector<dtype> vec1(std::move(_vec1));
  Vector<dtype> vec2(std::move(_vec2));
  Vector<dtype> vec3(std::move(_vec3));

  unsigned long start_ms_no_ets =
    std::chrono::duration_cast<std::chrono::milliseconds>
    (std::chrono::system_clock::now().time_since_epoch()).count();
  std::cout << "\nNo-ETs evaluation starts.\n";
  
  Vector<dtype> result_no_ets = vec1 + (vec2*vec3);
  
  unsigned long stop_ms_no_ets =
    std::chrono::duration_cast<std::chrono::milliseconds>
    (std::chrono::system_clock::now().time_since_epoch()).count();
  std::cout << std::setprecision(6) << std::fixed
            << "No-ETs. Time eclapses: " << (stop_ms_no_ets-start_ms_no_ets)/1000.0
            << " s.\n" << std::endl;
  
  unsigned long start_ms_ets =
    std::chrono::duration_cast<std::chrono::milliseconds>
    (std::chrono::system_clock::now().time_since_epoch()).count();
  std::cout << "Evaluation using ETs starts.\n";
  
  expr::terminal<Vector<dtype>> vec4(vec1);
  expr::terminal<Vector<dtype>> vec5(vec2);
  expr::terminal<Vector<dtype>> vec6(vec3);
  
  Vector<dtype> result_ets = (vec4 + vec5*vec6);
  
  unsigned long stop_ms_ets =
    std::chrono::duration_cast<std::chrono::milliseconds>
    (std::chrono::system_clock::now().time_since_epoch()).count();
  std::cout << std::setprecision(6) << std::fixed
            << "With ETs. Time eclapses: " << (stop_ms_ets-start_ms_ets)/1000.0
            << " s.\n" << std::endl;
  
  auto ets_ret_type = (vec4 + vec5*vec6);
  std::cout << "\nETs result's type:\n";
  std::cout << boost::core::demangle( typeid(decltype(ets_ret_type)).name() ) << '\n'; 

  return 0;
}

```

Here's one possible output when compiled with `-O3 -std=c++14` using GCC 5.3:

```cpp
ctor called.
ctor called.
ctor called.

No-ETs evaluation starts.
ctor called.
ctor called.
No-ETs. Time eclapses: 0.571000 s.

Evaluation using ETs starts.
ctor called.
With ETs. Time eclapses: 0.164000 s.


ETs result's type:
expr::binary_ops<expr::vec_plus_t, expr::terminal<Vector<float> >, expr::binary_ops<expr::vec_prod_t, expr::terminal<Vector<float> >, expr::terminal<Vector<float> > > >

```

The observations are:

- Using **ETs** achieves rather significant performance boost **in this case** ( > 3x).
- Creation of temporary Vector object is eliminated. As in the **ETs** case, ctor is called only once.
- Boost::demangle was used to visualize the type of ETs return before conversion: it clearly constructed exactly the same expression graph demonstrated above.

**Draw-backs and caveats**

<li>
An obvious disadvantage of **ETs** is the learning curve, the complexity of implementation and code-maintenance difficulty. In the above example where only element-wise operations are considered, the implementation contains already enormous amount of boilerplates, let alone in real world, where more complex algebraic expressions occur in every computation and element-wise independence no longer hold (for example matrix multiplication), the difficulty will be exponential.
</li>
<li>
Another caveat of using **ETs** is that they do play well with the `auto` keyword. As mentioned above, **PAE**s are essentially proxies: and proxies basically do not play well with `auto`. Consider the following example:

```cpp
 auto result = ...;                // Some expensive expression: 
                                   // auto returns the expr graph, 
                                   // NOT the computed value.
 for(auto i = 0; i < 100; ++i)
     ScalrType value = result* ... // Some other expensive computations using result.

```


</li>

Here **in each iteration of the for loop, result will be re-evaluated**, since the expression graph instead of the computed value is passed to the for loop.

Existing libraries implementing **ETs**

- [**boost::proto**](http://www.boost.org/doc/libs/1_61_0/doc/html/proto.html) is a powerful library letting you define your own rules & grammars for your own expressions and execute using **ETs**.
- [**Eigen**](http://eigen.tuxfamily.org/index.php?title=Main_Page) is a library for linear algebra that implements various algebraic computations efficiently using **ETs**.

