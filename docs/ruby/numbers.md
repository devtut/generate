---
metaTitle: "Ruby - Numbers"
description: "Converting a String to Integer, Creating an Integer, Rounding Numbers, Even and Odd Numbers, Converting a number to a string, Rational Numbers, Complex Numbers, Dividing two numbers"
---

# Numbers



## Converting a String to Integer


You can use the `Integer` method to convert a `String` to an `Integer`:

```ruby
Integer("123")      # => 123
Integer("0xFF")     # => 255
Integer("0b100")    # => 4
Integer("0555")     # => 365

```

You can also pass a base parameter to the `Integer` method to convert numbers from a certain base

```ruby
Integer('10', 5)    # => 5
Integer('74', 8)    # => 60
Integer('NUM', 36)  # => 30910

```

Note that the method raises an `ArgumentError` if the parameter cannot be converted:

```ruby
Integer("hello")
# raises ArgumentError: invalid value for Integer(): "hello"
Integer("23-hello")
# raises ArgumentError: invalid value for Integer(): "23-hello"

```

You can also use the `String#to_i` method. However, this method is slightly more permissive and has a different behavior than `Integer`:

```ruby
"23".to_i         # => 23
"23-hello".to_i   # => 23
"hello".to_i      # => 0

```

`String#to_i` accepts an argument, the base to interpret the number as:

```ruby
"10".to_i(2) # => 2
"10".to_i(3) # => 3
"A".to_i(16) # => 10

```



## Creating an Integer


```ruby
0       # creates the Fixnum 0
123     # creates the Fixnum 123
1_000   # creates the Fixnum 1000. You can use _ as separator for readability

```

By default the notation is base 10. However, there are some other built-in notations for different bases:

```ruby
0xFF    # Hexadecimal representation of 255, starts with a 0x
0b100   # Binary representation of 4, starts with a 0b
0555    # Octal representation of 365, starts with a 0 and digits

```



## Rounding Numbers


The `round` method will round a number up if the first digit after its decimal place is 5 or higher and round down if that digit is 4 or lower. This takes in an optional argument for the precision you're looking for.

```ruby
4.89.round        # => 5
4.25.round        # => 4
3.141526.round(1) # => 3.1
3.141526.round(2) # => 3.14
3.141526.round(4) # => 3.1415

```

Floating point numbers can also be rounded down to the highest integer lower than the number with the `floor` method

```ruby
4.9999999999999.floor # => 4

```

They can also be rounded up to the lowest integer higher than the number using the `ceil` method

```ruby
4.0000000000001.ceil  # => 5

```



## Even and Odd Numbers


The `even?` method can be used to determine if a number is even

```ruby
4.even?      # => true
5.even?      # => false

```

The `odd?` method can be used to determine if a number is odd

```ruby
4.odd?       # => false
5.odd?       # => true

```



## Converting a number to a string


Fixnum#to_s takes an optional base argument and represents the given number in that base:

```ruby
2.to_s(2)   # => "10"
3.to_s(2)   # => "11"
3.to_s(3)   # => "10"
10.to_s(16) # => "a"

```

If no argument is provided, then it represents the number in base 10

```ruby
2.to_s # => "2"
10423.to_s # => "10423"

```



## Rational Numbers


`Rational` represents a rational number as numerator and denominator:

```ruby
r1 = Rational(2, 3)
r2 = 2.5.to_r
r3 = r1 + r2
r3.numerator   # => 19
r3.denominator # => 6
Rational(2, 4) # => (1/2)

```

Other ways of creating a Rational

```ruby
Rational('2/3')  # => (2/3)
Rational(3)      # => (3/1)
Rational(3, -5)  # => (-3/5)
Rational(0.2)    # => (3602879701896397/18014398509481984)
Rational('0.2')  # => (1/5)
0.2.to_r         # => (3602879701896397/18014398509481984)
0.2.rationalize  # => (1/5)
'1/4'.to_r       # => (1/4)

```



## Complex Numbers


```ruby
1i     # => (0+1i)
1.to_c # => (1+0i)
rectangular = Complex(2, 3)  # => (2+3i)
polar       = Complex('1@2') # => (-0.4161468365471424+0.9092974268256817i)

polar.rectangular # => [-0.4161468365471424, 0.9092974268256817]
rectangular.polar # => [3.605551275463989, 0.982793723247329]
rectangular + polar # => (1.5838531634528576+3.909297426825682i)

```



## Dividing two numbers


When dividing two numbers pay attention to the type you want in return. Note that dividing **two integers will invoke the integer division**. If your goal is to run the float division, at least one of the parameters should be of `float` type.

Integer division:

```ruby
3 / 2 # => 1

```

Float division

```ruby
3 / 3.0 # => 1.0

16 / 2 / 2    # => 4
16 / 2 / 2.0  # => 4.0
16 / 2.0 / 2  # => 4.0
16.0 / 2 / 2  # => 4.0

```



#### Remarks


### Numbers hierarchy

Ruby includes several built-in classes to represent numbers:

```ruby
Numeric
  Integer
    Fixnum    # 1
    Bignum    # 10000000000000000000
  Float       # 1.0
  Complex     # (1+0i)
  Rational    # Rational(2, 3) == 2/3
  BigDecimal  # not loaded by default

```

The most common are:

- `Fixnum` to represent, for instance positive and negative integers
- `Float` to represent floating point numbers

`BigDecimal` is the only one not loaded by default. You can load it with:

```ruby
require "bigdecimal"

```

Note that in ruby 2.4+, `Fixnum` and `Bignum` are unified; **all** integers are now just members of the `Integer` class. For backwards compatibility, `Fixnum == Bignum == Integer`.

