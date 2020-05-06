---
metaTitle: "Ruby - Constants"
description: "Define a constant, Modify a Constant, Constants cannot be defined in methods, Define and change constants in a class"
---

# Constants



## Define a constant


```ruby
MY_CONSTANT = "Hello, world" # constant
Constant = 'This is also constant' # constant
my_variable = "Hello, venus" # not constatn

```

Constant name start with capital letter. Everything that start with capital letter are considered as `constant` in Ruby. So `class` and `module` are also constant.
Best practice is use all capital letter for declaring constant.



## Modify a Constant


```ruby
MY_CONSTANT = "Hello, world"
MY_CONSTANT = "Hullo, world"

```

The above code results in a warning, because you should be using variables if you want to change their values. However it is possible to change one letter at a time in a constant without a warning, like this:

```ruby
MY_CONSTANT = "Hello, world"
MY_CONSTANT[1] = "u"

```

Now, after changing the second letter of `MY_CONSTANT`, it becomes `"Hullo, world"`.



## Constants cannot be defined in methods


```ruby
def say_hi
  MESSAGE = "Hello"
  puts MESSAGE
end

```

The above code results in an error: `SyntaxError: (irb):2: dynamic constant assignment`.



## Define and change constants in a class


```ruby
class Message
  DEFAULT_MESSAGE = "Hello, world"

  def speak(message = nil)
    if message
      puts message
    else
      puts DEFAULT_MESSAGE
    end
  end
end

```

The constant `DEFAULT_MESSAGE` can be changed with the following code:

```ruby
Message::DEFAULT_MESSAGE = "Hullo, world"

```



#### Syntax


- MY_CONSTANT_NAME = "my value"



#### Remarks


Constants are useful in Ruby when you have values that you do not want to be mistakenly changed in a program, such as API keys.

