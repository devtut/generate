---
metaTitle: "Ruby - Ruby Access Modifiers"
description: "Instance Variables and Class Variables, Access Controls"
---

# Ruby Access Modifiers


Access control(scope) to various methods, data members, initialize methods.



## Instance Variables and Class Variables


Let's first brush up with what are the
**Instance Variables:** They behave more like properties for an object. They are initialized on an object creation.
Instance variables are accessible through instance methods. Per Object has per instance variables. Instance Variables are not shared between objects.

Sequence class has @from, @to and @by as the instance variables.

```ruby
class Sequence
    include Enumerable

    def initialize(from, to, by)
        @from = from
        @to = to
        @by = by
    end

    def each
        x = @from
        while x < @to
            yield x
            x = x + @by
        end
    end

    def *(factor)
        Sequence.new(@from*factor, @to*factor, @by*factor)
    end

    def +(offset)
        Sequence.new(@from+offset, @to+offset, @by+offset)
    end
end

object = Sequence.new(1,10,2)
object.each do |x|
    puts x
end

Output:
1
3
5
7
9

object1 = Sequence.new(1,10,3)
object1.each do |x|
    puts x
end

Output:
1
4
7

```

**Class Variables**
Treat class variable same as static variables of java, which are shared among the various objects of that class. Class Variables are stored in heap memory.

```ruby
class Sequence
    include Enumerable
    @@count = 0
    def initialize(from, to, by)
        @from = from
        @to = to
        @by = by
        @@count = @@count + 1
    end

    def each
        x = @from
        while x < @to
            yield x
            x = x + @by
        end
    end

    def *(factor)
        Sequence.new(@from*factor, @to*factor, @by*factor)
    end

    def +(offset)
        Sequence.new(@from+offset, @to+offset, @by+offset)
    end

    def getCount
        @@count
    end
end

object = Sequence.new(1,10,2)
object.each do |x|
    puts x
end

Output:
1
3
5
7
9

object1 = Sequence.new(1,10,3)
object1.each do |x|
    puts x
end

Output:
1
4
7

puts object1.getCount
Output: 2

```

Shared among object and object1.

**Comparing the instance and class variables of Ruby against Java:**

```ruby
Class Sequence{
    int from, to, by;
    Sequence(from, to, by){// constructor method of Java is equivalent to initialize method of ruby
        this.from = from;// this.from of java is equivalent to @from indicating currentObject.from
        this.to = to;
        this.by = by;
    }
    public void each(){
        int x = this.from;//objects attributes are accessible in the context of the object.
        while x > this.to
            x = x + this.by
    }
}

```



## Access Controls


**Comparison of access controls of Java against Ruby:**
If method is declared private in Java, it can only be accessed by other methods within the same class.
If a method is declared protected it can be accessed by other classes which exist within the same package as well as by subclasses of the class in a different package.
When a method is public it is visible to everyone.
In Java, access control visibility concept depends on where these classes lie's in the inheritance/package hierarchy.

**Whereas in Ruby, the inheritance hierarchy or the package/module don't fit.**
**It's all about which object is the receiver of a method**.

**For a private method in Ruby**, it can never be called with an explicit receiver. We can (only) call the private method with an implicit receiver.

This also means we can call a private method from within a class it is declared in as well as all subclasses of this class.

```ruby
class Test1
  def main_method
    method_private
  end

  private
  def method_private
    puts "Inside methodPrivate for #{self.class}"
  end
end

class Test2 < Test1
  def main_method
    method_private
  end
end

Test1.new.main_method
Test2.new.main_method

Inside methodPrivate for Test1
Inside methodPrivate for Test2

class Test3 < Test1
  def main_method
    self.method_private #We were trying to call a private method with an explicit receiver and if called in the same class with self would fail.
  end
end

Test1.new.main_method
This will throw NoMethodError

You can never call the private method from outside the class hierarchy where it was defined.

```

**Protected method** can be called with an implicit receiver, as like private.
In addition protected method can also be called by an explicit receiver (only) if the receiver is "self" or "an object of the same class".

```ruby
class Test1
  def main_method
    method_protected
  end

  protected
  def method_protected
    puts "InSide method_protected for #{self.class}"
  end
end

class Test2 < Test1
  def main_method
    method_protected # called by implicit receiver
  end
end

class Test3 < Test1
  def main_method
    self.method_protected # called by explicit receiver "an object of the same class"
  end
end


InSide method_protected for Test1
InSide method_protected for Test2
InSide method_protected for Test3


class Test4 < Test1
  def main_method
    Test2.new.method_protected # "Test2.new is the same type of object as self"
  end
end

Test4.new.main_method

class Test5
  def main_method
    Test2.new.method_protected
  end
end

Test5.new.main_method
This would fail as object Test5 is not subclass of Test1

```

**Consider Public methods with maximum visibility**

**Summary**

<li>
**Public:** Public methods have maximum visibility
</li>
<li>
<p>**Protected:** **Protected method** can be called with an implicit receiver, as like private.
In addition protected method can also be called by an explicit receiver (only) if the receiver is "self" or "an object of the same class".</p>
</li>
<li>
**Private:** **For a private method in Ruby**, it can never be called with an explicit receiver. We can (only) call the private method with an implicit receiver.  This also means we can call a private method from within a class it is declared in as well as all subclasses of this class.
</li>

