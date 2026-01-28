---
metaTitle: "Ruby - Refinements"
description: "Monkey patching with limited scope, Dual-purpose modules (refinements or global patches), Dynamic refinements"
---

# Refinements



## Monkey patching with limited scope


Monkey patching's main issue is that it pollutes the global scope. Your code working is at the mercy of all the modules you use not stepping on each others toes. The Ruby solution to this is refinements, which are basically monkey patches in a limited scope.

```ruby
module Patches
  refine Fixnum do
    def plus_one
      self + 1
    end

    def plus(num)
      self + num
    end

    def concat_one
      self.to_s + '1'
    end
  end
end

class RefinementTest
  # has access to our patches
  using Patches

  def initialize
    puts 1.plus_one
    puts 3.concat_one
  end
end

# Main scope doesn't have changes

1.plus_one
# => undefined method `plus_one' for 1:Fixnum (NoMethodError)

RefinementTest.new
# => 2
# => '31'

```



## Dual-purpose modules (refinements or global patches)


It's a good practice to scope patches using Refinements, but sometimes it's nice to load it globally (for example in development, or testing).

Say for example you want to start a console, require your library, and then have the patched methods available in the global scope. You couldn't do this with refinements because `using` needs to be called in a class/module definition. But it's possible to write the code in such a way that it's dual purpose:

```ruby
module Patch
  def patched?; true; end
  refine String do
    include Patch
  end
end

# globally
String.include Patch
"".patched? # => true

# refinement
class LoadPatch
  using Patch
  "".patched? # => true
end

```



## Dynamic refinements


Refinements have special limitations.

`refine` can only be used in a module scope, but can be programmed using `send :refine`.

`using` is more limited. It can only be called in a class/module definition. Still, it can accept a variable pointing to a module, and can be invoked in a loop.

An example showing these concepts:

```ruby
module Patch
  def patched?; true; end
end

Patch.send(:refine, String) { include Patch }

patch_classes = [Patch]

class Patched
  patch_classes.each { |klass| using klass }
  "".patched? # => true
end

```

Since `using` is so static, there can be issued with load order if the refinement files are not loaded first. A way to address this is to wrap the patched class/module definition in a proc. For example:

```ruby
module Patch
  refine String do
    def patched; true; end
  end
end

class Foo
end

# This is a proc since methods can't contain class definitions
create_patched_class = Proc.new do
  Foo.class_exec do
    class Bar
      using Patch
      def self.patched?; ''.patched == true; end
    end
  end
end
create_patched_class.call
Foo::Bar.patched? # => true

```

Calling the proc creates the patched class `Foo::Bar`. This can be delayed until after all the code has loaded.



#### Remarks


Refinements are scope lexically, meaning they're in effect from the time they're activated (with the `using` keyword) until control shifts. Usually control is changed by the end of a module, class, or file.

