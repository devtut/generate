---
metaTitle: "Ruby - C Extensions"
description: "Your first extension, Working with C Structs, Writing Inline C  - RubyInLine"
---

# C Extensions




## Your first extension


C extensions are comprised of two general pieces:

1. The C Code itself.
1. The extension configuration file.

To get started with your first extension put the following in a file named `extconf.rb`:

```ruby
require 'mkmf'

create_makefile('hello_c')

```

A couple of things to point out:

First, the name `hello_c` is what the output of your compiled extension is going to be named. It will be what you use in conjunction with `require`.

Second, the `extconf.rb` file can actually be named anything, it's just traditionally what is used to build gems that have native code, the file that is actually going to compile the extension is the Makefile generated when running `ruby extconf.rb`. The default Makefile that is generated compiles all `.c` files in the current directory.

Put the following in a file named `hello.c` and run `ruby extconf.rb && make`

```ruby
#include <stdio.h>
#include "ruby.h"

VALUE world(VALUE self) {
  printf("Hello World!\n");
  return Qnil;
}

// The initialization method for this module
void Init_hello_c() {
  VALUE HelloC = rb_define_module("HelloC");
  rb_define_singleton_method(HelloC, "world", world, 0);
}

```

A breakdown of the code:

The name `Init_hello_c` must match the name defined in your `extconf.rb` file, otherwise when dynamically loading the extension, Ruby won't be able to find the symbol to bootstrap your extension.

The call to `rb_define_module` is creating a Ruby module named `HelloC` which we're going to namespace our C functions under.

Finally, the call to `rb_define_singleton_method` makes a module level method tied directly to the `HelloC` module which we can invoke from ruby with `HelloC.world`.

After having compiled the extension with the call to `make` we can run the code in our C extension.

Fire up a console!

```ruby
irb(main):001:0> require './hello_c'
=> true
irb(main):002:0> HelloC.world
Hello World!
=> nil

```



## Working with C Structs


In order to be able to work with C structs as Ruby objects, you need to wrap them with calls to `Data_Wrap_Struct` and `Data_Get_Struct`.

`Data_Wrap_Struct` wraps a C data structure in a Ruby object. It takes a pointer to your data structure, along with a few pointers to callback functions, and returns a VALUE. The `Data_Get_Struct` macro takes that VALUE and gives you back a pointer to your C data structure.

Here's a simple example:

```ruby
#include <stdio.h>
#include <ruby.h>

typedef struct example_struct {
  char *name;
} example_struct;

void example_struct_free(example_struct * self) {
  if (self->name != NULL) {
    free(self->name);
  }
  ruby_xfree(self);
}

static VALUE rb_example_struct_alloc(VALUE klass) {
  return Data_Wrap_Struct(klass, NULL, example_struct_free, ruby_xmalloc(sizeof(example_struct)));
}

static VALUE rb_example_struct_init(VALUE self, VALUE name) {
  example_struct* p;

  Check_Type(name, T_STRING);

  Data_Get_Struct(self, example_struct, p);
  p->name = (char *)malloc(RSTRING_LEN(name) + 1);
  memcpy(p->name, StringValuePtr(name), RSTRING_LEN(name) + 1);

  return self;
}

static VALUE rb_example_struct_name(VALUE self) {
  example_struct* p;
  Data_Get_Struct(self, example_struct, p);

  printf("%s\n", p->name);

  return Qnil;
}

void Init_example()
{
  VALUE mExample = rb_define_module("Example");
  VALUE cStruct = rb_define_class_under(mExample, "Struct", rb_cObject);

  rb_define_alloc_func(cStruct, rb_example_struct_alloc);
  rb_define_method(cStruct, "initialize", rb_example_struct_init, 1);
  rb_define_method(cStruct, "name", rb_example_struct_name, 0);
}

```

And the `extconf.rb`:

```ruby
require 'mkmf'

create_makefile('example')

```

After compiling the extension:

```ruby
irb(main):001:0> require './example'
=> true
irb(main):002:0> test_struct = Example::Struct.new("Test Struct")
=> #<Example::Struct:0x007fc741965068>
irb(main):003:0> test_struct.name
Test Struct
=> nil

```



## Writing Inline C  - RubyInLine


RubyInline is a framework that lets you embed other languages inside your Ruby code. It defines the Module# inline method, which returns a builder object. You pass the builder a string containing code written in a language other than Ruby, and the builder transforms it into something that you can call from Ruby.

When given C or C++ code (the two languages supported in the default RubyInline install), the builder objects writes a small extension to disk, compiles it, and loads it. You don't have to deal with the compilation yourself, but you can see the generated code and compiled extensions in the .ruby_inline subdirectory of your home directory.

**Embed C code right in your Ruby program:**

- RubyInline (available as the [rubyinline](https://rubygems.org/gems/RubyInline/versions/3.12.4) gem) create an extension automatically

> 
RubyInline won't work from within irb


```ruby
#!/usr/bin/ruby -w
    # copy.rb
    require 'rubygems'
    require 'inline'

    class Copier
    inline do |builder|
      builder.c <<END
    void copy_file(const char *source, const char *dest)
    {
      FILE *source_f = fopen(source, "r");
      if (!source_f)
      {
        rb_raise(rb_eIOError, "Could not open source : '%s'", source);
      }

      FILE *dest_f = fopen(dest, "w+");
      if (!dest_f)
      {
        rb_raise(rb_eIOError, "Could not open destination : '%s'", dest);
      }

      char buffer[1024];

      int nread = fread(buffer, 1, 1024, source_f);
      while (nread > 0)
      {
        fwrite(buffer, 1, nread, dest_f);
        nread = fread(buffer, 1, 1024, source_f);
      }
    }
    END
     end
    end

```

C function `copy_file` now exists as an instance method of `Copier`:

```ruby
open('source.txt', 'w') { |f| f << 'Some text.' }
Copier.new.copy_file('source.txt', 'dest.txt')
puts open('dest.txt') { |f| f.read }

```

