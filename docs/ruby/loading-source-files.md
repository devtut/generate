---
metaTitle: "Ruby - Loading Source Files"
description: "Require files to be loaded only once, Automatically loading source files, Loading optional files, Loading files repeatedly, Loading several files"
---

# Loading Source Files



## Require files to be loaded only once


The [Kernel#require](http://www.rubydoc.info/stdlib/core/Kernel%3Arequire) method will load files only once (several calls to `require` will result in the code in that file being evaluated only once). It will search your ruby `$LOAD_PATH` to find the required file if the parameter is not an absolute path. Extensions like `.rb`, `.so`, `.o` or `.dll` are optional. Relative paths will be resolved to the current working directory of the process.

```ruby
require 'awesome_print'

```

The [Kernel#require_relative](http://www.rubydoc.info/stdlib/core/Kernel%3Arequire_relative) allows you to load files relative to the file in which `require_relative` is called.

```ruby
# will search in directory myproj relative to current source file.
#
require_relative 'myproj/version'  

```



## Automatically loading source files


The method [`Kernel#autoload`](http://www.rubydoc.info/stdlib/core/Kernel%3Aautoload) registers filename to be loaded (using `Kernel::require`) the first time that module (which may be a String or a symbol) is accessed.

```ruby
autoload :MyModule, '/usr/local/lib/modules/my_module.rb' 

```

The method [Kernel#autoload?](http://www.rubydoc.info/stdlib/core/Kernel%3Aautoload%253F) returns filename to be loaded if name is registered as `autoload`.

```ruby
autoload? :MyModule  #=> '/usr/local/lib/modules/my_module.rb'

```



## Loading optional files


When files are not available, the `require` family will throw a [`LoadError`](http://www.rubydoc.info/stdlib/core/LoadError). This is an example which illustrates loading optional modules only if they exist.

```ruby
module TidBits

@@unavailableModules = []

[
      { name: 'CoreExtend', file: 'core_extend/lib/core_extend'  } \
    , { name: 'Fs'        , file: 'fs/lib/fs'                    } \
    , { name: 'Options'   , file: 'options/lib/options'          } \
    , { name: 'Susu'      , file: 'susu/lib/susu'                } \

].each do |lib|

    begin

        require_relative lib[ :file ]

    rescue LoadError

        @@unavailableModules.push lib

    end

end

end # module TidBits

```



## Loading files repeatedly


The [Kernel#load](http://www.rubydoc.info/stdlib/core/Kernel%3Aload) method will evaluate the code in the given file. The search path will be constructed as with `require`. It will re-evaluate that code on every subsequent call unlike `require`. There is no `load_relative`.

```ruby
load `somefile`

```



## Loading several files


You can use any ruby technique to dynamically create a list of files to load. Illustration of globbing for files starting with `test`, loaded in alphabetical order.

```ruby
Dir[ "#{ __dir__ }**/test*.rb" ) ].sort.each do |source|

    require_relative source

end

```

