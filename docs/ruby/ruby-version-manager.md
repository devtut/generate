---
metaTitle: "Ruby - Ruby Version Manager"
description: "How to create gemset, Installing Ruby with RVM"
---

# Ruby Version Manager



## How to create gemset


To create a gemset we need to create a `.rvmrc` file.

**Syntax:**

```

$ rvm --rvmrc --create <ruby-version>@<gemsetname>

```

**Example:**

```

$ rvm --rvmrc --create ruby-2.2.2@myblog

```

The above line will create a `.rvmrc` file in the root directory of the app.

To get the list of available gemsets, use the following command:

```

$ rvm list gemsets

```



## Installing Ruby with RVM


The **Ruby Version Manager** is a command line tool to simply install and manage different versions of Ruby.

<li>
`rvm istall 2.3.1` for example installs Ruby version 2.3.1 on your machine.
</li>
<li>
With `rvm list` you can see which versions are installed and which is actually set for use.

```ruby
  user@dev:~$ rvm list
  
  rvm rubies
  
  =* ruby-2.3.1 [ x86_64 ]
  
  # => - current
  # =* - current && default
  #  * - default

```


</li>
<li>
With `rvm use 2.3.0` you can change between installed versions.
</li>

