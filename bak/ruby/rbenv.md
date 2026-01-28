---
metaTitle: "Ruby - rbenv"
description: "1. Install and manage versions of Ruby with rbenv, Uninstalling a Ruby"
---

# rbenv



## 1. Install and manage versions of Ruby with rbenv


The easiest way to install and manage various versions of Ruby with rbenv is to use the ruby-build plugin.

First clone the rbenv repository to your home directory:

```ruby
$ git clone https://github.com/rbenv/rbenv.git ~/.rbenv

```

Then clone the ruby-build plugin:

```ruby
$ git clone https://github.com/rbenv/ruby-build.git ~/.rbenv/plugins/ruby-build

```

Ensure that rbenv is initialized in your shell session, by adding this to your `.bash_profile` or `.zshrc`:

```ruby
type rbenv > /dev/null
if [ "$?" = "0" ]; then
    eval "$(rbenv init -)"
fi

```

(This essentially first checks if `rbenv` is available, and initializes it).

You will probably have to restart your shell session - or simply open a new Terminal window.

**Note:** If you're running on OSX, you will also need to install the Mac OS Command Line Tools with:

```ruby
$ xcode-select --install

```

You can also install `rbenv` using [Homebrew](http://brew.sh/) instead of building from the source:

```ruby
$ brew update
$ brew install rbenv

```

Then follow the instructions given by:

```ruby
$ rbenv init

```

**Install a new version of Ruby:**

List the versions available with:

```ruby
$ rbenv install --list

```

Choose a version and install it with:

```ruby
$ rbenv install 2.2.0

```

Mark the installed version as the global version - i.e. the one that your system uses by default:

```ruby
$ rbenv global 2.2.0

```

Check what your global version is with:

```ruby
$ rbenv global
=> 2.2.0

```

You can specify a local project version with:

```ruby
$ rbenv local 2.1.2
=> (Creates a .ruby-version file at the current directory with the specified version)

```

Footnotes:

[1]: [Understanding PATH](https://github.com/rbenv/rbenv#understanding-path)



## Uninstalling a Ruby


There are two ways to uninstall a particular version of Ruby. The easiest is to simply remove the directory from `~/.rbenv/versions`:

```ruby
$ rm -rf ~/.rbenv/versions/2.1.0

```

Alternatively, you can use the uninstall command, which does exactly the same thing:

```ruby
$ rbenv uninstall 2.1.0

```

If this version happens to be in use somewhere, you'll need to update your global or local version. To revert to the version that's first in your path (usually the default provided by your system) use:

```ruby
$ rbenv global system

```

