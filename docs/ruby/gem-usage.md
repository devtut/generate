---
metaTitle: "Ruby - Gem Usage"
description: "Installing ruby gems, Gem installation from github/filesystem, Checking if a required gem is installed from within code, Using a Gemfile and Bundler, Bundler/inline (bundler v1.10 and later)"
---

# Gem Usage




## Installing ruby gems


This guide assumes you already have Ruby installed.
If you're using Ruby < `1.9` you'll have to manually [install RubyGems](https://rubygems.org/pages/download) as it won't be [included natively](http://guides.rubygems.org/rubygems-basics/).

To install a ruby gem, enter the command:

```ruby
gem install [gemname]

```

If you are working on a project with a list of gem dependencies, then these will be listed in a file named `Gemfile`. To install a new gem in the project, add the following line of code in the `Gemfile`:

```ruby
gem 'gemname'

```

This `Gemfile` is used by the [Bundler gem](https://rubygems.org/gems/bundler) to install dependencies your project requires, this does however mean that you'll have to install Bundler first by running (if you haven't already):

```ruby
gem install bundler

```

Save the file, and then run the command:

```ruby
bundle install

```

### Specifying versions

The version number can be specified on the command live, with the `-v` flag, such as:

```ruby
gem install gemname -v 3.14

```

When specifying version numbers in a `Gemfile`, you have several options available:

- No version specified (`gem 'gemname')` -- Will install the **latest** version which is compatible with other gems in the `Gemfile`.
- Exact version specified (`gem 'gemname', '3.14'`) -- Will only attempt to install version `3.14` (and fail if this is incompatible with other gems in the `Gemfile`).
- **Optimistic** minimum version number (`gem 'gemname', '>=3.14'`) -- Will only attempt to install the **latest** version which is compatible with other gems in the `Gemfile`, and fails if no version greater than or equal to `3.14` is compatible. The operator `>` can also be used.
- **Pessimistic** minimum version number (`gem 'gemname', '~>3.14'`) -- This is functionally equivalent to using `gem 'gemname', '>=3.14', '<4'`. In other words, only the number after the **final period** is permitted to increase.

**As a best practice**: You might want to use one of the Ruby version management libraries like [rbenv](https://github.com/rbenv/rbenv) or [rvm](https://github.com/rvm/rvm). Through these libraries, you can install different versions of Ruby runtimes and gems accordingly. So, when working in a project, this will be especially handy because most of the projects are coded against a known Ruby version.



## Gem installation from github/filesystem


You can install a gem from github or filesystem. If the gem has been checked out from git or somehow already on the file system, you could install it using

```ruby
gem install --local path_to_gem/filename.gem

```

Installing gem from github. Download the sources from github

```ruby
mkdir newgem
cd newgem
git clone https://urltogem.git

```

Build the gem

```ruby
gem build GEMNAME.gemspec
gem install gemname-version.gem

```



## Checking if a required gem is installed from within code


To check if a required gem is installed, from within your code, you can use the following (using nokogiri as an example):

```ruby
begin
  found_gem = Gem::Specification.find_by_name('nokogiri')
  require 'nokogiri'
  ....
  <the rest of your code>
rescue Gem::LoadError
end

```

However, this can be further extended to a function that can be used in setting up functionality within your code.

```ruby
def gem_installed?(gem_name)
  found_gem = false
  begin
    found_gem = Gem::Specification.find_by_name(gem_name)
  rescue Gem::LoadError
     return false
  else
    return true
  end
end

```

Now you can check if the required gem is installed, and print an error message.

```ruby
if gem_installed?('nokogiri')
  require 'nokogiri'
else
  printf "nokogiri gem required\n"
  exit 1
end

```

or

```ruby
if gem_installed?('nokogiri')
  require 'nokogiri'
else
  require 'REXML'
end

```



## Using a Gemfile and Bundler


A `Gemfile` is the standard way to organize dependencies in your application. A basic Gemfile will look like this:

You can specify the versions of the gem you want as follows:

You can also pull gems straight from a git repo:

You can also group gems depending on what they are used for. For example:

You can specify which platform certain gems should run on if you application needs to be able to run on multiple platforms. For example:

To install all the gems from a Gemfile do:



## Bundler/inline (bundler v1.10 and later)


Sometimes you need to make a script for someone but you are not sure what he has on his machine. Is there everything that your script needs? Not to worry. Bundler has a great function called in line.

It provides a `gemfile` method and before the script is run it downloads and requires all the necessary gems. A little example:

```ruby
require 'bundler/inline' #require only what you need

#Start the bundler and in it use the syntax you are already familiar with
gemfile(true) do 
  source 'https://rubygems.org'
        gem 'nokogiri', '~> 1.6.8.1'
        gem 'ruby-graphviz'
end

```

