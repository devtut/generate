---
metaTitle: "Ruby - Gem Creation/Management"
description: "Gemspec Files, Building A Gem, Dependencies"
---

# Gem Creation/Management



## Gemspec Files


Each gem has a file in the format of `<gem name>.gemspec` which contains metadata about the gem and it's files. The format of a gemspec is as follows:

```ruby
Gem::Specification.new do |s|
  # Details about gem. They are added in the format:
  s.<detail name> = <detail value>
end

```

The fields required by RubyGems are:

Either `author = string` or `authors = array`

Use `author =` if there is only one author, and `authors =` when there are multiple. For `authors=` use an array which lists the authors names.

```ruby
files = array

```

Here `array` is a list of all the files in the gem. This can also be used with the `Dir[]` function, for example if all your files are in the `/lib/` directory, then you can use `files = Dir["/lib/"]`.

```ruby
name = string

```

Here string is just the name of your gem. Rubygems recommends a few rules you should follow when naming your gem.

1. Use underscores, NO SPACES
1. Use only lowercase letters
1. Use hypens for gem extension (e.g. if your gem is named `example` for an extension you would name it `example-extension`) so that when then extension is required it can be required as `require "example/extension"`.

[RubyGems](http://guides.rubygems.org/name-your-gem/) also adds "If you publish a gem on rubygems.org it may be removed if the name is objectionable, violates intellectual property or the contents of the gem meet these criteria. You can report such a gem on the RubyGems Support site."

```ruby
platform=

```

I don't know

```ruby
require_paths=

```

I don't know

```ruby
summary= string

```

String is a summery of the gems purpose and anything that you would like to share about the gem.

```ruby
version= string

```

The current version number of the gem.

The recommended fields are:

```ruby
email = string

```

An email address that will be associated with the gem.

```ruby
homepage= string

```

The website where the gem lives.

Either `license=` or `licenses=`

I don't know



## Building A Gem


Once you have created your gem to publish it you have to follow a few steps:

1. Build your gem with `gem build <gem name>.gemspec` (the gemspec file must exist)
1. Create a RubyGems account if you do not already have one [here](https://rubygems.org/sign_up)
1. Check to make sure that no gems exist that share your gems name
1. Publish your gem with `gem publish <gem name>.<gem version number>.gem`



## Dependencies


To list the dependency tree:

```ruby
gem dependency

```

To list which gems depend on a specific gem (bundler for example)

```ruby
gem dependency bundler --reverse-dependencies 

```

