---
metaTitle: "Ruby on Rails - Gems"
description: "What is a gem?, Bundler, Gemfiles, Gemsets"
---

# Gems



## What is a gem?


A gem is the equivalent to a plugin or an extension for the programming language ruby.

To be exact even rails is nothing more than a gem. A lot of gems are built on rails or other gems (they are dependent of said gem) or are standalone.

### In your Rails project

### Gemfile

For your Rails project you have a file called `Gemfile`. In here you can add gems you want to include and use in your project. Once added you need to install the gem by using `bundler` (See Bundler section).

### Gemfile.lock

Once you have done this, your `Gemfile.lock` will be updated with your newly added gems and their dependencies. This file locks your used gems so they use that specific version declared in that file.

```ruby
GEM
remote: https://rubygems.org/
specs:
devise (4.0.3)
bcrypt (~> 3.0)
orm_adapter (~> 0.1)
railties (>= 4.1.0, < 5.1)
responders
warden (~> 1.2.3)

```

This example is for the gem `devise`. In the `Gemfile.lock` the version `4.0.3` is declared, to tell when installing your project on an other machine or on your production server which specified version to use.

### Development

Either a single person, a group or a whole community works on and maintains a gem. Work done is usually released after certain `issues` have been fixed or `features` have been added.

Usually the releases follow the [Semantic Versioning 2.0.0](http://semver.org/) principle.



## Bundler


The easiest way to handle and manage gems is by using `bundler`. [Bundler](http://bundler.io) is a package manager comparable to bower.

To use bundler you first need to install it.

```ruby
gem install bundler

```

After you have bundler up and running all you need to do is add gems to your `Gemfile` and run

```ruby
bundle

```

in your terminal. This installs your newly added gems to your project. Should an issue arise, you would get a prompt in your terminal.

If you are interested in more details, I suggest you have a look at the [docs](http://bundler.io/docs.html).



## Gemfiles


To start, gemfiles require at least one source, in the form of the URL for a RubyGems server.

Generate a Gemfile with the default rubygems.org source by running `bundle init`. Use https so your connection to the server will be verified with SSL.

```ruby
source 'https://rubygems.org'

```

Next, declare the gems that you need, including version numbers.

```ruby
gem 'rails', '4.2.6'
gem 'rack',  '>=1.1'
gem 'puma',  '~>3.0'

```

Most of the version specifiers, like >= 1.0, are self-explanatory. The specifier ~> has a special meaning. ~> 2.0.3 is identical to >= 2.0.3 and < 2.1. ~> 2.1 is identical to >= 2.1 and < 3.0. ~> 2.2.beta will match prerelease versions like 2.2.beta.12.

Git repositories are also valid gem sources, as long as the repo contains one or more valid gems. Specify what to check out with `:tag`, `:branch`, or `:ref`. The default is the `master` branch.

```ruby
gem 'nokogiri', :git => 'https://github.com/sparklemotion/nokogiri', :branch => 'master'

```

If you would like to use an unpacked gem directly from the filesystem, simply set the :path option to the path containing the gem's files.

```ruby
gem 'extracted_library', :path => './vendor/extracted_library'

```

Dependencies can be placed into groups. Groups can be ignored at install-time (using `--without`) or required all at once (using `Bundler.require`).

```ruby
gem 'rails_12factor', group: :production

group :development, :test do
  gem 'byebug'
  gem 'web-console', '~> 2.0'
  gem 'spring'
  gem 'dotenv-rails'
end

```

You can specify the required version of Ruby in the Gemfile with `ruby`. If the Gemfile is loaded on a different Ruby version, Bundler will raise an exception with an explanation.

```ruby
ruby '2.3.1'

```



## Gemsets


If you are using `RVM(Ruby Version Manager)` then using a `gemset` for each project is a good idea. A `gemset` is just a container you can use to keep gems separate from each other. Creating a `gemset` per project allows you to change gems (and gem versions) for one project without breaking all your other projects. Each project need only worry about its own gems.

`RVM` provides (>= 0.1.8) a `@global gemset` per ruby interpreter. Gems you install to the `@global gemset` for a given ruby are available to all other gemsets you create in association with that ruby. This is a good way to allow all of your projects to share the same installed gem for a specific ruby interpreter installation.

**Creating gemsets**

Suppose you already have `ruby-2.3.1` installed and you have selected it using this command:

```ruby
rvm use ruby-2.3.1

```

Now to create gemset for this ruby version:

```ruby
rvm gemset create new_gemset

```

where the `new_gemset` is the name of gemset. To see the list of available gemsets for a ruby version:

```ruby
rvm gemset list

```

to list the gems of all ruby versions:

```ruby
rvm gemset list_all

```

to use a gemset from the list (suppose `new_gemset` is the gemset I want to use):

```ruby
rvm gemset use new_gemset

```

you can also specify the ruby version with the gemset if you want to shift to some other ruby version:

```ruby
rvm use ruby-2.1.1@new_gemset

```

to specify a default gemset for a particular ruby version:

```ruby
rvm use 2.1.1@new_gemset --default

```

to remove all the installed gems from a gemset you can empty it by:

```ruby
rvm gemset empty new_gemset

```

to copy a gemset from one ruby to another you can do it by:

```ruby
rvm gemset copy 2.1.1@rails4 2.1.2@rails4

```

to delete a gemset:

```ruby
rvm gemset delete new_gemset

```

to see the current gemset name:

```ruby
rvm gemset name

```

to install a gem in the global gemset:

```ruby
rvm @global do gem install ...

```

**Initializing Gemsets during Ruby Installs**

When you install a new ruby, RVM not only creates two gemsets (the default, empty gemset and the global gemset), it also uses a set of user-editable files to determine which gems to install.

Working in `~/.rvm/gemsets`, rvm searchs for `global.gems` and `default.gems` using a tree-hierachy based on the ruby string being installed. Using the example of `ree-1.8.7-p2010.02`, rvm will check (and import from) the following files:

```ruby
~/.rvm/gemsets/ree/1.8.7/p2010.02/global.gems
~/.rvm/gemsets/ree/1.8.7/p2010.02/default.gems
~/.rvm/gemsets/ree/1.8.7/global.gems
~/.rvm/gemsets/ree/1.8.7/default.gems
~/.rvm/gemsets/ree/global.gems
~/.rvm/gemsets/ree/default.gems
~/.rvm/gemsets/global.gems
~/.rvm/gemsets/default.gems

```

For example, if you edited `~/.rvm/gemsets/global.gems` by adding these two lines:

```ruby
bundler
awesome_print

```

every time you install a new ruby, these two gems are installed into your global gemset. `default.gems` and `global.gems` files are usually overwritten during update of rvm.



#### Remarks


### Gemfile documentation

For projects that are expected to grow, it is a good idea add comments your `Gemfile`. That way, even in large setups you will still know what each gem does even if the name is not self-explanatory and you added it 2 years ago.

This can also help you to remember why you chose a certain version and consequently re-evaluate the version requirement later on.

Examples:

```ruby
# temporary downgrade for TeamCity
gem 'rake', '~> 10.5.0'
# To upload invoicing information to payment provider
gem 'net-sftp'

```

