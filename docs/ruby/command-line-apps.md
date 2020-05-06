---
metaTitle: "Ruby - Command Line Apps"
description: "How to write a command line tool to get the weather by zip code"
---

# Command Line Apps



## How to write a command line tool to get the weather by zip code


This will be a relatively comprehensive tutorial of how to write a command line tool to print the weather from the zip code provided to the command line tool. The first step is to write the program in ruby to do this action. Let's start by writing a method `weather(zip_code)` (This method requires the `yahoo_weatherman` gem. If you do not have this gem you can install it by typing `gem install yahoo_weatherman` from the command line)

```ruby
require 'yahoo_weatherman'

def weather(zip_code)
  client = Weatherman::Client.new
  client.lookup_by_location(zip_code).condition['temp']
end

```

We now have a very basic method that gives the weather when a zip code is provided to it. Now we need to make this into a command line tool. Very quickly let's go over how a command line tool is called from the shell and the associated variables. When a tool is called like this `tool argument other_argument`, in ruby there is a variable `ARGV` which is an array equal to `['argument', 'other_argument']`. Now let us implement this in our application

```ruby
#!/usr/bin/ruby
require 'yahoo_weatherman'

def weather(zip_code)
  client = Weatherman::Client.new
  client.lookup_by_location(zip_code).condition['temp']
end 
 
puts weather(ARGV[0])

```

Good! Now we have a command line application that can be run. Notice the **she-bang** line at the beginning of the file (`#!/usr/bin/ruby`). This allows the file to become an executable. We can save this file as `weather`. (**Note**: Do not save this as `weather.rb`, there is no need for the file extension and the she-bang tells whatever you need to tell that this is a ruby file). Now we can run these commands in the shell (do not type in the `$`).

```ruby
$ chmod a+x weather
$ ./weather [ZIPCODE]

```

After testing that this works, we can now sym-link this to the `/usr/bin/local/` by running this command

```ruby
$ sudo ln -s weather /usr/local/bin/weather

```

Now `weather` can be called on the command line no matter the directory you are in.

