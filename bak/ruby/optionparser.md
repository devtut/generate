---
metaTitle: "Ruby - OptionParser"
description: "Mandatory and optional command line options, Default values, Long descriptions"
---

# OptionParser


[OptionParser](https://docs.ruby-lang.org/en/2.1.0/OptionParser.html) can be used for parsing command line options from `ARGV`.



## Mandatory and optional command line options


It's relatively easy to parse the command line by hand if you aren't looking for anything too complex:

```ruby
# Naive error checking
abort('Usage: ' + $0 + ' site id ...') unless ARGV.length >= 2

# First item (site) is mandatory
site = ARGV.shift

ARGV.each do | id |
  # Do something interesting with each of the ids
end

```

But when your options start to get more complicated, you probably will need to use an option parser such as, well, [OptionParser](https://docs.ruby-lang.org/en/2.1.0/OptionParser.html):

```ruby
require 'optparse'

# The actual options will be stored in this hash
options = {}

# Set up the options you are looking for
optparse = OptionParser.new do |opts|
  opts.banner = "Usage: #{$0} -s NAME id ..."

  opts.on("-s", "--site NAME", "Site name") do |s|
    options[:site] = s
  end

  opts.on( '-h', '--help', 'Display this screen' ) do
    puts opts
    exit
  end
end

# The parse! method also removes any options it finds from ARGV.
optparse.parse!

```

There's also a non-destructive `parse`, but it's a lot less useful if you plan on using the remainder of what's in `ARGV`.

The OptionParser class doesn't have a way to enforce mandatory arguments (such as `--site` in this case). However you can do you own checking after running `parse!`:

```ruby
# Slightly more sophisticated error checking
if options[:site].nil? or ARGV.length == 0
  abort(optparse.help)
end

```

For a more generic mandatory option handler, see [this answer](http://stackoverflow.com/a/2149183/7948068). In case it isn't clear, all options are optional unless you go out of your way to make them mandatory.



## Default values


With `OptionsParser`, it's really easy to set up default values. Just pre-populate the hash you store the options in:

```ruby
options = {
  :directory => ENV['HOME']
}

```

When you define the parser, it will overwrite the default if a user provide a value:

```ruby
OptionParser.new do |opts|
  opts.on("-d", "--directory HOME", "Directory to use") do |d|
    options[:directory] = d
  end
end

```



## Long descriptions


Sometimes your description can get rather long. For instance `irb -h` lists on argument that reads:

```

 --context-mode n  Set n[0-3] to method to create Binding Object,
                    when new workspace was created

```

It's not immediately clear how to support this. Most solutions require adjusting to make the indentation of the second and following lines align to the first. Fortunately, the `on` method supports multiple description lines by adding them as separate arguments:

```

 opts.on("--context-mode n",
          "Set n[0-3] to method to create Binding Object,",
          "when new workspace was created") do |n|
    optons[:context_mode] = n
  end

```

You can add as many description lines as you like to fully explain the option.

