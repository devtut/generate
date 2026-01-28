---
metaTitle: "Ruby - Splat operator (*)"
description: "Coercing arrays into parameter list, Variable number of arguments"
---

# Splat operator (*)




## Coercing arrays into parameter list


Suppose you had an array:

```ruby
pair = ['Jack','Jill']

```

And a method that takes two arguments:

```ruby
def print_pair (a, b)
  puts "#{a} and #{b} are a good couple!"
end

```

You might think you could just pass the array:

```ruby
print_pair(pair) # wrong number of arguments (1 for 2) (ArgumentError)

```

Since the array is just one argument, not two, so Ruby throws an exception. You **could** pull out each element individually:

```ruby
print_pair(pair[0], pair[1])

```

Or you can use the splat operator to save yourself some effort:

```ruby
print_pair(*pair)

```



## Variable number of arguments


The splat operator removes individual elements of an array and makes them into a list. This is most commonly used to create a method that accepts a variable number of arguments:

```ruby
# First parameter is the subject and the following parameters are their spouses
def print_spouses(person, *spouses)
  spouses.each do |spouse|
    puts "#{person} married #{spouse}."
  end
end

print_spouses('Elizabeth', 'Conrad', 'Michael', 'Mike', 'Eddie', 'Richard', 'John', 'Larry')

```

Notice that an array only counts as one item on the list, so you will need to us the splat operator on the calling side too if you have an array you want to pass:

```ruby
bonaparte = ['Napoleon','Joséphine','Marie Louise']
print_spouses(*bonaparte)    

```

