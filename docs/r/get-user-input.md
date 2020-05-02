---
metaTitle: "Get user input"
description: "User input in R"
---

# Get user input



## User input in R


Sometimes it can be interesting to have a cross-talk between the user and the program, one example being the [swirl](http://swirlstats.com/) package that had been designed to teach R in R.

One can ask for user input using the `readline` command:

```r
name <- readline(prompt = "What is your name?")

```

The user can then give any answer, such as a number, a character, vectors, and scanning the result is here to make sure that the user has given a proper answer. For example:

```r
result <- readline(prompt = "What is the result of 1+1?")
while(result!=2){
    readline(prompt = "Wrong answer. What is the result of 1+1?")
}

```

However, it is to be noted that this code be stuck in a never-ending loop, as user input is saved as a character.

We have to coerce it to a number, using `as.numeric`:

```r
result <- as.numeric(readline(prompt = "What is the result of 1+1?"))
while(result!=2){
    readline(prompt = "Wrong answer. What is the result of 1+1?")
}

```



#### Syntax


<li>
variable <- readline(prompt = "Any message for user")
</li>
<li>
name <- readline(prompt = "What's your name")
</li>

