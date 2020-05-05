---
metaTitle: "R - Implement State Machine Pattern using S4 Class"
description: "Parsing Lines using State Machine"
---

# Implement State Machine Pattern using S4 Class


[Finite States Machine](https://en.wikipedia.org/wiki/Finite-state_machine) concepts are usually implemented under Object Oriented Programming (OOP) languages, for example using [Java language, based on the State pattern](https://en.wikipedia.org/wiki/State_pattern) defined in GOF (refers to the book: "Design Patterns").

R provides several mechanisms to simulate the OO paradigm, let's apply [S4 Object System](http://adv-r.had.co.nz) for implementing this pattern.



## Parsing Lines using State Machine


Let's apply the [State Machine pattern](https://en.wikipedia.org/wiki/State_pattern) for parsing lines with the specific pattern using S4 Class feature from R.

**PROBLEM ENUNCIATION**

We need to parse a file where each line provides information about a person, using a delimiter (`";"`), but some information provided is optional, and instead of providing an empty field, it is missing. On each line we can have the following information: `Name;[Address;]Phone`. Where the address information is optional, sometimes we have it and sometimes don’t, for example:

```r
GREGORY BROWN; 25 NE 25TH; +1-786-987-6543
DAVID SMITH;786-123-4567
ALAN PEREZ; 25 SE 50TH; +1-786-987-5553

```

The second line does not provide address information. Therefore the number of delimiters may be deferent like in this case with one delimiter and for the other lines two delimiters.  Because the number of delimiters may vary, one way to atack this problem is to recognize the presence or not of a given field based on its pattern. In such case we can use a [regular expression](https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html) for identifying such patterns. For example:

- **Name**:  `"^([A-Z]'?\\s+)* *[A-Z]+(\\s+[A-Z]{1,2}\\.?,? +)*[A-Z]+((-|\\s+)[A-Z]+)*$"`. For example: `RAFAEL REAL, DAVID R. SMITH, ERNESTO PEREZ GONZALEZ, 0' CONNOR BROWN, LUIS PEREZ-MENA`, etc.
- **Address**: `"^\\s[0-9]{1,4}(\\s+[A-Z]{1,2}[0-9]{1,2}[A-Z]{1,2}|[A-Z\\s0-9]+)$"`. For example: `11020 LE JEUNE ROAD`, `87 SW 27TH`. For the sake of simplicity we don't include here the zipcode, city, state, but I can be included in this field or adding additional fields.
- **Phone**: `"^\\s*(\\+1(-|\\s+))*[0-9]{3}(-|\\s+)[0-9]{3}(-|\\s+)[0-9]{4}$"`. For example: `305-123-4567, 305 123 4567, +1-786-123-4567`.

**Notes**:

- I am considering the most common pattern of US addresses and phones, it can be easy extended to consider more general situations.
- In R the sign `"\"` has special meaning for character variables, therefore we need to escape it.
- In order to simplify the process of defining regular expressions a good recommendation is to use the following web page: [regex101.com](https://regex101.com), so you can play with it, with a given example, until you get the expected result for all possible combinations.

The idea is to identify each line field based on previously defined patterns. The State pattern define the following entities (classes) that collaborate to control the specific behavior (The State Pattern is a behavior pattern):

[<img src="https://i.stack.imgur.com/gToF2.png" alt="GOF State Pattern" />](https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html)

Let's describe each element considering the context of our problem:

<li>`Context`: Stores the context information of the parsing process, i.e. the current state and handles the entire State Machine Process. For each state, an action is executed (`handle()`), but the context delegates it, based on the state, on the action method defined for a particular state (`handle()` from `State` class).  It defines the interface of interest to clients. Our `Context` class can be defined like this:
<ul>
- Attributes: `state`
- Methods: `handle()`, ...

- Attributes: `name, pattern`
- Methods: `doAction()`, `isState` (using `pattern` attribute verify whether the input argument belong to this state pattern or not), …

**Note:** It is a matter of preference how to name the method that carries out the action, `handle()`, `doAction()` or `goNext()`. The method name `doAction()` can be the same for both classes (`State`or `Context`) we preferred to name as `handle()` in the `Context` class for avoiding a confusion when defining two generic methods with the same input arguments, but different class.

**PERSON CLASS**

Using the S4 syntax we can define a Person class like this:

```r
setClass(Class = "Person",
    slots = c(name = "character", address = "character", phone = "character")
)

```

It is a good recommendation to initialize the class attributes. The `setClass` documentation suggests using a generic method labeled as `"initialize"`, instead of using deprecated attributes such as: `prototype, representation`.

```r
setMethod("initialize", "Person",
  definition = function(.Object, name = NA_character_,
    address = NA_character_, phone = NA_character_) {
        .Object@name <- name
        .Object@address <- address
        .Object@phone <- phone
        .Object
    }
)

```

Because the initialize method is already a standard generic method of package `methods`, we need to respect the original argument definition. We can verify it typing on R prompt:

```r
> initialize

```

It returns the entire function definition, you can see at the top who the function is defined like:

```r
function (.Object, ...) {...}

```

Therefore when we use `setMethod` we need to follow **exaclty** the same syntax (`.Object`).

Another existing generic method is `show`, it is equivalent `toString()` method from Java and it is a good idea to have a specific implementation for class domain:

```r
setMethod("show", signature = "Person",
  definition = function(object) {
      info <- sprintf("%s@[name='%s', address='%s', phone='%s']", 
        class(object), object@name, object@address, object@phone)
      cat(info)
      invisible(NULL)
  }
)

```

**Note**: We use the same convention as in the default  `toString()` Java  implementation.

Let's say we want to save the parsed information (a list of `Person` objects) into a dataset, then we should be able first to convert a list of objects to into something the R can transform (for example coerce the object as a list). We can define the following additional method (for more detail about this see the [post](http://stackoverflow.com/questions/30386009/how-to-extend-as-list-in-a-canonical-way-to-s4-objects))

```r
setGeneric(name = "as.list", signature = c('x'),
    def = function(x) standardGeneric("as.list"))

# Suggestion taken from here:
# http://stackoverflow.com/questions/30386009/how-to-extend-as-list-in-a-canonical-way-to-s4-objects
setMethod("as.list", signature = "Person",
    definition = function(x) {
        mapply(function(y) {
        #apply as.list if the slot is again an user-defined object
        #therefore, as.list gets applied recursively
        if (inherits(slot(x,y),"Person")) {
          as.list(slot(x,y))
        } else {
          #otherwise just return the slot
          slot(x,y)
        }
      },
        slotNames(class(x)),
        SIMPLIFY=FALSE)
    }
)

```

R does not provide a sugar syntax for OO because the language was initially conceived to provide valuable functions for Statisticians. Therefore each user method requires two parts: 1) the Definition part (via `setGeneric`) and 2) the implementation part (via `setMethod`). Like in the above example.

**STATE CLASS**

Following S4 syntax, let's define the abstract `State` class.

```r
setClass(Class = "State", slots = c(name = "character", pattern = "character"))

setMethod("initialize", "State",
  definition = function(.Object, name = NA_character_, pattern = NA_character_) {
      .Object@name <- name
      .Object@pattern <- pattern
      .Object
  }
)

setMethod("show", signature = "State",
  definition = function(object) {
      info <- sprintf("%s@[name='%s', pattern='%s']", class(object), 
          object@name, object@pattern)
      cat(info)
      invisible(NULL)
  }
)

setGeneric(name = "isState", signature = c('obj', 'input'),
    def = function(obj, input) standardGeneric("isState"))

setGeneric(name = "doAction", signature = c('obj', 'input', 'context'),
    def = function(obj, input, context) standardGeneric("doAction"))

```

Every sub-class from `State` will have associated a `name` and `pattern`, but also a way to identify whether a given input belongs to this state or not (`isState()` method), and also implement the corresponding actions for this state (`doAction()` method).

In order to understand the process, let's define  the transition matrix for each state based on the input received:

|Input/Current State|Init|Name|Address|Phone
|---|---|---|---|---
|Name|Name|||
|Address||Address||
|Phone||Phone|Phone|
|End||||End

**Note:** The cell `[row, col]=[i,j]` represents the destination state for the current state `j`, when it receives the input `i`.

It means that under the state Name it can receive two inputs: an address or a phone number. Another way to represents the transaction table is using the following [UML State Machine](https://en.wikipedia.org/wiki/UML_state_machine) diagram:

[<img src="https://i.stack.imgur.com/119D9.png" alt="State Machine Diagram representation" />](https://i.stack.imgur.com/gToF2.png)

Let's implement each particular state as a sub-state of the class `State`

**STATE SUB-CLASSES**

**Init State**:

The initial state will be implemented via the following class:

```r
setClass("InitState", contains = "State")

setMethod("initialize", "InitState",
  definition = function(.Object, name = "init", pattern = NA_character_) {
      .Object@name <- name
      .Object@pattern <- pattern
      .Object
  }
)

setMethod("show", signature = "InitState",
  definition = function(object) {
      callNextMethod()
  }
)

```

In R to indicate a class is a sub-class of other class is using the attribute `contains` and indicating the class name of the parent class.

Because the sub-classes just implement the generic methods, without adding additional attributes, then the `show` method, just call the equivalent method from the upper class (via method: `callNextMethod()`)

The initial state does not have associated a pattern, it just represents the beginning of the process, then we initialize the class with an `NA` value.

Now lets to implement the generic methods from the `State` class:

```r
setMethod(f = "isState", signature = "InitState",
  definition = function(obj, input) {
      nameState <- new("NameState")
      result <- isState(nameState, input)
      return(result)
  }
)

```

For this particular state (without `pattern`), the idea it just initializes the parsing process expecting the first field will be a `name`, otherwise it will be an error.

```r
setMethod(f = "doAction", signature = "InitState",
    definition = function(obj, input, context) {
        nameState <- new("NameState")
        if (isState(nameState, input)) {
            person <- context@person
            person@name <- trimws(input)
            context@person <- person
            context@state <- nameState
        } else {
            msg <- sprintf("The input argument: '%s' cannot be identified", input)
            stop(msg)
        }
        return(context)
    }
)

```

The `doAction` method provides the transition and updates the context with the information extracted. Here we are accessing to context information via the `@-operator`. Instead, we can define `get/set` methods, to encapsulate this process (as it is mandated in OO best practices: encapsulation), but that would add four more methods per `get-set` without adding value for the purpose of this example.

It is a good recommendation in all `doAction` implementation, to add a safeguard when the input argument is not properly identified.

**Name State**

Here is the definition of this class definition:

```r
setClass ("NameState", contains = "State")

setMethod("initialize","NameState",
  definition=function(.Object, name="name",
        pattern = "^([A-Z]'?\\s+)* *[A-Z]+(\\s+[A-Z]{1,2}\\.?,? +)*[A-Z]+((-|\\s+)[A-Z]+)*$") {
        .Object@pattern <- pattern
        .Object@name <- name
        .Object       
  }
)

setMethod("show", signature = "NameState",
  definition = function(object) {
      callNextMethod()
  }
)

```

We use the function `grepl` for verifying the input belongs to a given pattern.

```r
setMethod(f="isState", signature="NameState",
  definition=function(obj, input) {
      result <- grepl(obj@pattern, input, perl=TRUE)
      return(result)
  }
)

```

Now we define the action to carry out for a given state:

```r
setMethod(f = "doAction", signature = "NameState",
  definition=function(obj, input, context) {
      addressState <- new("AddressState")
      phoneState <- new("PhoneState")
      person <- context@person
      if (isState(addressState, input)) {
          person@address <- trimws(input)
          context@person <- person
          context@state <- addressState
      } else if (isState(phoneState, input)) {
          person@phone <- trimws(input)
          context@person <- person
          context@state <- phoneState
      } else {
          msg <- sprintf("The input argument: '%s' cannot be identified", input)
          stop(msg)
      }
      return(context)
  }
)

```

Here we consider to possible transitions: one for Address state and the other one for Phone state. In all cases we update the context information:

- The `person` information: `address` or `phone` with the input argument.
- The `state` of the process

The way to identify the state is to invoke the method: `isState()` for a particular state. We create a default specific states (`addressState, phoneState`) and then ask for a particular validation.

The logic for the other sub-classes (one per state) implementation is very similar.

**Address State**

```r
setClass("AddressState", contains = "State")

setMethod("initialize", "AddressState",
  definition = function(.Object, name="address",
    pattern = "^\\s[0-9]{1,4}(\\s+[A-Z]{1,2}[0-9]{1,2}[A-Z]{1,2}|[A-Z\\s0-9]+)$") {
        .Object@pattern <- pattern
        .Object@name <- name
        .Object
    }
)

setMethod("show", signature = "AddressState",
  definition = function(object) {
      callNextMethod()
  }
)

setMethod(f="isState", signature="AddressState",
    definition=function(obj, input) {
        result <- grepl(obj@pattern, input, perl=TRUE)
        return(result)
    }
)
    
setMethod(f = "doAction", "AddressState",
    definition=function(obj, input, context) {
        phoneState <- new("PhoneState")
        if (isState(phoneState, input)) {
            person <- context@person
            person@phone <- trimws(input)
            context@person <- person
            context@state <- phoneState
        } else {
            msg <- sprintf("The input argument: '%s' cannot be identified", input)
            stop(msg)
        }
        return(context)
    }
)

```

**Phone State**

```r
setClass("PhoneState", contains = "State")

setMethod("initialize", "PhoneState",
  definition = function(.Object, name = "phone",
    pattern = "^\\s*(\\+1(-|\\s+))*[0-9]{3}(-|\\s+)[0-9]{3}(-|\\s+)[0-9]{4}$") {
        .Object@pattern <- pattern
        .Object@name <- name
        .Object
    }
)

setMethod("show", signature = "PhoneState",
  definition = function(object) {
      callNextMethod()
  }
)

setMethod(f = "isState", signature = "PhoneState",
    definition = function(obj, input) {
        result <- grepl(obj@pattern, input, perl = TRUE)
        return(result)
    }
)

```

Here is where we add the person information into the list of `persons` of the `context`.

```r
setMethod(f = "doAction", "PhoneState",
    definition = function(obj, input, context) {
        context <- addPerson(context, context@person)
        context@state <- new("InitState")
        return(context)
    }   
)

```

**CONTEXT CLASS**

Now the lets to explain the `Context` class implementation. We can define it considering the following attributes:

```r
setClass(Class = "Context",
     slots = c(state = "State", persons = "list", person = "Person")
)

```

Where

- `state`: The current state of the process
- `person`: The current person, it represents the information we have already parsed from the current line.
- `persons`: The list of parsed persons processed.

**Note**:  Optionally, we can add a `name` to identify the context by name in case we are working with more than one parser type.

```r
setMethod(f="initialize", signature="Context",
  definition = function(.Object) {
        .Object@state <- new("InitState")
        .Object@persons <- list()
        .Object@person <- new("Person")
        return(.Object)
    }
)

setMethod("show", signature = "Context",
  definition = function(object) {
      cat("An object of class ", class(object), "\n", sep = "")
      info <- sprintf("[state='%s', persons='%s', person='%s']", object@state,
          toString(object@persons), object@person)
      cat(info)
      invisible(NULL)
  }
)

setGeneric(name = "handle", signature = c('obj', 'input', 'context'),
    def = function(obj, input, context) standardGeneric("handle"))

setGeneric(name = "addPerson", signature = c('obj', 'person'),
    def = function(obj, person) standardGeneric("addPerson"))

setGeneric(name = "parseLine", signature = c('obj', 's'),
    def = function(obj, s) standardGeneric("parseLine"))

setGeneric(name = "parseLines", signature = c('obj', 's'),
    def = function(obj, s) standardGeneric("parseLines"))

setGeneric(name = "as.df", signature = c('obj'),
    def = function(obj) standardGeneric("as.df"))

```

With such generic methods, we control the entire behavior  of the parsing process:

- `handle()`: Will invoke the particular `doAction()` method of the current `state`.
- `addPerson`: Once we reach the end state, we need to add a `person` to the list of `persons` we have parsed.
- `parseLine()`: Parse a single line
- `parseLines()`: Parse multiple lines (an array of lines)
- `as.df()`: Extract the information from `persons` list into a data frame object.

Let's go on now with the corresponding implementations:

`handle()` method, delegates on `doAction()` method from the current `state` of the `context`:

```r
setMethod(f = "handle", signature = "Context",
    definition = function(obj, input) {
        obj <- doAction(obj@state, input, obj)
        return(obj)
    }
)

setMethod(f = "addPerson", signature = "Context",
  definition = function(obj, person) {
      obj@persons <- c(obj@persons, person)
      return(obj)
  }
)

```

First, we split the original line in an array using the delimiter to identify each element via the R-function `strsplit()`, then iterate for each element as an input value for a given state. The `handle()` method returns again the `context` with the updated information (`state`, `person`, `persons` attribute).

```r
setMethod(f = "parseLine", signature = "Context",
  definition = function(obj, s) {
      elements <- strsplit(s, ";")[[1]]
      # Adding an empty field for considering the end state.
      elements <- c(elements, "")
      n <- length(elements)
      input <- NULL
      for (i in (1:n)) {
        input <- elements[i]  
        obj <- handle(obj, input)
      }
      return(obj@person)
  }
)

```

Becuase R makes a copy of the input argument, we need to return the context (`obj`):

```r
setMethod(f = "parseLines", signature = "Context",
  definition = function(obj, s) {
      n <- length(s)
      listOfPersons <- list()
      for (i in (1:n)) {
          ipersons <- parseLine(obj, s[i])
          listOfPersons[[i]] <- ipersons
      }
      obj@persons <- listOfPersons
      return(obj)
  }
)

```

The attribute `persons` is a list of instance of S4 `Person` class. This something cannot be coerced to any standard type because R does not know of to treat an instance of a user defined class. The solution is to convert a `Person` into a list, using the `as.list` method previously defined.  Then we can apply this function to each element of the list `persons`, via the `lapply()` function. Then in the next invocation to `lappy()` function, now applies the `data.frame` function for converting each element of the `persons.list` into a data frame. Finally, the `rbind()` function is called for adding each element converted as a new row of the data frame generated (for more detail about this see this [post](http://stackoverflow.com/questions/4227223/r-list-to-data-frame))

```r
# Sugestion taken from this post:
# http://stackoverflow.com/questions/4227223/r-list-to-data-frame
setMethod(f = "as.df", signature = "Context",
  definition = function(obj) {
    persons <- obj@persons
    persons.list <- lapply(persons, as.list)
    persons.ds <- do.call(rbind, lapply(persons.list, data.frame, stringsAsFactors = FALSE))
    return(persons.ds)
  }
)

```

**PUTTING ALL TOGETHER**

Finally, lets to test the entire solution. Define the lines to parse where for the second line the address information is missing.

```r
s <- c(
    "GREGORY BROWN; 25 NE 25TH; +1-786-987-6543",
    "DAVID SMITH;786-123-4567",
     "ALAN PEREZ; 25 SE 50TH; +1-786-987-5553"
)

```

Now we initialize the `context`, and parse the lines:

```r
context <- new("Context")
context <- parseLines(context, s)

```

Finally obtain the corresponding dataset and print it:

```r
df <- as.df(context)
> df
           name    address           phone
1 GREGORY BROWN 25 NE 25TH +1-786-987-6543
2   DAVID SMITH       <NA>    786-123-4567
3    ALAN PEREZ 25 SE 50TH +1-786-987-5553

```

Let's test now the `show` methods:

```r
> show(context@persons[[1]])
Person@[name='GREGORY BROWN', address='25 NE 25TH', phone='+1-786-987-6543']

```

And for some sub-state:

```r
>show(new("PhoneState"))
PhoneState@[name='phone', pattern='^\s*(\+1(-|\s+))*[0-9]{3}(-|\s+)[0-9]{3}(-|\s+)[0-9]{4}$']

```

Finally, test the `as.list()` method:

```r
> as.list(context@persons[[1]])
$name
[1] "GREGORY BROWN"

$address
[1] "25 NE 25TH"

$phone
[1] "+1-786-987-6543"

> 

```

**CONCLUSION**

This example shows how to implement the State pattern, using one of the available mechanisms from R for using the OO paradigm. Nevertheless, the R OO solution is not user-friendly and differs so much from other OOP languages. You need to switch your mindset because the syntax is completely different, it reminds more the functional programming paradigm. For example instead of: `object.setID("A1")` as in Java/C#, for R you have to invoke the method in this way: `setID(object, "A1")`.  Therefore you always have to include the object as an input argument to provide the context of the function. On the same way, there is no special `this` class attribute and either a `"."` notation for accessing methods or attributes of the given class. It is more error prompt because to refer a class or methods is done via attribute value (`"Person"`, `"isState"`, etc.).

Said the above, S4 class solution, requires much more lines of codes than a traditional Java/C# languages for doing simple tasks.  Anyway, the State Pattern is a good and generic solution for such kind of problems. It simplifies the process delegating the logic into a particular state. Instead of having a big `if-else` block for controlling all situations, we have smaller `if-else` blocks inside on each `State` sub-class implementation for implementing the action to carry out in each state.

**Attachment**: [Here](https://www.sugarsync.com/pf/D309535_3_7753221989) you can download the entire script.

Any suggestion is welcome.

