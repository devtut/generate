---
metaTitle: "LaTex - Defining macros"
description: "Basic definition of macros"
---

# Defining macros



## Basic definition of macros


### Define a new basic command

A macro can be defined using `\newcommand`. For example:

```latex
\newcommand{\foo}{Just foo, you see?}

```

defines a macro `\foo` that expands to `Just foo, you see?`. It can then be used like any built-in command, for example after that definition:

```latex
He said: ``\foo''

```

expands to

```latex
He said: ``Just foo, you see?''

```

### Define a new command with arguments

Macros can also have arguments. The number of arguments is given as optional argument between the command name and the replacement text. In the replacement text, the arguments are accessed with `#1`, `#2` etc. For example:

```latex
\newcommand{\better}[2]{A #1 is better than a #2.}
\better{solution}{problem} % gives: A solution is better than a problem

```

### Redefining an existing command

If a macro has already been defined, `\newcommand` gives an error. To give a new definition for an existing command, `\renewcommand` is used instead. Other than the different name, the syntax is exactly the same. For example, after the definition of `\foo` above, one could use:

```latex
\renewcommand{\foo}{Another foo, please.}

```

After that redefinition, the macro `\foo` no longer expands to `Just foo, you see?` but to `Another foo, please.`



#### Syntax


- \newcommand{\macro}{replacement text}
- \newcommand{\macro}[argcount]{replacement text}
- \renewcommand{\macro}{replacement text}
- \renewcommand{\macro}[argcount]{replacement text}



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|`\macro`|The macro to define
|`argcount`|The number of arguments the macro expects (optional)
|`replacement text`|The replacement text for the macro. Inside that text `#1`, `#2` etc. are replaced with the macro arguments.

