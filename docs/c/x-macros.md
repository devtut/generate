---
metaTitle: "C - X-macros"
description: "Trivial use of X-macros for printfs, Extension: Give the X macro as an argument, Enum Value and Identifier, Code generation"
---

# X-macros


X-macros are a preprocessor-based technique for minimizing repetitious code and maintaining data / code correspondences.  Multiple distinct macro expansions based on a common set of data are supported by representing the whole group of expansions via a single master macro, with that macro's replacement text consisting of a sequence of expansions of an inner macro, one for each datum.  The inner macro is traditionally named `X()`, hence the name of the technique.



## Trivial use of X-macros for printfs


```c
/* define a list of preprocessor tokens on which to call X */
#define X_123 X(1) X(2) X(3)

/* define X to use */
#define X(val) printf("X(%d) made this print\n", val);
X_123
#undef X
/* good practice to undef X to facilitate reuse later on */

```

This example will result in the preprocessor generating the following code:

```c
printf("X(%d) made this print\n", 1);
printf("X(%d) made this print\n", 2);
printf("X(%d) made this print\n", 3);

```



## Extension: Give the X macro as an argument


The X-macro approach can be generalized a bit by making the name of the "X" macro an argument of the master macro.  This has the advantages of helping to avoid macro name collisions and of allowing use of a general-purpose macro as the "X" macro.

As always with X macros, the master macro represents a list of items whose significance is specific to that macro.  In this variation, such a macro might be defined like so:

```c
/* declare list of items */
#define ITEM_LIST(X) \
      X(item1) \
      X(item2) \
      X(item3) \
/* end of list */

```

One might then generate code to print the item names like so:

```c
/* define macro to apply */
#define PRINTSTRING(value) printf( #value "\n");

/* apply macro to the list of items */
ITEM_LIST(PRINTSTRING)

```

That expands to this code:

```c
printf( "item1" "\n"); printf( "item2" "\n"); printf( "item3" "\n");

```

In contrast to standard X macros, where the "X" name is a built-in characteristic of the master macro, with this style it may be unnecessary or even undesirable to afterward undefine the macro used as the argument (`PRINTSTRING` in this example).



## Enum Value and Identifier


```c
/* declare items of the enum */
#define FOREACH \
      X(item1) \
      X(item2) \
      X(item3) \
/* end of list */

/* define the enum values */
#define X(id) MyEnum_ ## id,
enum MyEnum { FOREACH };
#undef X

/* convert an enum value to its identifier */
const char * enum2string(int enumValue)
{
    const char* stringValue = NULL;
#define X(id) if (enumValue == MyEnum_ ## id) stringValue = #id;
    FOREACH
#undef X
    return stringValue;
}

```

Next you can use the enumerated value in your code and easily print its identifier using :

```c
printf("%s\n", enum2string(MyEnum_item2));

```



## Code generation


X-Macros can be used for code generation, by writing repetitive code: iterate over a list to do some tasks, or to declare a set of constants, objects or functions.

### Here we use X-macros to declare an enum containing 4 commands and a map of their names as strings

Then we can print the string values of the enum.

```c
/* All our commands */
#define COMMANDS(OP) OP(Open) OP(Close) OP(Save) OP(Quit)

/* generate the enum Commands: {cmdOpen, cmdClose, cmdSave, cmdQuit, }; */
#define ENUM_NAME(name) cmd##name,
enum Commands {
  COMMANDS(ENUM_NAME)
};
#undef ENUM_NAME

/* generate the string table */
#define COMMAND_OP(name) #name,
const char* const commandNames[] = {
  COMMANDS(COMMAND_OP)
};
#undef COMMAND_OP

/* the following prints "Quit\n": */
printf("%s\n", commandNames[cmdQuit]());

```

### Similarly, we can generate a jump table to call functions by the enum value.

This requires all functions to have the same signature. If they take no arguments and return an int, we would put this in a header with the enum definition:

```c
/* declare all functions as extern */
#define EXTERN_FUNC(name) extern int doCmd##name(void);
COMMANDS(EXTERN_FUNC)
#undef EXTERN_FUNC

/* declare the function pointer type and the jump table  */
typedef int (*CommandFunc)(void);
extern CommandFunc commandJumpTable[];

```

All of the following can be in different compilation units assuming the part above is included as a header:

```c
/* generate the jump table */
#define FUNC_NAME(name) doCmd##name,
CommandFunc commandJumpTable[] = {
  COMMANDS(FUNC_NAME)
};
#undef FUNC_NAME

/* call the save command like this: */
int result = commandJumpTable[cmdSave]();

/* somewhere else, we need the implementations of the commands */
int doCmdOpen(void) {/* code performing open command */}
int doCmdClose(void) {/* code performing close command */}
int doCmdSave(void) {/* code performing save command */}
int doCmdQuit(void) {/* code performing quit command */}

```

An example of this technique being used in real code is for <a href="https://src.chromium.org/viewvc/chrome/trunk/src/gpu/command_buffer/common/gles2_cmd_ids_autogen.h" rel="nofollow noreferrer">GPU command
dispatching in Chromium</a>.



#### Remarks


The user of an X-macro-style master macro is expected to provide his own definition for the inner `X()` macro, and within its scope to expand the master macro.  The master's inner macro references are thus expanded according to the user's definition of `X()`.  In this way, the amount of repetitive boilerplate code in the source file can be reduced (appearing only once, in the replacement text of `X()`), as is favored by adherents to the "Do not Repeat Yourself" (DRY) philosophy.

Additionally, by redefining `X()` and expanding the master macro one or more additional times, X macros can facilitate maintaining corresponding data and code -- one expansion of the macro declares the data (as array elements or enum members, for example), and the other expansions produce corresponding code.

Although the "X-macro" name comes from the traditional name of the inner macro, the technique does not depend on that particular name. Any valid macro name can be used in its place.

Criticisms include

- source files that rely on X macros are more difficult to read;
- like all macros, X macros are strictly textual -- they do not inherently provide any type safety; and
- X macros provide for **code generation**.  As compared to alternatives based on calling functions, X macros effectively make the code larger.

A good explanation of X macros can be found in Randy Meyers' article [X-Macros] in Dr. Dobbs ([http://www.drdobbs.com/the-new-c-x-macros/184401387)](http://www.drdobbs.com/the-new-c-x-macros/184401387)).

