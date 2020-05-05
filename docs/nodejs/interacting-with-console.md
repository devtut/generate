---
metaTitle: "Node.js - Interacting with Console"
description: "Logging"
---

# Interacting with Console



## Logging


### Console Module

Similar to the browser environment of JavaScript node.js provides a **console** module which provides simple logging and debugging possibilities.

The most important methods provided by the console module are `console.log`, `console.error` and `console.time`. But there are several others like `console.info`.

### console.log

The parameters will be printed to the standard output (`stdout`) with a new line.

`console.log('Hello World');`

[<img src="http://i.stack.imgur.com/ebKzm.png" alt="enter image description here" />](http://i.stack.imgur.com/ebKzm.png)

### console.error

The parameters will be printed to the standard error (`stderr`) with a new line.

`console.error('Oh, sorry, there is an error.');`

[<img src="http://i.stack.imgur.com/tYA8Q.png" alt="enter image description here" />](http://i.stack.imgur.com/tYA8Q.png)

### console.time, console.timeEnd

`console.time` starts a timer with an unique lable that can be used to compute the duration of an operation. When you call `console.timeEnd` with the same label, the timer stops and it prints the elapsed time in milliseconds to `stdout`.

[<img src="http://i.stack.imgur.com/lAryO.png" alt="enter image description here" />](http://i.stack.imgur.com/lAryO.png)

### Process Module

It is possible to use the **process** module to write **directly** into the standard output of the console. Therefore it exists the method `process.stdout.write`.
Unlike `console.log` this method does not add a new line before your output.

So in the following example the method is called two times, but no new line is added in between their outputs.

[<img src="http://i.stack.imgur.com/DlAXq.png" alt="enter image description here" />](http://i.stack.imgur.com/DlAXq.png)

### Formatting

One can use **terminal (control) codes** to issue specific commands like switching colors or positioning the cursor.

[<img src="http://i.stack.imgur.com/EfFXm.png" alt="enter image description here" />](http://i.stack.imgur.com/EfFXm.png)

### General

|Effect|Code
|---|---|---
|Reset|`\033[0m`
|Hicolor|`\033[1m`
|Underline|`\033[4m`
|Inverse|`\033[7m`

### Font Colors

|Effect|Code
|---|---|---
|Black|`\033[30m`
|Red|`\033[31m`
|Green|`\033[32m`
|Yellow|`\033[33m`
|Blue|`\033[34m`
|Magenta|`\033[35m`
|Cyan|`\033[36m`
|White|`\033[37m`

### Background Colors

|Effect|Code
|---|---|---
|Black|`\033[40m`
|Red|`\033[41m`
|Green|`\033[42m`
|Yellow|`\033[43m`
|Blue|`\033[44m`
|Magenta|`\033[45m`
|Cyan|`\033[46m`
|White|`\033[47m`



#### Syntax


- console.log([data][, ...])
- console.error([data][, ...])
- console.time(label)
- console.timeEnd(label)

