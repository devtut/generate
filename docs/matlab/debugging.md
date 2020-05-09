---
metaTitle: "MATLAB - Debugging"
description: "Working with Breakpoints, Debugging Java code invoked by MATLAB"
---

# Debugging



## Working with Breakpoints


### Definition

> 
**In software development, a **breakpoint** is an intentional stopping or pausing place in a program, put in place for debugging purposes.**
**More generally, a breakpoint is a means of acquiring knowledge about a program during its execution. During the interruption, the programmer inspects the test environment (general purpose registers, memory, logs, files, etc.) to find out whether the program is functioning as expected. In practice, a breakpoint consists of one or more conditions that determine when a program's execution should be interrupted.**
-Wikipedia


### Breakpoints in MATLAB

### Motivation

In MATLAB, when execution pauses at a breakpoint, variables existing in the current workspace (a.k.a. **scope**) or any of the calling workspaces, can be inspected (and usually also modified).

### Types of Breakpoints

MATLAB allow users to place two types of breakpoints in `.m` files:

- Standard (or "**unrestricted**") breakpoints (shown in red) - pause execution whenever the marked line is reached.
- "Conditional" breakpoints (shown in yellow) - pause execution whenever the marked line is reached AND the condition defined in the breakpoint is evaluated as `true`.

[<img src="https://i.stack.imgur.com/0ZYRW.png" alt="Different breakpoint symbols in the GUI" />](https://i.stack.imgur.com/0ZYRW.png)

### Placing Breakpoints

Both types of breakpoints can be created in several ways:

<li>
Using the MATLAB Editor GUI, by right clicking the horizontal line next to the line number.
</li>
<li>
Using the `dbstop` command:

```matlab
% Create an unrestricted breakpoint:
dbstop in file at location
% Create a conditional breakpoint:
dbstop in file at location if expression

% Examples and special cases: 
dbstop in fit at 99 % Standard unrestricted breakpoint.

dbstop in fit at 99 if nargin==3 % Standard conditional breakpoint.

dbstop if error % This special type of breakpoint is not limited to a specific file, and
                % will trigger *whenever* an error is encountered in "debuggable" code.

dbstop in file % This will create an unrestricted breakpoint on the first executable line
               % of "file".

dbstop if naninf % This special breakpoint will trigger whenever a computation result 
                 % contains either a NaN (indicates a division by 0) or an Inf

```


</li>
<li>
Using keyboard shortcuts: the default key for creating a standard breakpoint on Windows is <kbd>F12</kbd>; the default key for conditional breakpoints is **unset**.
</li>

### Disabling and Re-enabling Breakpoints

Disable a breakpoint to temporarily ignore it: disabled breakpoints do not pause execution. Disabling a breakpoint can be done in several ways:

- Right click on the red/yellow breakpoint circle > Disable Breakpoint.
- Left click on a conditional (yellow) breakpoint.
- In the Editor tab > Breakpoints > Enable\Disable.

### Removing Breakpoints

All breakpoints remain in a file until removed, either manually or automatically. Breakpoints are cleared **automatically** when ending the MATLAB session (i.e. terminating the program). Clearing breakpoints manually is done in one of the following ways:

<li>
Using the [`dbclear`](http://www.mathworks.com/help/matlab/ref/dbclear.html) command:

```matlab
dbclear all
dbclear in file   
dbclear in file at location    
dbclear if condition

```


</li>
<li>
Left clicking a standard breakpoint icon, or a disabled conditional breakpoint icon.
</li>
<li>
Right clicking on any breakpoint > Clear Breakpoint.
</li>
<li>
In the Editor tab > Breakpoints > Clear All.
</li>
<li>
In pre-R2015b versions of MATLAB, using the command [`clear`](http://stackoverflow.com/a/27493072/3372061).
</li>

### Resuming Execution

When execution is paused at a breakpoint, there are two ways to continue executing the program:

<li>
Execute the current line and pause  again before the next line.
<kbd>F10</kbd><sup>1</sup> in the Editor, `dbstep` in the Command Window, "Step" in Ribbon > Editor > DEBUG.
</li>
<li>
Execute until the next breakpoint (if there are no more breakpoints, the execution proceeds until the end of the program).
<kbd>F12</kbd><sup>1</sup> in the Editor, `dbcont` in the Command Window, "Continue" in Ribbon > Editor > DEBUG.
</li>

<sup>1</sup> - default on Windows.



## Debugging Java code invoked by MATLAB


### Overview

In order to debug Java classes that are called during MATLAB execution, it is necessary to perform two steps:

1. Run MATLAB in JVM debugging mode.
1. Attach a Java debugger to the MATLAB process.

When MATLAB is started in JVM debugging mode, the following message appears in the command window:

```matlab
JVM is being started with debugging enabled.
Use "jdb -connect com.sun.jdi.SocketAttach:port=4444" to attach debugger.

```

### MATLAB end

### Windows:

Create a shortcut to the MATLAB executable (`matlab.exe`) and add the `-jdb` flag at the end as shown below:

[<img src="http://i.stack.imgur.com/NAsCZ.png" alt="enter image description here" />](http://i.stack.imgur.com/NAsCZ.png)

When running MATLAB using this shortcut JVM debugging will be enabled.

Alternatively the `java.opts` file can be created/updated. This file is stored in "matlab-root\bin\arch", where "matlab-root" is the MATLAB installation directoy and "arch" is the architecture (e.g. "win32").

The following should be added in the file:

```matlab
-Xdebug
-Xrunjdwp:transport=dt_socket,address=1044,server=y,suspend=n

```

### Debugger end

### IntelliJ IDEA

Attaching this debugger requires the creation of a "remote debugging" configuration with the port exposed by MATLAB:

[<img src="http://i.stack.imgur.com/2mnDl.png" alt="Step 1: Creating a debug configuration" />](http://i.stack.imgur.com/2mnDl.png)

Then the debugger is started:

[<img src="http://i.stack.imgur.com/dJIp2.png" alt="Step 2: Starting the debugger" />](http://i.stack.imgur.com/dJIp2.png)

If everything is working as expected, the following message will appear in the console:

[<img src="http://i.stack.imgur.com/nc9aH.png" alt="Log output when all is working" />](http://i.stack.imgur.com/nc9aH.png)



#### Syntax


- dbstop in file at location if expression



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|file|Name of `.m` file (without extension), e.g. `fit`. This parameter is **(Required)** unless setting special conditional breakpoint types such as `dbstop if error` or `dbstop if naninf`.
|location|Line number where the breakpoint should be placed. If the specified line does not contain runnable code, the breakpoint will be placed on the first valid line **after** the specified one.
|expression|Any expression or combination thereof that evaluates to a boolean value. Examples: `ind == 1`, `nargin < 4 && isdir('Q:\')`.

