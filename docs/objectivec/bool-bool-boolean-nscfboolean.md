---
metaTitle: "Objective C - BOOL / bool / Boolean / NSCFBoolean"
description: "BOOL/Boolean/bool/NSCFBoolean, BOOL VS Boolean"
---

# BOOL / bool / Boolean / NSCFBoolean



## BOOL/Boolean/bool/NSCFBoolean


1. bool is a datatype defined in C99.
<li>Boolean values are used in conditionals, such as if or while
statements, to conditionally perform logic or repeat execution. When
evaluating a conditional statement, the value 0 is considered
“false”, while any other value is considered “true”. Because NULL
and nil are defined as 0, conditional statements on these
nonexistent values are also evaluated as “false”.</li>
<li>BOOL is an Objective-C type defined as signed char with the macros
YES and NO to represent true and false</li>

From the definition in objc.h:

```objectivec
#if (TARGET_OS_IPHONE && __LP64__)  ||  TARGET_OS_WATCH
typedef bool BOOL;
#else
typedef signed char BOOL; 
// BOOL is explicitly signed so @encode(BOOL) == "c" rather than "C" 
// even if -funsigned-char is used.
#endif

#define YES ((BOOL)1)
#define NO  ((BOOL)0)

```


<li>NSCFBoolean is a private class in the NSNumber class cluster. It is
a bridge to the CFBooleanRef type, which is used to wrap boolean
values for Core Foundation property lists and collections. CFBoolean
defines the constants kCFBooleanTrue and kCFBooleanFalse. Because
CFNumberRef and CFBooleanRef are different types in Core Foundation,
it makes sense that they are represented by different bridging
classes in NSNumber.</li>



## BOOL VS Boolean


**BOOL**

<li>Apple's Objective-C frameworks and most Objective-C/Cocoa code uses<br />
BOOL.</li>
- Use BOOL in objective-C, when dealing with any CoreFoundation    APIs

**Boolean**

- Boolean is an old Carbon keyword , defined as an unsigned char

