---
metaTitle: "Objective-C - Predefined Macros"
description: "Predefined Macros"
---

# Predefined Macros




## Predefined Macros


```objc
#import <Foundation/Foundation.h>

int main()
{
   NSLog(@"File :%s\n", __FILE__ );
   NSLog(@"Date :%s\n", __DATE__ );
   NSLog(@"Time :%s\n", __TIME__ );
   NSLog(@"Line :%d\n", __LINE__ );
   NSLog(@"ANSI :%d\n", __STDC__ );
   
   return 0;
}

```

**When the above code in a file main.m is compiled and executed, it produces the following result:**

```objc
2013-09-14 04:46:14.859 demo[20683] File :main.m
2013-09-14 04:46:14.859 demo[20683] Date :Sep 14 2013
2013-09-14 04:46:14.859 demo[20683] Time :04:46:14
2013-09-14 04:46:14.859 demo[20683] Line :8
2013-09-14 04:46:14.859 demo[20683] ANSI :1

```



#### Syntax


1. **DATE**    The current date as a character literal in "MMM DD YYYY" format
1. **TIME**    The current time as a character literal in "HH:MM:SS" format
1. **FILE**    This contains the current filename as a string literal.
1. **LINE**    This contains the current line number as a decimal constant.
1. **STDC**    Defined as 1 when the compiler complies with the ANSI standard.

