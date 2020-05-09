---
metaTitle: "Objective-C - Getting started with Objective-C Language"
description: "Hello World"
---

# Getting started with Objective-C Language



## Hello World


This program will output "Hello World!"

```objc
#import <Foundation/Foundation.h>

int main(int argc, char * argv[]) {
    NSLog(@"Hello World!");
}

```

`#import` is a pre-processor directive, which indicates we want to **import** or include the information from that file into the program. In this case, the compiler will copy the contents of `Foundation.h` in the `Foundation` framework to the top of the file.  The main difference between #import and #include is that #import is "smart" enough to not reprocess files that have already been included in other #includes.

The [C Language documentation](http://stackoverflow.com/documentation/c/213/hello-world/795/hello-world) explains the `main` function.

The `NSLog()` function will print the string provided to the console, along with some debugging information. In this case, we use an Objective-C string literal: `@"Hello World!"`. In C, you would write this as `"Hello World!"`, however, Apple's Foundation Framework adds the `NSString` class which provides a lot of useful functionality, and is used by NSLog. The simplest way to create an instance of `NSString` is like this: `@"****string content here****"`.

> 
Technically, NSLog() is part of Apple's Foundation Framework and is not actually part of the Objective-C language.  However, the Foundation Framework is ubiquitous throughout Objective-C programming.  Since the Foundation Framework is not open-source and cannot be used outside of Apple development, there are open-source alternatives to the framework which are associated with [OPENStep](http://toastytech.com/guis/openstep.html) and [GNUStep](http://www.nongnu.org/gap/index.html).


### Compiling the program

Assuming we want to compile our Hello World program, which consist of a single `hello.m` file, the command to compile the executable is:

```objc
clang -framework Foundation hello.m -o hello

```

Then you can run it:

```objc
./hello

```

This will output:

```objc
Hello World!

```

The options are:

<li>
`-framework`: Specifies a framework to use to compile the program. Since this program uses Foundation, we include the Foundation framework.
</li>
<li>
`-o`: This option indicate to which file we'd like to output our program. In our case `hello`. If not specified, the default value is `a.out`.
</li>

