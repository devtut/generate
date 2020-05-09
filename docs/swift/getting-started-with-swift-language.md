---
metaTitle: "Swift - Getting started with Swift Language"
description: "Your first Swift program, Your first program in Swift on a Mac (using a Playground), Installing Swift, Your first program in Swift Playgrounds app on iPad, Optional Value and Optional enum"
---

# Getting started with Swift Language



## Your first Swift program


Write your code in a file named `hello.swift`:

```swift
print("Hello, world!")

```


- To compile and run a script in one step, use `swift` from the terminal (in a directory where this file is located):

> 
To launch a terminal, press <kbd>CTRL</kbd>+<kbd>ALT</kbd>+<kbd>T</kbd> on **Linux**, or find it in Launchpad on **macOS**. To change directory, enter `cd`**`directory_name`** (or `cd ..` to go back)


> 
A **compiler** is a computer program (or a set of programs) that transforms source code written in a programming language (the source language) into another computer language (the target language), with the latter often having a binary form known as object code. ([Wikipedia](https://en.wikipedia.org/wiki/Compiler))


- To compile and run separately, use `swiftc`:

This will compile your code into `hello` file. To run it, enter `./`, followed by a filename.

- Or use the swift REPL (Read-Eval-Print-Loop), by typing `swift` from the command line, then entering your code in the interpreter:

**Code:**

> 
**Let's break this large code into pieces:**
<ul>
<li>
`func greet(name: String, surname: String) { // function body }` - create a **function** that takes a `name` and a `surname`.
</li>
<li>
`print("Greetings \(name) \(surname)")` - This prints out to the console "Greetings ", then `name`, then `surname`. Basically `\(`**`variable_name`**`)` prints out that variable's value.
</li>
<li>
`let myName = "Homer"` and `let mySurname = "Simpson"` - create **constants** (variables which value you can't change) using `let` with names: `myName`, `mySurname` and values: `"Homer"`, `"Simpson"` respectively.
</li>
<li>
`greet(name: myName, surname: mySurname)` - calls a **function** that we created earlier supplying the values of **constants** `myName`, `mySurname`.
</li>
</ul>


**Running it using REPL:**

<sup><sup>Press <kbd>CTRL</kbd>+<kbd>D</kbd> to quit from REPL.</sup></sup>



## Your first program in Swift on a Mac (using a Playground)


From your Mac, download and install Xcode from the Mac App Store following [this link](https://itunes.apple.com/it/app/xcode/id497799835?mt=12).

After the installation is complete, open Xcode and select **Get started with a Playground**:

[<img src="http://i.stack.imgur.com/Ox1wg.png" alt="enter image description here" />](http://i.stack.imgur.com/Ox1wg.png)

On the next panel, you can give your Playground a name or you can leave it `MyPlayground` and press **Next**:

[<img src="http://i.stack.imgur.com/sO7GW.png" alt="enter image description here" />](http://i.stack.imgur.com/sO7GW.png)

Select a location where to save the Playground and press **Create**:

[<img src="http://i.stack.imgur.com/DnLtL.png" alt="enter image description here" />](http://i.stack.imgur.com/DnLtL.png)

The Playground will open and your screen should look something like this:

[<img src="http://i.stack.imgur.com/BlAVs.png" alt="enter image description here" />](http://i.stack.imgur.com/BlAVs.png)

Now that the Playground is on the screen, press <kbd>⇧</kbd> + <kbd>cmd</kbd> + <kbd>Y</kbd> to show the **Debug Area**.

Finally delete the text inside Playground and type:

```swift
print("Hello world")

```

You should see 'Hello world' in the **Debug Area** and "Hello world\n" in the right **Sidebar**:

[<img src="http://i.stack.imgur.com/VMeXE.png" alt="enter image description here" />](http://i.stack.imgur.com/VMeXE.png)

Congratulations! You've created your first program in Swift!



## Installing Swift


First, [download](https://swift.org/download/) the compiler and components.

Next, add Swift to your path. On macOS, the default location for the downloadable toolchain is /Library/Developer/Toolchains. Run the following command in Terminal:

```swift
export PATH=/Library/Developer/Toolchains/swift-latest.xctoolchain/usr/bin:"${PATH}"

```

On Linux, you will need to install clang:

```swift
$ sudo apt-get install clang

```

If you installed the Swift toolchain to a directory other than the system root, you will need to run the following command, using the actual path of your Swift installation:

```swift
$ export PATH=/path/to/Swift/usr/bin:"${PATH}"

```

You can verify you have the current version of Swift by running this command:

```swift
$ swift --version

```



## Your first program in Swift Playgrounds app on iPad


Swift Playgrounds app is a great way to get started coding Swift on the go. To use it:

1- Download [Swift Playgrounds](https://itunes.apple.com/us/app/swift-playgrounds/id908519492?mt=8) for iPad from App Store.

[<img src="https://i.stack.imgur.com/Ig7wZ.png" alt="enter image description here" />](https://i.stack.imgur.com/Ig7wZ.png)

2- Open the app.

3- In the **My Playgrounds** tab, tap **+** on the top left corner and then select Blank template.

4- Enter your code.

5- Tap Run My Code to run your code.

6- At the front of each line, the result will be stored in a small square. Tap it to reveal the result.

7- To step slowly through code to trace it, tap the button next to Run My Code.



## Optional Value and Optional enum


Optionals type, which handles the absence of a value. Optionals say either "there is a value, and it equals x" or "there isn't a value at all".

An Optional is a type on its own, actually one of Swift’s new super-powered enums. It has two possible values, `None` and `Some(T)`, where T is an associated value of the correct data type available in Swift.

Let's have a look at this piece of code for example:

```swift
let x: String? = "Hello World"

if let y = x {
   print(y)
}

```

In fact if you add a **`print(x.dynamicType)`** statement in the code above you'll see this in the console:

```swift
Optional<String>

```

String? is actually syntactic sugar for Optional, and Optional is a type in its own right.

Here's a simplified version of the header of Optional, which you can see by command-clicking on the word Optional in your code from Xcode:

```swift
enum Optional<Wrapped> {

 /// The absence of a value.
 case none

 /// The presence of a value, stored as `Wrapped`.
 case some(Wrapped)
}

```

Optional is actually an enum, defined in relation to a generic type Wrapped. It has two cases: **`.none`** to represent the absence of a value, and **`.some`** to represent the presence of a value, which is stored as its associated value of type Wrapped.

Let me go through it again: `String?` is not a `String` but an `Optional<String>`.The fact that `Optional` is a type means that it has its own methods, for example `map` and `flatMap`.



#### Remarks


[<img src="https://i.stack.imgur.com/RFRe7.png" width="100" alt="Swift logo">](https://developer.apple.com/swift/)

[**Swift**](https://developer.apple.com/swift/) is an application and systems programming language developed by Apple and [distributed as open source](https://swift.org/). Swift interoperates with Objective-C and Cocoa/Cocoa touch APIs for Apple's macOS, iOS, tvOS, and watchOS operating systems. Swift currently supports macOS and Linux. Community efforts are in progress to support Android, Windows, and other platforms.

Swift development happens [on GitHub](https://github.com/apple/swift); contributions are usually submitted via [pull requests](https://github.com/apple/swift/pulls).

Bugs and other issues are tracked at [bugs.swift.org](https://bugs.swift.org/).

Discussions about Swift development, evolution, and usage are held on the [Swift mailing lists](https://lists.swift.org/mailman/listinfo).

### Other Resources

- [Swift (programming language)](https://en.wikipedia.org/wiki/Swift_(programming_language)) (Wikipedia)
- [The Swift Programming Language](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/index.html) (online)
- [Swift Standard Library Reference](https://developer.apple.com/reference/swift) (online)
- [API Design Guidelines](https://swift.org/documentation/api-design-guidelines/) (online)
- [Swift Programming Series](https://itunes.apple.com/us/book-series/swift-programming-series/id888896989?mt=11) (iBooks)
- ...and [more on developer.apple.com](https://developer.apple.com/swift/resources/).

