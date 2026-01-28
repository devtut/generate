---
metaTitle: "Swift - Strings and Characters"
description: "String & Character Literals, Concatenate strings, String Encoding and Decomposition, Examine and compare strings, Reversing Strings, Check if String contains Characters from a Defined Set, String Iteration, Unicode, Splitting a String into an Array, Uppercase and Lowercase Strings, Formatting Strings, Converting Swift string to a number type, Convert String to and from Data / NSData, Count occurrences of a Character into a String, Remove characters from a string not defined in Set, Remove leading and trailing WhiteSpace and NewLine"
---

# Strings and Characters




## String & Character Literals


[String](https://developer.apple.com/reference/swift/string) literals in Swift are delimited with double quotes (`"`):

```swift
let greeting = "Hello!"  // greeting's type is String

```

[Characters](https://developer.apple.com/reference/swift/character) can be initialized from string literals, as long as the literal contains only one grapheme cluster:

```swift
let chr: Character = "H" // valid
let chr2: Character = "😊" // valid
let chr3: Character = "abc" // invalid - multiple grapheme clusters

```

### String Interpolation

[**String interpolation**](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/StringsAndCharacters.html#//apple_ref/doc/uid/TP40014097-CH7-ID292) allows injecting an expression directly into a string literal. This can be done with all types of values, including strings, integers, floating point numbers and more.

The syntax is a backslash followed by parentheses wrapping the value: `\(value)`. Any valid expression may appear in the parentheses, including function calls.

```swift
let number = 5
let interpolatedNumber = "\(number)"  // string is "5"
let fortyTwo = "\(6 * 7)"             // string is "42"

let example = "This post has \(number) view\(number == 1 ? "" : "s")"
// It will output "This post has 5 views" for the above example.
// If the variable number had the value 1, it would output "This post has 1 view" instead.

```

For custom types, the [default behavior](https://github.com/apple/swift/blob/master/stdlib/public/core/StringInterpolation.swift.gyb) of string interpolation is that `"\(myobj)"` is equivalent to `String(myobj)`, the same representation used by `print(myobj)`. You can customize this behavior by implementing the [`CustomStringConvertible` protocol](https://developer.apple.com/library/ios/documentation/Swift/Reference/Swift_CustomStringConvertible_Protocol/index.html) for your type.

For Swift 3, in accordance with [SE-0089](https://github.com/apple/swift-evolution/blob/master/proposals/0089-rename-string-reflection-init.md), `String.init<T>(_:)` has been renamed to `String.init<T>(describing:)`.

The string interpolation `"\(myobj)"` will prefer the new `String.init<T: LosslessStringConvertible>(_:)` initializer, but will fall back to `init<T>(describing:)` if the value is not `LosslessStringConvertible`.

### Special Characters

Certain characters require a special **escape sequence** to use them in string literals:

|Character|Meaning
|---|---|---|---|---|---|---|---|---|---
|`\0`|the null character
|`\\`|a plain backslash, `\`
|`\t`|a tab character
|`\v`|a vertical tab
|`\r`|a [carriage return](https://en.wikipedia.org/wiki/Newline)
|`\n`|a [line feed](https://en.wikipedia.org/wiki/Newline) ("newline")
|`\"`|a double quote, `"`
|`\'`|a single quote, `'`
|`\u{n}`|the Unicode code point **n** (in hexadecimal)

**Example:**

```swift
let message = "Then he said, \"I \u{1F496} you!\""

print(message) // Then he said, "I 💖 you!"

```



## Concatenate strings


Concatenate strings with the `+` operator to produce a new string:

```swift
let name = "John"
let surname = "Appleseed"
let fullName = name + " " + surname  // fullName is "John Appleseed"

```

Append to a [**mutable**](http://stackoverflow.com/questions/24002092/what-is-the-difference-between-let-and-var-in-swift) string using the [`+=`](https://developer.apple.com/reference/swift/1540630) [compound assignment operator](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/BasicOperators.html#//apple_ref/doc/uid/TP40014097-CH6-ID69), or using a method:

```swift
let str2 = "there"
var instruction = "look over"
instruction += " " + str2  // instruction is now "look over there"

var instruction = "look over"
instruction.append(" " + str2)  // instruction is now "look over there"

```

Append a single character to a mutable String:

```swift
var greeting: String = "Hello"
let exclamationMark: Character = "!"
greeting.append(exclamationMark) 
// produces a modified String (greeting) = "Hello!"

```

Append multiple characters to a mutable String

```swift
var alphabet:String = "my ABCs: "
alphabet.append(contentsOf: (0x61...0x7A).map(UnicodeScalar.init)
                                         .map(Character.init) )
// produces a modified string (alphabet) = "my ABCs: abcdefghijklmnopqrstuvwxyz"

```

`appendContentsOf(_:)` has been renamed to [`append(_:)`](http://swiftdoc.org/v3.0/type/String/#func-append_-string).

Join a [sequence](http://swiftdoc.org/v2.2/protocol/SequenceType/) of strings to form a new string using [`joinWithSeparator(_:)`](https://developer.apple.com/library/tvos/documentation/Swift/Reference/Swift_SequenceType_Protocol/index.html#//apple_ref/swift/intfm/SequenceType/s:FesRxs12SequenceTypeWx9Generator7Element_zSSrS_17joinWithSeparatorFSSSS):

```swift
let words = ["apple", "orange", "banana"]
let str = words.joinWithSeparator(" & ")

print(str)   // "apple & orange & banana"

```

`joinWithSeparator(_:)` has been renamed to [`joined(separator:)`](http://swiftdoc.org/v3.0/protocol/Sequence/#func-iterator-element-string-joined_).

The `separator` is the empty string by default, so `["a", "b", "c"].joined() == "abc"`.



## String Encoding and Decomposition


A Swift [String](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/StringsAndCharacters.html) is made of [Unicode](http://www.unicode.org/standard/standard.html) code points. It can be decomposed and encoded in several different ways.

```swift
let str = "ที่👌①!"

```

### Decomposing Strings

A string's `characters` are Unicode [extended grapheme clusters](https://developer.apple.com/library/mac/documentation/Cocoa/Conceptual/Strings/Articles/stringsClusters.html):

```swift
Array(str.characters)  // ["ที่", "👌", "①", "!"]

```

The `unicodeScalars` are the Unicode [code points](https://en.wikipedia.org/wiki/Code_point) that make up a string (notice that `ที่` is one grapheme cluster, but 3 code points — 3607, 3637, 3656 — so the length of the resulting array is not the same as with `characters`):

```swift
str.unicodeScalars.map{ $0.value }  // [3607, 3637, 3656, 128076, 9312, 33]

```

You can encode and decompose strings as [UTF-8](https://en.wikipedia.org/wiki/UTF-8) (a sequence of `UInt8`s) or [UTF-16](https://en.wikipedia.org/wiki/UTF-16) (a sequence of `UInt16`s):

```swift
Array(str.utf8)   // [224, 184, 151, 224, 184, 181, 224, 185, 136, 240, 159, 145, 140, 226, 145, 160, 33]
Array(str.utf16)  // [3607, 3637, 3656, 55357, 56396, 9312, 33]

```

### String Length and Iteration

A string's `characters`, `unicodeScalars`, `utf8`, and `utf16` are all [Collection](https://developer.apple.com/library/ios/documentation/Swift/Reference/Swift_CollectionType_Protocol/index.html)s, so you can get their `count` and iterate over them:

```swift
// NOTE: These operations are NOT necessarily fast/cheap! 

str.characters.count     // 4
str.unicodeScalars.count // 6
str.utf8.count           // 17
str.utf16.count          // 7

```

```swift
for c in str.characters { // ...
for u in str.unicodeScalars { // ...
for byte in str.utf8 { // ...
for byte in str.utf16 { // ...

```



## Examine and compare strings


Check whether a string is empty:

```swift
if str.isEmpty {
    // do something if the string is empty
}

// If the string is empty, replace it with a fallback:
let result = str.isEmpty ? "fallback string" : str

```

Check whether two strings are equal (in the sense of [Unicode canonical equivalence](http://www.unicode.org/glossary/#canonical_equivalent)):

```swift
"abc" == "def"          // false
"abc" == "ABC"          // false
"abc" == "abc"          // true

// "LATIN SMALL LETTER A WITH ACUTE" == "LATIN SMALL LETTER A" + "COMBINING ACUTE ACCENT"
"\u{e1}" == "a\u{301}"  // true

```

Check whether a string starts/ends with another string:

```swift
"fortitude".hasPrefix("fort")      // true
"Swift Language".hasSuffix("age")  // true

```



## Reversing Strings


```swift
let aString = "This is a test string."

// first, reverse the String's characters 
let reversedCharacters = aString.characters.reverse()

// then convert back to a String with the String() initializer
let reversedString = String(reversedCharacters)

print(reversedString) // ".gnirts tset a si sihT"

```

```swift
let reversedCharacters = aString.characters.reversed()
let reversedString = String(reversedCharacters)

```



## Check if String contains Characters from a Defined Set


**Letters**

```swift
let letters = CharacterSet.letters

let phrase = "Test case"
let range = phrase.rangeOfCharacter(from: letters)

// range will be nil if no letters is found
if let test = range {
   print("letters found")
}
else {
   print("letters not found")
}

```

```swift
let letters = NSCharacterSet.letterCharacterSet()

let phrase = "Test case"
let range = phrase.rangeOfCharacterFromSet(letters)

// range will be nil if no letters is found
if let test = range {
   print("letters found")
}
else {
  print("letters not found")
}

```

The new `CharacterSet` struct that is also bridged to the Objective-C `NSCharacterSet` class define several predefined sets as:

- `decimalDigits`
- `capitalizedLetters`
- `alphanumerics`
- `controlCharacters`
- `illegalCharacters`
- and more you can find in the [NSCharacterSet](https://developer.apple.com/reference/foundation/nscharacterset) reference.

You also can define your own set of characters:

```swift
let phrase = "Test case"
let charset = CharacterSet(charactersIn: "t")

if let _ = phrase.rangeOfCharacter(from: charset, options: .caseInsensitive) {
   print("yes") 
}
else {
   print("no")
}

```

```swift
let charset = NSCharacterSet(charactersInString: "t")

if let _ = phrase.rangeOfCharacterFromSet(charset, options: .CaseInsensitiveSearch, range: nil) {
   print("yes")
}
else {
    print("no")
}

```

You can also include range:

```swift
let phrase = "Test case"
let charset = CharacterSet(charactersIn: "t")

if let _ = phrase.rangeOfCharacter(from: charset, options: .caseInsensitive, range: phrase.startIndex..<phrase.endIndex)) {
   print("yes") 
}
else {
   print("no")
}

```



## String Iteration


```swift
let string = "My fantastic string"
var index = string.startIndex

while index != string.endIndex {
    print(string[index])
    index = index.successor()
}

```

Note: `endIndex` is after the end of the string (i.e. `string[string.endIndex]` is an error, but `string[string.startIndex]` is fine). Also, in an empty string (`""`), `string.startIndex == string.endIndex` is `true`. Be sure to check for empty strings, since you cannot call `startIndex.successor()` on an empty string.

In Swift 3, String indexes no longer have `successor()`, `predecessor()`, `advancedBy(_:)`, `advancedBy(_:limit:)`, or `distanceTo(_:)`.

Instead, those operations are moved to the collection, which is now responsible for incrementing and decrementing its indices.

Available methods are `.index(after:)`, `.index(before:)` and `.index(_:, offsetBy:)`.

```swift
let string = "My fantastic string"
var currentIndex = string.startIndex

while currentIndex != string.endIndex {
    print(string[currentIndex])
    currentIndex = string.index(after: currentIndex)
}

```

**Note: we're using `currentIndex` as a variable name to avoid confusion with the `.index` method.**

And, for example, if you want to go the other way:

```swift
var index:String.Index? = string.endIndex.predecessor()

while index != nil {
    print(string[index!])
    if index != string.startIndex {
        index = index.predecessor()
    }
    else {
        index = nil
    }
}

```

(Or you could just reverse the string first, but if you don't need to go all the way through the string you probably would prefer a method like this)

```swift
var currentIndex: String.Index? = string.index(before: string.endIndex)

while currentIndex != nil {
    print(string[currentIndex!])
    if currentIndex != string.startIndex {
        currentIndex = string.index(before: currentIndex!)
    }
    else {
        currentIndex = nil
    }
}

```

Note, `Index` is an object type, and not an `Int`. You cannot access a character of string as follows:

```swift
let string = "My string"
string[2] // can't do this
string.characters[2] // and also can't do this

```

But you can get a specific index as follows:

```swift
index = string.startIndex.advanceBy(2)

```

```swift
currentIndex = string.index(string.startIndex, offsetBy: 2)

```

And can go backwards like this:

```swift
index = string.endIndex.advancedBy(-2)

```

```swift
currentIndex = string.index(string.endIndex, offsetBy: -2)

```

If you might exceed the string's bounds, or you want to specify a limit you can use:

```swift
index = string.startIndex.advanceBy(20, limit: string.endIndex)

```

```swift
currentIndex = string.index(string.startIndex, offsetBy: 20, limitedBy: string.endIndex)

```

Alternatively one can just iterate through the characters in a string, but this might be less useful depending on the context:

```swift
for c in string.characters {
    print(c)
}

```



## Unicode


### Setting values

**Using Unicode directly**

```swift
var str: String = "I want to visit 北京, Москва, मुंबई, القاهرة, and 서울시. 😊"
var character: Character = "🌍"

```

**Using hexadecimal values**

```swift
var str: String = "\u{61}\u{5927}\u{1F34E}\u{3C0}" // a大🍎π
var character: Character = "\u{65}\u{301}" // é = "e" + accent mark

```

Note that the Swift `Character` can be composed of multiple Unicode code points, but appears to be a single character. This is called an Extended Grapheme Cluster.

### Conversions

**String --> Hex**

```swift
// Accesses views of different Unicode encodings of `str`
str.utf8
str.utf16
str.unicodeScalars // UTF-32

```

**Hex --> String**

```swift
let value0: UInt8 = 0x61
let value1: UInt16 = 0x5927
let value2: UInt32 = 0x1F34E

let string0 = String(UnicodeScalar(value0)) // a
let string1 = String(UnicodeScalar(value1)) // 大
let string2 = String(UnicodeScalar(value2)) // 🍎

// convert hex array to String
let myHexArray = [0x43, 0x61, 0x74, 0x203C, 0x1F431] // an Int array
var myString = ""
for hexValue in myHexArray {
    myString.append(UnicodeScalar(hexValue))
}
print(myString) // Cat‼🐱

```

Note that for UTF-8 and UTF-16 the conversion is not always this easy because things like emoji cannot be encoded with a single UTF-16 value. It takes a surrogate pair.



## Splitting a String into an Array


In Swift you can easily separate a String into an array by slicing it at a certain character:

```swift
let startDate = "23:51"

let startDateAsArray = startDate.components(separatedBy: ":") // ["23", "51"]`

```

```swift
let startDate = "23:51"

let startArray = startDate.componentsSeparatedByString(":") // ["23", "51"]`

```

Or when the separator isn't present:

```swift
let myText = "MyText"

let myTextArray = myText.components(separatedBy: " ") // myTextArray is ["MyText"]

```

```swift
let myText = "MyText"

let myTextArray = myText.componentsSeparatedByString(" ") // myTextArray is ["MyText"]

```



## Uppercase and Lowercase Strings


To make all the characters in a String uppercase or lowercase:

```swift
let text = "AaBbCc"
let uppercase = text.uppercaseString // "AABBCC"
let lowercase = text.lowercaseString // "aabbcc"

```

```swift
let text = "AaBbCc"
let uppercase = text.uppercased() // "AABBCC"
let lowercase = text.lowercased() // "aabbcc"

```



## Formatting Strings


### Leading Zeros

```swift
let number: Int = 7
let str1 = String(format: "%03d", number) // 007
let str2 = String(format: "%05d", number) // 00007

```

### Numbers after Decimal

```swift
let number: Float = 3.14159
let str1 = String(format: "%.2f", number) // 3.14
let str2 = String(format: "%.4f", number) // 3.1416 (rounded)

```

### Decimal to Hexadecimal

```swift
let number: Int = 13627
let str1 = String(format: "%2X", number) // 353B
let str2 = String(format: "%2x", number) // 353b (notice the lowercase b)

```

Alternatively one could use specialized initializer that does the same:

```swift
let number: Int = 13627
let str1 = String(number, radix: 16, uppercase: true) //353B
let str2 = String(number, radix: 16) // 353b

```

### Decimal to a number with arbitrary radix

```swift
let number: Int = 13627
let str1 = String(number, radix: 36) // aij

```

Radix is `Int` in `[2, 36]`.



## Converting Swift string to a number type


```swift
Int("123") // Returns 123 of Int type
Int("abcd") // Returns nil
Int("10") // Returns 10 of Int type
Int("10", radix: 2) // Returns 2 of Int type
Double("1.5") // Returns 1.5 of Double type
Double("abcd") // Returns nil

```

Note that doing this returns an [`Optional`](http://stackoverflow.com/documentation/swift/247/optionals#t=201607230855233123287) value, which should be [unwrapped](http://stackoverflow.com/documentation/swift/247/optionals/913/unwrapping-an-optional#t=201607230855233123287) accordingly before being used.



## Convert String to and from Data / NSData


To convert String to and from Data / NSData we need to encode this string with a specific encoding. The most famous one is `UTF-8` which is an 8-bit representation of Unicode characters, suitable for transmission or storage by ASCII-based systems. Here is a list of all available [`String Encodings`](https://developer.apple.com/library/mac/documentation/Cocoa/Reference/Foundation/Classes/NSString_Class/#//apple_ref/c/tdef/NSStringEncoding)

**`String` to `Data`/`NSData`**

```swift
let data = string.data(using: .utf8)

```

```swift
let data = string.dataUsingEncoding(NSUTF8StringEncoding)

```

**`Data`/`NSData` to `String`**

```swift
let string = String(data: data, encoding: .utf8)

```

```swift
let string = String(data: data, encoding: NSUTF8StringEncoding)

```



## Count occurrences of a Character into a String


Given a `String` and a `Character`

```swift
let text = "Hello World"
let char: Character = "o"

```

We can count the number of times the `Character` appears into the `String` using

```swift
let sensitiveCount = text.characters.filter { $0 == char }.count // case-sensitive
let insensitiveCount = text.lowercaseString.characters.filter { $0 == Character(String(char).lowercaseString) } // case-insensitive

```



## Remove characters from a string not defined in Set


```swift
func removeCharactersNotInSetFromText(text: String, set: Set<Character>) -> String {
   return String(text.characters.filter { set.contains( $0) })
}

let text = "Swift 3.0 Come Out"
var chars = Set([Character]("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLKMNOPQRSTUVWXYZ".characters))
let newText = removeCharactersNotInSetFromText(text, set: chars) // "SwiftComeOut"

```

```swift
func removeCharactersNotInSetFromText(text: String, set: Set<Character>) -> String {
  return String(text.characters.filter { set.contains( $0) })
}

let text = "Swift 3.0 Come Out"
var chars = Set([Character]("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLKMNOPQRSTUVWXYZ".characters))
let newText = removeCharactersNotInSetFromText(text: text, set: chars)

```



## Remove leading and trailing WhiteSpace and NewLine


```swift
let someString = "  Swift Language  \n"
let trimmedString = someString.stringByTrimmingCharactersInSet(NSCharacterSet.whitespaceAndNewlineCharacterSet())
// "Swift Language"

```

Method `stringByTrimmingCharactersInSet` returns a new string made by removing from both ends of the String characters contained in a given character set.

We can also just remove only whitespace or newline.

Removing only whitespace:

```swift
let trimmedWhiteSpace = someString.stringByTrimmingCharactersInSet(NSCharacterSet.whitespaceCharacterSet())
// "Swift Language  \n"

```

Removing only newline:

```swift
let trimmedNewLine = someString.stringByTrimmingCharactersInSet(NSCharacterSet.newlineCharacterSet())
// "  Swift Language  "

```

```swift
let someString = "  Swift Language  \n"

let trimmedString = someString.trimmingCharacters(in: .whitespacesAndNewlines)
// "Swift Language"

let trimmedWhiteSpace = someString.trimmingCharacters(in: .whitespaces)
// "Swift Language  \n"

let trimmedNewLine = someString.trimmingCharacters(in: .newlines)
// "  Swift Language  "

```

**Note: all these methods belong to `Foundation`. Use `import Foundation` if Foundation isn't already imported via other libraries like Cocoa or UIKit.**



#### Syntax


- String.characters // Returns an Array of the characters in the String
- String.characters.count // Returns the number of characters
- String.utf8 // A String.UTF8View, returns the UTF-8 character points in the String
- String.utf16 // A String.UTF16View, returns the UTF-16 character points in the String
- String.unicodeScalars // A String.UnicodeScalarView, returns the UTF-32 character points in the String
- String.isEmpty // Returns true if the String does not contain any text
- String.hasPrefix(String) // Returns true if the String is prefixed with the argument
- String.hasSuffix(String) // Returns true if the String is suffixed with the argument
- String.startIndex // Returns the Index that corresponds to the first character in the string
- String.endIndex // Returns the Index that corresponds to the spot after the last character in the string
- String.components(separatedBy: String) // Returns an array containing the substrings separated by the given separator string
- String.append(Character) // Adds the character (given as argument) to the String



#### Remarks


A `String` in Swift is a collection of characters, and by extension a collection of Unicode scalars. Because Swift Strings are based on Unicode, they may be any Unicode scalar value, including languages other than English and emojis.

Because two scalars could combine to form a single character, the number of scalars in a String is not necessarily always the same as the number of characters.

For more information about Strings, see [The Swift Programming Language](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/StringsAndCharacters.html) and the [String Structure Reference](https://developer.apple.com/library/tvos/documentation/Swift/Reference/Swift_String_Structure/index.html).

For implementation details, see ["Swift String Design"](https://github.com/apple/swift/blob/master/docs/StringDesign.rst)

