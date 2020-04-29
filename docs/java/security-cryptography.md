---
metaTitle: "Security & Cryptography"
description: "The JCE, Keys and Key Management, Common Java vulnerabilities, Networking Concerns, Randomness and You, Hashing and Validation"
---

# Security & Cryptography


Security practices in Java can be separated into two broad, vaguely defined categories; Java platform security, and secure Java programming.

Java platform security practices deal with managing the security and integrity of the JVM. It includes such topics as managing JCE providers and security policies.

Secure Java programming practices concern the best ways to write secure Java programs. It includes such topics as using random numbers and cryptography, and preventing vulnerabilities.



## The JCE


The Java Cryptography Extension (JCE) is a framework built into the JVM to allow developers to easily and securely use cryptography in their programs. It does this by providing a simple, portable interface to programmers, while using a system of JCE Providers to securely implement the underlying cryptographic operations.



## Keys and Key Management


While the JCE secures cryptographic operations and key generation, it is up to the developer to actually manage their keys. More information needs to be provided here.

One commonly-accepted best practice for handling keys at runtime is to store them only as `byte` arrays, and never as strings. This is because Java strings are immutable, and cannot be manually "cleared" or "zeroed out" in memory; while a reference to a string can be removed, the exact string will remain in memory until its segment of memory is garbage-collected and reused. An attacker would have a large window in which they could dump the program's memory and easily find the key. Contrarily, `byte` arrays are mutable, and can have their contents overwritten in place; it is a good idea to 'zero-out' your keys as soon as you no longer need them.



## Common Java vulnerabilities


Needs content



## Networking Concerns


Needs content



## Randomness and You


Needs content

For most applications, the `java.utils.Random` class is a perfectly fine source of "random" data. If you need to choose a random element from an array, or generate a random string, or create a temporary "unique" identifier, you should probably use `Random`.

However, many cryptographic systems rely on randomness for their security, and the randomness provided by `Random` simply isn't of high enough quality. For any cryptographic operation that requires a random input, you should use `SecureRandom` instead.



## Hashing and Validation


More information needed.

A cryptographic hash function is a member of a class of functions with three vital properties; consistency, uniqueness, and irreversibility.

**Consistency:** Given the same data, a hash function will always return the same value. That is, if X = Y, f(x) will always equal f(y) for hash function f.

**Uniqueness:** No two inputs to a hash function will ever result in the same output. That is, if X != Y, f(x) != f(y), for any values of X and Y.

**Irreversibility:** It is impractically difficult, if not impossible, to "reverse" a hash function. That is, given only f(X), there should be no way of finding the original X short of putting every possible value of X through the function f (brute-force). There should be no function f1 such that f1(f(X)) = X.

Many functions lack at least one of these attributes. For example, MD5 and SHA1 are known to have collisions, i.e. two inputs that have the same output, so they lack uniqueness. Some functions that are currently believed to be secure are SHA-256 and SHA-512.



#### Remarks


While examples should be clearly made, some topics that must be covered are:

1. The JCE provider concept/structure
1. List item

