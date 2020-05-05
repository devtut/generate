---
metaTitle: "Java - Modules"
description: "Defining a basic module"
---

# Modules



## Defining a basic module


Modules are defined in a file named `module-info.java`, named a module descriptor. It should be placed in the source-code root:

```

|-- module-info.java
 |-- com
     |-- example
         |-- foo
             |-- Foo.java
         |-- bar
             |-- Bar.java

```

Here is a simple module descriptor:

```java
module com.example {
    requires java.httpclient;
    exports com.example.foo;
}

```

The module name should be unique and it is recommended that you use the same [Reverse-DNS naming notation](https://en.wikipedia.org/wiki/Reverse_domain_name_notation) as used by packages to help ensure this.

The module `java.base`, which contains Java's basic classes, is implicitly visible to any module and does not need to be included.

The `requires` declaration allows us to use other modules, in the example the module `java.httpclient` is imported.

A module can also specify which packages it `exports` and therefore makes it visible to other modules.

The package `com.example.foo` declared in the `exports` clause will be visible to other modules. Any sub-packages of `com.example.foo` will not be exported, they need their own `export` declarations.

Conversely, `com.example.bar` which is not listed in `exports` clauses will not be visible to other modules.



#### Syntax


- requires java.xml;
- requires public java.xml; # exposes module to dependents for use
- exports com.example.foo; # dependents can use public types in this package
- exports com.example.foo.impl to com.example.bar; # restrict usage to a module



#### Remarks


The use of modules is encouraged but not required, this allows existing code to continue working in Java 9. It also allows for a gradual transition to modular code.

Any non-modular code is put in an **unnamed module** when it is compiled. This is a special module that is able to use types from all other modules but **only from packages which have an `exports` declaration**.

All packages in the **unnamed module** are exported automatically.

Keywords, e.g. `module` etc..., are restricted in use within the module declaration but can be continue to be used as identifiers elsewhere.

