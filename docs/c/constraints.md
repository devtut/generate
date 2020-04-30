---
metaTitle: "Constraints"
description: "Duplicate variable names in the same scope, Unary arithmetic operators"
---

# Constraints



## Duplicate variable names in the same scope


An example of a constraint as expressed in the C standard is having two variables of the same name declared in a scope<sup>1)</sup>, for example:

```c
void foo(int bar)
{
    int var;
    double var;
}

```

This code breaches the constraint and must produce a diagnostic message at compile time. This is very useful as compared to undefined behavior as the developer will be informed of the issue before the program is run, potentially doing anything.

Constraints thus tend to be errors which are easily detectable at compile time such as this, issues which result in undefined behavior but would be difficult or impossible to detect at compile time are thus not constraints.

1) exact wording:

If an identifier has no linkage, there shall be no more than one declaration of the identifier
(in a declarator or type specifier) with the same scope and in the same name space, except
for tags as specified in 6.7.2.3.



## Unary arithmetic operators


The unary `+` and `-` operators are only usable on arithmetic types, therefore if for example one tries to use them on a struct the program will produce a diagnostic eg:

```c
struct foo
{
    bool bar;
};

void baz(void)
{
   struct foo testStruct;
   -testStruct; /* This breaks the constraint so must produce a diagnostic */
}

```



#### Remarks


Constraints are a term used in all of the existing C specifications (recently ISO-IEC 9899-2011). They are one of the three parts of the language described in clause 6 of the standard (along side syntax and semantics).

ISO-IEC 9899-2011 defines a constraint as a:

> 
<p>restriction, either syntactic or semantic, by which the exposition of language
elements is to be interpreted</p>


(Please also note, in terms of the C standard, a "runtime-constraint" is not a kind of constraint and has extensively different rules.)

In other words a constraint describes a rule of the language which would make an otherwise syntactically valid program illegal. In this respect constraints are somewhat like undefined behavior, any program which does not follow them is not defined in terms of the C language.

Constraints on the other hand have a very significant difference from Undefined Behaviors. Namely an implementation is required to provide a diagnostic message during the translation phase (part of compilation) if a constraint is breached, this message may be a warning or may halt the compilation.

