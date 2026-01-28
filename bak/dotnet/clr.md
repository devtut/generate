---
metaTitle: ".NET Framework - CLR"
description: "An introduction to Common Language Runtime"
---

# CLR



## An introduction to Common Language Runtime


The **Common Language Runtime (CLR)** is a virtual machine environment and part of the .NET Framework. It contains:

- A portable bytecode language called **Common Intermediate Language** (abbreviated CIL, or IL)
- A Just-In-Time compiler that generates machine code
- A tracing garbage collector that provides automatic memory management
- Support for lightweight sub-processes called AppDomains
- Security mechanisms through the concepts of verifiable code and trust levels

Code that runs in the CLR is referred to as **managed code** to distinguish it from code running outside the CLR (usually native code) which is referred to as **unmanaged code**. There are various mechanisms that facilitate interoperability between managed and unmanaged code.

