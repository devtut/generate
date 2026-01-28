---
metaTitle: ".NET Framework - Code Contracts"
description: "Contracts for Interfaces, Preconditions, Postconditions, Installing and Enabling Code Contracts"
---

# Code Contracts



## Contracts for Interfaces


Using Code Contracts it is possible to apply a contract to an interface.  This is done by declaring an abstract class that implments the interfaces.  The interface should be tagged with the `ContractClassAttribute` and the contract definition (the abstract class) should be tagged with the `ContractClassForAttribute`

**C# Example...**

```dotnet
[ContractClass(typeof(MyInterfaceContract))]
public interface IMyInterface
{
    string DoWork(string input);
}
//Never inherit from this contract defintion class
[ContractClassFor(typeof(IMyInterface))]
internal abstract class MyInterfaceContract : IMyInterface
{
    private MyInterfaceContract() { }

    public string DoWork(string input)
    {
        Contract.Requires(!string.IsNullOrEmpty(input));
        Contract.Ensures(!string.IsNullOrEmpty(Contract.Result<string>()));
        throw new NotSupportedException();
    }
}
public class MyInterfaceImplmentation : IMyInterface
{
    public string DoWork(string input)
    {
        return input;
    }
}

```

**Static Analysis Result...**

[<img src="https://i.stack.imgur.com/eDxbs.png" alt="enter image description here" />](https://i.stack.imgur.com/eDxbs.png)



## Preconditions


Preconditions allows methods to provide minimum required values for input parameters

**Example...**

```dotnet
void DoWork(string input)
{
    Contract.Requires(!string.IsNullOrEmpty(input));

    //do work
}

```

**Static Analysis Result...**

[<img src="http://i.stack.imgur.com/ZFVU0.png" alt="enter image description here" />](http://i.stack.imgur.com/ZFVU0.png)



## Postconditions


Postconditions ensure that the returned results from a method will match the provided definition.  This provides the caller with a definition of the expected result.  Postconditions may allowed for simplied implmentations as some possible outcomes can be provided by the static analyizer.

**Example...**

```dotnet
string GetValue()
{
    Contract.Ensures(Contract.Result<string>() != null);

    return null;
}

```

**Static Analyis Result...**

[<img src="http://i.stack.imgur.com/gpCrS.png" alt="enter image description here" />](http://i.stack.imgur.com/gpCrS.png)



## Installing and Enabling Code Contracts


While `System.Diagnostics.Contracts` is included within the .Net Framework.  To use Code Contracts you must install the Visual Studio extensions.

Under `Extensions and Updates` search for `Code Contracts` then install the `Code Contracts Tools`

[<img src="http://i.stack.imgur.com/hTYJ1.png" alt="Code Contract Tools install" />](http://i.stack.imgur.com/hTYJ1.png)

After the tools are installed you must enable `Code Contracts` within your Project solution.  At the minimum you probably want to enable the `Static Checking` (check after build).  If you are implementing a library that will be used by other solutions you may want to consider also enabling `Runtime Checking`.

[<img src="http://i.stack.imgur.com/f4f1Z.png" alt="Project Settings" />](http://i.stack.imgur.com/f4f1Z.png)



#### Remarks


Code contracts allow for compile or runtime analysis of pre/post conditions of methods and invariant conditions for objects.  These conditions may be used to ensure callers and return value match valid states for application processing.  Other uses for Code Contracts include documentation generation.

