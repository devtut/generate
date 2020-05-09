---
metaTitle: "PowerShell - Embedding Managed Code (C# | VB)"
description: "C# Example, VB.NET Example"
---

# Embedding Managed Code (C# | VB)


This topic is to briefly describe how C# or VB .NET Managed code can be scripted and utilised within a PowerShell script. This topic is not exploring all facets of the Add-Type cmdlet.

For more information on the Add-Type cmdlet, please refer to the MSDN documentation (for 5.1) here: [https://msdn.microsoft.com/en-us/powershell/reference/5.1/microsoft.powershell.utility/add-type](https://msdn.microsoft.com/en-us/powershell/reference/5.1/microsoft.powershell.utility/add-type)



## C# Example


This example shows how to embed some basic C# into a PowerShell script, add it to the runspace/session and utilise the code within PowerShell syntax.

```powershell
$code = "
using System;

namespace MyNameSpace
{
    public class Responder
    {
        public static void StaticRespond()
        {
            Console.WriteLine("Static Response");
        }

        public void Respond()
        {
            Console.WriteLine("Instance Respond");
        }
    }
}
"@

# Check the type has not been previously added within the session, otherwise an exception is raised
if (-not ([System.Management.Automation.PSTypeName]'MyNameSpace.Responder').Type)
{
    Add-Type -TypeDefinition $code -Language CSharp;
}

[MyNameSpace.Responder]::StaticRespond();

$instance = New-Object MyNameSpace.Responder;
$instance.Respond();

```



## VB.NET Example


This example shows how to embed some basic C# into a PowerShell script, add it to the runspace/session and utilise the code within PowerShell syntax.

```powershell
$code = @"
Imports System

Namespace MyNameSpace
    Public Class Responder
        Public Shared Sub StaticRespond()
            Console.WriteLine("Static Response")
        End Sub

        Public Sub Respond()
            Console.WriteLine("Instance Respond")
        End Sub
    End Class
End Namespace
"@

# Check the type has not been previously added within the session, otherwise an exception is raised
if (-not ([System.Management.Automation.PSTypeName]'MyNameSpace.Responder').Type)
{
    Add-Type -TypeDefinition $code -Language VisualBasic;
}

[MyNameSpace.Responder]::StaticRespond();

$instance = New-Object MyNameSpace.Responder;
$instance.Respond();

```



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|-TypeDefinition<String_>|Accepts the code as a string
|-Language<String_>|Specifies the Managed Code language.Accepted values: CSharp, CSharpVersion3, CSharpVersion2, VisualBasic, JScript



#### Remarks


### Removing Added types

In later versions of PowerShell, Remove-TypeData has been added to the PowerShell cmdlet libraries which can allow for removal of a type within a session. For more details on this cmdlet, go here: [https://msdn.microsoft.com/en-us/powershell/reference/4.0/microsoft.powershell.utility/remove-typedata](https://msdn.microsoft.com/en-us/powershell/reference/4.0/microsoft.powershell.utility/remove-typedata)

### CSharp and .NET syntax

For those experience with .NET it goes without saying that the differing versions of C# can be quite radically different in their level of support for certain syntax.

If utilising Powershell 1.0 and/or -Language CSharp, the managed code will be utilising .NET 2.0 which is lacking in a number of features which C# developers typically use without a second thought these days, such as Generics, Linq and Lambda. On top of this is formal polymorphism, which is handled with defaulted parameters in later versions of C#/.NET.

