---
metaTitle: "PowerShell - Creating DSC Class-Based Resources"
description: "Create a DSC Resource Skeleton Class, DSC Resource Skeleton with Key Property, DSC Resource with Mandatory Property, DSC Resource with Required Methods"
---

# Creating DSC Class-Based Resources


Starting with PowerShell version 5.0, you can use PowerShell class definitions to create Desired State Configuration (DSC) Resources.

To aid in building DSC Resource, there's a `[DscResource()]` attribute that's applied to the class definition, and a `[DscProperty()]` resource to designate properties as configurable by the DSC Resource user.



## Create a DSC Resource Skeleton Class


```powershell
[DscResource()]
class File {
}

```

This example demonstrates how to build the outer section of a PowerShell class, that declares a DSC Resource. You still need to fill in the contents of the class definition.



## DSC Resource Skeleton with Key Property


```powershell
[DscResource()]
class Ticket {
  [DscProperty(Key)]
  [string] $TicketId
}

```

A DSC Resource must declare at least one key property. The key property is what uniquely identifies the resource from other resources. For example, let's say that you're building a DSC Resource that represents a ticket in a ticketing system. Each ticket would be uniquely represented with a ticket ID.

Each property that will be exposed to the **user** of the DSC Resource must be decorated with the `[DscProperty()]` attribute. This attributes accepts a `key` parameter, to indicate that the property is a key attribute for the DSC Resource.



## DSC Resource with Mandatory Property


```powershell
[DscResource()]
class Ticket {
  [DscProperty(Key)]
  [string] $TicketId

  [DscProperty(Mandatory)]
  [string] $Subject
}

```

When building a DSC Resource, you'll often find that not every single property should be mandatory. However, there are some core properties that you'll want to ensure are configured by the user of the DSC Resource. You use the `Mandatory` parameter of the `[DscResource()]` attribute to declare a property as required by the DSC Resource's user.

In the example above, we've added a `Subject` property to a `Ticket` resource, that represents a unique ticket in a ticketing system, and designated it as a `Mandatory` property.



## DSC Resource with Required Methods


```powershell
[DscResource()]
class Ticket {
  [DscProperty(Key)]
  [string] $TicketId

  # The subject line of the ticket
  [DscProperty(Mandatory)]
  [string] $Subject

  # Get / Set if ticket should be open or closed
  [DscProperty(Mandatory)]
  [string] $TicketState

  [void] Set() {
    # Create or update the resource
  }

  [Ticket] Get() {
    # Return the resource's current state as an object
    $TicketState = [Ticket]::new()
    return $TicketState
  }

  [bool] Test() {
    # Return $true if desired state is met
    # Return $false if desired state is not met
    return $false
  }
}

```

This is a complete DSC Resource that demonstrates all of the core requirements to build a valid resource. The method implementations are not complete, but are provided with the intention of showing the basic structure.



#### Remarks


A class-based DSC Resource must:

- Be decorated with the `[DscResource()]` attribute
- Define a `Test()` method that returns `[bool]`
- Define a `Get()` method that returns its own object type (eg. `[Ticket]`)
- Define a `Set()` method that returns `[void]`
- At least one `Key` DSC Property

After creating a class-based PowerShell DSC Resource, it must be "exported" from a module, using a module manifest (.psd1) file. Within the module manifest, the `DscResourcesToExport` hashtable key is used to declare an array of DSC Resources (class names) to "export" from the module. This enables consumers of the DSC module to "see" the class-based resources inside the module.

