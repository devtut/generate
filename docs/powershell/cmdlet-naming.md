---
metaTitle: "PowerShell - Cmdlet Naming"
description: "Verbs, Nouns"
---

# Cmdlet Naming


CmdLets should be named using a `<verb>-<noun>` naming scheme in order to improve discoverability.



## Verbs


Verbs used to name CmdLets should be named from verbs from the list supplied be `Get-Verb`

Further details on how to use verbs can be found at [Approved Verbs for Windows PowerShell](https://msdn.microsoft.com/en-us/library/ms714428(v=vs.85).aspx)



## Nouns


Nouns should always be singular.

Be consistent with the nouns.  For instance `Find-Package` needs a provider the noun is `PackageProvider` not `ProviderPackage`.

