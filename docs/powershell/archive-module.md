---
metaTitle: "PowerShell - Archive Module"
description: "Compress-Archive with wildcard, Update existing ZIP with Compress-Archive, Extract a Zip with Expand-Archive"
---

# Archive Module


The Archive module `Microsoft.PowerShell.Archive` provides functions for storing files in ZIP archives (`Compress-Archive`) and extracting them (`Expand-Archive`). This module is available in PowerShell 5.0 and above.

In earlier versions of PowerShell the [Community Extensions](http://pscx.codeplex.com/) or .NET [System.IO.Compression.FileSystem](http://stackoverflow.com/a/20070550/559306) could be used.



## Compress-Archive with wildcard


```powershell
Compress-Archive -Path C:\Documents\* -CompressionLevel Optimal -DestinationPath C:\Archives\Documents.zip

```

This command:

- Compresses all files in `C:\Documents`
- Uses `Optimal` compression
<li>Save the resulting archive in `C:\Archives\Documents.zip`
<ul>
- `-DestinationPath` will add `.zip`if not present.
- `-LiteralPath` can be used if you require naming it without `.zip`.



## Update existing ZIP with Compress-Archive


```powershell
Compress-Archive -Path C:\Documents\* -Update -DestinationPath C:\Archives\Documents.zip

```


- this will add or replace all files `Documents.zip` with the new ones from `C:\Documents`



## Extract a Zip with Expand-Archive


```powershell
Expand-Archive -Path C:\Archives\Documents.zip -DestinationPath C:\Documents

```


- this will extract all files from `Documents.zip` into the folder `C:\Documents`



#### Syntax


- **Expand-Archive** / **Compress-Archive**
<li>**-Path**
<ul>
<li>the path of the file(s) to compress (Compress-Archive) or
the path of the archive to extract the file(s) form (Expand-Archive)</li>
- there are several other Path related options, please see below.

- if you do not supply this path, the archive will be created in the current working directory (Compress-Archive) or the contents of the archive will be extracted into the current working directory (Expand-Archive)



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|CompressionLevel|**(Compress-Archive only)** Set compression level to either `Fastest`, `Optimal` or `NoCompression`
|Confirm|Prompts for confirmation before running
|Force|Forces the command to run without confirmation
|LiteralPath|Path that is used literaly, **no wildcards supported**,  use `,` to specify multiple paths
|Path|Path that can contain wildcards, use `,` to specify multiple paths
|Update|**(Compress-Archive only)** Update existing archive
|WhatIf|Simulate the command



#### Remarks


See [MSDN Microsoft.PowerShell.Archive (5.1)](https://msdn.microsoft.com/en-us/powershell/reference/5.1/microsoft.powershell.archive/microsoft.powershell.archive) for further reference

