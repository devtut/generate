---
metaTitle: "PowerShell - Signing Scripts"
description: "Bypassing execution policy for a single script, Signing a script, Changing the execution policy using Set-ExecutionPolicy, Get the current execution policy, Getting the signature from a signed script, Creating a self-signed code signing certificate for testing"
---

# Signing Scripts




## Bypassing execution policy for a single script


Often you might need to execute an unsigned script that doesn't comply with the current execution policy. An easy way to do this is by bypassing the execution policy for that single process. Example:

```powershell
powershell.exe -ExecutionPolicy Bypass -File C:\MyUnsignedScript.ps1

```

Or you can use the shorthand:

```powershell
powershell -ep Bypass C:\MyUnsignedScript.ps1

```

### Other Execution Policies:

|Policy|Description
|---|---|---|---|---|---|---|---|---|---
|`AllSigned`|Only scripts signed by a trusted publisher can be run.
|`Bypass`|No restrictions; all Windows PowerShell scripts can be run.
|`Default`|Normally `RemoteSigned`, but is controlled via ActiveDirectory
|`RemoteSigned`|Downloaded scripts must be signed by a trusted publisher before they can be run.
|`Restricted`|No scripts can be run. Windows PowerShell can be used only in interactive mode.
|`Undefined`|NA
|`Unrestricted`*|Similar to `bypass`

****`Unrestricted*` Caveat:** If you run an unsigned script that was downloaded from the Internet, you are prompted for permission before it runs.**

More Information available [here](https://blog.netspi.com/15-ways-to-bypass-the-powershell-execution-policy/).



## Signing a script


Signing a script is done by using the `Set-AuthenticodeSignature`-cmdlet and a code-signing certificate.

```powershell
#Get the first available personal code-signing certificate for the logged on user
$cert = @(Get-ChildItem -Path Cert:\CurrentUser\My -CodeSigningCert)[0]
    
#Sign script using certificate
Set-AuthenticodeSignature -Certificate $cert -FilePath c:\MyScript.ps1

```

You can also read a certificate from a `.pfx`-file using:

```powershell
$cert = Get-PfxCertificate -FilePath "C:\MyCodeSigningCert.pfx"

```

The script will be valid until the cetificate expires. If you use a timestamp-server during the signing, the script will continue to be valid after the certificate expires. It is also useful to add the trust chain for the certificate (including root authority) to help most computers trust the certificated used to sign the script.

```powershell
Set-AuthenticodeSignature -Certificate $cert -FilePath c:\MyScript.ps1 -IncludeChain All -TimeStampServer "http://timestamp.verisign.com/scripts/timstamp.dll"

```

It's recommended to use a timestamp-server from a trusted certificate provider like Verisign, Comodo, Thawte etc.



## Changing the execution policy using Set-ExecutionPolicy


To change the execution policy for the default scope (LocalMachine), use:

```powershell
Set-ExecutionPolicy AllSigned

```

To change the policy for a specific scope, use:

```powershell
Set-ExecutionPolicy -Scope CurrentUser -ExecutionPolicy AllSigned

```

You can suppress the prompts by adding the `-Force` switch.



## Get the current execution policy


Getting the effective execution policy for the current session:

```powershell
PS> Get-ExecutionPolicy
RemoteSigned

```

List all effective execution policies for the current session:

```powershell
PS> Get-ExecutionPolicy -List

        Scope ExecutionPolicy
        ----- ---------------
MachinePolicy       Undefined
   UserPolicy       Undefined
      Process       Undefined
  CurrentUser       Undefined
 LocalMachine    RemoteSigned

```

List the execution policy for a specific scope, ex. process:

```powershell
PS> Get-ExecutionPolicy -Scope Process
Undefined

```



## Getting the signature from a signed script


Get information about the Authenticode signature from a signed script by using the `Get-AuthenticodeSignature`-cmdlet:

```powershell
Get-AuthenticodeSignature .\MyScript.ps1 | Format-List *

```



## Creating a self-signed code signing certificate for testing


When signing personal scripts or when testing code signing it can be useful to create a self-signed code signing certificate.

Beginning with PowerShell 5.0 you can generate a self-signed code signing certificate by using the `New-SelfSignedCertificate`-cmdlet:

```powershell
New-SelfSignedCertificate -FriendlyName "StackOverflow Example Code Signing" -CertStoreLocation Cert:\CurrentUser\My -Subject "SO User" -Type CodeSigningCert

```

In earlier versions, you can create a self-signed certificate using the `makecert.exe` tool found in the .NET Framework SDK and Windows SDK.

A self-signed ceriticate will only be trusted by computers that have installed the certificate. For scripts that will be shared, a certificate from a trusted certificate authority (internal or trusted third-party) are recommended.



#### Remarks


Signing a script will make your scripts comply with all exeuction policies in PowerShell and ensure the integrity of a script. Signed scripts will fail to run if they have been modified after being signed.

Scripts signing requires a code signing certificate. Recommendations:

- Personal scripts/testing (not shared): Certificate from trusted certifiate authority (internal or third-party) **OR** a self-signed certificate.
- Shared inside organization: Certificate from trusted certifiate authority (internal or third-party)
- Shared outside organization: Certificate from trusted third party certifiate authority

Read more at [about_Signing @ TechNet](https://technet.microsoft.com/en-us/library/hh847874(v=wps.640).aspx)

### Execution policies

PowerShell has configurable execution policies that control which conditions are required for a script or configuration to be executed. An excecution policy can be set for multiple scopes; computer, current user and current process. **Execution policies can easily be bypassed and is not designed to restrict users, but rather protect them from violating signing policies unintentionally.**

The available policies are:

|Setting|Description
|---|---|---|---|---|---|---|---|---|---
|Restricted|No scripts allowed
|AllSigned|All scripts need to be signed
|RemoteSigned|All local scripts allowed; only signed remote scripts
|Unrestricted|No requirements. All scripts allowed, but will warn before running scripts downloaded from the internet
|Bypass|All scripts are allowed and no warnings are displayed
|Undefined|Remove the current execution policy for the current scope. Uses the parent policy. If all policies are undefined, restricted will be used.

You can modify the current execution policies using `Set-ExecutionPolicy`-cmdlet, Group Policy or the `-ExecutionPolicy` parameter when launching a `powershell.exe` process.

Read more at [about_Execution_Policies @ TechNet](https://technet.microsoft.com/en-us/library/hh847748(v=wps.620).aspx)

