---
metaTitle: "PowerShell - Handling Secrets and Credentials"
description: "Accessing the Plaintext Password, Prompting for Credentials, Working with Stored Credentials, Storing the credentials in Encrypted form and Passing it as parameter when Required"
---

# Handling Secrets and Credentials


In Powershell, to avoid storing the password in **clear text** we use different methods of encryption and store it as secure string. When you are not specifying a key or securekey, this will only work for the same user on the same computer will be able to decrypt the encrypted string if you’re not using Keys/SecureKeys. Any process that runs under that same user account will be able to decrypt that encrypted string on that same machine.



## Accessing the Plaintext Password


The password in a credential object is an encrypted `[SecureString]`. The most straightforward way is to get a `[NetworkCredential]` which does not store the password encrypted:

```powershell
$credential = Get-Credential
$plainPass = $credential.GetNetworkCredential().Password

```

The helper method (`.GetNetworkCredential()`) only exists on `[PSCredential]` objects.<br> To directly deal with a `[SecureString]`, use .NET methods:

```powershell
$bstr = [System.Runtime.InteropServices.Marshal]::SecureStringToBSTR($secStr)
$plainPass = [System.Runtime.InteropServices.Marshal]::PtrToStringAuto($bstr)

```



## Prompting for Credentials


To prompt for credentials, you should almost always use the [`Get-Credential`](https://technet.microsoft.com/en-us/library/hh849815.aspx) cmdlet:

```powershell
$credential = Get-Credential

```

Pre-filled user name:

```powershell
$credential = Get-Credential -UserName 'myUser'

```

Add a custom prompt message:

```powershell
$credential = Get-Credential -Message 'Please enter your company email address and password.'

```



## Working with Stored Credentials


To store and retrieve encrypted credentials easily, use PowerShell's built-in XML serialization (Clixml):

```powershell
$credential = Get-Credential

$credential | Export-CliXml -Path 'C:\My\Path\cred.xml'

```

To re-import:

```powershell
$credential = Import-CliXml -Path 'C:\My\Path\cred.xml'

```

The important thing to remember is that by default this uses the Windows data protection API, and the key used to encrypt the password is specific to both the **user and the machine** that the code is running under.

**As a result, the encrypted credential cannot be imported by a different user nor the same user on a different computer.**

By encrypting several versions of the same credential with different running users and on different computers, you can have the same secret available to multiple users.

By putting the user and computer name in the file name, you can store all of the encrypted secrets in a way that allows for the same code to use them without hard coding anything:

### Encrypter

```powershell
# run as each user, and on each computer

$credential = Get-Credential

$credential | Export-CliXml -Path "C:\My\Secrets\myCred_${env:USERNAME}_${env:COMPUTERNAME}.xml"

```

### The code that uses the stored credentials:

```powershell
$credential = Import-CliXml -Path "C:\My\Secrets\myCred_${env:USERNAME}_${env:COMPUTERNAME}.xml"

```

The correct version of the file for the running user will be loaded automatically (or it will fail because the file doesn't exist).



## Storing the credentials in Encrypted form and Passing it as parameter when Required


```powershell
$username = "user1@domain.com"
$pwdTxt = Get-Content "C:\temp\Stored_Password.txt"
$securePwd = $pwdTxt | ConvertTo-SecureString 
$credObject = New-Object System.Management.Automation.PSCredential -ArgumentList $username, $securePwd
# Now, $credObject is having the credentials stored and you can pass it wherever you want.


## Import Password with AES

$username = "user1@domain.com"
$AESKey = Get-Content $AESKeyFilePath
$pwdTxt = Get-Content $SecurePwdFilePath
$securePwd = $pwdTxt | ConvertTo-SecureString -Key $AESKey
$credObject = New-Object System.Management.Automation.PSCredential -ArgumentList $username, $securePwd

# Now, $credObject is having the credentials stored with AES Key and you can pass it wherever you want.

```

