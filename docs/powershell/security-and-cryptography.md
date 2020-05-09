---
metaTitle: "PowerShell - Security and Cryptography"
description: "Calculating a  string's hash codes via .Net Cryptography"
---

# Security and Cryptography



## Calculating a  string's hash codes via .Net Cryptography


Utilizing .Net `System.Security.Cryptography.HashAlgorithm` namespace to generate the message hash code with the algorithms  supported.

```powershell
$example="Nobody expects the Spanish Inquisition."

#calculate
$hash=[System.Security.Cryptography.HashAlgorithm]::Create("sha256").ComputeHash(
[System.Text.Encoding]::UTF8.GetBytes($example))

#convert to hex
[System.BitConverter]::ToString($hash) 

#2E-DF-DA-DA-56-52-5B-12-90-FF-16-FB-17-44-CF-B4-82-DD-29-14-FF-BC-B6-49-79-0C-0E-58-9E-46-2D-3D

```

The  `"sha256"` part was the hash algorithm used.

the `-` can be removed or change to lower case

```powershell
#convert to lower case hex without '-' 
[System.BitConverter]::ToString($hash).Replace("-","").ToLower()

#2edfdada56525b1290ff16fb1744cfb482dd2914ffbcb649790c0e589e462d3d

```

If base64 format was preferred,  using base64 converter for output

```powershell
#convert to base64
[Convert]::ToBase64String($hash)

#Lt/a2lZSWxKQ/xb7F0TPtILdKRT/vLZJeQwOWJ5GLT0=

```

