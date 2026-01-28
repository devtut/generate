---
metaTitle: "Microsoft SQL Server - Encryption"
description: "Encryption by certificate, Encryption of database, Encryption by symmetric key, Encryption by passphrase"
---

# Encryption



## Encryption by certificate


```sql
CREATE CERTIFICATE My_New_Cert
FROM FILE = 'D:\Temp\CertTest\certificateDER.cer'
GO

```

Create the certificate

```sql
SELECT EncryptByCert(Cert_ID('My_New_Cert'), 
'This text will get encrypted') encryption_test

```

Usually, you would encrypt with a symmetric key, that key would get encrypted by the asymmetric key (public key) from your certificate.

Also, note that encryption is limited to certain lengths depending on key length and returns NULL otherwise. Microsoft writes:
"The limits are: a 512 bit RSA key can encrypt up to 53 bytes, a 1024 bit key can encrypt up to 117 bytes, and a 2048 bit key can encrypt up to 245 bytes."

EncryptByAsymKey has the same limits.
For UNICODE this would be divided by 2 (16 bits per character), so 58 characters for a 1024 bit key.



## Encryption of database


```sql
USE TDE
CREATE DATABASE ENCRYPTION KEY
WITH ALGORITHM = AES_256
ENCRYPTION BY SERVER CERTIFICATE My_New_Cert
GO

ALTER DATABASE TDE
SET ENCRYPTION ON
GO

```

This uses 'Transparent Data Encryption' (TDE)



## Encryption by symmetric key


```sql
-- Create the key and protect it with the cert
CREATE SYMMETRIC KEY My_Sym_Key
WITH ALGORITHM = AES_256  
ENCRYPTION BY CERTIFICATE My_New_Cert;
GO

-- open the key
OPEN SYMMETRIC KEY My_Sym_Key
DECRYPTION BY CERTIFICATE My_New_Cert;

-- Encrypt
SELECT EncryptByKey(Key_GUID('SSN_Key_01'), 'This text will get encrypted');

```



## Encryption by passphrase


```sql
SELECT EncryptByPassphrase('MyPassPhrase', 'This text will get encrypted')

```

This will also encrypt but then by passphrase instead of asymmetric(certificate) key or by an explicit symmetric key.



#### Parameters


|Optional Parameters|Details
|---|---|---|---
|`WITH PRIVATE KEY`|For CREATE CERTIFICATE, a private key can be specified: `(FILE='D:\Temp\CertTest\private.pvk', DECRYPTION BY PASSWORD = 'password');`



#### Remarks


Creation of a DER certificate will work fine. When a Base64 certificate is used however, SQL server will complain with the cryptic message:

```sql
Msg 15468, Level 16, State 6, Line 1
An error occurred during the generation of the certificate.

```

Import your Base64 certificate to your OS's certificate store to be able to re-export it into DER binary format.

Another important thing to do is having an Encryption Hierarchy so that one protects the other, all the way to OS level. See the article on 'Encryption of database/TDE'

For more information for creation of certificates go to:
[https://msdn.microsoft.com/en-us/library/ms187798.aspx](https://msdn.microsoft.com/en-us/library/ms187798.aspx)

For more information for encryption of database/TDE go to:
[https://msdn.microsoft.com/en-us/library/bb934049.aspx](https://msdn.microsoft.com/en-us/library/bb934049.aspx)

For more information for encryption of data go to:
[https://msdn.microsoft.com/en-us/library/ms188061.aspx](https://msdn.microsoft.com/en-us/library/ms188061.aspx)

