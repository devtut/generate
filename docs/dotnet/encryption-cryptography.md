---
metaTitle: ".NET Framework - Encryption / Cryptography"
description: "RijndaelManaged, Encrypt and decrypt data using AES (in C#), Create a Key from a Password / Random SALT (in C#), Encryption and Decryption using Cryptography (AES)"
---

# Encryption / Cryptography




## RijndaelManaged


Required Namespace: `System.Security.Cryptography`

```dotnet
private class Encryption {
    
          private const string SecretKey = "topSecretKeyusedforEncryptions";
    
          private const string SecretIv = "secretVectorHere";
    
          public string Encrypt(string data) {
            return string.IsNullOrEmpty(data) ? data : Convert.ToBase64String(this.EncryptStringToBytesAes(data, this.GetCryptographyKey(), this.GetCryptographyIv()));
          }
    
          public string Decrypt(string data) {
            return string.IsNullOrEmpty(data) ? data : this.DecryptStringFromBytesAes(Convert.FromBase64String(data), this.GetCryptographyKey(), this.GetCryptographyIv());
          }
    
          private byte[] GetCryptographyKey() {
            return Encoding.ASCII.GetBytes(SecretKey.Replace('e', '!'));
          }
    
          private byte[] GetCryptographyIv() {
            return Encoding.ASCII.GetBytes(SecretIv.Replace('r', '!'));
          }
    
          private byte[] EncryptStringToBytesAes(string plainText, byte[] key, byte[] iv) {
            MemoryStream encrypt;
            RijndaelManaged aesAlg = null;
            try {
              aesAlg = new RijndaelManaged {
                Key = key,
                IV = iv
              };
              var encryptor = aesAlg.CreateEncryptor(aesAlg.Key, aesAlg.IV);
              encrypt = new MemoryStream();
              using (var csEncrypt = new CryptoStream(encrypt, encryptor, CryptoStreamMode.Write)) {
                using (var swEncrypt = new StreamWriter(csEncrypt)) {
                  swEncrypt.Write(plainText);
                }
              }
            } finally {
              aesAlg?.Clear();
            }
            return encrypt.ToArray();
          }
    
          private string DecryptStringFromBytesAes(byte[] cipherText, byte[] key, byte[] iv) {
            RijndaelManaged aesAlg = null;
            string plaintext;
            try {
              aesAlg = new RijndaelManaged {
                Key = key,
                IV = iv
              };
              var decryptor = aesAlg.CreateDecryptor(aesAlg.Key, aesAlg.IV);
              using (var msDecrypt = new MemoryStream(cipherText)) {
                using (var csDecrypt = new CryptoStream(msDecrypt, decryptor, CryptoStreamMode.Read)) {
                  using (var srDecrypt = new StreamReader(csDecrypt))
                    plaintext = srDecrypt.ReadToEnd();
                }
              }
            } finally {
              aesAlg?.Clear();
            }
            return plaintext;
          }
        }

```

**Usage**

```dotnet
var textToEncrypt = "hello World";

 var encrypted = new Encryption().Encrypt(textToEncrypt); //-> zBmW+FUxOvdbpOGm9Ss/vQ==

 var decrypted = new Encryption().Decrypt(encrypted); //-> hello World

```

**Note:**

- Rijndael is the predecessor of the standard symmetric cryptographic algorithm AES.



## Encrypt and decrypt data using AES (in C#)


```dotnet
using System;
using System.IO;
using System.Security.Cryptography;

namespace Aes_Example
{
    class AesExample
    {
        public static void Main()
        {
            try
            {
                string original = "Here is some data to encrypt!";

                // Create a new instance of the Aes class.
                // This generates a new key and initialization vector (IV).
                using (Aes myAes = Aes.Create())
                {
                    // Encrypt the string to an array of bytes.
                    byte[] encrypted = EncryptStringToBytes_Aes(original, 
                                                                myAes.Key,
                                                                myAes.IV);

                    // Decrypt the bytes to a string.
                    string roundtrip = DecryptStringFromBytes_Aes(encrypted, 
                                                                  myAes.Key, 
                                                                  myAes.IV);

                    //Display the original data and the decrypted data.
                    Console.WriteLine("Original:   {0}", original);
                    Console.WriteLine("Round Trip: {0}", roundtrip);
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("Error: {0}", e.Message);
            }
        }

        static byte[] EncryptStringToBytes_Aes(string plainText, byte[] Key, byte[] IV)
        {
            // Check arguments.
            if (plainText == null || plainText.Length <= 0)
                throw new ArgumentNullException("plainText");
            if (Key == null || Key.Length <= 0)
                throw new ArgumentNullException("Key");
            if (IV == null || IV.Length <= 0)
                throw new ArgumentNullException("IV");

            byte[] encrypted;

            // Create an Aes object with the specified key and IV.
            using (Aes aesAlg = Aes.Create())
            {
                aesAlg.Key = Key;
                aesAlg.IV = IV;

                // Create a decrytor to perform the stream transform.
                ICryptoTransform encryptor = aesAlg.CreateEncryptor(aesAlg.Key,
                                                                    aesAlg.IV);

                // Create the streams used for encryption.
                using (MemoryStream msEncrypt = new MemoryStream())
                {
                    using (CryptoStream csEncrypt = new CryptoStream(msEncrypt,
                                                                     encryptor,
                                                                     CryptoStreamMode.Write))
                    {
                        using (StreamWriter swEncrypt = new StreamWriter(csEncrypt))
                        {
                            //Write all data to the stream.
                            swEncrypt.Write(plainText);
                        }

                        encrypted = msEncrypt.ToArray();
                    }
                }
            }

            // Return the encrypted bytes from the memory stream.
            return encrypted;
        }

        static string DecryptStringFromBytes_Aes(byte[] cipherText, byte[] Key, byte[] IV)
        {
            // Check arguments.
            if (cipherText == null || cipherText.Length <= 0)
                throw new ArgumentNullException("cipherText");
            if (Key == null || Key.Length <= 0)
                throw new ArgumentNullException("Key");
            if (IV == null || IV.Length <= 0)
                throw new ArgumentNullException("IV");

            // Declare the string used to hold the decrypted text.
            string plaintext = null;

            // Create an Aes object with the specified key and IV.
            using (Aes aesAlg = Aes.Create())
            {
                aesAlg.Key = Key;
                aesAlg.IV = IV;

                // Create a decrytor to perform the stream transform.
                ICryptoTransform decryptor = aesAlg.CreateDecryptor(aesAlg.Key,
                                                                    aesAlg.IV);

                // Create the streams used for decryption.
                using (MemoryStream msDecrypt = new MemoryStream(cipherText))
                {
                    using (CryptoStream csDecrypt = new CryptoStream(msDecrypt,
                                                                     decryptor,
                                                                     CryptoStreamMode.Read))
                    {
                        using (StreamReader srDecrypt = new StreamReader(csDecrypt))
                        {

                            // Read the decrypted bytes from the decrypting stream
                            // and place them in a string.
                            plaintext = srDecrypt.ReadToEnd();
                        }
                    }
                }
            }

            return plaintext;
        }
    }
}

```

This example is from [MSDN](https://msdn.microsoft.com/en-us/library/system.security.cryptography.aes(v=vs.110).aspx).

It is a console demo application, showing how to encrypt a string by using the standard **AES** encryption, and how to decrypt it afterwards.

(**[AES = Advanced Encryption Standard](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard),** a specification for the encryption of electronic data established by the U.S. National Institute of Standards and Technology (NIST) in 2001 which is still the de-facto standard for symmetric encryption)

**Notes:**

<li>
In a real encryption scenario, you need to choose a proper cipher mode (can be assigned to the `Mode` property by selecting a value from the `CipherMode` enumeration). **Never** use the `CipherMode.ECB` (electronic codebook mode), since this procuces a weak cypher stream
</li>
<li>
To create a good (and not a weak) `Key`, either use a cryptographic random generator or use the example above (**Create a Key from a Password**). The recommended **KeySize** is 256 bit. Supported key sizes are available via the `LegalKeySizes` property.
</li>
<li>
To initialize the initialization vector `IV`, you can use a SALT as shown in the example above (**Random SALT**)
</li>
<li>
Supported block sizes are available via the `SupportedBlockSizes` property, the block size can be assigned via the `BlockSize` property
</li>

**Usage:** see Main() method.



## Create a Key from a Password / Random SALT (in C#)


```dotnet
using System;
using System.Security.Cryptography;
using System.Text;

public class PasswordDerivedBytesExample
{
    public static void Main(String[] args)
    {
        // Get a password from the user.
        Console.WriteLine("Enter a password to produce a key:");

        byte[] pwd = Encoding.Unicode.GetBytes(Console.ReadLine());

        byte[] salt = CreateRandomSalt(7);

        // Create a TripleDESCryptoServiceProvider object.
        TripleDESCryptoServiceProvider tdes = new TripleDESCryptoServiceProvider();

        try
        {
            Console.WriteLine("Creating a key with PasswordDeriveBytes...");

            // Create a PasswordDeriveBytes object and then create
            // a TripleDES key from the password and salt.
            PasswordDeriveBytes pdb = new PasswordDeriveBytes(pwd, salt);

            // Create the key and set it to the Key property
            // of the TripleDESCryptoServiceProvider object.
            tdes.Key = pdb.CryptDeriveKey("TripleDES", "SHA1", 192, tdes.IV);

            Console.WriteLine("Operation complete.");
        }
        catch (Exception e)
        {
            Console.WriteLine(e.Message);
        }
        finally
        {
            // Clear the buffers
            ClearBytes(pwd);
            ClearBytes(salt);

            // Clear the key.
            tdes.Clear();
        }

        Console.ReadLine();
    }

    #region Helper methods

    /// <summary>
    /// Generates a random salt value of the specified length.
    /// </summary>
    public static byte[] CreateRandomSalt(int length)
    {
        // Create a buffer
        byte[] randBytes;

        if (length >= 1)
        {
            randBytes = new byte[length];
        }
        else
        {
            randBytes = new byte[1];
        }

        // Create a new RNGCryptoServiceProvider.
        RNGCryptoServiceProvider rand = new RNGCryptoServiceProvider();

        // Fill the buffer with random bytes.
        rand.GetBytes(randBytes);

        // return the bytes.
        return randBytes;
    }

    /// <summary>
    /// Clear the bytes in a buffer so they can't later be read from memory.
    /// </summary>
    public static void ClearBytes(byte[] buffer)
    {
        // Check arguments.
        if (buffer == null)
        {
            throw new ArgumentNullException("buffer");
        }

        // Set each byte in the buffer to 0.
        for (int x = 0; x < buffer.Length; x++)
        {
            buffer[x] = 0;
        }
    }

    #endregion
}

```

This example is taken from [MSDN.](https://msdn.microsoft.com/en-us/library/system.security.cryptography.passwordderivebytes(v=vs.110).aspx)

It is a console demo, and it shows how to create a secure key based on a user-defined password, and how to create a random SALT based on the cryptographic random generator.

**Notes:**

<li>
The built-in function `PasswordDeriveBytes` uses the standard PBKDF1 algorithm to generate a key from the password. Per default, it uses 100 iterations to generate the key to slow down brute force attacks. The SALT generated randomly further strenghens the key.
</li>
<li>
The function `CryptDeriveKey` converts the key generated by `PasswordDeriveBytes` into a key compatible with the specified encryption algorithm (here "TripleDES") by using the specified hash algorithm (here "SHA1"). The keysize in this example is 192 bytes, and the initialization vector IV is taken from the triple-DES crypto provider
</li>
<li>
Usually, this mechanism is used to protect a stronger random generated key by a password, which encrypts large amount of data. You can also use it to provide multiple passwords of different users to give access to the same data (being protected by a different random key).
</li>
<li>
Unfortunately, `CryptDeriveKey` does currently not support AES. See [here.](https://social.msdn.microsoft.com/Forums/vstudio/en-US/61d85001-2eae-4419-b4bf-ce98d46f4d21/passwordderivebytescryptderivekey-derives-an-aes-key-but-gets-object-identifier-oid-is-unknown?forum=netfxbcl) <br/> **NOTE:** As a workaround, you can create a random AES key for encryption of the data to be protected with AES and store the AES key in a TripleDES-Container which uses the key generated by `CryptDeriveKey`. But that limits the security to TripleDES, does not take advantage of the larger keysizes of AES and creates a dependency to TripleDES.
</li>

**Usage:** See Main() method.



## Encryption and Decryption using Cryptography (AES)


Decryption Code

```

   public static string Decrypt(string cipherText)
    {
        if (cipherText == null)
            return null;

        byte[] cipherBytes = Convert.FromBase64String(cipherText);
        using (Aes encryptor = Aes.Create())
        {
            Rfc2898DeriveBytes pdb = new Rfc2898DeriveBytes(CryptKey, new byte[] { 0x49, 0x76, 0x61, 0x6e, 0x20, 0x4d, 0x65, 0x64, 0x76, 0x65, 0x64, 0x65, 0x76 });
            encryptor.Key = pdb.GetBytes(32);
            encryptor.IV = pdb.GetBytes(16);

            using (MemoryStream ms = new MemoryStream())
            {
                using (CryptoStream cs = new CryptoStream(ms, encryptor.CreateDecryptor(), CryptoStreamMode.Write))
                {
                    cs.Write(cipherBytes, 0, cipherBytes.Length);
                    cs.Close();
                }

                cipherText = Encoding.Unicode.GetString(ms.ToArray());
            }
        }
    
        return cipherText;
    }

```

Encryption Code

```

   public static string Encrypt(string cipherText)
    {
        if (cipherText == null)
            return null;
      
        byte[] clearBytes = Encoding.Unicode.GetBytes(cipherText);
        using (Aes encryptor = Aes.Create())
        {
            Rfc2898DeriveBytes pdb = new Rfc2898DeriveBytes(CryptKey, new byte[] { 0x49, 0x76, 0x61, 0x6e, 0x20, 0x4d, 0x65, 0x64, 0x76, 0x65, 0x64, 0x65, 0x76 });
            encryptor.Key = pdb.GetBytes(32);
            encryptor.IV = pdb.GetBytes(16);

            using (MemoryStream ms = new MemoryStream())
            {
                using (CryptoStream cs = new CryptoStream(ms, encryptor.CreateEncryptor(), CryptoStreamMode.Write))
                {
                    cs.Write(clearBytes, 0, clearBytes.Length);
                    cs.Close();
                }

                cipherText = Convert.ToBase64String(ms.ToArray());
            }
        }
        return cipherText;
    }

```

Usage

```dotnet
var textToEncrypt = "TestEncrypt";

var encrypted = Encrypt(textToEncrypt);

var decrypted = Decrypt(encrypted);

```



#### Remarks


.NET Framework provides implementation of many cryptographic algorithms. They include basically symmetric algorithms, asymmetric algorithms and hashes.

