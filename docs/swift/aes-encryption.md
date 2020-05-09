---
metaTitle: "Swift - AES encryption"
description: "AES encryption in CBC mode with a random IV (Swift 3.0), AES encryption in CBC mode with a random IV (Swift 2.3), AES encryption in ECB mode with PKCS7 padding"
---

# AES encryption



## AES encryption in CBC mode with a random IV (Swift 3.0)


The iv is prefixed to the encrypted data

`aesCBC128Encrypt` will create a random IV and prefixed to the encrypted code.<br />
`aesCBC128Decrypt` will use the prefixed IV during decryption.

Inputs are the data and key are Data objects. If an encoded form such as Base64 if required convert to and/or from in the calling method.

The key should be exactly 128-bits (16-bytes), 192-bits (24-bytes) or 256-bits (32-bytes) in length. If another key size is used an error will be thrown.

[PKCS#7 padding](https://en.wikipedia.org/wiki/Padding_(cryptography)#PKCS7) is set by default.

This example requires Common Crypto<br />
It is necessary to have a bridging header to the project:<br />
`#import <CommonCrypto/CommonCrypto.h>`<br />
Add the `Security.framework` to the project.

This is example, not production code.

```swift
enum AESError: Error {
    case KeyError((String, Int))
    case IVError((String, Int))
    case CryptorError((String, Int))
}

// The iv is prefixed to the encrypted data
func aesCBCEncrypt(data:Data, keyData:Data) throws -> Data {
    let keyLength = keyData.count
    let validKeyLengths = [kCCKeySizeAES128, kCCKeySizeAES192, kCCKeySizeAES256]
    if (validKeyLengths.contains(keyLength) == false) {
        throw AESError.KeyError(("Invalid key length", keyLength))
    }

    let ivSize = kCCBlockSizeAES128;
    let cryptLength = size_t(ivSize + data.count + kCCBlockSizeAES128)
    var cryptData = Data(count:cryptLength)

    let status = cryptData.withUnsafeMutableBytes {ivBytes in
        SecRandomCopyBytes(kSecRandomDefault, kCCBlockSizeAES128, ivBytes)
    }
    if (status != 0) {
        throw AESError.IVError(("IV generation failed", Int(status)))
    }

    var numBytesEncrypted :size_t = 0
    let options   = CCOptions(kCCOptionPKCS7Padding)

    let cryptStatus = cryptData.withUnsafeMutableBytes {cryptBytes in
        data.withUnsafeBytes {dataBytes in
            keyData.withUnsafeBytes {keyBytes in
                CCCrypt(CCOperation(kCCEncrypt),
                        CCAlgorithm(kCCAlgorithmAES),
                        options,
                        keyBytes, keyLength,
                        cryptBytes,
                        dataBytes, data.count,
                        cryptBytes+kCCBlockSizeAES128, cryptLength,
                        &numBytesEncrypted)
            }
        }
    }

    if UInt32(cryptStatus) == UInt32(kCCSuccess) {
        cryptData.count = numBytesEncrypted + ivSize
    }
    else {
        throw AESError.CryptorError(("Encryption failed", Int(cryptStatus)))
    }

    return cryptData;
}

// The iv is prefixed to the encrypted data
func aesCBCDecrypt(data:Data, keyData:Data) throws -> Data? {
    let keyLength = keyData.count
    let validKeyLengths = [kCCKeySizeAES128, kCCKeySizeAES192, kCCKeySizeAES256]
    if (validKeyLengths.contains(keyLength) == false) {
        throw AESError.KeyError(("Invalid key length", keyLength))
    }

    let ivSize = kCCBlockSizeAES128;
    let clearLength = size_t(data.count - ivSize)
    var clearData = Data(count:clearLength)

    var numBytesDecrypted :size_t = 0
    let options   = CCOptions(kCCOptionPKCS7Padding)

    let cryptStatus = clearData.withUnsafeMutableBytes {cryptBytes in
        data.withUnsafeBytes {dataBytes in
            keyData.withUnsafeBytes {keyBytes in
                CCCrypt(CCOperation(kCCDecrypt),
                        CCAlgorithm(kCCAlgorithmAES128),
                        options,
                        keyBytes, keyLength,
                        dataBytes,
                        dataBytes+kCCBlockSizeAES128, clearLength,
                        cryptBytes, clearLength,
                        &numBytesDecrypted)
            }
        }
    }

    if UInt32(cryptStatus) == UInt32(kCCSuccess) {
        clearData.count = numBytesDecrypted
    }
    else {
        throw AESError.CryptorError(("Decryption failed", Int(cryptStatus)))
    }
    
    return clearData;
}

```

Example usage:

```swift
let clearData = "clearData0123456".data(using:String.Encoding.utf8)!
let keyData   = "keyData890123456".data(using:String.Encoding.utf8)!
print("clearData:   \(clearData as NSData)")
print("keyData:     \(keyData as NSData)")

var cryptData :Data?
do {
    cryptData = try aesCBCEncrypt(data:clearData, keyData:keyData)
    print("cryptData:   \(cryptData! as NSData)")
}
catch (let status) {
    print("Error aesCBCEncrypt: \(status)")
}

let decryptData :Data?
do {
    let decryptData = try aesCBCDecrypt(data:cryptData!, keyData:keyData)
    print("decryptData: \(decryptData! as NSData)")
}
catch (let status) {
    print("Error aesCBCDecrypt: \(status)")
}

```

Example Output:

```swift
clearData:   <636c6561 72446174 61303132 33343536>
keyData:     <6b657944 61746138 39303132 33343536>
cryptData:   <92c57393 f454d959 5a4d158f 6e1cd3e7 77986ee9 b2970f49 2bafcf1a 8ee9d51a bde49c31 d7780256 71837a61 60fa4be0>
decryptData: <636c6561 72446174 61303132 33343536>

```

Notes:<br />
One typical problem with CBC mode example code is that it leaves the creation and sharing of the random IV to the user. This example includes generation of the IV, prefixed the encrypted data and uses the prefixed IV during decryption. This frees the casual user from the details that are necessary for [CBC mode](https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation#Cipher_Block_Chaining_.28CBC.29).

For security the encrypted data also should have authentication, this example code does not provide that in order to be small and allow better interoperability for other platforms.

Also missing is key derivation of the key from a password, it is suggested that [PBKDF2](https://en.wikipedia.org/wiki/PBKDF2) be used is text passwords are used as keying material.

For robust production ready multi-platform encryption code see [RNCryptor](https://github.com/RNCryptor).

> 
Updated to use throw/catch and multiple key sizes based on the provided key.




## AES encryption in CBC mode with a random IV (Swift 2.3)


The iv is prefixed to the encrypted data

aesCBC128Encrypt will create a random IV and prefixed to the encrypted code.
aesCBC128Decrypt will use the prefixed IV during decryption.

Inputs are the data and key are Data objects. If an encoded form such as Base64 if required convert to and/or from in the calling method.

The key should be exactly 128-bits (16-bytes). For other key sizes see the Swift 3.0 example.

PKCS#7 padding is set by default.

This example requires Common Crypto
It is necessary to have a bridging header to the project:
#import <CommonCrypto/CommonCrypto.h>
Add the Security.framework to the project.

See Swift 3 example for notes.

This is example, not production code.

```swift
func aesCBC128Encrypt(data data:[UInt8], keyData:[UInt8]) -> [UInt8]? {
    let keyLength   = size_t(kCCKeySizeAES128)
    let ivLength    = size_t(kCCBlockSizeAES128)
    let cryptDataLength = size_t(data.count + kCCBlockSizeAES128)
    var cryptData = [UInt8](count:ivLength + cryptDataLength, repeatedValue:0)

    let status = SecRandomCopyBytes(kSecRandomDefault, Int(ivLength), UnsafeMutablePointer<UInt8>(cryptData));
    if (status != 0) {
        print("IV Error, errno: \(status)")
        return nil
    }

    var numBytesEncrypted :size_t = 0
    let cryptStatus = CCCrypt(CCOperation(kCCEncrypt),
                              CCAlgorithm(kCCAlgorithmAES128),
                              CCOptions(kCCOptionPKCS7Padding),
                              keyData, keyLength,
                              cryptData,
                              data, data.count,
                              &cryptData + ivLength, cryptDataLength,
                              &numBytesEncrypted)

    if UInt32(cryptStatus) == UInt32(kCCSuccess) {
        cryptData.removeRange(numBytesEncrypted+ivLength..<cryptData.count)
    }
    else {
        print("Error: \(cryptStatus)")
        return nil;
    }

    return cryptData;
}

func aesCBC128Decrypt(data data:[UInt8], keyData:[UInt8]) -> [UInt8]? {
    let clearLength = size_t(data.count)
    var clearData   = [UInt8](count:clearLength, repeatedValue:0)

    let keyLength   = size_t(kCCKeySizeAES128)
    let ivLength    = size_t(kCCBlockSizeAES128)

    var numBytesDecrypted :size_t = 0
    let cryptStatus = CCCrypt(CCOperation(kCCDecrypt),
                              CCAlgorithm(kCCAlgorithmAES128),
                              CCOptions(kCCOptionPKCS7Padding),
                              keyData, keyLength,
                              data,
                              UnsafePointer<UInt8>(data) + ivLength, data.count - ivLength,
                              &clearData, clearLength,
                              &numBytesDecrypted)

    if UInt32(cryptStatus) == UInt32(kCCSuccess) {
        clearData.removeRange(numBytesDecrypted..<clearLength)

    } else {
        print("Error: \(cryptStatus)")
        return nil;
    }

    return clearData;
}

```

Example usage:

```swift
let clearData = toData("clearData0123456")
let keyData   = toData("keyData890123456")

print("clearData:   \(toHex(clearData))")
print("keyData:     \(toHex(keyData))")
let cryptData = aesCBC128Encrypt(data:clearData, keyData:keyData)!
print("cryptData:   \(toHex(cryptData))")
let decryptData = aesCBC128Decrypt(data:cryptData, keyData:keyData)!
print("decryptData: \(toHex(decryptData))")

```

Example Output:

```swift
clearData:   <636c6561 72446174 61303132 33343536>
keyData:     <6b657944 61746138 39303132 33343536>
cryptData:   <9fce4323 830e3734 93dd93bf e464f72a a653a3a5 2c40d5ea e90c1017 958750a7 ff094c53 6a81b458 b1fbd6d4 1f583298>
decryptData: <636c6561 72446174 61303132 33343536>

```



## AES encryption in ECB mode with PKCS7 padding


From Apple documentation for IV,

> 
<p>This parameter is ignored if ECB mode is used or
if a stream cipher algorithm is selected.</p>


```swift
func AESEncryption(key: String) -> String? {
        
        let keyData: NSData! = (key as NSString).data(using: String.Encoding.utf8.rawValue) as NSData!
        
        let data: NSData! = (self as NSString).data(using: String.Encoding.utf8.rawValue) as NSData!
        
        let cryptData    = NSMutableData(length: Int(data.length) + kCCBlockSizeAES128)!
        
        let keyLength              = size_t(kCCKeySizeAES128)
        let operation: CCOperation = UInt32(kCCEncrypt)
        let algoritm:  CCAlgorithm = UInt32(kCCAlgorithmAES128)
        let options:   CCOptions   = UInt32(kCCOptionECBMode + kCCOptionPKCS7Padding)
        
        var numBytesEncrypted :size_t = 0
        
        
        let cryptStatus = CCCrypt(operation,
                                  algoritm,
                                  options,
                                  keyData.bytes, keyLength,
                                  nil,
                                  data.bytes, data.length,
                                  cryptData.mutableBytes, cryptData.length,
                                  &numBytesEncrypted)
        
        if UInt32(cryptStatus) == UInt32(kCCSuccess) {
            cryptData.length = Int(numBytesEncrypted)
            
            var bytes = [UInt8](repeating: 0, count: cryptData.length)
            cryptData.getBytes(&bytes, length: cryptData.length)
            
            var hexString = ""
            for byte in bytes {
                hexString += String(format:"%02x", UInt8(byte))
            }
            
            return hexString
        }
        
        return nil
    }

```

