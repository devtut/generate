---
metaTitle: hashlib
description: MD5 hash of a string, algorithm provided by OpenSSL
---

# hashlib


hashlib implements a common interface to many different secure hash and message digest algorithms. Included are the FIPS secure hash algorithms SHA1, SHA224, SHA256, SHA384, and SHA512.



## MD5 hash of a string


This module implements a common interface to many different secure hash and message digest algorithms. Included are the FIPS secure hash algorithms SHA1, SHA224, SHA256, SHA384, and SHA512 (defined in FIPS 180-2) as well as RSA’s MD5 algorithm (defined in Internet RFC 1321).

There is one constructor method named for each type of hash. All return a hash object with the same simple interface. For example: use `sha1()` to create a SHA1 hash object.

```
hash.sha1()

```

Constructors for hash algorithms that are always present in this module are `md5()`, `sha1()`, `sha224()`, `sha256()`, `sha384()`, and `sha512()`.

You can now feed this object with arbitrary strings using the `update()` method. At any point you can ask it for the digest of the concatenation of the strings fed to it so far using the `digest()` or `hexdigest()` methods.

```
hash.update(arg)

```

> 
<p>Update the hash object with the string arg. Repeated calls are
equivalent to a single call with the concatenation of all the
arguments: m.update(a); m.update(b) is equivalent to m.update(a+b).</p>


```
hash.digest()

```

> 
<p>Return the digest of the strings passed to the update() method so far.
This is a string of digest_size bytes which may contain non-ASCII
characters, including null bytes.</p>


```
hash.hexdigest()

```

> 
<p>Like digest() except the digest is returned as a string of double
length, containing only hexadecimal digits. This may be used to
exchange the value safely in email or other non-binary environments.</p>


Here is an example:

```
>>> import hashlib
>>> m = hashlib.md5()
>>> m.update("Nobody inspects")
>>> m.update(" the spammish repetition")
>>> m.digest()
'\xbbd\x9c\x83\xdd\x1e\xa5\xc9\xd9\xde\xc9\xa1\x8d\xf0\xff\xe9'
>>> m.hexdigest()
'bb649c83dd1ea5c9d9dec9a18df0ffe9'
>>> m.digest_size
16
>>> m.block_size
64

```

or:

```
hashlib.md5("Nobody inspects the spammish repetition").hexdigest()
    'bb649c83dd1ea5c9d9dec9a18df0ffe9'

```



## algorithm provided by OpenSSL


A generic `new()` constructor that takes the string name of the desired algorithm as its first parameter also exists to allow access to the above listed hashes as well as any other algorithms that your OpenSSL library may offer. The named constructors are much faster than `new()` and should be preferred.

Using `new()` with an algorithm provided by OpenSSL:

```
>>> h = hashlib.new('ripemd160')
>>> h.update("Nobody inspects the spammish repetition")
>>> h.hexdigest()
'cc4a5ce1b3df48aec5d22d1f16b894a0b894eccc'

```

