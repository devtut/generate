---
metaTitle: "MongoDB - Authentication Mechanisms in MongoDB"
description: "Authentication Mechanisms"
---

# Authentication Mechanisms in MongoDB


Authentication is the process of verifying the identity of a client. When access control, i.e. authorization, is enabled, MongoDB requires all clients to authenticate themselves in order to determine their access.

MongoDB supports a number of authentication mechanisms that clients can use to verify their identity. These mechanisms allow MongoDB to integrate into your existing authentication system.



## Authentication Mechanisms


MongoDB supports multiple authentication mechanisms.

**Client and User Authentication Mechanisms**

<li>
SCRAM-SHA-1
</li>
<li>
X.509 Certificate Authentication
</li>
<li>
MongoDB Challenge and Response (MONGODB-CR)
</li>
<li>
LDAP proxy authentication, and
</li>
<li>
Kerberos authentication
</li>

**Internal Authentication Mechanisms**

- Keyfile
- X.509

