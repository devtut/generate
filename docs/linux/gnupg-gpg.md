---
metaTitle: "Linux - GnuPG (GPG)"
description: "Create and use a GnuPG key quickly, Exporting your public key"
---

# GnuPG (GPG)


GnuPG is a sophisticated key management system which allows for secure signing or encrypting data. GPG is a command-line tool used to create and manipulate GnuPG keys.

GnuPG is most widely used for having SSH (Secure Shell) connections without password or any means of interactive authentication, which improves security level significantly.

Following sections describe ways to create, use, and maintain security of GnuPG keys.



## Create and use a GnuPG key quickly


Install haveged (example `sudo apt-get install haveged`) to speed up the random byte process. Then:

```bash
gpg --gen-key
gpg --list-keys

```

outputs:

```bash
pub   2048R/NNNNNNNN 2016-01-01
uid                  Name <name@example.com>
sub   2048R/xxxxxxxx 2016-01-01

```

Then publish:

```bash
gpg --keyserver pgp.mit.edu --send-keys NNNNNNNN

```

Then plan to revoke: [https://www.hackdiary.com/2004/01/18/revoking-a-gpg-key/](https://www.hackdiary.com/2004/01/18/revoking-a-gpg-key/)



## Exporting your public key


In order for your public-private keypair to be of use, you must make your public key freely available to others.  Be sure that you are working with your public key here since you should **never** share your private key.  You can export your public key with the following command:

`gpg —armor —export EMAIL_ADDRESS > public_key.asc`

where EMAIL_ADDRESS is the email address associated with the key

Alternately, you can upload your public key to a public key server such as keys.gnupg.net so that others can use it.  To do so, enter the following in a terminal:

`gpg —list-keys`

Then, search for the 8-digit string (the primary ID) associated with the key you want to export.  Then, issue the command:

`gpg —send-keys PRIMARY_ID`

where PRIMARY_ID is the actual ID of that key.

Now, the private key has been uploaded to the key server and is publicly available.

