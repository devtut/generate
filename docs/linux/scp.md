---
metaTitle: "Linux - SCP"
description: "Basic Usage, Secure Copy"
---

# SCP



## Basic Usage


```bash
# Copy remote file to local dir
scp user@remotehost.com:/remote/path/to/foobar.md /local/dest

# Copy local file to remote dir
scp foobar.md user@remotehost.com:/remote/dest

# Key files can be used (just like ssh)
scp -i my_key.pem foobar.md user@remotehost.com:/remote/dest

```



## Secure Copy


scp command is used to  securely copy a file to or from a remote destination. If the file is in current working directly only filename is sufficient else full path is required which included the remote hostname e.g. remote_user@some_server.org:/path/to/file

### Copy local file in your CWD to new directory

```bash
scp localfile.txt /home/friend/share/

```

### Copy remote file to you current working directory

```bash
scp rocky@arena51.net:/home/rocky/game/data.txt ./

```

### Copy file from one remote location to another remote location

```bash
scp mars@universe.org:/beacon/light/bitmap.conf jupiter@universe.org:/beacon/night/

```

### To copy directory and sub-directories use '-r' recursive option to scp

```bash
scp -r user@192.168.0.4:~/project/* ./workspace/

```



#### Syntax


- scp [-rv] [-i identity_file] [[user@]host1:]file1 ... [[user@]host2:]file2

