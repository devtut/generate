---
metaTitle: "Linux - Secure Shell (SSH)"
description: "Connecting to a remote server, Installing OpenSSH suite, Generate public and private key, Configuring an SSH server to accept connections, Passwordless connection (using a key pair), Disable ssh service"
---

# Secure Shell (SSH)


A secure [shell](https://stackoverflow.com/documentation/linux/2731/shell) is used to remotely access a server from a client over an encrypted connection. OpenSSH is used as an alternative to Telnet connections that achieve remote shell access but are unencrypted. The OpenSSH Client is installed on most GNU/Linux distributions by default and is used to connect to a server. These examples show use how to use the SSH suite to for accept SSH connections and connecting to another host.



## Connecting to a remote server


To connect to a server we must use SSH on the client as follows,

```bash
# ssh -p port user@server-address

```


- **port** - The listening ssh port of the server (default port 22).
- **user** - Must be an existing user on the server with SSH privileges.
- **server address** - The IP/Domain of the server.

For a real world example lets pretend that you're making a website. The company you chose to host your site tells you that the server is located at **web-servers.com** on a custom port of **2020** and your account name **usr1** has been chosen to create a user on the server with SSH privileges. In this case the SSH command used would be as such

```bash
# ssh -p 2020 usr1@web-servers.com

```

If account name on the remote system is the same as the one one the local client you may leave the user name off. So if you are `usr1` on both systems then you my simply use `web-servers.com` instead of `usr1@web-servers.com`.

When a server you want to connect to is not directly accessible to you, you can try using ProxyJump switch to connect to it through another server which is accessible to you and can connect to the desired server.

```bash
# ssh -J usr1@10.0.0.1:2020 usr2@10.0.0.2 -p 2222

```

This will let you connect to the server 10.0.0.2 (running ssh on port 2222) through server at 10.0.0.1 (running ssh on port 2020). You will need to have accounts on both servers of course. Also note that the -J switch is introduced in OpenSSH version 7.3.



## Installing OpenSSH suite


Both connecting to a remove SSH server and accepting SSH connections require installation of `openssh`

**Debian:**

```bash
# apt-get install openssh

```

**Arch Linux:**

```bash
# pacman -S openssh

```

**Yum:**

```bash
# yum install openssh

```



## Generate public and private key


To generate keys for SSH client:

```bash
ssh-keygen [-t rsa | rsa1 | dsa ] [-C <comment>] [-b bits]

```

For example:

```bash
ssh-keygen -t rsa -b 4096 - C myemail@email.com

```

Default location is `~/.ssh/id_rsa` for private and `~/.ssh/id_rsa.pub` for public key.

For more info, please visit [man.openbsd.org](http://man.openbsd.org/OpenBSD-current/man1/ssh-keygen.1)



## Configuring an SSH server to accept connections


First we must edit the SSH daemon config file. Though under different Linux distributions this may be located in different directories, usually it is stored under `/etc/ssh/sshd_config`

Use your text editor to change the values set in this file, all lines starting with # are commented out and must have this character removed to take any effect. A list of recommendations follow as such.

```bash
Port (chose a number between 0 - 65535, normaly greater than four digits)
PasswordAuthentication yes
AllowUsers    user1 user2 ...etc

```

**Note that it is preferable to disable password logins all together and use SSH Keys for improved security as explained in this document.**



## Passwordless connection (using a key pair)


First of all you'll need to have a key pair. If you don't have one yet, take a look at the 'Generate public and private key topic'.

Your key pair is composed by a private key (id_rsa) and a public key (id_rsa.pub). All you need to do is to copy the public key to the remote host and add its contents to the `~/.ssh/authorized_keys` file.

One simple way to do that is:

`ssh <user>@<ssh-server> 'cat >> ~/.ssh/authorized_keys' < id_rsa.pub`

Once the public key is properly placed in your user's home directory, you just need to login using the respective private key:

`ssh <user>@<ssh-server> -i id_rsa`



## Disable ssh service


This will disable the SSH server side service, as if needed this will insure that clients cannot connect via ssh

**Ubuntu**

```bash
sudo service ssh stop

```

**Debian**

```bash
sudo /etc/init.d/ssh stop

```

**Arch Linux**

```bash
sudo killall sshd

```

