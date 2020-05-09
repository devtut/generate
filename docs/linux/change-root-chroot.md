---
metaTitle: "Linux - Change root (chroot)"
description: "Manually changing root in a directory, Requirements, Reasons to use chroot"
---

# Change root (chroot)


Change root (chroot) is an operation that changes the apparent root directory for the current running process and their children. A program that is run in such a modified environment cannot access files and commands outside that environmental directory tree.



## Manually changing root in a directory


<li>
Ensure you met all requirements, as per Requirements
</li>
<li>
Mount the temporary API filesystems:

```bash
cd /location/of/new/root
mount -t proc proc proc/
mount --rbind /sys sys/
mount --rbind /dev dev/
mount --rbind /run run/ (optionally)

```


</li>

<li>
If you need to use an internet connection in the chroot environment, copy over the DNS details:

```bash
cp /etc/resolv.conf etc/resolv.conf

```


</li>
<li>
Change root into /location/of/new/root, specifying the shell (`/bin/bash` in this example):

```bash
chroot /location/of/new/root /bin/bash

```


</li>
<li>
After chrooting it may be necessary to load the local bash configuration:

```bash
source /etc/profile
source ~/.bashrc

```


</li>
<li>
Optionally, create a unique prompt to be able to differentiate your chroot environment:

```bash
export PS1="(chroot) $PS1"

```


</li>
<li>
When finished with the chroot, you can exit it via:

```bash
exit

```


</li>
<li>
Unmount the temporary file systems:

```bash
cd /
umount --recursive /location/of/new/root

```


</li>



## Requirements


- root privileges
- another working Linux environment,such as Live CD boot or an existing distribution
- matching environment architectures of `chroot` source and destination (check current environment architecture with `uname -m`)
- kernel modules which you may need in `chroot` environment must be loaded (for example, with `modprobe`)



## Reasons to use chroot


Changing root is commonly done for performing system maintenance on systems where booting and/or logging in is no longer possible.

Common examples are:

- reinstalling the bootloader
- rebuilding the initramfs image
- upgrading or downgrading packages
- resetting a forgotten password
- building software in a clean root environment



#### Syntax


- chroot [destination path] [shell or command]

