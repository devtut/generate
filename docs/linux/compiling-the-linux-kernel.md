---
metaTitle: "Linux - Compiling the Linux kernel"
description: "Compilation of Linux Kernel on Ubuntu"
---

# Compiling the Linux kernel




## Compilation of Linux Kernel on Ubuntu


> 
**Warning:** be sure you have at least 15 GB of free disk space.


### Compilation in Ubuntu >=13.04

**Option A) Use Git**

Use git if you want to stay in sync with the latest Ubuntu kernel source. Detailed instructions can be found in the Kernel Git Guide.
The git repository does not include necessary control files, so you must build them by:

```bash
fakeroot debian/rules clean

```

**Option B) Download the source archive**

Download the source archive - This is for users who want to rebuild the standard Ubuntu packages with additional patches.
Use a follow command to install the build dependencies and extract the source (to the current directory):

<li>
Install the following packages:

```bash
sudo apt-get build-dep linux-image-`uname -r`

```


</li>

**Option C) Download the source package and build**

This is for users who want to modify, or play around with, the Ubuntu-patched kernel source.

<li>
Retrieve the latest kernel source from [kernel.org](https://www.kernel.org/).
</li>
<li>
Extract the archive to a directory and `cd` into it:

```bash
tar xf linux-*.tar.xz
cd linux-*

```


</li>
<li>
Build the ncurses configuration interface:

```bash
make menuconfig

```


</li>
<li>
To accept the default configuration, press <kbd>→</kbd> to highlight `< Exit >` and then <kbd>Return</kbd>.
</li>
<li>
Press <kbd>Return</kbd> again to save the configuration.
</li>
<li>
Use `make` to build the kernel:

```bash
make

```


Note that you can use the `-j**n**` flag to compile files in parallel and take advantage of multiple cores.
</li>

The compressed kernel image can be found at `arch/[arch]/boot/bzImage`, where `[arch]` is equal to `uname -a`.

