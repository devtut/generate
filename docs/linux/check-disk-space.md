---
metaTitle: "Linux - Check Disk Space"
description: "Checking Disk Space, Investigate Directories For Disk Usage"
---

# Check Disk Space



## Checking Disk Space


It's quite common to want to check the status of the various partitions/drives on your server/computer to see how full they are. The following command is the one you'll want to run:

```bash
df -h

```

This will produce output similar to the following:

```bash
[root@mail ~]# df -h
Filesystem            Size  Used Avail Use% Mounted on
/dev/mapper/VolGroup-lv_root
                       19G  1.6G   16G   9% /
tmpfs                 245M     0  245M   0% /dev/shm
/dev/sda1             485M   47M  413M  11% /boot

```

In this basic example, we can see that the `/` partition only has `9%` used.

For a more complex example that also covers using `df` to see various mountpoints, see below:

```bash
[root@mail ~]# df -h
Filesystem            Size  Used Avail Use% Mounted on
/dev/mapper/VG-root   1.9T  1.7T   89G  95% /
/dev/mapper/VG-var    431G  145G  264G  36% /var
devtmpfs              7.8G  204K  7.8G   1% /dev
tmpfs                 7.8G  4.0K  7.8G   1% /dev/shm
/dev/md1              495M  126M  344M  27% /boot
ku.example.com:9421   2.5T  487G  2.0T  20% /mnt/test
tmpfs                 500M   86M  415M  18% /var/ngx_pagespeed_cache

```

In this example, we have a `/` partition that's `95%` full along with an additional `/var` partition that's only `36%` full.

It's got an external network mount of `2T` that's mounted on `/mnt/test` and a ramdisk/tmpfs mount of 500M mounted on `/var/ngx_pagespeed_cache`.



## Investigate Directories For Disk Usage


Sometimes it may be required to find out which directory consuming how much disk space especially when you are used `df -h` and realized your available disk space is low.

**du:**

`du` command summarizes disk usage of the set of FILEs, recursively for directories.

It's often uses with `-sh` option:

```bash
-s, --summarize
              display only a total for each argument
-h, --human-readable
              print sizes in human readable format (e.g., 1K 234M 2G)

```

For summarizing disk usages of the files in the current directory we use:

```bash
du -sh *

```

Example output:

```bash
572K    Documents
208M    Downloads
4,0K    Music
724K    Pictures
4,0K    Public
4,0K    Templates
4,0K    Videos

```

We can also include hidden files with using:

```bash
du -sh .[!.]* *

```

Example output:

```bash
6,3M    .atom
4,0K    .bash_history
4,0K    .bash_logout
8,0K    .bashrc
350M    .cache
195M    .config
12K    .dbus
4,0K    .dmrc
44K    .gconf
60K    .gem
520K    .gimp-2.8
28K    .gnome
4,0K    .ICEauthority
8,3M    .local
8,0K    .nano
404K    .nv
36K    .pki
4,0K    .profile
8,0K    .ssh
0    .sudo_as_admin_successful
4,0K    .Xauthority
4,0K    .xsession-errors
4,0K    .xsession-errors.old
572K    Documents
208M    Downloads
4,0K    Music
724K    Pictures
4,0K    Public
4,0K    Templates
4,0K    Videos

```

Thirdly, you can add total to the output by adding ,-c, option:

```bash
du -sch .[!.]* *

```

Result:

```bash
.
.
.
4,0K    Templates
4,0K    Videos
769M    total

```

**Most importantly** using `du` command properly on the root directory is a life saving action to find out what application/service or user is consuming your disk space wildly. For example, in case of a ridiculously low level of disk space availability for a web and mail server, the reason could be a spam attack to your mail service and you can diagnose it just by using `du` command.

Investigate root directory for disk usage:

```bash
sudo du -sch /.[!.]* /*

```

Example output:

```bash
16K    /.VolumeIcon.icns
24K    /.VolumeIcon.png
13M    /bin
57M    /boot
4,0K    /cdrom
620K    /dev
13M    /etc
779M    /home
0    /initrd.img
406M    /lib
3,9M    /lib32
4,0K    /lib64
16K    /lost+found
4,0K    /media
4,0K    /mnt
367M    /opt
du: cannot access '/proc/18221/task/18221/fd/4': No such file or directory
du: cannot access '/proc/18221/task/18221/fdinfo/4': No such file or directory
du: cannot access '/proc/18221/fd/4': No such file or directory
du: cannot access '/proc/18221/fdinfo/4': No such file or directory
0    /proc
20K    /root
du: cannot access '/run/user/1000/gvfs': Permission denied
9,4M    /run
13M    /sbin
4,0K    /srv
0    /sys
72K    /tmp
3,5G    /usr
639M    /var
0    /vmlinuz
5,8G    total

```

Lastly, the best method forms when you add a threshold size value for directories to ignore small ones. This command will only show folders with more than 1GB in size which located under root directory up to the farthermost branch of the whole directory tree in your file system:

```bash
sudo du --threshold=1G -ch /.[!.]* /*

```

Example output:

```bash
1,4G    /usr/lib
1,8G    /usr/share
3,5G    /usr
5,8G    total

```

