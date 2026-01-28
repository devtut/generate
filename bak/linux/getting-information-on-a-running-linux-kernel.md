---
metaTitle: "Linux - Getting information on a running Linux kernel"
description: "Getting details of Linux kernel., All information"
---

# Getting information on a running Linux kernel




## Getting details of Linux kernel.


We can use command uname with various options to get complete details of running kernel.

uname -a
Linux df1-ws-5084 4.4.0-64-generic #85-Ubuntu SMP Mon Feb 20 11:50:30 UTC 2017 x86_64 x86_64 x86_64 GNU/Linux

As per man page here few more options

Usage: uname [OPTION]...
Print certain system information.  With no OPTION, same as -s.

-a, --all                print all information, in the following order,
except omit -p and -i if unknown:
-s, --kernel-name        print the kernel name
-n, --nodename           print the network node hostname
-r, --kernel-release     print the kernel release
-v, --kernel-version     print the kernel version
-m, --machine            print the machine hardware name
-p, --processor          print the processor type (non-portable)
-i, --hardware-platform  print the hardware platform (non-portable)
-o, --operating-system   print the operating system
--help     display this help and exit
--version  output version information and exit



## All information


Using the -a/--all flag will print all the available information about the kernel.

```bash
$uname -a
Linux hostname 3.13.0-88-generic #135-Ubuntu SMP Wed Jun 8 21:10:42 UTC 2016 x86_64 x86_64 x86_64 GNU/Linux

```

In this example, we see the kernel name, the hostname, the kernel release number, the kernel version, the machine hardware name, the processor type, the hardware platform , and the operating system name.

Any of those fields can be queried individually using other flags.

