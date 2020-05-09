---
metaTitle: "Linux - Getting System Information"
description: "Statistics about CPU, Memory, Network and Disk (I/O operations), List Hardware, Find CPU model/speed information, Process monitoring and information gathering, Using tools like lscpu and lshw"
---

# Getting System Information


Collection of commands to fetch system related information.



## Statistics about CPU, Memory, Network and Disk (I/O operations)


To get general statistics about main components of Linux family of `stat` commands are extremely useful

### CPU

To get processors related statistics you can use `mpstat` command but with some options it will provide better visibility:

```bash
$ mpstat 2 10

```

### Memory

We all know command `free` to show amount of (remaining) RAM but to see all statistic including I/O operations:

```bash
$ vmstat 2 10

```

### Disk

To get general information about your disk operations in real time you can utilise `iostat`.

```bash
$ iostat -kx 2

```

### Network

To be able to see what is happening with your network services you can use `netstat`

```bash
$ netstat -ntlp # open TCP sockets 
$ netstat -nulp # open UDP sockets 
$ netstat -nxlp # open Unix sockets 

```

But you can find useful monitoring to see network traffic in real time:

```bash
$ sudo iftop

```

### Optional

To generate statistics in real time related to I/O operations across all components you can use `dstat`. That tool that is a versatile replacement for `vmstat`, `iostat` and `ifstat`



## List Hardware


**Ubuntu:**

**lshw** is a small tool to extract detailed information on the hardware configuration of the machine. It can report exact memory configuration, firmware version, mainboard configuration,  CPU version and speed, cache configuration, bus speed, etc.

```bash
$ sudo lshw | less (or more)
$ sudo lshw -html > myhardware.html
$ sudo lshw -xml > myhardware.xml

```

To show PCI info

```bash
$ lspci -tv

```

To see USB info

```bash
$ lsusb -tv

```

To display BIOS informations

```bash
$ dmidecode -q | less

```

To see specific information about disk (disk sda in example) you can use:

```bash
$ hdparm -i /dev/sda

```

Few additional utilities/commands will help gather some extra information:

```bash
$ smartctl -A /dev/sda | grep Power_On_Hours # How long has this disk (system) been powered on in total
$ hdparm -tT /dev/sda # Do a read speed test on disk sda
$ badblocks -s /dev/sda # Test for unreadable blocks on disk sda

```



## Find CPU model/speed information


**Ubuntu:**

```bash
$ cat /proc/cpuinfo

```

Sample Output:

```bash
processor    : 0
vendor_id    : GenuineIntel
cpu family    : 6
model        : 15
model name    : Intel(R) Core(TM)2 Quad CPU    Q6600  @ 2.40GHz
stepping    : 11
cpu MHz        : 1596.000
cache size    : 4096 KB
physical id    : 0
siblings    : 4
core id        : 0
cpu cores    : 4
apicid        : 0
initial apicid    : 0
fpu        : yes
fpu_exception    : yes
cpuid level    : 10
wp        : yes
flags        : fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush dts acpi mmx fxsr sse sse2 ss ht tm pbe syscall nx lm constant_tsc arch_perfmon pebs bts rep_good pni dtes64 monitor ds_cpl vmx est tm2 ssse3 cx16 xtpr pdcm lahf_lm tpr_shadow vnmi flexpriority
bogomips    : 4800.18
clflush size    : 64
cache_alignment    : 64
address sizes    : 36 bits physical, 48 bits virtual
power management:
....
..
processor    : 3
vendor_id    : GenuineIntel
cpu family    : 6
model        : 15
model name    : Intel(R) Core(TM)2 Quad CPU    Q6600  @ 2.40GHz
stepping    : 11
cpu MHz        : 1596.000
cache size    : 4096 KB
physical id    : 0
siblings    : 4
core id        : 3
cpu cores    : 4
apicid        : 3
initial apicid    : 3
fpu        : yes
fpu_exception    : yes
cpuid level    : 10
wp        : yes
flags        : fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush dts acpi mmx fxsr sse sse2 ss ht tm pbe syscall nx lm constant_tsc arch_perfmon pebs bts rep_good pni dtes64 monitor ds_cpl vmx est tm2 ssse3 cx16 xtpr pdcm lahf_lm tpr_shadow vnmi flexpriority
bogomips    : 4800.30
clflush size    : 64
cache_alignment    : 64
address sizes    : 36 bits physical, 48 bits virtual
power management:

```

**count processor (including cores):**

```bash
$ grep -c processor /proc/cpuinfo

```



## Process monitoring and information gathering


Overall you have two ways to monitor processes at linux host

### Static monitoring

Most widely used command is `ps` (i.e., process status) command is used to provide information about the currently running processes, including their process identification numbers (PIDs).

Here few useful options to gather specific informations.

List processes in a hierarchy

```bash
$ ps -e -o pid,args --forest

```

List processes sorted by % cpu usage

```bash
$ ps -e -o pcpu,cpu,nice,state,cputime,args --sort pcpu | sed '/^ 0.0 /d'

```

List processes sorted by mem (KB) usage.

```bash
$ ps -e -orss=,args= | sort -b -k1,1n | pr -TW$COLUMNS

```

List all threads for a particular process ("firefox-bin" process in example )

```bash
$ ps -C firefox-bin -L -o pid,tid,pcpu,state

```

After finding specific process you can gather information related to it using `lsof` to list paths that process id has open

```bash
$ lsof -p $$ 

```

Or based on path find out list processes that have specified path open

```bash
$ lsof ~

```

### Interactive monitoring

Most commonly known tool for dynamic monitoring is:

```bash
$ top

```

That mostly default command that have huge amount options to filter and represent information in real time (in comparison to `ps` command.

Still there are more advance options that can be considered and installed as `top` replacement

```bash
$ htop -d 5

```

or

```bash
$ atop 

```

Which has ability to log all the activities into log file (default atop will log all the activity on every 600 seconds)
To this list there are few specialised commands as `iotop` or `iftop`

```bash
$ sudo iotop

```



## Using tools like lscpu and lshw


By using tools like lscpu as lscpu is an easy way to get CPU information.

```bash
$ lscpu
Architecture:          x86_64
CPU op-mode(s):        32-bit, 64-bit
Byte Order:            Little Endian
CPU(s):                4
On-line CPU(s) list:   0-3
Thread(s) per core:    1
Core(s) per socket:    4
Socket(s):             1
NUMA node(s):          1
Vendor ID:             GenuineIntel
CPU family:            6
Model:                 23
Stepping:              10
CPU MHz:               1998.000
BogoMIPS:              5303.14
Virtualization:        VT-x
L1d cache:             32K
L1i cache:             32K
L2 cache:              2048K
NUMA node0 CPU(s):     0-3

```

By using tool lshw

```bash
$ lshw | grep cpu

df1-ws-5084               
    description: Computer
    width: 64 bits
    capabilities: vsyscall32
  *-core
       description: Motherboard
       physical id: 0
     *-memory
          description: System memory
          physical id: 0
          size: 5881MiB
     *-cpu
          product: Intel(R) Pentium(R) CPU G3220 @ 3.00GHz
          vendor: Intel Corp.
          physical id: 1
          bus info: cpu@0
          size: 3GHz
          capacity: 3GHz
          width: 64 bits

```

