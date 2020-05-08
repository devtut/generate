---
metaTitle: "Android - Android Kernel Optimization"
description: "Low RAM Configuration, How to add a CPU Governor, I/O Schedulers"
---

# Android Kernel Optimization




## Low RAM Configuration


Android now supports devices with 512MB of RAM. This documentation is intended to help OEMs optimize and configure Android 4.4 for low-memory devices. Several of these optimizations are generic enough that they can be applied to previous releases as well.

**Enable Low Ram Device flag**

We are introducing a new API called ActivityManager.isLowRamDevice() for applications to determine if they should turn off specific memory-intensive features that work poorly on low-memory devices.

For 512MB devices, this API is expected to return: "true" It can be enabled by the following system property in the device makefile.

```java
PRODUCT_PROPERTY_OVERRIDES += ro.config.low_ram=true

```

**Disable JIT**

System-wide JIT memory usage is dependent on the number of applications running and the code footprint of those applications. The JIT establishes a maximum translated code cache size and touches the pages within it as needed. JIT costs somewhere between 3M and 6M across a typical running system.

The large apps tend to max out the code cache fairly quickly (which by default has been 1M). On average, JIT cache usage runs somewhere between 100K and 200K bytes per app. Reducing the max size of the cache can help somewhat with memory usage, but if set too low will send the JIT into a thrashing mode. For the really low-memory devices, we recommend the JIT be disabled entirely.

This can be achieved by adding the following line to the product makefile:

```java
PRODUCT_PROPERTY_OVERRIDES += dalvik.vm.jit.codecachesize=0

```



## How to add a CPU Governor


The CPU governor itself is just 1 C file, which is located in  kernel_source/drivers/cpufreq/,  for example: cpufreq_smartass2.c. You are responsible yourself for find the governor (look in an existing kernel repo for your device)
But in order to successfully call and compile this file into your kernel you will have to make the following changes:

1. Copy your governor file (cpufreq_govname.c) and browse to  kernel_source/drivers/cpufreq, now paste it.
1. and open Kconfig (this is the interface of the config menu layout) when adding a kernel, you want it to show up in your config. You can do that by adding the choice of governor.

```java
config CPU_FREQ_GOV_GOVNAMEHERE
tristate "'gov_name_lowercase' cpufreq governor"
depends on CPU_FREQ
help
governor' - a custom governor!

```

for example, for smartassV2.

```java
config CPU_FREQ_GOV_SMARTASS2
 tristate "'smartassV2' cpufreq governor"
 depends on CPU_FREQ
 help
 'smartassV2' - a "smart" optimized governor! 

```

next to adding the choice, you also must declare the possibility that the governor gets chosen as default governor.

```

config CPU_FREQ_DEFAULT_GOV_GOVNAMEHERE
 bool "gov_name_lowercase"
 select CPU_FREQ_GOV_GOVNAMEHERE
 help
 Use the CPUFreq governor 'govname' as default.

```

for example, for smartassV2.

```java
config CPU_FREQ_DEFAULT_GOV_SMARTASS2
 bool "smartass2"
 select CPU_FREQ_GOV_SMARTASS2
 help
 Use the CPUFreq governor 'smartassV2' as default.

```

– can’t find the right place to put it? Just search for  `“CPU_FREQ_GOV_CONSERVATIVE”`,  and place the code beneath, same thing counts for `“CPU_FREQ_DEFAULT_GOV_CONSERVATIVE”`

Now that Kconfig is finished you can save and close the file.

<li>While still in the `/drivers/cpufreq` folder, open Makefile.
In Makefile, add the line corresponding to your CPU Governor. for example:</li>

```java
obj-$(CONFIG_CPU_FREQ_GOV_SMARTASS2)    += cpufreq_smartass2.o

```

Be ware that you do not call the native C file, but the O file! which is the compiled C file. Save the file.

<li>Move to:  `kernel_source/includes/linux`.  Now open `cpufreq.h`
Scroll down until you see something like:</li>

```java
#elif defined(CONFIG_CPU_FREQ_DEFAULT_GOV_ONDEMAND)
 extern struct cpufreq_governor cpufreq_gov_ondemand;
 #define CPUFREQ_DEFAULT_GOVERNOR    (&amp;cpufreq_gov_ondemand)

```

(other cpu governors are also listed there)

Now add your entry with the selected CPU Governor, example:

```java
#elif defined(CONFIG_CPU_FREQ_DEFAULT_GOV_SMARTASS2)
 extern struct cpufreq_governor cpufreq_gov_smartass2;
 #define CPUFREQ_DEFAULT_GOVERNOR (&amp;cpufreq_gov_smartass2)

```

Save the file and close it.

The initial CPU Governor setup is now complete. when you’ve done all steps successfully, you should be able to choose your governor from the menu (`menuconfig`, `xconfig`, `gconfig`, `nconfig`). Once checked in the menu it will be included to the kernel.

Commit that is nearly the same as above instructions:  [Add smartassV2 and lulzactive governor commit](https://github.com/broodplank/samsung-kernel-msm7x30/commit/bd319103c20c9a9357852d6f535aaa999cee867a)



## I/O Schedulers


You can enhance your kernel by adding new I/O schedulers if needed. Globally, governors and schedulers are the same; they both provide a way how the system should work. However, for the schedulers it is all about the input/output datastream except for the CPU settings. I/O schedulers decide how an upcoming I/O activity will be scheduled. The standard schedulers such as **noop** or **cfq** are performing very reasonably.

I/O schedulers can be found in **kernel_source/block**.

<li>
Copy your I/O scheduler file (for example, **sio-iosched.c**) and browse to **kernel_source/block**. Paste the scheduler file there.
</li>
<li>
Now open **Kconfig.iosched** and add your choice to the **Kconfig**, for example for **SIO**:

```java
config IOSCHED_SIO
  tristate "Simple I/O scheduler"
  default y
  ---help---
    The Simple I/O scheduler is an extremely simple scheduler,
    based on noop and deadline, that relies on deadlines to
    ensure fairness. The algorithm does not do any sorting but
    basic merging, trying to keep a minimum overhead. It is aimed
    mainly for aleatory access devices (eg: flash devices).

```


</li>
<li>
Then set the default choice option as follows:

```java
default "sio" if DEFAULT_SIO

```


Save the file.
</li>
<li>
Open the **Makefile** in **kernel_source/block/** and simply add the following line for **SIO**:

```java
obj-$(CONFIG_IOSCHED_SIO)    += sio-iosched.o

```


Save the file and you are done! The I/O schedulers should now pop up at the menu config.
</li>

Similar commit on GitHub: [added Simple I/O scheduler](https://github.com/broodplank/samsung-kernel-msm7x30/commit/8c13ea91070ff072a64de4358d0429e35697678d).

