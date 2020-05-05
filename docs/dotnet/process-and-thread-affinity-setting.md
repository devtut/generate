---
metaTitle: ".NET Framework - Process and Thread affinity setting"
description: "Get process affinity mask, Set process affinity mask"
---

# Process and Thread affinity setting

## Get process affinity mask

```

   public static int GetProcessAffinityMask(string processName = null)
    {
        Process myProcess = GetProcessByName(ref processName);

        int processorAffinity = (int)myProcess.ProcessorAffinity;
        Console.WriteLine("Process {0} Affinity Mask is : {1}", processName, FormatAffinity(processorAffinity));

        return processorAffinity;
    }

    public static Process GetProcessByName(ref string processName)
    {
        Process myProcess;
        if (string.IsNullOrEmpty(processName))
        {
            myProcess = Process.GetCurrentProcess();
            processName = myProcess.ProcessName;
        }
        else
        {
            Process[] processList = Process.GetProcessesByName(processName);
            myProcess = processList[0];
        }
        return myProcess;
    }

    private static string FormatAffinity(int affinity)
    {
        return Convert.ToString(affinity, 2).PadLeft(Environment.ProcessorCount, '0');
    }
}

```

Example of usage :

```dotnet
private static void Main(string[] args)
{
    GetProcessAffinityMask();

    Console.ReadKey();
}
// Output:
// Process Test.vshost Affinity Mask is : 11111111

```

## Set process affinity mask

```

   public static void SetProcessAffinityMask(int affinity, string processName = null)
    {
        Process myProcess = GetProcessByName(ref processName);

        Console.WriteLine("Process {0} Old Affinity Mask is : {1}", processName, FormatAffinity((int)myProcess.ProcessorAffinity));

        myProcess.ProcessorAffinity = new IntPtr(affinity);
        Console.WriteLine("Process {0} New Affinity Mask is : {1}", processName, FormatAffinity((int)myProcess.ProcessorAffinity));
    }

```

Example of usage :

```dotnet
private static void Main(string[] args)
{
    int newAffinity = Convert.ToInt32("10101010", 2);
    SetProcessAffinityMask(newAffinity);

    Console.ReadKey();
}
// Output :
// Process Test.vshost Old Affinity Mask is : 11111111
// Process Test.vshost New Affinity Mask is : 10101010

```

#### Parameters

| Parameter | Details                                                                                                                                                                                                                                                    |
| --------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| affinity  | integer that describes the set of processors on which the process is allowed to run. For example, on a 8 processor system if you want your process to be executed only on processors 3 and 4 than you choose affinity like this : 00001100 which equals 12 |

#### Remarks

The processor affinity of a thread is the set of processors it has a relationship to. In other words, those it can be scheduled to run on.

Processor affinity represents each processor as a bit. Bit 0 represents processor one, bit 1 represents processor two, and so on.
