---
metaTitle: ".NET Framework - Platform Invoke"
description: "Calling a Win32 dll function, Using Windows API, Marshalling arrays, Marshaling structs, Marshaling unions"
---

# Platform Invoke



## Calling a Win32 dll function


```dotnet
using System.Runtime.InteropServices;

class PInvokeExample
{
    [DllImport("user32.dll", CharSet = CharSet.Auto)]
    public static extern uint MessageBox(IntPtr hWnd, String text, String caption, int options);

    public static void test()
    {
        MessageBox(IntPtr.Zero, "Hello!", "Message", 0);
    }
}

```

Declare a function as `static extern` stting `DllImportAttribute` with its `Value` property set to .dll name. Don't forget to use `System.Runtime.InteropServices` namespace. Then call it as an regular static method.

The Platform Invocation Services will take care of loading the .dll and finding the desired finction. The P/Invoke in most simple cases will also marshal parameters and return value to and from the .dll (i.e. convert from .NET datatypes to Win32 ones and vice versa).



## Using Windows API


Use [pinvoke.net](http://pinvoke.net/).

Before declaring an `extern` Windows API function in your code, consider looking for it on [pinvoke.net](http://pinvoke.net/). They most likely already have a suitable declaration with all supporting types and good examples.



## Marshalling arrays


**Arrays of simple type**

```dotnet
[DllImport("Example.dll")]
static extern void SetArray(
    [MarshalAs(UnmanagedType.LPArray, SizeConst = 128)]
    byte[] data);

```

**Arrays of string**

```dotnet
[DllImport("Example.dll")]
static extern void SetStrArray(string[] textLines);

```



## Marshaling structs


**Simple struct**

C++ signature:

```dotnet
typedef struct _PERSON
{
    int age;
    char name[32];
} PERSON, *LP_PERSON;

void GetSpouse(PERSON person, LP_PERSON spouse);

```

C# definition

```dotnet
[StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
public struct PERSON
{
    public int age;
    [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)]
    public string name;
}

[DllImport("family.dll", CharSet = CharSet.Auto)]
public static extern bool GetSpouse(PERSON person, ref PERSON spouse);

```

**Struct with unknown size array fields. Passing in**

C++ signature

```dotnet
typedef struct
{
    int length;
    int *data;
} VECTOR;

void SetVector(VECTOR &vector);

```

When passed from managed to unmanaged code, this

The `data` array should be defined as IntPtr and memory should be explicitly allocated with [`Marshal.AllocHGlobal()`](https://msdn.microsoft.com/en-us/library/system.runtime.interopservices.marshal.allochglobal(v=vs.110).aspx) (and freed with [`Marshal.FreeHGlobal()`](https://msdn.microsoft.com/en-us/library/system.runtime.interopservices.marshal.freehglobal(v=vs.110).aspx) afterwords):

```dotnet
[StructLayout(LayoutKind.Sequential)]
public struct VECTOR : IDisposable
{
    int length;
    IntPtr dataBuf;

    public int[] data
    {
        set
        {
            FreeDataBuf();
            if (value != null && value.Length > 0)
            {
                dataBuf = Marshal.AllocHGlobal(value.Length * Marshal.SizeOf(value[0]));
                Marshal.Copy(value, 0, dataBuf, value.Length);
                length = value.Length;
            }
        }
    }
    void FreeDataBuf()
    {
        if (dataBuf != IntPtr.Zero)
        {
            Marshal.FreeHGlobal(dataBuf);
            dataBuf = IntPtr.Zero;
        }
    }
    public void Dispose()
    {
        FreeDataBuf();
    }
}

[DllImport("vectors.dll")]
public static extern void SetVector([In]ref VECTOR vector);

```

**Struct with unknown size array fields. Receiving**

C++ signature:

```dotnet
typedef struct
{
    char *name;
} USER;

bool GetCurrentUser(USER *user);

```

When such data is passed out of unmanaged code and memory is allocated by the unmanaged functions, the managed caller should receive it into an `IntPrt` variable and convert the buffer to a managed array. In case of strings there is a convenient [`Marshal.PtrToStringAnsi()`](https://msdn.microsoft.com/en-us/library/7b620dhe(v=vs.110).aspx) method:

```dotnet
[StructLayout(LayoutKind.Sequential)]
public struct USER
{
    IntPtr nameBuffer;
    public string name { get { return Marshal.PtrToStringAnsi(nameBuffer); } }
}

[DllImport("users.dll")]
public static extern bool GetCurrentUser(out USER user);

```



## Marshaling unions


**Value-type fields only**

C++ declaration

```dotnet
typedef union
{
    char c;
    int i;
} CharOrInt;

```

C# declaration

```dotnet
[StructLayout(LayoutKind.Explicit)]
public struct CharOrInt
{
    [FieldOffset(0)]
    public byte c;
    [FieldOffset(0)]
    public int i;
}

```

**Mixing value-type and reference fields**

Overlapping a reference value with a value type one is not allowed so you cannot simply use the ~~`FieldOffset(0) text; FieldOffset(0) i;`~~ will not compile for

```dotnet
typedef union
{
    char text[128];
    int i;
} TextOrInt;

```

and generally you would have to employ custom marshaling. However, in particular cases like this simpler technics may be used:

```dotnet
[StructLayout(LayoutKind.Sequential)]
public struct TextOrInt
{
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 128)]
    public byte[] text;
    public int i { get { return BitConverter.ToInt32(text, 0); } }
}

```



#### Syntax


<li>[DllImport("Example.dll")]
static extern void SetText(string inString);</li>
<li>[DllImport("Example.dll")]
static extern void GetText(StringBuilder outString);</li>
- [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)] string text;
- [MarshalAs(UnmanagedType.ByValArray, SizeConst = 128)] byte[] byteArr;
- [StructLayout(LayoutKind.Sequential)] public struct PERSON {...}
- [StructLayout(LayoutKind.Explicit)] public struct MarshaledUnion { [FieldOffset(0)]... }

