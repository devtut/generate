---
metaTitle: "C# | Interoperability"
description: "Import function from unmanaged C++ DLL, Calling conventions, C++ name mangling, Dynamic loading and unloading of unmanaged DLLs, Dealing with Win32 Errors, Reading structures with Marshal, Simple code to expose class for com, Pinned Object"
---

# Interoperability



## Import function from unmanaged C++ DLL


Here is an example of how to import a function that is defined in an unmanaged C++ DLL. In the C++ source code for "myDLL.dll", the function `add` is defined:

```cs
extern "C" __declspec(dllexport) int __stdcall add(int a, int b)
{
    return a + b;
}

```

Then it can be included into a C# program as follows:

```cs
class Program
{
    // This line will import the C++ method.
    // The name specified in the DllImport attribute must be the DLL name.
    // The names of parameters are unimportant, but the types must be correct.
    [DllImport("myDLL.dll")]
    private static extern int add(int left, int right);

    static void Main(string[] args)
    {
        //The extern method can be called just as any other C# method.
        Console.WriteLine(add(1, 2));
    }
}

```

See [Calling conventions](http://stackoverflow.com/documentation/c%23/3278/interoperability/16910/calling-conventions#t=201609062059032452959) and [C++ name mangling](http://stackoverflow.com/documentation/c%23/3278/interoperability/16909/c-name-mangling) for explanations about why `extern "C"` and `__stdcall` are necessary.

### Finding the dynamic library

When the extern method is first invoked the C# program will search for and load the appropriate DLL. For more information about where is searched to find the DLL, and how you can influence the search locations see  [this stackoverflow question](http://stackoverflow.com/questions/8836093/how-can-i-specify-a-dllimport-path-at-runtime).



## Calling conventions


There're several conventions of calling functions, specifying who (caller or callee) pops arguments from the stack, how arguments are passed and in what order. C++ uses `Cdecl` calling convention by default, but C# expects `StdCall`, which is usually used by Windows API. You need to change one or the other:

<li>
Change calling convention to `StdCall` in C++:

```cs
extern "C" __declspec(dllexport) int __stdcall add(int a, int b)

```


  

```cs
[DllImport("myDLL.dll")]

```


</li>
<li>
Or, change calling convention to `Cdecl` in C#:

```cs
extern "C" __declspec(dllexport) int /*__cdecl*/ add(int a, int b)

```


  

```cs
[DllImport("myDLL.dll", CallingConvention = CallingConvention.Cdecl)]

```


</li>

If you want to use a function with `Cdecl` calling convention and a mangled name, your code will look like this:

```cs
__declspec(dllexport) int add(int a, int b)

```

```cs
[DllImport("myDLL.dll", CallingConvention = CallingConvention.Cdecl,
           EntryPoint = "?add@@YAHHH@Z")]

```


<li>
**thiscall**(**__thiscall**) is mainly used in functions that are members of a class.
</li>
<li>
When a function uses **thiscall**(**__thiscall**) , a pointer to the class is passed down as the first parameter.
</li>



## C++ name mangling


C++ compilers encode additional information in the names of exported functions, such as argument types, to make overloads with different arguments possible. This process is called [name mangling](https://en.wikipedia.org/wiki/Name_mangling). This causes problems with importing functions in C# (and interop with other languages in general), as the name of `int add(int a, int b)` function is no longer `add`, it can be `?add@@YAHHH@Z`, `_add@8` or anything else, depending on the compiler and the calling convention.

There're several ways of solving the problem of name mangling:

<li>
Exporting functions using `extern "C"` to switch to C external linkage which uses C name mangling:

```cs
extern "C" __declspec(dllexport) int __stdcall add(int a, int b)

```


  

```cs
[DllImport("myDLL.dll")]

```


Function name will still be mangled (`_add@8`), but `StdCall`+`extern "C"` name mangling is recognized by C# compiler.
</li>
<li>
Specifying exported function names in `myDLL.def` module definition file:

```cs
EXPORTS
  add

```


  

```cs
int __stdcall add(int a, int b)

```


  

```cs
[DllImport("myDLL.dll")]

```


The function name will be pure `add` in this case.
</li>
<li>
Importing mangled name. You'll need some DLL viewer to see the mangled name, then you can specify it explicitly:

```cs
__declspec(dllexport) int __stdcall add(int a, int b)

```


  

```cs
[DllImport("myDLL.dll", EntryPoint = "?add@@YGHHH@Z")]

```


</li>



## Dynamic loading and unloading of unmanaged DLLs


When using the `DllImport` attribute you have to know the correct dll and method name at **compile time**. If you want to be more flexible and decide at **runtime** which dll and methods to load, you can use the Windows API methods `LoadLibrary()`, [`GetProcAddress()`](https://msdn.microsoft.com/en-us/library/windows/desktop/ms683212(v=vs.85).aspx) and `FreeLibrary()`. This can be helpful if the library to use depends on runtime conditions.

The pointer returned by `GetProcAddress()` can be casted into a delegate using [`Marshal.GetDelegateForFunctionPointer()`](https://msdn.microsoft.com/en-us/library/system.runtime.interopservices.marshal.getdelegateforfunctionpointer(v=vs.110).aspx).

The following code sample demonstrates this with the `myDLL.dll` from the previous examples:

```cs
class Program
{
    // import necessary API as shown in other examples
    [DllImport("kernel32.dll", SetLastError = true)]
    public static extern IntPtr LoadLibrary(string lib);
    [DllImport("kernel32.dll", SetLastError = true)]
    public static extern void FreeLibrary(IntPtr module);
    [DllImport("kernel32.dll", SetLastError = true)]
    public static extern IntPtr GetProcAddress(IntPtr module, string proc);

    // declare a delegate with the required signature
    private delegate int AddDelegate(int a, int b);

    private static void Main()
    {
        // load the dll
        IntPtr module = LoadLibrary("myDLL.dll");
        if (module == IntPtr.Zero) // error handling
        {
            Console.WriteLine($"Could not load library: {Marshal.GetLastWin32Error()}");
            return;
        }

        // get a "pointer" to the method
        IntPtr method = GetProcAddress(module, "add");
        if (method == IntPtr.Zero) // error handling
        {
            Console.WriteLine($"Could not load method: {Marshal.GetLastWin32Error()}");
            FreeLibrary(module);  // unload library
            return;
        }
            
        // convert "pointer" to delegate
        AddDelegate add = (AddDelegate)Marshal.GetDelegateForFunctionPointer(method, typeof(AddDelegate));
    
        // use function    
        int result = add(750, 300);
        
        // unload library   
        FreeLibrary(module);
    }
}

```



## Dealing with Win32 Errors


When using interop methods, you can use **GetLastError** API to get additional information on you API calls.

**DllImport Attribute SetLastError Attribute**

**SetLastError=true**

Indicates that the callee will call SetLastError (Win32 API function).

**SetLastError=false**

Indicates that the callee **will not** call SetLastError (Win32 API function), therefore you will not get an error information.

<li>
When SetLastError isn't set, it is set to false (Default value).
</li>
<li>
You can obtain the error code using Marshal.GetLastWin32Error Method:
</li>

**Example:**

```cs
[DllImport("kernel32.dll", SetLastError=true)]
public static extern IntPtr OpenMutex(uint access, bool handle, string lpName);

```

If you trying to open mutex which does not exist, GetLastError will return **ERROR_FILE_NOT_FOUND**.

```cs
var lastErrorCode = Marshal.GetLastWin32Error();

if (lastErrorCode == (uint)ERROR_FILE_NOT_FOUND)
{
    //Deal with error         
}

```

System Error Codes can be found here:

[https://msdn.microsoft.com/en-us/library/windows/desktop/ms681382(v=vs.85).aspx](https://msdn.microsoft.com/en-us/library/windows/desktop/ms681382(v=vs.85).aspx)

**GetLastError API**

There is a native **GetLastError** API which you can use as well :

```cs
[DllImport("coredll.dll", SetLastError=true)]
static extern Int32 GetLastError();

```


- When calling Win32 API from managed code, you must always use the **Marshal.GetLastWin32Error**.

Here's why:

Between your Win32 call which sets the error (calls SetLastError), the CLR can call other Win32 calls which could call **SetLastError** as well, this behavior can override your error value. In this scenario, if you call **GetLastError** you can obtain an invalid error.

Setting **SetLastError = true**, makes sure that the CLR retrieves the error code before it executes other Win32 calls.



## Reading structures with Marshal


Marshal class contains a function named **PtrToStructure**, this function gives us the ability of reading structures by an unmanaged pointer.

**PtrToStructure** function got many overloads, but they all have the same intention.

Generic **PtrToStructure**:

```cs
public static T PtrToStructure<T>(IntPtr ptr);

```

**T** - structure type.

**ptr** -   A pointer to an unmanaged block of memory.

Example:

```cs
NATIVE_STRUCT result = Marshal.PtrToStructure<NATIVE_STRUCT>(ptr);       

```


- If you dealing with managed objects while reading native structures, don't forget to pin your object :)

```cs

T Read<T>(byte[] buffer)
    {
        T result = default(T);
        
        var gch = GCHandle.Alloc(buffer, GCHandleType.Pinned);
    
        try
        {
            result = Marshal.PtrToStructure<T>(gch.AddrOfPinnedObject());
        }
        finally
        {
            gch.Free();
        }
        
        return result;
    }

```



## Simple code to expose class for com


```cs
using System;
using System.Runtime.InteropServices;
 
namespace ComLibrary
{
    [ComVisible(true)]
    public interface IMainType
    {
        int GetInt();
 
        void StartTime();
 
        int StopTime();
    }
 
    [ComVisible(true)]
    [ClassInterface(ClassInterfaceType.None)]
    public class MainType : IMainType
    {
        private Stopwatch stopWatch;
 
        public int GetInt()
        {
            return 0;
        }
 
        public void StartTime()
        {
            stopWatch= new Stopwatch();
            stopWatch.Start();
        }
 
        public int StopTime()
        {
            return (int)stopWatch.ElapsedMilliseconds;
        }
    }
}

```



## Pinned Object


**GC** (Garbage Collector) is responsible for cleaning our garbage.

While **GC** cleans our garbage, he removes the unused objects from the managed heap which cause heap fragmentation. When **GC** is done with the removal, it performs a heap compression (defragmintation) which involves moving objects on the heap.

Since **GC** isn't deterministic, when passing managed object reference/pointer to native code, **GC** can kick in at any time, if it occurs just after Inerop call, there is a very good possibility that object (which reference passed to native) will be moved on the managed heap - as a result, we get an invalid reference on managed side.

In this scenario, you should **pin** the object before passing it to native code.

**Pinned Object**

Pinned object is an object that is not allowed to move by GC.

**Gc Pinned Handle**

You can create a pin object using **Gc.Alloc** method

```cs
GCHandle handle = GCHandle.Alloc(yourObject, GCHandleType.Pinned); 

```


- Obtaining a pinned **GCHandle** to managed object marks a specific object as one that cannot be moved by **GC**, until freeing the handle

Example:

```cs
[DllImport("kernel32.dll", SetLastError = true)]
public static extern void EnterCriticalSection(IntPtr ptr);

[DllImport("kernel32.dll", SetLastError = true)]
public static extern void LeaveCriticalSection(IntPtr ptr);
       
public void EnterCriticalSection(CRITICAL_SECTION section)
{
    try
    {
        GCHandle handle = GCHandle.Alloc(section, GCHandleType.Pinned); 
        EnterCriticalSection(handle.AddrOfPinnedObject());
        //Do Some Critical Work
        LeaveCriticalSection(handle.AddrOfPinnedObject());
    }
    finaly
    {
        handle.Free()
    }
}

```

**Precautions**

- When pinning (especially large ones) object try to release the pinned **GcHandle** as fast as possible, since it interrupt heap defragmentation.
- If you forget to free **GcHandle** nothing will. Do it in a safe code section (such as finaly)



#### Remarks


**Working with Win32 API using C#**

Windows exposes lots of functionality in the form of Win32 API. Using these API you can perform direct operation in windows, which increases performance of your application.Source [Click here](http://www.c-sharpcorner.com/article/working-with-win32-api-in-net/)

Windows exposes a broad range of API. To get information about various APIs you can check sites like [pinvoke](http://pinvoke.net).

