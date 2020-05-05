---
metaTitle: "C# 5.0 Features"
description: "Async & Await, Caller Information Attributes"
---

# C# 5.0 Features



## Async & Await


`async` and `await` are two operators that are intended to improve performance by freeing up Threads and waiting for operations to complete before moving forward.

Here's an example of getting a string before returning it's length:

```cs
//This method is async because:
//1. It has async and Task or Task<T> as modifiers
//2. It ends in "Async"
async Task<int> GetStringLengthAsync(string URL){
    HttpClient client = new HttpClient();
    //Sends a GET request and returns the response body as a string
    Task<string> getString = client.GetStringAsync(URL);
    //Waits for getString to complete before returning its length
    string contents = await getString;
    return contents.Length;
}

private async void doProcess(){
    int length = await GetStringLengthAsync("http://example.com/");
    //Waits for all the above to finish before printing the number
    Console.WriteLine(length);
}

```

Here's another example of downloading a file and handling what happens when it's progress has changed and when the download completes (there are two ways to do this):

Method 1:

```cs
//This one using async event handlers, but not async coupled with await
private void DownloadAndUpdateAsync(string uri, string DownloadLocation){
    WebClient web = new WebClient();
    //Assign the event handler
    web.DownloadProgressChanged += new DownloadProgressChangedEventHandler(ProgressChanged);
    web.DownloadFileCompleted += new AsyncCompletedEventHandler(FileCompleted);
    //Download the file asynchronously
    web.DownloadFileAsync(new Uri(uri), DownloadLocation);
}

//event called for when download progress has changed
private void ProgressChanged(object sender, DownloadProgressChangedEventArgs e){
    //example code
    int i = 0;
    i++;
    doSomething();
}

//event called for when download has finished
private void FileCompleted(object sender, AsyncCompletedEventArgs e){
    Console.WriteLine("Completed!")
}

```

Method 2:

```cs
//however, this one does
//Refer to first example on why this method is async
private void DownloadAndUpdateAsync(string uri, string DownloadLocation){
    WebClient web = new WebClient();
    //Assign the event handler
    web.DownloadProgressChanged += new DownloadProgressChangedEventHandler(ProgressChanged);
    //Download the file async
    web.DownloadFileAsync(new Uri(uri), DownloadLocation);
    //Notice how there is no complete event, instead we're using techniques from the first example
}
private void ProgressChanged(object sender, DownloadProgressChangedEventArgs e){
    int i = 0;
    i++;
    doSomething();
}
private void doProcess(){
    //Wait for the download to finish
    await DownloadAndUpdateAsync(new Uri("http://example.com/file"))
    doSomething();
}

```



## Caller Information Attributes


C.I.A.s are intended as a simple way of getting attributes from whatever is calling the targeted method. There is really only 1 way to use them and there are only 3 attributes.

Example:

```cs
//This is the "calling method": the method that is calling the target method
public void doProcess()
{
    GetMessageCallerAttributes("Show my attributes.");
}
//This is the target method
//There are only 3 caller attributes
public void GetMessageCallerAttributes(string message,
    //gets the name of what is calling this method
    [System.Runtime.CompilerServices.CallerMemberName] string memberName = "",
    //gets the path of the file in which the "calling method" is in
    [System.Runtime.CompilerServices.CallerFilePath] string sourceFilePath = "",
    //gets the line number of the "calling method"
    [System.Runtime.CompilerServices.CallerLineNumber] int sourceLineNumber = 0)
{
    //Writes lines of all the attributes
    System.Diagnostics.Trace.WriteLine("Message: " + message);
    System.Diagnostics.Trace.WriteLine("Member: " + memberName);
    System.Diagnostics.Trace.WriteLine("Source File Path: " + sourceFilePath);
    System.Diagnostics.Trace.WriteLine("Line Number: " + sourceLineNumber);
}

```

Example Output:

```cs
//Message: Show my attributes.
//Member: doProcess
//Source File Path: c:\Path\To\The\File
//Line Number: 13

```



#### Syntax


<li>
****Async & Await****
</li>
<li>
public **Task** MyTask**Async**(){ doSomething(); }
await MyTaskAsync();
</li>
<li>
public **Task`<string>`** MyStringTask**Async**(){ return getSomeString(); }
string MyString = await MyStringTaskAsync();
</li>
<li>
****Caller Information Attributes****
</li>
<li>
public void MyCallerAttributes(string MyMessage,
[CallerMemberName] string MemberName = "",
[CallerFilePath] string SourceFilePath = "",
[CallerLineNumber] int LineNumber = 0)
</li>
<li>
Trace.WriteLine("My Message: " + MyMessage);
Trace.WriteLine("Member: " + MemberName);
Trace.WriteLine("Source File Path: " + SourceFilePath);
Trace.WriteLine("Line Number: " + LineNumber);
</li>



#### Parameters


|Method/Modifier with Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|`Type<T>`|`T` is the return type



#### Remarks


C# 5.0 is coupled with Visual Studio .NET 2012

