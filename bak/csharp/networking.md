---
metaTitle: "C# | Networking"
description: "Basic TCP Communication Client, Download a file from a web server, Async TCP Client, Basic UDP Client"
---

# Networking




## Basic TCP Communication Client


This code example creates a TCP client, sends "Hello World" over the socket connection, and then writes the server response to the console before closing the connection.

```cs
// Declare Variables
string host = "stackoverflow.com";
int port = 9999;
int timeout = 5000;

// Create TCP client and connect
using (var _client = new TcpClient(host, port))
using (var _netStream = _client.GetStream()) 
{
    _netStream.ReadTimeout = timeout;

    // Write a message over the socket
    string message = "Hello World!";
    byte[] dataToSend = System.Text.Encoding.ASCII.GetBytes(message);
    _netStream.Write(dataToSend, 0, dataToSend.Length);
    
    // Read server response
    byte[] recvData = new byte[256];
    int bytes = _netStream.Read(recvData, 0, recvData.Length);
    message = System.Text.Encoding.ASCII.GetString(recvData, 0, bytes);
    Console.WriteLine(string.Format("Server: {0}", message));                
};// The client and stream will close as control exits the using block (Equivilent but safer than calling Close();

```



## Download a file from a web server


Downloading a file from the internet is a very common task required by almost every application your likely to build.

To accomplish this, you can use the "[System.Net.WebClient](https://msdn.microsoft.com/en-us/library/system.net.webclient.aspx%22System.Net.WebClient%22)" class.

The simplest use of this, using the "using" pattern, is shown below:

```cs
using (var webClient = new WebClient())
{
    webClient.DownloadFile("http://www.server.com/file.txt", "C:\\file.txt");
}

```

What this example does is it uses "using" to make sure that your web client is cleaned up correctly when finished, and simply transfers the named resource from the URL in the first parameter, to the named file on your local hard drive in the second parameter.

The first parameter is of type "[System.Uri](https://msdn.microsoft.com/en-us/library/system.uri.aspx%22System.Uri%22)", the second parameter is of type "[System.String](https://msdn.microsoft.com/en-us/library/system.string.aspx%22System.String%22)"

You can also use this function is an async form, so that it goes off and performs the download in the background, while your application get's on with something else, using the call in this way is of major importance in modern applications, as it helps to keep your user interface responsive.

When you use the Async methods, you can hook up event handlers that allow you to monitor the progress, so that you could for example, update a progress bar, something like the following:

```cs
var webClient = new WebClient())
webClient.DownloadFileCompleted += new AsyncCompletedEventHandler(Completed);
webClient.DownloadProgressChanged += new DownloadProgressChangedEventHandler(ProgressChanged);
webClient.DownloadFileAsync("http://www.server.com/file.txt", "C:\\file.txt");

```

One important point to remember if you use the Async versions however, and that's "Be very carefull about using them in a 'using' syntax".

The reason for this is quite simple.  Once you call the download file method, it will return immediately.  If you have this in a using block, you will return then exit that block, and immediately dispose the class object, and thus cancel your download in progress.

If you use the 'using' way of performing an Async transfer, then be sure to stay inside the enclosing block until the transfer completes.



## Async TCP Client


Using `async/await` in C# applications simplifies multi-threading. This is how you can use `async/await` in conjunction with a TcpClient.

```cs
// Declare Variables
string host = "stackoverflow.com";
int port = 9999;
int timeout = 5000;

// Create TCP client and connect
// Then get the netstream and pass it
// To our StreamWriter and StreamReader
using (var client = new TcpClient())
using (var netstream = client.GetStream()) 
using (var writer = new StreamWriter(netstream))
using (var reader = new StreamReader(netstream))
{
    // Asynchronsly attempt to connect to server
    await client.ConnectAsync(host, port);
    
    // AutoFlush the StreamWriter
    // so we don't go over the buffer
    writer.AutoFlush = true;
    
    // Optionally set a timeout
    netstream.ReadTimeout = timeout;

    // Write a message over the TCP Connection
    string message = "Hello World!";
    await writer.WriteLineAsync(message);
    
    // Read server response
    string response = await reader.ReadLineAsync();
    Console.WriteLine(string.Format($"Server: {response}"));                
}
// The client and stream will close as control exits
// the using block (Equivilent but safer than calling Close();

```



## Basic UDP Client


This code example creates a UDP client then sends "Hello World" across the network to the intended recipient.  A listener does not have to be active, as UDP Is connectionless and will broadcast the message regardless.  Once the message is sent, the clients work is done.

```cs
byte[] data = Encoding.ASCII.GetBytes("Hello World");
string ipAddress = "192.168.1.141";
string sendPort = 55600;
try
{
     using (var client = new UdpClient())
     {
         IPEndPoint ep = new IPEndPoint(IPAddress.Parse(ipAddress), sendPort);
         client.Connect(ep);
         client.Send(data, data.Length);
     }
}
catch (Exception ex)
{
     Console.WriteLine(ex.ToString());
}

```

Below is an example of a UDP listener to complement the above client.  It will constantly sit and listen for traffic on a given port and simply write that data to the console.  This example contains a control flag '`done`' that is not set internally and relies on something to set this to allow for ending the listener and exiting.

```cs
bool done = false;
int listenPort = 55600;
using(UdpClinet listener = new UdpClient(listenPort))
{
    IPEndPoint listenEndPoint = new IPEndPoint(IPAddress.Any, listenPort);
    while(!done)
    {
        byte[] receivedData = listener.Receive(ref listenPort);

        Console.WriteLine("Received broadcast message from client {0}", listenEndPoint.ToString());

        Console.WriteLine("Decoded data is:");
        Console.WriteLine(Encoding.ASCII.GetString(receivedData)); //should be "Hello World" sent from above client
    }
}

```



#### Syntax


- TcpClient(string host, int port);



#### Remarks


You can get the `NetworkStream` from a `TcpClient` with `client.GetStream()` and pass it into a `StreamReader/StreamWriter` to gain access to their async read and write methods.

