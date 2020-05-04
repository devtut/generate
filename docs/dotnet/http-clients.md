---
metaTitle: "HTTP clients"
description: "Reading GET response as string using System.Net.HttpClient, Basic HTTP downloader using System.Net.Http.HttpClient, Reading GET response as string using System.Net.HttpWebRequest, Reading GET response as string using System.Net.WebClient, Sending a POST request with a string payload using System.Net.HttpWebRequest, Sending a POST request with a string payload using System.Net.WebClient, Sending a POST request with a string payload using System.Net.HttpClient"
---

# HTTP clients



## Reading GET response as string using System.Net.HttpClient


`HttpClient` is available through [NuGet: Microsoft HTTP Client Libraries](https://www.nuget.org/packages/Microsoft.Net.Http/).

```dotnet
string requestUri = "http://www.example.com";
string responseData;

using (var client = new HttpClient())
{
    using(var response = client.GetAsync(requestUri).Result)
    {
       response.EnsureSuccessStatusCode();
       responseData = response.Content.ReadAsStringAsync().Result;
    }
}

```



## Basic HTTP downloader using System.Net.Http.HttpClient


```dotnet
using System;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Threading.Tasks;

class HttpGet
{
    private static async Task DownloadAsync(string fromUrl, string toFile)
    {
        using (var fileStream = File.OpenWrite(toFile))
        {
            using (var httpClient = new HttpClient())
            {
                Console.WriteLine("Connecting...");
                using (var networkStream = await httpClient.GetStreamAsync(fromUrl))
                {
                    Console.WriteLine("Downloading...");
                    await networkStream.CopyToAsync(fileStream);
                    await fileStream.FlushAsync();
                }
            }
        }
    }

    static void Main(string[] args)
    {
        try
        {
            Run(args).Wait();
        }
        catch (Exception ex)
        {
            if (ex is AggregateException)
                ex = ((AggregateException)ex).Flatten().InnerExceptions.First();

            Console.WriteLine("--- Error: " + 
                (ex.InnerException?.Message ?? ex.Message));
        }
    }
    static async Task Run(string[] args)
    {
        if (args.Length < 2)
        {
            Console.WriteLine("Basic HTTP downloader");
            Console.WriteLine();
            Console.WriteLine("Usage: httpget <url>[<:port>] <file>");
            return;
        }

        await DownloadAsync(fromUrl: args[0], toFile: args[1]);

        Console.WriteLine("Done!");
    }
}

```



## Reading GET response as string using System.Net.HttpWebRequest


```dotnet
string requestUri = "http://www.example.com";
string responseData;

HttpWebRequest request = (HttpWebRequest)WebRequest.Create(parameters.Uri);
WebResponse response = request.GetResponse();

using (StreamReader responseReader = new StreamReader(response.GetResponseStream()))
{
    responseData = responseReader.ReadToEnd();
}

```



## Reading GET response as string using System.Net.WebClient


```dotnet
string requestUri = "http://www.example.com";
string responseData;

using (var client = new WebClient())
{    
    responseData = client.DownloadString(requestUri);
}

```



## Sending a POST request with a string payload using System.Net.HttpWebRequest


```dotnet
string requestUri = "http://www.example.com";
string requestBodyString = "Request body string.";
string contentType = "text/plain";
string requestMethod = "POST";

HttpWebRequest request = (HttpWebRequest)WebRequest.Create(requestUri)
{
  Method = requestMethod,
  ContentType = contentType,
};

byte[] bytes = Encoding.UTF8.GetBytes(requestBodyString);
Stream stream = request.GetRequestStream();
stream.Write(bytes, 0, bytes.Length);
stream.Close();

HttpWebResponse response = (HttpWebResponse)request.GetResponse();

```



## Sending a POST request with a string payload using System.Net.WebClient


```dotnet
string requestUri = "http://www.example.com";
string requestBodyString = "Request body string.";
string contentType = "text/plain";
string requestMethod = "POST";
    
byte[] responseBody;    
byte[] requestBodyBytes = Encoding.UTF8.GetBytes(requestBodyString);

using (var client = new WebClient())
{
    client.Headers[HttpRequestHeader.ContentType] = contentType;
    responseBody = client.UploadData(requestUri, requestMethod, requestBodyBytes);
}

```



## Sending a POST request with a string payload using System.Net.HttpClient


`HttpClient` is available through [NuGet: Microsoft HTTP Client Libraries](https://www.nuget.org/packages/Microsoft.Net.Http/).

```dotnet
string requestUri = "http://www.example.com";
string requestBodyString = "Request body string.";
string contentType = "text/plain";
string requestMethod = "POST";

var request = new HttpRequestMessage
{
    RequestUri = requestUri,
    Method = requestMethod,
};

byte[] requestBodyBytes = Encoding.UTF8.GetBytes(requestBodyString);
request.Content = new ByteArrayContent(requestBodyBytes);

request.Content.Headers.ContentType = new MediaTypeHeaderValue(contentType);

HttpResponseMessage result = client.SendAsync(request).Result;
result.EnsureSuccessStatusCode();

```



#### Remarks


The currently relevant HTTP/1.1 RFCs are:

- [7230: Message Syntax and Routing](https://tools.ietf.org/html/rfc7230)
- [7231: Semantics and Content](https://tools.ietf.org/html/rfc7231)
- [7232: Conditional Requests](https://tools.ietf.org/html/rfc7232)
- [7233: Range Requests](https://tools.ietf.org/html/rfc7233)
- [7234: Caching](https://tools.ietf.org/html/rfc7234)
- [7235: Authenticaion](https://tools.ietf.org/html/rfc7235)
- [7239: Forwarded HTTP Extension](https://tools.ietf.org/html/rfc7239)
- [7240: Prefer Header for HTTP](https://tools.ietf.org/html/rfc7240)

There's also the following informational RFCs:

- [7236: Authentication Scheme Registrations](https://tools.ietf.org/html/rfc7236)
- [7237: Method Registrations](https://tools.ietf.org/html/rfc7237)

And the experimental RFC:

- [7238: The Hypertext Transfer Protocol Status Code 308 (Permanent Redirect)](https://tools.ietf.org/html/rfc7238)

Related protocols:

<li><a href="https://tools.ietf.org/html/rfc4918" rel="nofollow">4918: HTTP Extensions for Web Distributed Authoring and Versioning (WebDAV)
</a></li>
<li><a href="https://tools.ietf.org/html/rfc4791" rel="nofollow">4791: Calendaring Extensions to WebDAV (CalDAV)
</a></li>

