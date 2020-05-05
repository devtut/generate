---
metaTitle: "Java - HttpURLConnection"
description: "Get response body from a URL as a String, POST data, Delete resource, Check if resource exists"
---

# HttpURLConnection




## Get response body from a URL as a String


```java
String getText(String url) throws IOException {
    HttpURLConnection connection = (HttpURLConnection) new URL(url).openConnection();
    //add headers to the connection, or check the status if desired..
    
    // handle error response code it occurs
    int responseCode = conn.getResponseCode();
    InputStream inputStream;
    if (200 <= responseCode && responseCode <= 299) {
        inputStream = connection.getInputStream();
    } else {
        inputStream = connection.getErrorStream();
    }

    BufferedReader in = new BufferedReader(
        new InputStreamReader(
            inputStream));

    StringBuilder response = new StringBuilder();
    String currentLine;

    while ((currentLine = in.readLine()) != null) 
        response.append(currentLine);

    in.close();

    return response.toString();
}

```

This will download text data from the specified URL, and return it as a String.

**How this works:**

<li>
First, we create a `HttpUrlConnection` from our URL, with `new URL(url).openConnection()`. We cast the `UrlConnection` this returns to a `HttpUrlConnection`, so we have access to things like adding headers (such as User Agent), or checking the response code. (This example does not do that, but it's easy to add.)
</li>
<li>
Then, create `InputStream` basing on the response code (for error handling)
</li>
<li>
Then, create a `BufferedReader` which allows us to read text from `InputStream` we get from the connection.
</li>
<li>
Now, we append the text to a `StringBuilder`, line by line.
</li>
<li>
Close the `InputStream`, and return the String we now have.
</li>

**Notes:**

<li>
This method will throw an `IoException` in case of failure (such as a network error, or no internet connection), and it will also throw an **unchecked** `MalformedUrlException` if the given URL is not valid.
</li>
<li>
It can be used for reading from any URL which returns text, such as webpages (HTML), REST APIs which return JSON or XML, etc.
</li>
<li>
See also: [Read URL to String in few lines of Java code](http://stackoverflow.com/questions/4328711/read-url-to-string-in-few-lines-of-java-code).
</li>

Usage:

Is very simple:

```java
String text = getText(”http://example.com");
//Do something with the text from example.com, in this case the HTML.

```



## POST data


```java
public static void post(String url, byte [] data, String contentType) throws IOException {
    HttpURLConnection connection = null;
    OutputStream out = null;
    InputStream in = null;

    try {
        connection = (HttpURLConnection) new URL(url).openConnection();
        connection.setRequestProperty("Content-Type", contentType);
        connection.setDoOutput(true);

        out = connection.getOutputStream();
        out.write(data);
        out.close();

        in = connection.getInputStream();
        BufferedReader reader = new BufferedReader(new InputStreamReader(in));
        String line = null;
        while ((line = reader.readLine()) != null) {
            System.out.println(line);
        }
        in.close();

    } finally {
        if (connection != null) connection.disconnect();
        if (out != null) out.close();
        if (in != null) in.close();
    }
}

```

This will POST data to the specified URL, then read the response line-by-line.

### How it works

- As usual we obtain the `HttpURLConnection` from a `URL`.
- Set the content type using `setRequestProperty`, by default it's `application/x-www-form-urlencoded`
- `setDoOutput(true)` tells the connection that we will send data.
- Then we obtain the `OutputStream` by calling `getOutputStream()` and write data to it. Don't forget to close it after you are done.
- At last we read the server response.



## Delete resource


```

  public static void delete (String urlString, String contentType) throws IOException {
        HttpURLConnection connection = null;
    
        try {
            URL url = new URL(urlString);
            connection = (HttpURLConnection) url.openConnection();
            connection.setDoInput(true);
            connection.setRequestMethod("DELETE");
            connection.setRequestProperty("Content-Type", contentType);
    
            Map<String, List<String>> map = connection.getHeaderFields();
            StringBuilder sb = new StringBuilder();
            Iterator<Map.Entry<String, String>> iterator = responseHeader.entrySet().iterator();
            while(iterator.hasNext())
            {
                Map.Entry<String, String> entry = iterator.next();
                sb.append(entry.getKey());
                sb.append('=').append('"');
                sb.append(entry.getValue());
                sb.append('"');
                if(iterator.hasNext())
                {
                    sb.append(',').append(' ');
                }
            }
            System.out.println(sb.toString());
    
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if (connection != null) connection.disconnect();
        }
    }

```

This will DELETE the resource in the specified URL, then print the response header.

### How it works

- we obtain the `HttpURLConnection` from a `URL`.
- Set the content type using `setRequestProperty`, by default it's `application/x-www-form-urlencoded`
- `setDoInput(true)` tells the connection that we intend to use the URL connection for input.
- `setRequestMethod("DELETE")` to perform HTTP DELETE

At last we print the server response header.



## Check if resource exists


```java
/**
 * Checks if a resource exists by sending a HEAD-Request.
 * @param url The url of a resource which has to be checked.
 * @return true if the response code is 200 OK.
 */
public static final boolean checkIfResourceExists(URL url) throws IOException {
    HttpURLConnection conn = (HttpURLConnection) url.openConnection();
    conn.setRequestMethod("HEAD");
    int code = conn.getResponseCode();
    conn.disconnect();
    return code == 200;
}

```

### Explanation:

If you are just checking if a resource exists, it better to use a HEAD request than a GET. This avoids the overhead of transferring the resource.

Note that the method only returns `true` if the response code is `200`.  If you anticipate redirect (i.e. 3XX) responses, then the method may need to be enhanced to honor them.

### Example:

```java
checkIfResourceExists(new URL("http://images.google.com/")); // true
checkIfResourceExists(new URL("http://pictures.google.com/")); // false

```



#### Remarks


<li>
Using HttpUrlConnection on Android requires that you add the Internet permission to your app (in the `AndroidManifest.xml`).
</li>
<li>
There are also other Java HTTP clients and libraries, such as Square's [OkHttp](http://github.com/square/okhttp/), which are easier to use, and may offer better performance or more features.
</li>

