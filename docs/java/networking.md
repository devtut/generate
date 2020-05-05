---
metaTitle: "Java - Networking"
description: "Basic Client and Server Communication using a Socket, Basic Client/Server Communication using UDP (Datagram), Loading TrustStore and KeyStore from InputStream, Socket example - reading a web page using a simple socket, Multicasting, Temporarily disable SSL verification (for testing purposes), Downloading a file using Channel"
---

# Networking



## Basic Client and Server Communication using a Socket


### Server: Start, and wait for incoming connections

```java
//Open a listening "ServerSocket" on port 1234.
ServerSocket serverSocket = new ServerSocket(1234); 

while (true) {
    // Wait for a client connection.
    // Once a client connected, we get a "Socket" object
    // that can be used to send and receive messages to/from the newly 
    // connected client
    Socket clientSocket = serverSocket.accept();            
    
    // Here we'll add the code to handle one specific client.
}

```

### Server: Handling clients

We'll handle each client in a separate thread so multiple clients could interact with the server at the same time. This technique works fine as long as the number of clients is low (<< 1000 clients, depending on the OS architecture and the expected load of each thread).

```java
new Thread(() -> {
    // Get the socket's InputStream, to read bytes from the socket
    InputStream in = clientSocket.getInputStream();
    // wrap the InputStream in a reader so you can read a String instead of bytes
    BufferedReader reader = new BufferedReader(
            new InputStreamReader(in, StandardCharsets.UTF_8));
    // Read text from the socket and print line by line
    String line;
    while ((line = reader.readLine()) != null) {
        System.out.println(line);
    }
    }).start();

```

### Client: Connect to the server and send a message

```java
// 127.0.0.1 is the address of the server (this is the localhost address; i.e.
// the address of our own machine)
// 1234 is the port that the server will be listening on
Socket socket = new Socket("127.0.0.1", 1234);

// Write a string into the socket, and flush the buffer
OutputStream outStream = socket.getOutputStream();
PrintWriter writer = new PrintWriter(
        new OutputStreamWriter(outStream, StandardCharsets.UTF_8));
writer.println("Hello world!");
writer.flush();

```

### Closing Sockets and Handling Exceptions

The above examples left out some things to make them easier to read.

<li>
Just like files and other external resources, it's important we tell the OS when we're done with them. When we're done with a socket, call `socket.close()` to properly close it.
</li>
<li>
<p>Sockets handle I/O (Input/Output) operations that depend on a variety of external factors.
For example what if the other side suddenly disconnects? What if there are network error? These things are beyond our control.
This is why many socket operations might throw exceptions, especially `IOException`.</p>
</li>

A more complete code for the client would therefore be something like this:

```

// "try-with-resources" will close the socket once we leave its scope
 try (Socket socket = new Socket("127.0.0.1", 1234)) {
     OutputStream outStream = socket.getOutputStream();
     PrintWriter writer = new PrintWriter(
             new OutputStreamWriter(outStream, StandardCharsets.UTF_8));
     writer.println("Hello world!");
     writer.flush();
 } catch (IOException e) {
     //Handle the error
 }

```

### Basic Server and Client - complete examples

Server:

```java
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.charset.StandardCharsets;

public class Server {
    public static void main(String args[]) {
        try (ServerSocket serverSocket = new ServerSocket(1234)) {
            while (true) {
                // Wait for a client connection.
                Socket clientSocket = serverSocket.accept();
                
                // Create and start a thread to handle the new client
                new Thread(() -> {
                    try {
                        // Get the socket's InputStream, to read bytes 
                        // from the socket
                        InputStream in = clientSocket.getInputStream();
                        // wrap the InputStream in a reader so you can 
                        // read a String instead of bytes
                        BufferedReader reader = new BufferedReader(
                             new InputStreamReader(in, StandardCharsets.UTF_8));
                        // Read from the socket and print line by line
                        String line;
                        while ((line = reader.readLine()) != null) {
                            System.out.println(line);
                        }
                    }
                    catch (IOException e) {
                        e.printStackTrace();
                    } finally {
                        // This finally block ensures the socket is closed.
                        // A try-with-resources block cannot be used because
                        // the socket is passed into a thread, so it isn't 
                        // created and closed in the same block
                        try {
                            clientSocket.close();
                        } catch (IOException e) {
                            e.printStackTrace();
                        }
                    }
                }).start();
            }
        }
        catch (IOException e) {
            e.printStackTrace();
        }

    }
}

```

Client:

```java
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.Socket;
import java.nio.charset.StandardCharsets;

public class Client {
    public static void main(String args[]) {
        try (Socket socket = new Socket("127.0.0.1", 1234)) {
            // We'll reach this code once we've connected to the server
            
            // Write a string into the socket, and flush the buffer
            OutputStream outStream = socket.getOutputStream();
            PrintWriter writer = new PrintWriter(
                    new OutputStreamWriter(outStream, StandardCharsets.UTF_8));
            writer.println("Hello world!");
            writer.flush();
        } catch (IOException e) {
            // Exception should be handled.
            e.printStackTrace();
        }
    }
}

```



## Basic Client/Server Communication using UDP (Datagram)


Client.java

```java
import java.io.*;
import java.net.*;
    
public class Client{
    public static void main(String [] args) throws IOException{
        DatagramSocket clientSocket = new DatagramSocket(); 
        InetAddress address = InetAddress.getByName(args[0]);

        String ex = "Hello, World!";
        byte[] buf = ex.getBytes();

        DatagramPacket packet = new DatagramPacket(buf,buf.length, address, 4160); 
        clientSocket.send(packet);
    }
}

```

In this case, we pass in the address of the server, via an argument (`args[0]`). The port we are using is 4160.

Server.java

```java
import java.io.*;
import java.net.*;

public class Server{
    public static void main(String [] args) throws IOException{
        DatagramSocket serverSocket = new DatagramSocket(4160);

        byte[] rbuf = new byte[256];
        DatagramPacket packet = new DatagramPacket(rbuf, rbuf.length);        
        serverSocket.receive(packet);
        String response = new String(packet.getData());
        System.out.println("Response: " + response);
    }
}

```

On the server-side, declare a DatagramSocket on the same port which we sent our message to (4160) and wait for a response.



## Loading TrustStore and KeyStore from InputStream


```java
public class TrustLoader {
    
    public static void main(String args[]) {
        try {
                //Gets the inputstream of a a trust store file under ssl/rpgrenadesClient.jks
                //This path refers to the ssl folder in the jar file, in a jar file in the same directory
                //as this jar file, or a different directory in the same directory as the jar file
                InputStream stream = TrustLoader.class.getResourceAsStream("/ssl/rpgrenadesClient.jks");
                //Both trustStores and keyStores are represented by the KeyStore object
                KeyStore trustStore = KeyStore.getInstance(KeyStore.getDefaultType());
                //The password for the trustStore
                char[] trustStorePassword = "password".toCharArray();
                //This loads the trust store into the object
                trustStore.load(stream, trustStorePassword);
                
                //This is defining the SSLContext so the trust store will be used
                //Getting default SSLContext to edit.
                SSLContext context = SSLContext.getInstance("SSL");
                //TrustMangers hold trust stores, more than one can be added
                TrustManagerFactory factory = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
                //Adds the truststore to the factory
                factory.init(trustStore);
                //This is passed to the SSLContext init method
                TrustManager[] managers = factory.getTrustManagers();
                context.init(null, managers, null);
                //Sets our new SSLContext to be used.
                SSLContext.setDefault(context);
            } catch (KeyStoreException | IOException | NoSuchAlgorithmException 
                    | CertificateException | KeyManagementException ex) {
                //Handle error
                ex.printStackTrace();
            }
        
    }
}

```

Intiating a KeyStore works the same, except replace any word `Trust` in a object name with `Key`. Additionally, the `KeyManager[]` array must be passed to the the first argument of `SSLContext.init`. That is `SSLContext.init(keyMangers, trustMangers, null)`



## Socket example - reading a web page using a simple socket


```java
import java.io.*;
import java.net.Socket;

public class Main {

    public static void main(String[] args) throws IOException {//We don't handle Exceptions in this example 
        //Open a socket to stackoverflow.com, port 80
        Socket socket = new Socket("stackoverflow.com",80);

        //Prepare input, output stream before sending request
        OutputStream outStream = socket.getOutputStream();
        InputStream inStream = socket.getInputStream();
        BufferedReader reader = new BufferedReader(new InputStreamReader(inStream));
        PrintWriter writer = new PrintWriter(new BufferedOutputStream(outStream));

        //Send a basic HTTP header
        writer.print("GET / HTTP/1.1\nHost:stackoverflow.com\n\n");
        writer.flush();

        //Read the response
        System.out.println(readFully(reader));

        //Close the socket
        socket.close();
    }
    
    private static String readFully(Reader in) {
        StringBuilder sb = new StringBuilder();
        int BUFFER_SIZE=1024;
        char[] buffer = new char[BUFFER_SIZE]; // or some other size, 
        int charsRead = 0;
        while ( (charsRead  = rd.read(buffer, 0, BUFFER_SIZE)) != -1) {
          sb.append(buffer, 0, charsRead);
        }
    }
}

```

You should get a response that starts with `HTTP/1.1 200 OK`, which indicates a normal HTTP response, followed by the rest of the HTTP header, followed by the raw web page in HTML form.

Note the `readFully()` method is important to prevent a premature EOF exception. The last line of the web page may be missing a return, to signal the end of line, then `readLine()` will complain, so one must read it by hand or use utility methods from [Apache commons-io IOUtils](https://github.com/apache/commons-io/blob/2.5/src/main/java/org/apache/commons/io/IOUtils.java)

This example is meant as a simple demonstration of connecting to an existing resource using a socket, it's not a practical way of accessing web pages. If you need to access a web page using Java, it's best to use an existing HTTP client library such as [Apache's HTTP Client](https://hc.apache.org/httpcomponents-client-ga/) or [Google's HTTP Client](https://developers.google.com/api-client-library/java/google-http-java-client/)



## Multicasting


Multicasting is a type of Datagram Socket. Unlike regular Datagrams, Multicasting doesn't handle each client individually instead it sends it out to one IP Address and all subscribed clients will get the message.

[<img src="http://i.stack.imgur.com/QW3lT.png" alt="enter image description here" />](http://i.stack.imgur.com/QW3lT.png)

Example code for a server side:

```java
public class Server {
    
    private DatagramSocket serverSocket;
    
    private String ip;
    
    private int port;
    
    public Server(String ip, int port) throws SocketException, IOException{
        this.ip = ip;
        this.port = port;
        // socket used to send
        serverSocket = new DatagramSocket();
    }
    
    public void send() throws IOException{
        // make datagram packet
        byte[] message = ("Multicasting...").getBytes();
        DatagramPacket packet = new DatagramPacket(message, message.length, 
            InetAddress.getByName(ip), port);
        // send packet
        serverSocket.send(packet);
    }
    
    public void close(){
        serverSocket.close();
    }
}

```

Example code for a client side:

```java
public class Client {
    
    private MulticastSocket socket;
    
    public Client(String ip, int port) throws IOException {
        
        // important that this is a multicast socket
        socket = new MulticastSocket(port);
        
        // join by ip
        socket.joinGroup(InetAddress.getByName(ip));
    }
    
    public void printMessage() throws IOException{
        // make datagram packet to recieve
        byte[] message = new byte[256];
        DatagramPacket packet = new DatagramPacket(message, message.length);
        
        // recieve the packet
        socket.receive(packet);
        System.out.println(new String(packet.getData()));
    }
    
    public void close(){
        socket.close();
    }
}

```

Code for running the Server:

```java
public static void main(String[] args) {
    try {
        final String ip = args[0];
        final int port = Integer.parseInt(args[1]);
        Server server = new Server(ip, port);
        server.send();
        server.close();
    } catch (IOException ex) {
        ex.printStackTrace();
    }
}

```

Code for running a Client:

```java
public static void main(String[] args) {
    try {
        final String ip = args[0];
        final int port = Integer.parseInt(args[1]);
        Client client = new Client(ip, port);
        client.printMessage();
        client.close();
    } catch (IOException ex) {
        ex.printStackTrace();
    }
}

```

**Run the Client First:** The Client must subscribe to the IP before it can start receiving any packets. If you start the server and call the `send()` method, and then make a client (& call `printMessage()`). Nothing will happen because the client connected after the message was sent.



## Temporarily disable SSL verification (for testing purposes)


Sometimes in a development or testing environment, the SSL certificate chain might not have been fully established (yet).

To continue developing and testing, you can turn off SSL verification programmatically by installing an "all-trusting" trust manager:

```java
try {
   // Create a trust manager that does not validate certificate chains
   TrustManager[] trustAllCerts = new TrustManager[] {
      new X509TrustManager() {
       public X509Certificate[] getAcceptedIssuers() {
           return null;
       }
       public void checkClientTrusted(X509Certificate[] certs, String authType) {
       }
       public void checkServerTrusted(X509Certificate[] certs, String authType) {
       }
      }
   };

   // Install the all-trusting trust manager
   SSLContext sc = SSLContext.getInstance("SSL");
   sc.init(null, trustAllCerts, new java.security.SecureRandom());
   HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory());

   // Create all-trusting host name verifier
   HostnameVerifier allHostsValid = new HostnameVerifier() {
       public boolean verify(String hostname, SSLSession session) {
           return true;
       }
   };

   // Install the all-trusting host verifier
   HttpsURLConnection.setDefaultHostnameVerifier(allHostsValid);
} catch (NoSuchAlgorithmException | KeyManagementException e) {
    e.printStackTrace();
}

```



## Downloading a file using Channel


**If the file already exists, it will be overwritten!**

```java
String fileName     = "file.zip";                  // name of the file
String urlToGetFrom = "http://www.mywebsite.com/"; // URL to get it from
String pathToSaveTo = "C:\\Users\\user\\";         // where to put it

//If the file already exists, it will be overwritten!
  
//Opening OutputStream to the destination file
try (ReadableByteChannel rbc = 
      Channels.newChannel(new URL(urlToGetFrom + fileName).openStream()) ) {
    try ( FileChannel channel = 
        new FileOutputStream(pathToSaveTo + fileName).getChannel(); ) {
      channel.transferFrom(rbc, 0, Long.MAX_VALUE);
    }
    catch (FileNotFoundException e) { /* Output directory not found */ }
    catch (IOException e)           { /* File IO error */ }
}
catch (MalformedURLException e)     { /* URL is malformed */ }
catch (IOException e)               { /* IO error connecting to website */ }

```

### Notes

- Don't leave the catch blocks empty!
- In case of error, check if the remote file exists
- This is a blocking operation, can take long time with large files



#### Syntax


- new Socket("localhost", 1234); //Connects to a server at address "localhost" and port 1234
- new SocketServer("localhost", 1234); //Creates a socket server that can listen for new sockets at address localhost and port 1234
- socketServer.accept(); //Accepts a new Socket object which can be used to communicate with the client

