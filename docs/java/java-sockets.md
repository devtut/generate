---
metaTitle: "Java Sockets"
description: "A simple TCP echo back server"
---

# Java Sockets


Sockets are a low-level network interface that helps in creating a connection between two program mainly clients which may or may not be running on the same machine.

Socket Programming is one of the most widely used networking concepts.



## A simple TCP echo back server


Our TCP echo back server will be a separate thread.
It's simple as its a start. It will just echo back whatever you send it but in capitalised form.

```java
public class CAPECHOServer extends Thread{

    // This class implements server sockets. A server socket waits for requests to come 
    // in over the network only when it is allowed through the local firewall
    ServerSocket serverSocket;
    
    public CAPECHOServer(int port, int timeout){
        try {
            // Create a new Server on specified port.
            serverSocket = new ServerSocket(port);
            // SoTimeout is basiacally the socket timeout.
            // timeout is the time until socket timeout in milliseconds
            serverSocket.setSoTimeout(timeout);
        } catch (IOException ex) {
            Logger.getLogger(CAPECHOServer.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
    @Override
    public void run(){ 
        try {
            // We want the server to continuously accept connections
            while(!Thread.interrupted()){
                
            }
            // Close the server once done.
            serverSocket.close();
        } catch (IOException ex) {
            Logger.getLogger(CAPECHOServer.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
}

```

Now to accept connections. Let's update the run method.

```java
@Override
public void run(){ 
    while(!Thread.interrupted()){
        try {
            // Log with the port number and machine ip
            Logger.getLogger((this.getClass().getName())).log(Level.INFO, "Listening for Clients at {0} on {1}", new Object[]{serverSocket.getLocalPort(), InetAddress.getLocalHost().getHostAddress()});
            Socket client = serverSocket.accept();  // Accept client conncetion
            // Now get DataInputStream and DataOutputStreams
            DataInputStream istream = new DataInputStream(client.getInputStream()); // From client's input stream
            DataOutputStream ostream = new DataOutputStream(client.getOutputStream());
            // Important Note
            /*
                The server's input is the client's output
                The client's input is the server's output
            */
            // Send a welcome message
            ostream.writeUTF("Welcome!");
            
            // Close the connection
            istream.close();
            ostream.close();
            client.close();
        } catch (IOException ex) {
            Logger.getLogger(CAPECHOServer.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
    // Close the server once done
    
    try {
        serverSocket.close();
    } catch (IOException ex) {
        Logger.getLogger(CAPECHOServer.class.getName()).log(Level.SEVERE, null, ex);
    }
}

```

Now if you can open telnet and try connecting
You'll see a Welcome message.

You must connect with the port you specified and IP Adress.

You should see a result similar to this:

```java
Welcome!

Connection to host lost.

```

Well, the connection was lost because we terminated it.
Sometimes we would have to program our own TCP client.
In this case, we need a client to request input from the user and send it across the network, receive the capitalised input.

If the server sends data first, then the client must read the data first.

```java
public class CAPECHOClient extends Thread{

Socket server;
Scanner key; // Scanner for input

    public CAPECHOClient(String ip, int port){
        try {
            server = new Socket(ip, port);
            key = new Scanner(System.in);
        } catch (IOException ex) {
            Logger.getLogger(CAPECHOClient.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
    @Override
    public void run(){
        DataInputStream istream = null;
        DataOutputStream ostream = null;
        try {
            istream = new DataInputStream(server.getInputStream()); // Familiar lines
            ostream = new DataOutputStream(server.getOutputStream());
            System.out.println(istream.readUTF());  // Print what the server sends
            System.out.print(">");
            String tosend = key.nextLine();
            ostream.writeUTF(tosend);   // Send whatever the user typed to the server
            System.out.println(istream.readUTF());  // Finally read what the server sends before exiting.
        } catch (IOException ex) {
            Logger.getLogger(CAPECHOClient.class.getName()).log(Level.SEVERE, null, ex);
        } finally {
            try {
                istream.close();
                ostream.close();
                server.close();
            } catch (IOException ex) {
                Logger.getLogger(CAPECHOClient.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }
}

```

Now update the server

```java
ostream.writeUTF("Welcome!");
            
String inString = istream.readUTF();    // Read what the user sent
String outString = inString.toUpperCase();  // Change it to caps
ostream.writeUTF(outString);
            
// Close the connection
istream.close();

```

And now run the server and client,
You should have an output similar to this

```java
Welcome!
>

```



#### Remarks


There are two types of Internet Protocol Traffic -<br />
1. TCP - Transmission Control Protocol
2. UDP - User Datagram Protocol

TCP is a connection-oriented protocol.<br />
UDP is a connectionless protocol.

TCP is suited for applications that require high reliability, and transmission time is relatively less critical.

UDP is suitable for applications that need fast, efficient transmission, such as games. UDP's stateless nature is also useful for servers that answer small queries from huge numbers of clients.

In simpler words -<br />
Use TCP when you cannot afford to loose data and when time to send and receive data doesn't matter.
Use UDP when you cannot afford to loose time and when loss of data doesn't matter.

There is an absolute guarantee that the data transferred remains intact and arrives in the same order in which it was sent in case of TCP.<br />
whereas there is no guarantee that the messages or packets sent would reach at all in UDP.

