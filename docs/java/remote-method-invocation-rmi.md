---
metaTitle: "Java - Remote Method Invocation (RMI)"
description: "Callback: invoking methods on a client, Client-Server: invoking methods in one JVM from another, Simple RMI example with Client and Server implementation"
---

# Remote Method Invocation (RMI)



## Callback: invoking methods on a "client"


### Overview

In this example 2 clients send information to each other through a server. One client sends the server a number which is relayed to the second client. The second client halves the number and sends it back to the first client through the server. The first client does the same. The server stops the communication when the number returned to it by any of the clients is less than 10. The return value from the server to the clients (the number it got converted to string representation) then backtracks the process.

1. A login server binds itself to a registry.
<li>A client looks up the login server and calls the `login` method with its information. Then:
<ul>
1. The login server stores the client information. It includes the client's stub with the callback methods.
1. The login server creates and returns a server stub ("connection" or "session") to the client to store. It includes the server's stub with its methods including a `logout` method (unused in this example).
</ul>
</li>
1. A client calls the server's `passInt` with the name of the recipient client and an `int`.
1. The server calls the `half` on the recipient client with that `int`. This initiates a back-and-forth (calls and callbacks) communication until stopped by the server.

### The shared remote interfaces

The login server:

```java
package callbackRemote;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface RemoteLogin extends Remote {

    RemoteConnection login(String name, RemoteClient client) throws RemoteException;
}

```

The server:

```java
package callbackRemote;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface RemoteConnection extends Remote {

    void logout() throws RemoteException;

    String passInt(String name, int i) throws RemoteException;
}

```

The client:

```java
package callbackRemote;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface RemoteClient extends Remote {

    void half(int i) throws RemoteException;
}

```

### The implementations

The login server:

```java
package callbackServer;

import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.HashMap;
import java.util.Map;

import callbackRemote.RemoteClient;
import callbackRemote.RemoteConnection;
import callbackRemote.RemoteLogin;

public class LoginServer implements RemoteLogin {

    static Map<String, RemoteClient> clients = new HashMap<>();

    @Override
    public RemoteConnection login(String name, RemoteClient client) {

        Connection connection = new Connection(name, client);
        clients.put(name, client);
        System.out.println(name + " logged in");
        return connection;
    }

    public static void main(String[] args) {

        try {
            Registry reg = LocateRegistry.createRegistry(Registry.REGISTRY_PORT);
            LoginServer server = new LoginServer();
            UnicastRemoteObject.exportObject(server, Registry.REGISTRY_PORT);
            reg.rebind("LoginServerName", server);
        } catch (RemoteException e) {
            e.printStackTrace();
        }
    }
}

```

The server:

```java
package callbackServer;

import java.rmi.NoSuchObjectException;
import java.rmi.RemoteException;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.rmi.server.Unreferenced;

import callbackRemote.RemoteClient;
import callbackRemote.RemoteConnection;

public class Connection implements RemoteConnection, Unreferenced {

    RemoteClient client;
    String name;

    public Connection(String name, RemoteClient client) {

        this.client = client;
        this.name = name;
        try {
            UnicastRemoteObject.exportObject(this, Registry.REGISTRY_PORT);
        } catch (RemoteException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void unreferenced() {

        try {
            UnicastRemoteObject.unexportObject(this, true);
        } catch (NoSuchObjectException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void logout() {

        try {
            UnicastRemoteObject.unexportObject(this, true);
        } catch (NoSuchObjectException e) {
            e.printStackTrace();
        }
    }

    @Override
    public String passInt(String recipient, int i) {

        System.out.println("Server received from " + name + ":" + i);
        if (i < 10)
            return String.valueOf(i);
        RemoteClient client = LoginServer.clients.get(recipient);
        try {
            client.half(i);
        } catch (RemoteException e) {
            e.printStackTrace();
        }
        return String.valueOf(i);
    }
}

```

The client:

```java
package callbackClient;

import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;

import callbackRemote.RemoteClient;
import callbackRemote.RemoteConnection;
import callbackRemote.RemoteLogin;

public class Client implements RemoteClient {

    RemoteConnection connection;
    String name, target;

    Client(String name, String target) {

        this.name = name;
        this.target = target;
    }

    public static void main(String[] args) {

        Client client = new Client(args[0], args[1]);
        try {
            Registry reg = LocateRegistry.getRegistry();
            RemoteLogin login = (RemoteLogin) reg.lookup("LoginServerName");
            UnicastRemoteObject.exportObject(client, Integer.parseInt(args[2]));
            client.connection = login.login(client.name, client);
        } catch (RemoteException | NotBoundException e) {
            e.printStackTrace();
        }

        if ("Client1".equals(client.name)) {
            try {
                client.connection.passInt(client.target, 120);
            } catch (RemoteException e) {
                e.printStackTrace();
            }
        }
    }

    @Override
    public void half(int i) throws RemoteException {

        String result = connection.passInt(target, i / 2);
        System.out.println(name + " received: \"" + result + "\"");
    }
}

```

Running the example:

1. Run the login server.
1. Run a client with the arguments `Client2 Client1 1097`.
1. Run a client with the arguments `Client1 Client2 1098`.

The outputs will appear in 3 consoles since there are 3 JVMs. here they are lumped together:

> 
<p>Client2 logged in<br />
Client1 logged in<br />
Server received from Client1:120<br />
Server received from Client2:60<br />
Server received from Client1:30<br />
Server received from Client2:15<br />
Server received from Client1:7<br />
Client1 received: "7"<br />
Client2 received: "15"<br />
Client1 received: "30"<br />
Client2 received: "60"</p>




## Client-Server: invoking methods in one JVM from another


The shared remote interface:

```java
package remote;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface RemoteServer extends Remote {

    int stringToInt(String string) throws RemoteException;
}

```

The server implementing the shared remote interface:

```java
package server;

import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;

import remote.RemoteServer;

public class Server implements RemoteServer {

    @Override
    public int stringToInt(String string) throws RemoteException {

        System.out.println("Server received: \"" + string + "\"");
        return Integer.parseInt(string);
    }

    public static void main(String[] args) {

        try {
            Registry reg = LocateRegistry.createRegistry(Registry.REGISTRY_PORT);
            Server server = new Server();
            UnicastRemoteObject.exportObject(server, Registry.REGISTRY_PORT);
            reg.rebind("ServerName", server);
        } catch (RemoteException e) {
            e.printStackTrace();
        }
    }
}

```

The client invoking a method on the server (remotely):

```java
package client;

import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

import remote.RemoteServer;

public class Client {

    static RemoteServer server;

    public static void main(String[] args) {

        try {
            Registry reg = LocateRegistry.getRegistry();
            server = (RemoteServer) reg.lookup("ServerName");
        } catch (RemoteException | NotBoundException e) {
            e.printStackTrace();
        }

        Client client = new Client();
        client.callServer();
    }

    void callServer() {
    
        try {
            int i = server.stringToInt("120");
            System.out.println("Client received: " + i);
        } catch (RemoteException e) {
            e.printStackTrace();
        }
    }
}

```

Output:

> 
<p>Server received: "120"<br />
Client received: 120</p>




## Simple RMI example with Client and Server implementation


This is a simple RMI example with five Java classes and two packages, **server** and **client**.

### Server Package

**PersonListInterface.java**

```java
public interface PersonListInterface extends Remote
{
    /**
     * This interface is used by both client and server
     * @return List of Persons
     * @throws RemoteException
     */
    ArrayList<String> getPersonList() throws RemoteException;
}

```

**PersonListImplementation.java**

```java
public class PersonListImplementation 
extends UnicastRemoteObject 
implements PersonListInterface
{

    private static final long serialVersionUID = 1L;

    // standard constructor needs to be available
    public PersonListImplementation() throws RemoteException
    {}

    /**
     * Implementation of "PersonListInterface"
     * @throws RemoteException
     */
    @Override
    public ArrayList<String> getPersonList() throws RemoteException
    {
        ArrayList<String> personList = new ArrayList<String>();
        
        personList.add("Peter Pan");
        personList.add("Pippi Langstrumpf");
        // add your name here :)
        
        return personList;
    }
}

```

**Server.java**

```java
public class Server {

    /**
     * Register servicer to the known public methods
     */
    private static void createServer() {
        try {
            // Register registry with standard port 1099
            LocateRegistry.createRegistry(Registry.REGISTRY_PORT);
            System.out.println("Server : Registry created.");

            // Register PersonList to registry 
            Naming.rebind("PersonList", new PersonListImplementation());
            System.out.println("Server : PersonList registered");

        } catch (final IOException e) {
            e.printStackTrace();
        }
    }

    public static void main(final String[] args) {
        createServer();
    }
}

```

### Client package

**PersonListLocal.java**

```java
public class PersonListLocal {
    private static PersonListLocal instance;
    private PersonListInterface personList;

    /**
     * Create a singleton instance
     */
    private PersonListLocal() {
        try {
            // Lookup to the local running server with port 1099
            final Registry registry = LocateRegistry.getRegistry("localhost",
                    Registry.REGISTRY_PORT);

            // Lookup to the registered "PersonList"
            personList = (PersonListInterface) registry.lookup("PersonList");
        } catch (final RemoteException e) {
            e.printStackTrace();
        } catch (final NotBoundException e) {
            e.printStackTrace();
        }
    }

    public static PersonListLocal getInstance() {
        if (instance == null) {
            instance = new PersonListLocal();
        }

        return instance;
    }

    /**
     * Returns the servers PersonList
     */
    public ArrayList<String> getPersonList() {
        if (instance != null) {
            try {
                return personList.getPersonList();
            } catch (final RemoteException e) {
                e.printStackTrace();
            }
        }

        return new ArrayList<>();
    }
       }

```

**PersonTest.java**

```java
public class PersonTest
{
    public static void main(String[] args)
    {
        // get (local) PersonList 
        ArrayList<String> personList = PersonListLocal.getInstance().getPersonList();
        
        // print all persons
        for(String person : personList)
        {
            System.out.println(person);
        }
    }
}

```

### Test your application

- Start main method of Server.java. Output:

```java
Server : Registry created.
Server : PersonList registered

```


- Start main method of PersonTest.java. Output:

```java
Peter Pan
Pippi Langstrumpf

```



#### Remarks


RMI requires 3 components: client, server and a shared remote interface. The shared remote interface defines the client-server contract by specifying the methods a server must implement. The interface must be visible to the server so that it can implement the methods; the interface must be visible to the client so that it knows which methods ("services") the server provides.<br />
Any object implementing a remote interface is destined to take the role of a server. As such, a client-server relationship in which the server can also invoke methods in the client is in fact a server-server relationship. This is termed **callback** since the server can call back the "client". With  this in mind, it is acceptable to use the designation **client** for the servers that function as such.

The shared remote interface is any interface extending [`Remote`](https://docs.oracle.com/javase/8/docs/api/java/rmi/Remote.html). An object that functions as a server undergoes the following:

1. Implements the shared remote interface, either explicitly or implicitly by extending [`UnicastRemoteObject`](https://docs.oracle.com/javase/8/docs/api/java/rmi/server/UnicastRemoteObject.html) which implements `Remote`.
1. Exported, either implicitly if it extends `UnicastRemoteObject`, or explicitly by being passed to `UnicastRemoteObject#exportObject`.
1. Binded in a registry, either directly through [`Registry`](https://docs.oracle.com/javase/8/docs/api/java/rmi/registry/Registry.html) or indirectly through [`Naming`](https://docs.oracle.com/javase/8/docs/api/java/rmi/Naming.html). This is only necessary for establishing initial communication since further stubs can be passed directly through RMI.

In the project setup, the client and server projects are completely unrelated, but each specifies a shared project in its build path. The shared project contains the remote interfaces.

