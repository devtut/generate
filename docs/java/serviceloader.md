---
metaTitle: "ServiceLoader"
description: "Logger Service, Simple ServiceLoader Example"
---

# ServiceLoader



## Logger Service


The following example shows how to instantiate a class for logging via the `ServiceLoader`.

### Service

```java
package servicetest;

import java.io.IOException;

public interface Logger extends AutoCloseable {
    
    void log(String message) throws IOException;
}

```

### Implementations of the service

The following implementation simply writes the message to `System.err`

```java
package servicetest.logger;

import servicetest.Logger;

public class ConsoleLogger implements Logger {

    @Override
    public void log(String message) {
        System.err.println(message);
    }

    @Override
    public void close() {
    }

}

```

The following implementation writes the messages to a text file:

```java
package servicetest.logger;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import servicetest.Logger;

public class FileLogger implements Logger {

    private final BufferedWriter writer;

    public FileLogger() throws IOException {
        writer = new BufferedWriter(new FileWriter("log.txt"));
    }

    @Override
    public void log(String message) throws IOException {
        writer.append(message);
        writer.newLine();
    }

    @Override
    public void close() throws IOException {
        writer.close();
    }

}

```

### META-INF/services/servicetest.Logger

The `META-INF/services/servicetest.Logger` file lists the names of the `Logger` implementations.

```java
servicetest.logger.ConsoleLogger
servicetest.logger.FileLogger

```

### Usage

The following `main` method writes a message to all available loggers. The loggers are instantiated using `ServiceLoader`.

```java
public static void main(String[] args) throws Exception {
    final String message = "Hello World!";

    // get ServiceLoader for Logger
    ServiceLoader<Logger> loader = ServiceLoader.load(servicetest.Logger.class);

    // iterate through instances of available loggers, writing the message to each one
    Iterator<Logger> iterator = loader.iterator();
    while (iterator.hasNext()) {
        try (Logger logger = iterator.next()) {
            logger.log(message);
        }
    }
}

```



## Simple ServiceLoader Example


The ServiceLoader is a simple and easy to use built-in mechanism for dynamic loading of interface implementations. With the service loader - providing means for instantation (but not the wiring) - a simple dependency injection mechanism can be built in Java SE.
With the ServiceLoader interface and implementation separation becomes natural and programs can be conveniently extended. Actually a lot of Java API are implented based on the ServiceLoader

The basic concepts are

- Operating on **interfaces** of services
- Obtaining implementation(s) of the service via `ServiceLoader`
- Providing implementation of servics

Lets start with the interface and put it in a jar, named for example `accounting-api.jar`

```java
package example;

public interface AccountingService {

  long getBalance();
}

```

Now we provide an implementation of that service in a jar named `accounting-impl.jar`, containing an implementation of the service

```java
package example.impl;
import example.AccountingService;

public interface DefaultAccountingService implements AccouningService {

  public long getBalance() {
    return balanceFromDB();
  }

  private long balanceFromDB(){
    ...
  }
}

```

further, the `accounting-impl.jar` contains a file declaring that this jar provides an implementation of `AccountingService`. The file has to have a path starting with `META-INF/services/` and must have the same name as the **fully-qualified** name of the interface:

- `META-INF/services/example.AccountingService`

The content of the file is the **fully-qualfified** name of the implementation:

```java
example.impl.DefaultAccountingService

```

Given both jars are in the classpath of the program, that consumes the `AccountingService`, an instance of the Service can be obtained by using the ServiceLauncher

```java
ServiceLoader<AccountingService> loader = ServiceLoader.load(AccountingService.class)
AccountingService service = loader.next();
long balance = service.getBalance();

```

As the `ServiceLoader` is an `Iterable`, it supports multiple implementation providers, where the program may choose from:

```java
ServiceLoader<AccountingService> loader = ServiceLoader.load(AccountingService.class)
for(AccountingService service : loader) {
   //...
}

```

Note that when invoking `next()` a new instance will allways be created. If you want to re-use an instance, you have to use the `iterator()` method of the ServiceLoader or the for-each loop as shown above.



#### Remarks


`ServiceLoader` can be used to get instances of classes extending a given type(=service) that are specified in a file packed in a `.jar` file. The service that is extended/implemented is often a interface, but this is not required.

The extending/implementing classes need to provide a zero argument constructor for the `ServiceLoader` to instantiate them.

To be discovered by the `ServiceLoader` a text file with the name of the fully qualified type name of the implemented service needs to be stored inside the `META-INF/services` directory in the jar file. This file contains one fully qualified name of a class implementing the service per line.

