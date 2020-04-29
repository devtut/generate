---
metaTitle: "JMX"
description: "Simple example with Platform MBean Server"
---

# JMX


The JMX technology provides the tools for building distributed, Web-based, modular and dynamic solutions for managing and monitoring devices, applications, and service-driven networks. By design, this standard is suitable for adapting legacy systems, implementing new management and monitoring solutions, and plugging into those of the future.



## Simple example with Platform MBean Server


Let's say we have some server that registers new users and greets them with some message. And we want to monitor this server and change some of it's parameters.

First, we need an interface with our monitoring and control methods

```java
public interface UserCounterMBean {
    long getSleepTime();

    void setSleepTime(long sleepTime);

    int getUserCount();

    void setUserCount(int userCount);

    String getGreetingString();

    void setGreetingString(String greetingString);

    void stop();
}

```

And some simple implementation that will let us see how it's working and how we affect it

```java
public class UserCounter implements UserCounterMBean, Runnable {
    private AtomicLong sleepTime = new AtomicLong(10000);
    private AtomicInteger userCount = new AtomicInteger(0);
    private AtomicReference<String> greetingString = new AtomicReference<>("welcome");
    private AtomicBoolean interrupted = new AtomicBoolean(false);

    @Override
    public long getSleepTime() {
        return sleepTime.get();
    }

    @Override
    public void setSleepTime(long sleepTime) {
        this.sleepTime.set(sleepTime);
    }

    @Override
    public int getUserCount() {
        return userCount.get();
    }

    @Override
    public void setUserCount(int userCount) {
        this.userCount.set(userCount);
    }

    @Override
    public String getGreetingString() {
        return greetingString.get();
    }

    @Override
    public void setGreetingString(String greetingString) {
        this.greetingString.set(greetingString);
    }

    @Override
    public void stop() {
        this.interrupted.set(true);
    }

    @Override
    public void run() {
        while (!interrupted.get()) {
            try {
                System.out.printf("User %d, %s%n", userCount.incrementAndGet(), greetingString.get());
                Thread.sleep(sleepTime.get());
            } catch (InterruptedException ignored) {
            }
        }
    }
}

```

For simple example with local or remote management, we need to register our MBean:

```java
import javax.management.InstanceAlreadyExistsException;
import javax.management.MBeanRegistrationException;
import javax.management.MBeanServer;
import javax.management.MalformedObjectNameException;
import javax.management.NotCompliantMBeanException;
import javax.management.ObjectName;
import java.lang.management.ManagementFactory;

public class Main {
    public static void main(String[] args) throws MalformedObjectNameException, NotCompliantMBeanException, InstanceAlreadyExistsException, MBeanRegistrationException, InterruptedException {
        final UserCounter userCounter = new UserCounter();
        final MBeanServer mBeanServer = ManagementFactory.getPlatformMBeanServer();
        final ObjectName objectName = new ObjectName("ServerManager:type=UserCounter");
        mBeanServer.registerMBean(userCounter, objectName);

        final Thread thread = new Thread(userCounter);
        thread.start();
        thread.join();
    }
}

```

After that we can run our application and connect to it via jConsole, which can be found in your `$JAVA_HOME/bin` directory.
First, we need to find our local java process with our application
[<img src="https://i.stack.imgur.com/21xsM.png" alt="enter image description here" />](https://i.stack.imgur.com/21xsM.png)

then switch to MBeans tab and find that MBean that we used in our Main class as an `ObjectName` (in the example above it's `ServerManager`).
In `Attributes` section we can see out attributes. If you specified get method only, attribute will be readable but not writeable. If you specified both get and set methods, attribute would be readable and writeable.
[<img src="https://i.stack.imgur.com/tlDAo.png" alt="enter image description here" />](https://i.stack.imgur.com/tlDAo.png)

Specified methods can be invoked in `Operations` section.
[<img src="https://i.stack.imgur.com/9SFoH.png" alt="enter image description here" />](https://i.stack.imgur.com/9SFoH.png)

If you want to be able to use remote management, you will need additional JVM parameters, like:

```java
-Dcom.sun.management.jmxremote=true //true by default
-Dcom.sun.management.jmxremote.port=36006 
-Dcom.sun.management.jmxremote.authenticate=false 
-Dcom.sun.management.jmxremote.ssl=false

```

These parameters can be found in [Chapter 2 of JMX guides](https://docs.oracle.com/javase/8/docs/technotes/guides/management/agent.html). After that you will be able to connect to your application via jConsole remotely with `jconsole host:port` or with specifying `host:port` or `service:jmx:rmi:///jndi/rmi://hostName:portNum/jmxrmi` in jConsole GUI.

Useful links:

- [JMX guides](https://docs.oracle.com/javase/8/docs/technotes/guides/management/overview.html)
- [JMX Best practices](http://www.oracle.com/us/technologies/java/best-practices-jsp-136021.html)

