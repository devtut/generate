---
metaTitle: "Spring - Spring Core"
description: "Introduction to Spring Core, Understanding How Spring Manage Dependency ?"
---

# Spring Core



## Introduction to Spring Core


Spring is a vast framework, so the Spring framework has been divided in several modules which makes spring lightweight.
Some important modules are:

1. Spring Core
1. Spring AOP
1. Spring JDBC
1. Spring Transaction
1. Spring ORM
1. Spring MVC

All the modules of Spring are independent of each other except Spring Core.
As Spring core is the base module, so in all module we have to use Spring Core

****Spring Core****

Spring Core talking  all about dependency management.That means if any arbitrary classes provided to spring then Spring can manage dependency.

**What is a dependency:**<br />
From project point of view, in a project or application multiple classes are there with different functionality. and each classes required some functionality of other classes.

**Example:**

```java
class Engine {

  public void start() {
    System.out.println("Engine started");
  }    
}

class Car {

  public void move() {
    // For moving start() method of engine class is required
  }
}

```

Here class Engine is required by class car so we can say class engine is
dependent to class Car,
So instead of we managing those dependency by  Inheritance or creating object as fallows.

****By Inheritance:****

```java
class Engine {

  public void start() {
    System.out.println("Engine started");
  }
}

class Car extends Engine {

  public void move() {
    start(); //Calling super class start method,
  }
}

```

****By creating object of dependent class:****

```java
class Engine {

  public void start() {
    System.out.println("Engine started");
  }    
}

class Car {

  Engine eng = new Engine();
 
  public void move() {
   eng.start();
  }
}

```

So instead of we managing dependency between classes spring core takes the responsibility  dependency management.
But Some rule are there,
The classes must be designed with some design technique that is Strategy design pattern.



## Understanding How Spring Manage Dependency ?


Let me write a piece of code which shows completely loosely coupled, Then you can easily understand how Spring core manage the dependency internally.
Consider an scenario, Online business Flipkart is there, it uses some times DTDC or Blue Dart courier service , So let me design a application which shows complete loosely coupled. The Eclipse Directory as fallows:

[<img src="https://i.stack.imgur.com/LEJqC.jpg" alt="enter image description here" />](https://i.stack.imgur.com/LEJqC.jpg)

```java
//Interface
package com.sdp.component;

public interface Courier {
    public String deliver(String iteams,String address);

}

```

//implementation classes

```

 package com.sdp.component;

public class BlueDart implements Courier {

    
    public String deliver(String iteams, String address) {
        
        return iteams+ "Shiped to Address "+address +"Through BlueDart";
    }

}

package com.sdp.component;

public class Dtdc implements Courier {

    
    public String deliver(String iteams, String address) {
        return iteams+ "Shiped to Address "+address +"Through Dtdc";    }

}

```

//Component classe

```java
package com.sdp.service;

import com.sdp.component.Courier;

public class FlipKart {
    private Courier courier;

    public void setCourier(Courier courier) {
        this.courier = courier;
    }
    public void shopping(String iteams,String address)
    {
        String status=courier.deliver(iteams, address);
        System.out.println(status);
    }

}

```

//Factory classes to create and return Object

```java
package com.sdp.util;

import java.io.IOException;
import java.util.Properties;

import com.sdp.component.Courier;

public class ObjectFactory {
private static Properties props;
static{
    
    props=new Properties();
    try {
        props.load(ObjectFactory.class.getClassLoader().getResourceAsStream("com//sdp//common//app.properties"));
    } catch (IOException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
    }
    
}
public static Object getInstance(String logicalclassName)
{
    Object obj = null;
    String originalclassName=props.getProperty(logicalclassName);
    try {
         obj=Class.forName(originalclassName).newInstance();
    } catch (InstantiationException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
    } catch (IllegalAccessException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
    } catch (ClassNotFoundException e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
    }
    return obj;
}
    
    }

```

//properties file

```java
BlueDart.class=com.sdp.component.BlueDart
Dtdc.class=com.sdp.component.Dtdc
FlipKart.class=com.sdp.service.FlipKart

```

//Test class

```java
package com.sdp.test;

import com.sdp.component.Courier;
import com.sdp.service.FlipKart;
import com.sdp.util.ObjectFactory;

public class FlipKartTest {
    public static void main(String[] args) {
        Courier courier=(Courier)ObjectFactory.getInstance("Dtdc.class");
        FlipKart flipkart=(FlipKart)ObjectFactory.getInstance("FlipKart.class");
        flipkart.setCourier(courier);
        flipkart.shopping("Hp Laptop", "SR Nagar,Hyderabad");
        
    }

}

```

If we write this code then we can manually achieve loose coupling,this is applicable if all the classes want either BlueDart or Dtdc , But if some class want BlueDart and some other class want Dtdc then again it will be tightly coupled, So instead of we creating and managing the dependency injection Spring core  takes the responsibility of creating and managing the beans, Hope This will helpful, in next example we wil see the !st application on Spring core with deitals

