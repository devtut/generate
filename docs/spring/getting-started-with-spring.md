---
metaTitle: "Spring - Getting started with spring"
description: "Setup (XML Configuration), Showcasing Core Spring Features by example, What is Spring Framework, why should we go for it ?"
---

# Getting started with spring



## Setup (XML Configuration)


Steps to create Hello Spring:

1. Investigate [Spring Boot](http://stackoverflow.com/documentation/spring-boot/topics) to see if that would better suit your needs.
1. Have a project set up with the correct dependencies. It is recommended that you are using [Maven](http://stackoverflow.com/documentation/maven/topics) or [Gradle](http://stackoverflow.com/documentation/gradle/topics).
1. create a POJO class, e.g. `Employee.java`
1. create a XML file where you can define your class and variables. e.g `beans.xml`
1. create your main class e.g. `Customer.java`
1. Include [spring-beans](http://search.maven.org/#artifactdetails%7Corg.springframework%7Cspring-beans%7C4.3.1.RELEASE%7Cjar) (and its transitive dependencies!) as a dependency.

`Employee.java`:

```java
package com.test;

public class Employee {

    private String name;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void displayName() {
        System.out.println(name);
    }
}

```

`beans.xml`:

```java
<?xml version="1.0" encoding="UTF-8"?>
<beans xmlns="http://www.springframework.org/schema/beans"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="http://www.springframework.org/schema/beans
    http://www.springframework.org/schema/beans/spring-beans-4.3.xsd">
        
    <bean id="employee" class="com.test.Employee">
        <property name="name" value="test spring"></property>
    </bean>

</beans>

```

`Customer.java`:

```java
package com.test;

import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

public class Customer {
   public static void main(String[] args) {
      ApplicationContext context = new ClassPathXmlApplicationContext("beans.xml");

      Employee obj = (Employee) context.getBean("employee");
      obj.displayName();
   }
}

```



## Showcasing Core Spring Features by example


### Description

This is a self-contained running example including/showcasing: minimum **dependencies** needed, Java **Configuration**, **Bean declaration** by annotation and Java Configuration, **Dependency Injection** by Constructor and by Property, and **Pre/Post** hooks.

### Dependencies

These dependencies are needed in the classpath:

1. [spring-core](http://search.maven.org/#artifactdetails%7Corg.springframework%7Cspring-core%7C4.3.1.RELEASE%7Cjar)
1. [spring-context](http://search.maven.org/#artifactdetails%7Corg.springframework%7Cspring-context%7C4.3.1.RELEASE%7Cjar)
1. [spring-beans](http://search.maven.org/#artifactdetails%7Corg.springframework%7Cspring-beans%7C4.3.1.RELEASE%7Cjar)
1. [spring-aop](http://search.maven.org/#artifactdetails%7Corg.springframework%7Cspring-aop%7C4.3.1.RELEASE%7Cjar)
1. [spring-expression](http://search.maven.org/#artifactdetails%7Corg.springframework%7Cspring-expression%7C4.3.1.RELEASE%7Cjar)
1. [commons-logging](http://search.maven.org/#artifactdetails%7Ccommons-logging%7Ccommons-logging%7C1.2%7Cjar)

### Main Class

Starting from the end, this is our Main class that serves as a placeholder for the `main()` method which initialises the Application Context by pointing to the Configuration class and loads all the various beans needed to showcase particular functionality.

```java
package com.stackoverflow.documentation;

import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;



public class Main {

    public static void main(String[] args) {

        //initializing the Application Context once per application.
        ApplicationContext applicationContext = 
                new AnnotationConfigApplicationContext(AppConfig.class);

        //bean registered by annotation
        BeanDeclaredByAnnotation beanDeclaredByAnnotation = 
                applicationContext.getBean(BeanDeclaredByAnnotation.class);
        beanDeclaredByAnnotation.sayHello();

        //bean registered by Java configuration file
        BeanDeclaredInAppConfig beanDeclaredInAppConfig = 
                applicationContext.getBean(BeanDeclaredInAppConfig.class);
        beanDeclaredInAppConfig.sayHello();

        //showcasing constructor injection
        BeanConstructorInjection beanConstructorInjection = 
                applicationContext.getBean(BeanConstructorInjection.class);
        beanConstructorInjection.sayHello();

        //showcasing property injection
        BeanPropertyInjection beanPropertyInjection = 
                applicationContext.getBean(BeanPropertyInjection.class);
        beanPropertyInjection.sayHello();

        //showcasing PreConstruct / PostDestroy hooks
        BeanPostConstructPreDestroy beanPostConstructPreDestroy = 
                applicationContext.getBean(BeanPostConstructPreDestroy.class);
        beanPostConstructPreDestroy.sayHello();
    }
}

```

### Application Configuration file

The configuration class is annotated by `@Configuration` and is used as a parameter in the initialised Application Context. The `@ComponentScan` annotation at the class level of the configuration class points to a package to be scanned for Beans and dependencies registered using annotations. Finally the `@Bean` annotation serves as a bean definition in the configuration class.

```java
package com.stackoverflow.documentation;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan("com.stackoverflow.documentation")
public class AppConfig {

    @Bean
    public BeanDeclaredInAppConfig beanDeclaredInAppConfig() {
        return new BeanDeclaredInAppConfig();
    }
}

```

### Bean Declaration by Annotation

The `@Component` annotation serves to demarcate the POJO as a Spring bean available for registration during component scanning.

```java
@Component
public class BeanDeclaredByAnnotation {

    public void sayHello() {
        System.out.println("Hello, World from BeanDeclaredByAnnotation !");
    }
}

```

### Bean Declaration by Application Configuration

Notice that we don't need to annotate or otherwise mark our POJO since the bean declaration/definition is happening in the Application Configuration class file.

```java
public class BeanDeclaredInAppConfig {

    public void sayHello() {
        System.out.println("Hello, World from BeanDeclaredInAppConfig !");
    }
}

```

### Constructor Injection

Notice that the `@Autowired` annotation is set at the constructor level. Also notice that unless explicitely defined by name the default autowiring is happening **based on the type** of the bean (in this instance `BeanToBeInjected`).

```java
package com.stackoverflow.documentation;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class BeanConstructorInjection {

    private BeanToBeInjected dependency;

    @Autowired
    public BeanConstructorInjection(BeanToBeInjected dependency) {
        this.dependency = dependency;
    }

    public void sayHello() {
        System.out.print("Hello, World from BeanConstructorInjection with dependency: ");
        dependency.sayHello();
    }
}

```

### Property Injection

Notice that the `@Autowired` annotation demarcates the setter method whose name follows the JavaBeans standard.

```java
package com.stackoverflow.documentation;


import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class BeanPropertyInjection {

    private BeanToBeInjected dependency;

    @Autowired
    public void setBeanToBeInjected(BeanToBeInjected beanToBeInjected) {
        this.dependency = beanToBeInjected;
    }

    public void sayHello() {
        System.out.println("Hello, World from BeanPropertyInjection !");
    }
}

```

### PostConstruct / PreDestroy hooks

We can intercept initialisation and destruction of a Bean by the `@PostConstruct` and `@PreDestroy` hooks.

```java
package com.stackoverflow.documentation;

import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

@Component
public class BeanPostConstructPreDestroy {

    @PostConstruct
    public void pre() {
        System.out.println("BeanPostConstructPreDestroy - PostConstruct");
    }

    public void sayHello() {
        System.out.println(" Hello World, BeanPostConstructPreDestroy !");
    }

    @PreDestroy
    public void post() {
        System.out.println("BeanPostConstructPreDestroy - PreDestroy");
    }
}

```



## What is Spring Framework, why should we go for it ?


Spring is a framework, which provides bunch of classes, by using this we don't need to write boiler plate logic in our code, so Spring provides an abstract layer on J2ee.

For Example in Simple JDBC Application programmer is responsible for

1. Loading the driver class
1. Creating the connection
1. Creating statement object
1. Handling the exceptions
1. Creating query
1. Executing query
1. Closing the connection

Which is treated as boilerplate code as every programmer write the same code.
So for simplicity the framework takes care of boilerplate logic and the programmer has to write only business logic. So by using Spring framework we can develop projects rapidly with minimum lines of code, without any bug, the development cost and time also reduced.

### **So Why to choose Spring as struts is there**

Strut is a framework which provide solution to web aspects only and struts is invasive in  nature.
Spring has many features over struts so we have to choose Spring.

1. Spring is ****Noninvasive**** in nature: That means you don't need to extend any classes or implement any interfaces to your class.
1. Spring is ****versatile****: That means it can integrated with any existing technology in your project.
1. Spring provides ****end to end**** project development: That means we can develop all the modules like business layer, persistence layer.
1. Spring is ****light weight****: That means if you want to work on particular module then , you don't need to learn complete spring, only learn that particular module(eg. Spring Jdbc, Spring DAO)
1. Spring supports ****dependency injection****.
1. Spring supports ****multiple project development****  eg: Core java Application, Web Application, Distributed Application, Enterprise Application.
1. Spring supports Aspect oriented Programming for cross cutting concerns.

So finally we can say Spring is an alternative to Struts. But Spring is not a replacement of J2EE API, As Spring supplied classes internally uses J2EE API classes.
Spring is a vast framework so it has divided into several modules. No module is dependent to another except Spring Core.
Some Important modules are

1. Spring Core
1. Spring JDBC
1. Spring AOP
1. Spring Transaction
1. Spring ORM
1. Spring MVC



#### Remarks


The Spring Framework is an open source application framework and inversion of control container for the Java platform.

