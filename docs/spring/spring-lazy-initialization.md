---
metaTitle: "Spring - Spring Lazy Initialization"
description: "Example of Lazy Init in Spring, For component scanning and auto-wiring, Lazy initialization in the configuration class"
---

# Spring Lazy Initialization



## Example of Lazy Init in Spring


The `@Lazy` allow us to instruct the IOC container to delay the initialization of a bean.
By default, beans are instantiated as soon as the IOC container is created, The `@Lazy` allow us to change this instantiation process.

lazy-init in spring is the attribute of bean tag. The values of lazy-init are true and false. If lazy-init is true, then that bean will be initialized when a request is made to bean. This bean will not be initialized when the spring container is initialized. If lazy-init is false then the bean will be initialized with the spring container initialization and this is the default behavior.

**app-conf.xml**

```java
<beans xmlns="http://www.springframework.org/schema/beans" 
xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
xmlns:util="http://www.springframework.org/schema/util"
xsi:schemaLocation="http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans-3.0.xsd
http://www.springframework.org/schema/util http://www.springframework.org/schema/util/spring-util-3.0.xsd">
 
<bean id="testA" class="com.concretepage.A"/>
<bean id="testB" class="com.concretepage.B" lazy-init="true"/>

```

**A.java**

```java
package com.concretepage;
public class A {
public A(){
    System.out.println("Bean A is initialized");
   }
}

```

**B.java**

```java
package com.concretepage;
public class B {
public B(){
    System.out.println("Bean B is initialized");
   }
} 

```

**SpringTest.java**

```java
package com.concretepage;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
public class SpringTest {
public static void main(String[] args) {
     ApplicationContext context = new ClassPathXmlApplicationContext("app-conf.xml");
     System.out.println("Feth bean B.");
     context.getBean("testB");
   }
}

```

**Output**

```java
Bean A is initialized
Feth bean B.
Bean B is initialized

```



## For component scanning and auto-wiring


```java
@Component
@Lazy
public class Demo {
    ....
    ....
}

@Component
public class B {

    @Autowired
    @Lazy // If this is not here, Demo will still get eagerly instantiated to satisfy this request.
    private Demo demo;

    .......
 }

```



## Lazy initialization in the configuration class


```java
@Configuration
// @Lazy - For all Beans to load lazily
public class AppConf {

    @Bean
    @Lazy
    public Demo demo() {
        return new Demo();
    }
}

```

