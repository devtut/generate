---
metaTitle: "Spring - Bean scopes"
description: "Additional scopes in web-aware contexts, Prototype scope, Singleton scope"
---

# Bean scopes



## Additional scopes in web-aware contexts


There are several scopes that are available only in a web-aware application context:

- **request** - new bean instance is created per HTTP request
- **session** - new bean instance is created per HTTP session
- **application** - new bean instance is created per `ServletContext`
- **globalSession** - new bean instance is created per global session in Portlet environment (in Servlet environment global session scope is equal to session scope)
- **websocket** - new bean instance is created per WebSocket session

No additional setup is required to declare and access web-scoped beans in Spring Web MVC environment.

### XML Configuration

```java
<bean id="myRequestBean" class="OneClass" scope="request"/>
<bean id="mySessionBean" class="AnotherClass" scope="session"/>
<bean id="myApplicationBean" class="YetAnotherClass" scope="application"/>
<bean id="myGlobalSessionBean" class="OneMoreClass" scope="globalSession"/>

```

### Java Configuration (prior to Spring 4.3)

```java
@Configuration
public class MyConfiguration {

    @Bean
    @Scope(value = WebApplicationContext.SCOPE_REQUEST, proxyMode = ScopedProxyMode.TARGET_CLASS)
    public OneClass myRequestBean() {
        return new OneClass();
    }

    @Bean
    @Scope(value = WebApplicationContext.SCOPE_SESSION, proxyMode = ScopedProxyMode.TARGET_CLASS)
    public AnotherClass mySessionBean() {
        return new AnotherClass();
    }

    @Bean
    @Scope(value = WebApplicationContext.SCOPE_APPLICATION, proxyMode = ScopedProxyMode.TARGET_CLASS)
    public YetAnotherClass myApplicationBean() {
        return new YetAnotherClass();
    }

    @Bean
    @Scope(value = WebApplicationContext.SCOPE_GLOBAL_SESSION, proxyMode = ScopedProxyMode.TARGET_CLASS)
    public OneMoreClass myGlobalSessionBean() {
        return new OneMoreClass();
    }
}

```

### Java Configuration (after Spring 4.3)

```java
@Configuration
public class MyConfiguration {

    @Bean
    @RequestScope
    public OneClass myRequestBean() {
        return new OneClass();
    }

    @Bean
    @SessionScope
    public AnotherClass mySessionBean() {
        return new AnotherClass();
    }

    @Bean
    @ApplicationScope
    public YetAnotherClass myApplicationBean() {
        return new YetAnotherClass();
    }
}

```

### Annotation-Driven Components

```java
@Component
@RequestScope
public class OneClass {
    ...
}

@Component
@SessionScope
public class AnotherClass {
    ...
}

@Component
@ApplicationScope
public class YetAnotherClass {
    ...
}

@Component
@Scope(scopeName = WebApplicationContext.SCOPE_GLOBAL_SESSION, proxyMode = ScopedProxyMode.TARGET_CLASS)
public class OneMoreClass {
    ...
}

@Component
@Scope(scopeName = "websocket", proxyMode = ScopedProxyMode.TARGET_CLASS)
public class AndOneMoreClass {
    ...
}

```



## Prototype scope


A prototype-scoped bean is not pre-created on Spring container startup. Instead, a new fresh instance will be created every time a request to retrieve this bean is sent to the container. This scope is recommended for stateful objects, since its state won't be shared by other components.

In order to define a prototype-scoped bean, we need to add the @Scope annotation, specifying the type of scope we want.

Given the following MyBean class:

```java
public class MyBean {
    private static final Logger LOGGER = LoggerFactory.getLogger(MyBean.class);
    private String property;

    public MyBean(String property) {
        this.property = property;
        LOGGER.info("Initializing {} bean...", property);
    }

    public String getProperty() {
        return this.property;
    }

    public void setProperty(String property) {
        this.property = property;
    }
}

```

We define a bean definition, stating its scope as prototype:

```java
@Configuration
public class PrototypeConfiguration {

    @Bean
    @Scope("prototype")
    public MyBean prototypeBean() {
        return new MyBean("prototype");
    }
}

```

In order to see how it works, we retrieve the bean from the Spring container and set a different value for its property field. Next, we will again retrieve the bean from the container and look up its value:

```java
MyBean prototypeBean1 = context.getBean("prototypeBean", MyBean.class);
prototypeBean1.setProperty("changed property");

MyBean prototypeBean2 = context.getBean("prototypeBean", MyBean.class);

logger.info("Prototype bean 1 property: " + prototypeBean1.getProperty());
logger.info("Prototype bean 2 property: " + prototypeBean2.getProperty());

```

Looking at the following result, we can see how a new instance has been created on each bean request:

```java
Initializing prototype bean...
Initializing prototype bean...
Prototype bean 1 property: changed property
Prototype bean 2 property: prototype

```

A common mistake is to assume that the bean is recreated per invocation or per thread, this is **NOT** the case.  Instead an instance is created PER INJECTION (or retrieval from the context).  If a Prototype scoped bean is only ever injected into a single singleton bean, there will only ever be one instance of that Prototype scoped bean.

Spring does not manage the complete lifecycle of a prototype bean: the container instantiates, configures, decorates and otherwise assembles a prototype object, hands it to the client and then has no further knowledge of that prototype instance.



## Singleton scope


If a bean is defined with singleton scope, there will only be one single object instance initialized in the Spring container. All requests to this bean will return the same shared instance. This is the default scope when defining a bean.

Given the following MyBean class:

```java
public class MyBean {
    private static final Logger LOGGER = LoggerFactory.getLogger(MyBean.class);
    private String property;

    public MyBean(String property) {
        this.property = property;
        LOGGER.info("Initializing {} bean...", property);
    }

    public String getProperty() {
        return this.property;
    }

    public void setProperty(String property) {
        this.property = property;
    }
}

```

We can define a singleton bean with the @Bean annotation:

```java
@Configuration
public class SingletonConfiguration {

    @Bean
    public MyBean singletonBean() {
        return new MyBean("singleton");
    } 
}

```

The following example retrieves the same bean twice from the Spring context:

```java
MyBean singletonBean1 = context.getBean("singletonBean", MyBean.class);
singletonBean1.setProperty("changed property");

MyBean singletonBean2 = context.getBean("singletonBean", MyBean.class);

```

When logging the singletonBean2 property, the message **"changed property"** will be shown, since we just retrieved the same shared instance.

Since the instance is shared among different components, it is recommended to define singleton scope for stateless objects.

### Lazy singleton beans

By default, singleton beans are pre-instantiated. Hence, the shared object instance will be created when the Spring container is created. If we start the application, the **"Initializing singleton bean..."** message will be shown.

If we don't want the bean to be pre-instantiated, we can add the @Lazy annotation to the bean definition. This will prevent the bean from being created until it is first requested.

```java
@Bean
@Lazy
public MyBean lazySingletonBean() {
    return new MyBean("lazy singleton");
}

```

Now, if we start the Spring container, no **"Initializing lazy singleton bean..."** message will appear. The bean won't be created until it is requested for the first time:

```java
logger.info("Retrieving lazy singleton bean...");
context.getBean("lazySingletonBean");

```

If we run the application with both singleton and lazy singleton beans defined, It will produce the following messages:

```java
Initializing singleton bean...
Retrieving lazy singleton bean...
Initializing lazy singleton bean...

```

