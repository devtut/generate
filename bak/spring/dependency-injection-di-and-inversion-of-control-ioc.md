---
metaTitle: "Spring - Dependency Injection (DI) and Inversion of Control (IoC)"
description: "Autowiring a dependency through Java configuration, Autowiring a dependency through XML configuration, Injecting a dependency manually through XML configuration, Injecting a dependency manually through Java configuration"
---

# Dependency Injection (DI) and Inversion of Control (IoC)



## Autowiring a dependency through Java configuration


Constructor injection through Java configuration can also utilize autowiring, such as:

```java
@Configuration
class AppConfig {
  @Bean
  public Bar bar() { return new Bar(); }

  @Bean
  public Foo foo(Bar bar) { return new Foo(bar); }
}

```



## Autowiring a dependency through XML configuration


Dependencies can be autowired when using the component scan feature of the Spring framework. For autowiring to work, the following XML configuration must be made:

```java
<context:annotation-config/>
<context:component-scan base-package="[base package]"/>

```

where, `base-package` is the fully-qualified Java package within which Spring should perform component scan.

> 
Constructor injection


Dependencies can be injected through the class constructor as follows:

```java
@Component
class Bar { ... }

@Component
class Foo {
  private Bar bar;

  @Autowired
  public Foo(Bar bar) { this.bar = bar; }
}

```

Here, `@Autowired` is a Spring-specific annotation. Spring also supports [JSR-299](http://docs.oracle.com/javaee/6/tutorial/doc/giwhb.html) to enable application portability to other Java-based dependency injection frameworks. This allows `@Autowired` to be replaced with `@Inject` as:

```java
@Component
class Foo {
  private Bar bar;

  @Inject
  public Foo(Bar bar) { this.bar = bar; }
}

```

> 
Property injection


Dependencies can also be injected using setter methods as follows:

```java
@Component
class Foo {
  private Bar bar;

  @Autowired
  public void setBar(Bar bar) { this.bar = bar; }
}

```

> 
Field injection


Autowiring also allows initializing fields within class instances directly, as follows:

```java
@Component
class Foo {
  @Autowired
  private Bar bar;
}

```

For Spring versions 4.1+ you can use [Optional](https://docs.oracle.com/javase/8/docs/api/java/util/Optional.html) for optional dependencies.

```java
@Component
class Foo {

    @Autowired
    private Optional<Bar> bar;
}

```

The same approach can be used for constructor DI.

```java
@Component
class Foo {
    private Optional<Bar> bar;

    @Autowired
    Foo(Optional<Bar> bar) {
        this.bar = bar;
    }
}

```



## Injecting a dependency manually through XML configuration


Consider the following Java classes:

```java
class Foo {
  private Bar bar;

  public void foo() {
    bar.baz();
  }
}

```

As can be seen, the class `Foo` needs to call the method `baz` on an instance of another class `Bar` for its method `foo` to work successfully. `Bar` is said to be a dependency for `Foo` since `Foo` cannot work correctly without a `Bar` instance.

> 
Constructor injection


When using XML configuration for Spring framework to define Spring-managed beans, a bean of type `Foo` can be configured as follows:

```java
<bean class="Foo">
  <constructor-arg>
    <bean class="Bar" />
  </constructor-arg>
</bean>

```

or, alternatively (more verbose):

```java
<bean id="bar" class="bar" />

<bean class="Foo">
  <constructor-arg ref="bar" />
</bean>

```

In both cases, Spring framework first creates an instance of `Bar` and `injects` it into an instance of `Foo`. This example assumes that the class `Foo` has a constructor that can take a `Bar` instance as a parameter, that is:

```java
class Foo {
  private Bar bar;

  public Foo(Bar bar) { this.bar = bar; }
}

```

This style is known as **constructor injection** because the dependency (`Bar` instance) is being injected into through the class constructor.

> 
Property injection


Another option to inject the `Bar` dependency into `Foo` is:

```java
<bean class="Foo">
  <property name="bar">
    <bean class="Bar" />
  </property>
</bean>

```

or, alternatively (more verbose):

```java
<bean id="bar" class="bar" />

<bean class="Foo">
  <property name="bar" ref="bar" />
</bean>

```

This requires the `Foo` class to have a setter method that accepts a `Bar` instance, such as:

```java
class Foo {
  private Bar bar;

  public void setBar(Bar bar) { this.bar = bar; }
}

```



## Injecting a dependency manually through Java configuration


The same examples as shown above with XML configuration can be re-written with Java configuration as follows.

> 
Constructor injection


```java
@Configuration
class AppConfig {
  @Bean
  public Bar bar() { return new Bar(); }

  @Bean
  public Foo foo() { return new Foo(bar()); }
}

```

> 
Property injection


```java
@Configuration
class AppConfig {
  @Bean
  public Bar bar() { return new Bar(); }

  @Bean
  public Foo foo() {
    Foo foo = new Foo();
    foo.setBar(bar());

    return foo;
  }
}

```



#### Remarks


The source code for large software applications is typically organized into multiple units. The definition of a unit normally varies by the programming language used. For example, code written in a procedural programming language (like C) is organized into `functions` or `procedures`. Similarly, code in an object-oriented programming language (like Java, Scala and C#) is organized into `classes`, `interfaces` and so on. These units of code organization can be thought of as individual units making up the overall software application.

When applications have multiple units, inter-dependencies between those units crop up when one unit has to use others to complete its functionality. The dependent units can be thought of as `consumers` and the units on which they depend upon as `providers` of specific functionality.

The easiest programming approach is for the consumers to fully control the flow of a software application by deciding which providers should be instantiated, used and destroyed at what points in the overall execution of the application. The consumers are said to have full control over the providers during the execution flow, which are `dependencies` for the consumers. In case the providers have their own dependencies, the consumers may have to worry about how the providers should be initialized (and released), making the control flow more and more complicated as the number of units in the software goes up. This approach also increases the coupling between units, making it increasingly difficult to change units individually without worrying about breaking others parts of the software.

[Inversion of Control](https://en.wikipedia.org/wiki/Inversion_of_control) (IoC) is a design-principle that advocates outsourcing control flow activities like unit discovery, instantiation and destruction to a framework independent of the consumers and providers. The underlying principle behind IoC is to decouple consumers and providers, freeing software units from having to worry about discovering, instantiating and cleaning up their dependencies and allowing units to focus on their own functionality. This decoupling helps keep the software extensible and maintainable.

[Dependency injection](https://en.wikipedia.org/wiki/Dependency_injection) is one of the techniques for implementing the inversion of control principle whereby instances of dependencies (providers) are injected into a software unit (the consumer) instead of the consumer having to find and instantiate them.

The Spring framework contains a dependency injection module at its core which allows Spring-managed beans to be injected into other Spring-managed beans as dependencies.

