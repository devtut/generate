---
metaTitle: "Spring - Creating and using beans"
description: "Autowiring all beans of a specific type, Basic annotation autowiring, Declaring Bean, Using FactoryBean for dynamic bean instantiation, Inject prototype-scoped beans into singletons, Autowiring specific bean instances with @Qualifier, Autowiring specific instances of classes using generic type parameters"
---

# Creating and using beans



## Autowiring all beans of a specific type


If you've got multiple implementations of the same interface, Spring can autowire them all into a collection object. I'm going to use an example using a Validator pattern<sup>1</sup>

Foo Class:

```java
public class Foo {
     private String name;
     private String emailAddress;
     private String errorMessage;
     /** Getters & Setters omitted **/
}

```

Interface:

```java
public interface FooValidator {
    public Foo validate(Foo foo);
}

```

Name Validator Class:

```java
@Component(value="FooNameValidator")
public class FooNameValidator implements FooValidator {
    @Override
    public Foo validate(Foo foo) {
        //Validation logic goes here.
    }
}

```

Email Validator Class:

```java
@Component(value="FooEmailValidator")
public class FooEmailValidator implements FooValidator {
    @Override
    public Foo validate(Foo foo) {
        //Different validation logic goes here.
    }
}

```

You can now autowire these validators individually or together into a class.

Interface:

```java
public interface FooService {
    public void handleFoo(Foo foo);
}

```

Class:

```java
@Service
public class FooServiceImpl implements FooService {
    /** Autowire all classes implementing FooValidator interface**/
    @Autowired
    private List<FooValidator> allValidators;

    @Override
    public void handleFoo(Foo foo) {
       /**You can use all instances from the list**/
       for(FooValidator validator : allValidators) {
           foo = validator.validate(foo);               
       }
    }   
}

```

It's worth noting that if you have more than one implementation of an interface in the Spring IoC container and don't specify which one you want to use with the `@Qualifier` annotation, Spring will throw an exception when trying to start, because it won't know which instance to use.

<sub>1: This is not the right way to do such simple validations. This is a simple example about autowiring. If you want an idea of a much easier validation method look up how Spring does validation with annotations.</sub>



## Basic annotation autowiring


Interface:

```java
public interface FooService {
    public int doSomething();
}

```

Class:

```java
@Service
public class FooServiceImpl implements FooService {
    @Override
    public int doSomething() {
        //Do some stuff here
        return 0;
    }
}

```

It should be noted that a class must implement an interface for Spring to be able to autowire this class. There is a method to allow Spring to autowire stand-alone classes using load time weaving, but that is out of scope for this example.

You can gain access to this bean in any class that instantiated by the Spring IoC container using the `@Autowired` annotation.

Usage:

```java
@Autowired([required=true])

```

The `@Autowired` annotation will first attempt to autowire by type, and then fall back on bean name in the event of ambiguity.

This annotation can be applied in several different ways.

Constructor injection:

```java
public class BarClass() {
    private FooService fooService         

    @Autowired
    public BarClass(FooService fooService) {
        this.fooService = fooService;
    }
}

```

Field injection:

```java
public class BarClass() {
    @Autowired
    private FooService fooService;
}

```

Setter injection:

```java
public class BarClass() {
    private FooService fooService;

    @Autowired
    public void setFooService(FooService fooService) {
        this.fooService = fooService;
    }
}

```



## Declaring Bean


To declare a bean, simply annotate a method with the `@Bean` annotation or annotate a class with the `@Component` annotation (annotations `@Service`, `@Repository`, `@Controller` could be used as well).

When JavaConfig encounters such a method, it will execute that method and register the return value as a bean within a BeanFactory. By default, the bean name will be that of the method name.

We can create bean using one of three ways:

<li>
**Using Java based Configuration**: In Configuration file we need to declare bean using @bean annotation

```java
@Configuration
public class AppConfig {
    @Bean
    public TransferService transferService() {
        return new TransferServiceImpl();
    }
}

```


</li>
<li>
**Using XML based configuration**: For XML based configuration we need to create declare bean in application configuration XML i.e.

```java
 <beans>
     <bean name="transferService" class="com.acme.TransferServiceImpl"/>
 </beans>

```


</li>
<li>
**Annotation-Driven Component**: For annotation-driven components, we need to add the @Component annotation to the class we want to declare as bean.

```java
 @Component("transferService")
 public class TransferServiceImpl implements TransferService {
     ...
 }

```


</li>

Now all three beans with name `transferService` are available in `BeanFactory` or `ApplicationContext`.



## Using FactoryBean for dynamic bean instantiation


In order to dynamically decide what beans to inject, we can use `FactoryBean`s. These are classes which implement the factory method pattern, providing instances of beans for the container. They are recognized by Spring and can be used transparently, without need to know that the bean comes from a factory. For example:

```java
public class ExampleFactoryBean extends AbstractFactoryBean<String> {
    // This method determines the type of the bean for autowiring purposes
    @Override
    public Class<?> getObjectType() {
        return String.class;
    }

    // this factory method produces the actual bean
    @Override
    protected String createInstance() throws Exception {
        // The thing you return can be defined dynamically,
        // that is read from a file, database, network or just
        // simply randomly generated if you wish.
        return "Something from factory";
    }
}

```

Configuration:

```java
@Configuration
public class ExampleConfig {
    @Bean
    public FactoryBean<String> fromFactory() {
        return new ExampleFactoryBean();
    }
}

```

Getting the bean:

```java
AbstractApplicationContext context = new AnnotationConfigApplicationContext(ExampleConfig.class);
String exampleString = (String) context.getBean("fromFactory");

```

To get the actual FactoryBean, use the ampersand prefix before the bean's name:

```java
FactoryBean<String> bean = (FactoryBean<String>) context.getBean("&fromFactory");

```

Please note that you can only use `prototype` or `singleton` scopes - to change the scope to `prototype` override `isSingleton` method:

```java
public class ExampleFactoryBean extends AbstractFactoryBean<String> {
    @Override
    public boolean isSingleton() {
        return false;
    }

    // other methods omitted for readability reasons
}

```

Note that scoping refers to the actual instances being created, not the factory bean itself.



## Inject prototype-scoped beans into singletons


The container creates a singleton bean and injects collaborators into it only once. This is not the desired behavior when a singleton bean has a prototype-scoped collaborator, since the prototype-scoped bean should be injected every time it is being accessed via accessor.

There are several solutions to this problem:

1. Use lookup method injection
1. Retrieve a prototype-scoped bean via `javax.inject.Provider`
1. Retrieve a prototype-scoped bean via `org.springframework.beans.factory.ObjectFactory` (an equivalent of #2, but with the class that is specific to Spring)
1. Make a singleton bean container aware via implementing `ApplicationContextAware` interface

Approaches #3 and #4 are generally discouraged, since they strongly tie an app to Spring framework. Thus, they are not covered in this example.

**Lookup method injection via XML configuration and an abstract method**

Java Classes

```java
public class Window {
}

public abstract class WindowGenerator {

    public Window generateWindow() {
        Window window = createNewWindow(); // new instance for each call
        ... 
    }

    protected abstract Window createNewWindow(); // lookup method
}

```

XML

```java
<bean id="window" class="somepackage.Window" scope="prototype" lazy-init="true"/>

<bean id="windowGenerator" class="somepackage.WindowGenerator">
    <lookup-method name="createNewWindow" bean="window"/>
</bean>

```

**Lookup method injection via Java configuration and @Component**

Java Classes

```java
public class Window {
}

@Component
public class WindowGenerator {

    public Window generateWindow() {
        Window window = createNewWindow(); // new instance for each call
        ...
    }

    @Lookup
    protected Window createNewWindow() {
        throw new UnsupportedOperationException();
    }
}

```

Java configuration

```java
@Configuration
@ComponentScan("somepackage") // package where WindowGenerator is located
public class MyConfiguration {

    @Bean
    @Lazy
    @Scope(scopeName = ConfigurableBeanFactory.SCOPE_PROTOTYPE)
    public Window window() {
        return new Window();
    }
}

```

**Manual lookup method injection via Java configuration**

Java Classes

```java
public class Window {
}

public abstract class WindowGenerator {

    public Window generateWindow() {
        Window window = createNewWindow(); // new instance for each call
        ...
    }

    protected abstract Window createNewWindow(); // lookup method
}

```

Java configuration

```java
@Configuration
public class MyConfiguration {

    @Bean
    @Lazy
    @Scope(scopeName = ConfigurableBeanFactory.SCOPE_PROTOTYPE)
    public Window window() {
        return new Window();
    }

    @Bean
    public WindowGenerator windowGenerator(){
        return new WindowGenerator() {
            @Override
            protected Window createNewWindow(){
                return window();
            }
        };
    }
}

```

**Injection of a protoype-scoped bean into singleton via `javax.inject.Provider`**

Java classes

```java
public class Window {
}

public class WindowGenerator {

    private final Provider<Window> windowProvider;

    public WindowGenerator(final Provider<Window> windowProvider) {
        this.windowProvider = windowProvider;
    }

    public Window generateWindow() {
        Window window = windowProvider.get(); // new instance for each call   
        ...
    }
}

```

XML

```java
<bean id="window" class="somepackage.Window" scope="prototype" lazy-init="true"/>

<bean id="windowGenerator" class="somepackage.WindowGenerator">
    <constructor-arg>
        <bean class="org.springframework.beans.factory.config.ProviderCreatingFactoryBean">
            <property name="targetBeanName" value="window"/>
        </bean>
    </constructor-arg>
</bean>

```

The same approaches can be used for other scopes as well (e.g. for injection a request-scoped bean into singleton).



## Autowiring specific bean instances with @Qualifier


If you've got multiple implementations of the same interface, Spring needs to know which one it should autowire into a class. I'm going to use a Validator pattern in this example.<sup>1</sup>

Foo Class:

```java
public class Foo {
     private String name;
     private String emailAddress;
     private String errorMessage;
     /** Getters & Setters omitted **/
}

```

Interface:

```java
public interface FooValidator {
    public Foo validate(Foo foo);
}

```

Name Validator Class:

```java
@Component(value="FooNameValidator")
public class FooNameValidator implements FooValidator {
    @Override
    public Foo validate(Foo foo) {
        //Validation logic goes here.
    }
}

```

Email Validator Class:

```java
@Component(value="FooEmailValidator")
public class FooEmailValidator implements FooValidator {
    @Override
    public Foo validate(Foo foo) {
        //Different validation logic goes here.
    }
}

```

You can now autowire these validators individually into a class.

Interface:

```java
public interface FooService {
    public void handleFoo(Foo foo);
}

```

Class:

```java
@Service
public class FooServiceImpl implements FooService {
    /** Autowire validators individually **/
    @Autowired
    /* 
     * Notice how the String value here matches the value 
     * on the @Component annotation? That's how Spring knows which 
     * instance to autowire.
     */
    @Qualifier("FooNameValidator")
    private FooValidator nameValidator;

    @Autowired
    @Qualifier("FooEmailValidator")
    private FooValidator emailValidator;

    @Override
    public void handleFoo(Foo foo) {
        /**You can use just one instance if you need**/
        foo = nameValidator.validate(foo);
    }   
}

```

It's worth noting that if you have more than one implementation of an interface in the Spring IoC container and don't specify which one you want to use with the `@Qualifier` annotation, Spring will throw an exception when trying to start, because it won't know which instance to use.

<sub>1: This is not the right way to do such simple validations. This is a simple example about autowiring. If you want an idea of a much easier validation method look up how Spring does validation with annotations.</sub>



## Autowiring specific instances of classes using generic type parameters


If you've got an interface with a generic type parameter, Spring can use that to only autowire implementations that implement a type parameter you specify.

Interface:

```java
public interface GenericValidator<T> {
    public T validate(T object);
}

```

Foo Validator Class:

```java
@Component
public class FooValidator implements GenericValidator<Foo> {
    @Override
    public Foo validate(Foo foo) {
        //Logic here to validate foo objects.
    }
}

```

Bar Validator Class:

```java
@Component
public class BarValidator implements GenericValidator<Bar> {
    @Override
    public Bar validate(Bar bar) {
        //Bar validation logic here
    }
}

```

You can now autowire these validators using type parameters to decide which instance to autowire.

Interface:

```java
public interface FooService {
    public void handleFoo(Foo foo);
}

```

Class:

```java
@Service
public class FooServiceImpl implements FooService {
    /** Autowire Foo Validator **/
    @Autowired
    private GenericValidator<Foo> fooValidator;

    @Override
    public void handleFoo(Foo foo) {
        foo = fooValidator.validate(foo);
    }   
}

```

