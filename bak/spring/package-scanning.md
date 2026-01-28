---
metaTitle: "Spring Boot - Package scanning"
description: "@ComponentScan, Creating your own auto-configuration, @SpringBootApplication"
---

# Package scanning


In this topic I will overview spring boot package scanning.

You can find some basic information in spring boot docs in the following link ([using-boot-structuring-your-code](http://docs.spring.io/spring-boot/docs/current/reference/html/using-boot-structuring-your-code.html)) but I will try to provide more detailed information.

Spring boot, and spring in general, provide a feature to automatically scan packages for certain annotations in order to create `beans` and `configuration`.



## @ComponentScan


You can use `@ComponentScan` in order to configure more complex package scanning. There is also `@ComponentScans` that act as a container annotation that aggregates several `@ComponentScan` annotations.

**Basic code examples**

```java
@ComponentScan
public class DemoAutoConfiguration {
}

```

```java
@ComponentScans({@ComponentScan("com.example1"), @ComponentScan("com.example2")})
public class DemoAutoConfiguration {
}

```

Stating `@ComponentScan` with no configuration acts like `@SpringBootApplication` and scans all packages under the class annotated with this annotation.

In this example I will state some of the useful attributes of `@ComponentScan`:

1. **basePackages** - can be used to state specific packages to scan.
1. **useDefaultFilters** - by setting this attribute to false (defaults true) you can make sure spring does not scan `@Component`, `@Repository`, `@Service`, or `@Controller` automatically.
1. **includeFilters** - can be used to **include** specific spring annotations / regex patterns to include in package scanning.
1. **excludeFilters** - can be used to **exclude** specific spring annotations / regex patterns to include in package scanning.

There are many more attributes but those are the most commonly used in order to customize package scanning.



## Creating your own auto-configuration


Spring boot is based on a lot of pre-made auto-configuration parent projects. You should already be familiar with spring boot starter projects.

You can easily create your own starter project by doing the following easy steps:

1. Create some `@Configuration` classes to define default beans. You should use external properties as much as possible to allow customization and try to use auto-configuration helper annotations like `@AutoConfigureBefore`, `@AutoConfigureAfter`, `@ConditionalOnBean`, `@ConditionalOnMissingBean` etc. You can find more detailed information on each annotation in the official documentation [Condition annotations](http://docs.spring.io/spring-boot/docs/current/reference/html/boot-features-developing-auto-configuration.html#boot-features-condition-annotations)
1. Place an auto-configuration file/files that aggregates all of the `@Configuration` classes.
1. Create a file named `spring.factories` and place it in `src/main/resources/META-INF`.
1. In `spring.factories`, set `org.springframework.boot.autoconfigure.EnableAutoConfiguration` property with comma separated values of your `@Configuration` classes:

```java
org.springframework.boot.autoconfigure.EnableAutoConfiguration=\
com.mycorp.libx.autoconfigure.LibXAutoConfiguration,\
com.mycorp.libx.autoconfigure.LibXWebAutoConfiguration

```

Using this method you can create your own auto-configuration classes that will be picked by spring-boot. Spring-boot automatically scan all maven/gradle dependencies for a `spring.factories` file, if it finds one, it adds all `@Configuration` classes specified in it to its auto-configuration process.

Make sure your `auto-configuration` starter project does not contain `spring boot maven plugin` because it will package the project as an executable JAR and won't be loaded by the classpath as intended - spring boot will not be able to find your `spring.factories` and won't load your configuration



## @SpringBootApplication


The most basic way to structure your code using spring boot for good automatic package scanning is using `@SpringBootApplication` annotation. This annotation provide in itself 3 other annotations that helps with automatic scanning: `@SpringBootConfiguration`, `@EnableAutoConfiguration`, `@ComponentScan` (more info about each annotation in the `Parameters` section).

`@SpringBootApplication` will usualy be placed in the main package and all other components will be placed in packages under this file:

```java
com
 +- example
     +- myproject
         +- Application.java (annotated with @SpringBootApplication)
         |
         +- domain
         |   +- Customer.java
         |   +- CustomerRepository.java
         |
         +- service
         |   +- CustomerService.java
         |
         +- web
             +- CustomerController.java

```

Unless mentioned otherwise, spring boot detects `@Configuration`, `@Component`, `@Repository`, `@Service`, `@Controller`, `@RestController` annotations automatically under the scanned packages (`@Configuration` and `@RestController` are being picked because they are annotated by `@Component` and `@Controller` accordingly).

**Basic Code example:**

```java
@SpringBootApplication
public class Application {

    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }
    
}

```

**Setting packages/classes explicitly**

Since version **1.3** you can also tell spring boot to scan specific packages by setting `scanBasePackages` or `scanBasePackageClasses` in `@SpringBootApplication` instead of specifying `@ComponentScan`.

1. `@SpringBootApplication(scanBasePackages = "com.example.myproject")` - set `com.example.myproject` as the base package to scan.
1. `@SpringBootApplication(scanBasePackageClasses = CustomerController.class)` - type-safe alternative to `scanBasePackages` sets the package of `CustomerController.java`, `com.example.myproject.web`, as the base package to scan.

**Excluding auto-configuration**

Another important feature is the ability to exclude specific auto-configuration classes using `exclude` or `excludeName` (`excludeName` exist since version **1.3**).

1. `@SpringBootApplication(exclude = DemoConfiguration.class)` - will exclude `DemoConfiguration` from auto package scanning.
1. `@SpringBootApplication(excludeName = "DemoConfiguration")` - will do the same using class fully classified name.



#### Parameters


|Annotation|Details
|---|---|---|---|---|---|---|---|---|---
|@SpringBootApplication|Main spring boot application annotation. used one time in the application, contains a main method, and act as main package for package scanning
|@SpringBootConfiguration|Indicates that a class provides Spring Boot application. Should be declared only once in the application, usually automatically by setting `@SpringBootApplication`
|@EnableAutoConfiguration|Enable auto-configuration of the Spring Application Context. Should be declared only once in the application, usually automatically by setting `@SpringBootApplication`
|@ComponentScan|Used to trigger automatic package scanning on a certain package and its children or to set custom package scanning
|@Configuration|Used to declare one or more `@Bean` methods. Can be picked by auto package scanning in order to declare one or more `@Bean` methods instead of traditional xml configuration
|@Bean|Indicates that a method produces a bean to be managed by the Spring container. Usually `@Bean` annotated methods will be placed in `@Configuration` annotated classes that will be picked by package scanning to create java configuration based beans.
|@Component|By declaring a class as a `@Component` it becomes a candidates for auto-detection when using annotation-based configuration and classpath scanning. Usually a class annotated with `@Component` will become a `bean` in the application
|@Repository|Originally defined by Domain-Driven Design (Evans, 2003) as "a mechanism for encapsulating storage. It is usualy used to indicate a `Repository` for `spring data`
|@Service|Very similar in practice to `@Component`. originally defined by Domain-Driven Design (Evans, 2003) as "an operation offered as an interface that stands alone in the model, with no encapsulated state."
|@Controller|Indicates that an annotated class is a "Controller" (e.g. a web controller).
|@RestController|A convenience annotation that is itself annotated with `@Controller` and `@ResponseBody`. Will be automatically picked by default because it contains the `@Controller` annotation that is picked by default.

