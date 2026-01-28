---
metaTitle: "Spring - Conditional bean registration in Spring"
description: "Register beans only when a property or value is specified, Condition annotations"
---

# Conditional bean registration in Spring



## Register beans only when a property or value is specified


A spring bean can be configured such that it will register **only** if it has a particular value **or** a specified property is met. To do so, implement [`Condition.matches`](http://docs.spring.io/spring/docs/current/javadoc-api/org/springframework/context/annotation/Condition.html#matches-org.springframework.context.annotation.ConditionContext-org.springframework.core.type.AnnotatedTypeMetadata-) to check the property/value:

```java
public class PropertyCondition implements Condition {
    @Override
    public boolean matches(ConditionContext context, AnnotatedTypeMetadata metadata) {
        return context.getEnvironment().getProperty("propertyName") != null;
        // optionally check the property value
    }
}

```

In Java config, use the above implementation as a condition to register the bean. Note the use of [@Conditional](http://docs.spring.io/spring/docs/current/javadoc-api/org/springframework/context/annotation/Conditional.html) annotation.

```java
@Configuration
public class MyAppConfig {

    @Bean
    @Conditional(PropertyCondition.class)
    public MyBean myBean() {
      return new MyBean();
    }
}

```

In `PropertyCondition`, any number of conditions can be evaluated. However it is advised to separate the implementation for each condition to keep them loosely coupled. For example:

```java
@Configuration
public class MyAppConfig {

    @Bean
    @Conditional({PropertyCondition.class, SomeOtherCondition.class})
    public MyBean myBean() {
      return new MyBean();
    }
}

```



## Condition annotations


Except main `@conditional` annotation there are set of similar annotation to be used for different cases.

### Class conditions

The `@ConditionalOnClass` and `@ConditionalOnMissingClass` annotations allows configuration to be included based on the presence or absence of specific classes.

E.g. when `OObjectDatabaseTx.class` is added to dependencies and there is no `OrientWebConfigurer` bean we create the configurer.

```java
@Bean
@ConditionalOnWebApplication
@ConditionalOnClass(OObjectDatabaseTx.class)
@ConditionalOnMissingBean(OrientWebConfigurer.class)
public OrientWebConfigurer orientWebConfigurer() {
    return new OrientWebConfigurer();
}

```

### Bean conditions

The `@ConditionalOnBean` and `@ConditionalOnMissingBean` annotations allow a bean to be included based on the presence or absence of specific beans. You can use the value attribute to specify beans by type, or name to specify beans by name. The search attribute allows you to limit the `ApplicationContext` hierarchy that should be considered when searching for beans.

See the example above when we check whether there is no defined bean.

### Property conditions

The `@ConditionalOnProperty` annotation allows configuration to be included based on a Spring Environment property. Use the prefix and name attributes to specify the property that should be checked. By default any property that exists and is not equal to `false` will be matched. You can also create more advanced checks using the `havingValue` and `matchIfMissing` attributes.

```java
@ConditionalOnProperty(value='somebean.enabled', matchIfMissing = true, havingValue="yes")
@Bean 
public SomeBean someBean(){
}

```

### Resource conditions

The `@ConditionalOnResource` annotation allows configuration to be included only when a specific resource is present.

```java
@ConditionalOnResource(resources = "classpath:init-db.sql") 

```

### Web application conditions

The `@ConditionalOnWebApplication` and `@ConditionalOnNotWebApplication` annotations allow configuration to be included depending on whether the application is a 'web application'.

```java
@Configuration
@ConditionalOnWebApplication
public class MyWebMvcAutoConfiguration {...}

```

### SpEL expression conditions

The `@ConditionalOnExpression` annotation allows configuration to be included based on the result of a SpEL expression.

```java
@ConditionalOnExpression("${rest.security.enabled}==false")

```



#### Remarks


Important point to note while using condition

- The condition class is referred as direct class (not as spring bean) so it **can't** use the `@Value` property injection i.e. no other spring beans can be injected within it.
- From java docs - **Conditions must follow the same restrictions as `BeanFactoryPostProcessor` and take care to never interact with bean instances**. The restrictions refereed here are **A `BeanFactoryPostProcessor` may interact with and modify bean definitions, but never bean instances. Doing so may cause premature bean instantiation, violating the container and causing unintended side-effects.**

