---
metaTitle: "Spring - Spring profile"
description: "Spring Profiles allows to configure parts available for certain environment"
---

# Spring profile




## Spring Profiles allows to configure parts available for certain environment


Any `@Component` or `@Configuration` could be marked with `@Profile` annotation

```java
@Configuration
@Profile("production")
public class ProductionConfiguration {

    // ...
}

```

The same in XML config

```java
<beans profile="dev">
    <bean id="dataSource" class="<some data source class>" />
</beans>

```

Active profiles could be configured in the `application.properties` file

```java
spring.profiles.active=dev,production

```

or specified from command line

```java
--spring.profiles.active=dev,hsqldb

```

or in SpringBoot

```java
SpringApplication.setAdditionalProfiles("dev");

```

It is possible to enable profiles in Tests using the annotation `@ActiveProfiles("dev")`

