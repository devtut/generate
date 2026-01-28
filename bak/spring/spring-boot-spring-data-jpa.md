---
metaTitle: "Spring Boot - Spring boot + Spring Data JPA"
description: "Spring Boot and Spring Data JPA integration basic example"
---

# Spring boot + Spring Data JPA


[Spring Boot](https://projects.spring.io/spring-boot/) makes it easy to create Spring-powered, production-grade applications and services with absolute minimum fuss. It favors convention over configuration.

[Spring Data JPA](http://projects.spring.io/spring-data-jpa/), part of the larger [Spring Data](http://projects.spring.io/spring-data/) family, makes it easy to implement JPA based repositories. It makes it easier to build apps that use data access technologies.



## Spring Boot and Spring Data JPA integration basic example


We're going to build an application that stores POJOs in a database. The application uses Spring Data JPA to store and retrieve data in a relational database. Its most compelling feature is the ability to create repository implementations automatically, at runtime, from a repository interface.

### **Main Class**

```java
package org.springboot;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class Application {
    public static void main(String[] args) {
        SpringApplication.run(Application.class,args);
    }
}

```

The `main()` method uses Spring Boot’s `SpringApplication.run()` method to launch an application. Please notice that there isn’t a single line of XML. No web.xml file either. This web application is 100% pure Java and you don’t have to deal with configuring any plumbing or infrastructure.

### **Entity Class**

```java
package org.springboot.model;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;

@Entity
public class Greeting {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    private String text;

    public Greeting() {
        super();
    }

    public Greeting(String text) {
        super();
        this.text = text;
    }

    /* In this example, the typical getters and setters have been left out for brevity. */
}

```

Here you have a `Greeting` class with two attributes, the `id`, and the `text`. You also have two constructors. The default constructor only exists for the sake of JPA. You won’t use it directly, so it can be designated as `protected`. The other constructor is the one you’ll use to create instances of `Greeting` to be saved to the database.

The `Greeting` class is annotated with `@Entity`, indicating that it is a JPA entity. For lack of a `@Table` annotation, it is assumed that this entity will be mapped to a table named 'Greeting'.

The Greeting’s `id` property is annotated with `@Id` so that JPA will recognize it as the object’s ID. The `id` property is also annotated with `@GeneratedValue` to indicate that the ID should be generated automatically.

The other property, `text` is left unannotated. It is assumed that it’ll be mapped to a column that share the same name as the property itself.

### **Transient Properties**

In an entity class similar to the one above, we can have properties that we don't want to be persisted in the database or created as columns in our database maybe because we just want to set them at runtime and use them in our application, hence we can have that property annotated with the @Transient annotation.

```java
package org.springboot.model;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Transient;

@Entity
public class Greeting {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    private String text;
    @Transient
    private String textInSomeLanguage;
    
    public Greeting() {
        super();
    }
 
    public Greeting(String text) {
        super();
        this.text = text;
        this.textInSomeLanguage = getTextTranslationInSpecifiedLanguage(text);
    }

    /* In this example, the typical getters and setters have been left out for brevity. */
}

```

Here you have the same Greeting class that now has a transient property `textInSomeLanguage` that can be initialized and used at runtime and won't be persisted in the database.

### **DAO Class**

```java
package org.springboot.repository;

import org.springboot.model.Greeting;
import org.springframework.data.repository.CrudRepository;

public interface GreetingRepository extends CrudRepository<Greeting, Long> {

    List<Greeting> findByText(String text);
}

```

`GreetingRepository` extends the `CrudRepository` interface. The type of entity and ID that it works with, `Greeting` and `Long`, are specified in the generic parameters on `CrudRepository`. By extending `CrudRepository`, `GreetingRepository` inherits several methods for working with `Greeting` persistence, including methods for saving, deleting, and finding `Greeting` entities.<br />
See [this discussion](http://stackoverflow.com/q/14014086/1429387) for comparison of [`CrudRepository`](http://docs.spring.io/spring-data/data-commons/docs/current/api/org/springframework/data/repository/CrudRepository.html), [`PagingAndSortingRepository`](http://docs.spring.io/spring-data/data-commons/docs/current/api/org/springframework/data/repository/PagingAndSortingRepository.html), [`JpaRepository`](http://docs.spring.io/spring-data/data-jpa/docs/current/api/org/springframework/data/jpa/repository/JpaRepository.html).

Spring Data JPA also allows you to define other query methods by simply declaring their method signature. In the case of `GreetingRepository`, this is shown with a `findByText()` method.

In a typical Java application, you’d expect to write a class that implements `GreetingRepository`. But that’s what makes Spring Data JPA so powerful: You don’t have to write an implementation of the repository interface. Spring Data JPA creates an implementation on the fly when you run the application.

### **Service Class**

```java
package org.springboot.service;

import java.util.Collection
import org.springboot.model.Greeting;

public interface GreetingService {

    Collection<Greeting> findAll();
    Greeting findOne(Long id);
    Greeting create(Greeting greeting);
    Greeting update(Greeting greeting);
    void delete(Long id);

}

```

### **Service Bean**

```java
package org.springboot.service;

import java.util.Collection;
import org.springboot.model.Greeting;
import org.springboot.repository.GreetingRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class GreetingServiceBean implements GreetingService {

    @Autowired
    private GreetingRepository greetingRepository;

    @Override
    public Collection<Greeting> findAll() {
        Collection<Greeting> greetings = greetingRepository.findAll();
        return greetings;
    }

    @Override
    public Greeting findOne(Long id) {
        Greeting greeting = greetingRepository.findOne(id);
        return greeting;
    }

    @Override
    public Greeting create(Greeting greeting) {
        if (greeting.getId() != null) {
            //cannot create Greeting with specified Id value
            return null;
        }
        Greeting savedGreeting = greetingRepository.save(greeting);
        return savedGreeting;
    }

    @Override
    public Greeting update(Greeting greeting) {
        Greeting greetingPersisted = findOne(greeting.getId());
        if (greetingPersisted == null) {
            //cannot find Greeting with specified Id value
            return null;
        }
        Greeting updatedGreeting = greetingRepository.save(greeting);
        return updatedGreeting;
    }

    @Override
    public void delete(Long id) {
         greetingRepository.delete(id);
    }

}

```

### **Controller Class**

```java
package org.springboot.web.api;

import java.util.Collection;
import org.springboot.model.Greeting;
import org.springboot.service.GreetingService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = "/api")
public class GreetingController {

    @Autowired
    private GreetingService greetingService;

    // GET [method = RequestMethod.GET] is a default method for any request. 
    // So we do not need to mention explicitly

    @RequestMapping(value = "/greetings", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Collection<Greeting>> getGreetings() {
        Collection<Greeting> greetings = greetingService.findAll();        
        return new ResponseEntity<Collection<Greeting>>(greetings, HttpStatus.OK);
    }

    @RequestMapping(value = "/greetings/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Greeting> getGreeting(@PathVariable("id") Long id) {
        Greeting greeting = greetingService.findOne(id);
        if(greeting == null) {
            return new ResponseEntity<Greeting>(HttpStatus.NOT_FOUND);
        }
        return new ResponseEntity<Greeting>(greeting, HttpStatus.OK);
    }
    
    @RequestMapping(value = "/greetings", method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Greeting> createGreeting(@RequestBody Greeting greeting) {
        Greeting savedGreeting = greetingService.create(greeting);
        return new ResponseEntity<Greeting>(savedGreeting, HttpStatus.CREATED);
    }

    @RequestMapping(value = "/greetings/{id}", method = RequestMethod.PUT, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Greeting> updateGreeting(@PathVariable("id") Long id, @RequestBody Greeting greeting) {
        Greeting updatedGreeting = null;
        if (greeting != null && id == greeting.getId()) {
            updatedGreeting = greetingService.update(greeting); 
        }
        if(updatedGreeting == null) {
            return new ResponseEntity<Greeting>(HttpStatus.INTERNAL_SERVER_ERROR);
        }
        return new ResponseEntity<Greeting>(updatedGreeting, HttpStatus.OK);
    }

    @RequestMapping(value = "/greetings/{id}", method = RequestMethod.DELETE)
    public ResponseEntity<Greeting> deleteGreeting(@PathVariable("id") Long id) {
        greetingService.delete(id);
        return new ResponseEntity<Greeting>(HttpStatus.NO_CONTENT);
    }

}

```

### **Application properties file for MySQL database**

```java
#mysql config
spring.datasource.url=jdbc:mysql://localhost:3306/springboot
spring.datasource.username=root
spring.datasource.password=Welcome@123
spring.datasource.driver-class-name=com.mysql.jdbc.Driver
spring.jpa.show-sql=true
spring.jpa.properties.hibernate.dialect=org.hibernate.dialect.MySQLDialect
spring.jpa.hibernate.ddl-auto = update

spring.jpa.hibernate.naming-strategy=org.hibernate.cfg.DefaultNamingStrategy

#initialization
spring.datasource.schema=classpath:/data/schema.sql

```

### **SQL file**

```java
drop table if exists greeting;
create table greeting (
    id bigint not null auto_increment, 
    text varchar(100) not null, 
    primary key(id)
);

```

### **pom.xml file**

```java
<project xmlns="http://maven.apache.org/POM/4.0.0" 
xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
http://maven.apache.org/xsd/maven-4.0.0.xsd">

<modelVersion>4.0.0</modelVersion>

<groupId>org</groupId>
<artifactId>springboot</artifactId>
<version>0.0.1-SNAPSHOT</version>
<packaging>war</packaging>
<parent>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-parent</artifactId>
    <version>1.2.1.RELEASE</version>
</parent>

<dependencies>
    <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-web</artifactId>
    </dependency>

    <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-data-jpa</artifactId>
    </dependency>
    
    <dependency>
        <groupId>mysql</groupId>
        <artifactId>mysql-connector-java</artifactId>
        <scope>runtime</scope>
    </dependency>

</dependencies>

<build>
    <plugins>
        <plugin>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-maven-plugin</artifactId>
        </plugin>
    </plugins>
</build>

```

### Building an executable JAR

You can run the application from the command line with Maven. Or you can build a single executable JAR file that contains all the necessary dependencies, classes, and resources, and run that. This makes it easy to ship, version, and deploy the service as an application throughout the development lifecycle, across different environments, and so forth.

Run the application using `./mvnw spring-boot:run`. Or you can build the JAR file with `./mvnw clean package`. Then you can run the JAR file:

```java
java -jar target/springboot-0.0.1-SNAPSHOT.jar

```



#### Remarks


### Annotations

`@Repository`: Indicates that an annotated class is a "Repository", a mechanism for encapsulating storage, retrieval, and search behavior which emulates a collection of objects. Teams implementing traditional J2EE patterns such as "Data Access Object" may also apply this stereotype to DAO classes, though care should be taken to understand the distinction between Data Access Object and DDD-style repositories before doing so. This annotation is a general-purpose stereotype and individual teams may narrow their semantics and use as appropriate.

`@RestController`: A convenience annotation that is itself annotated with `@Controller` and `@ResponseBody.Types` that carry this annotation are treated as controllers where `@RequestMapping` methods assume `@ResponseBody` semantics by default.

`@Service`: Indicates that an annotated class is a "Service" (e.g. a business service facade). This annotation serves as a specialization of `@Component`, allowing for implementation classes to be autodetected through classpath scanning.

`@SpringBootApplication`: Many Spring Boot developers always have their main class annotated with `@Configuration`, `@EnableAutoConfiguration` and `@ComponentScan`. Since these annotations are so frequently used together (especially if you follow the best practices above), Spring Boot provides a convenient `@SpringBootApplication` alternative.

`@Entity`: Specifies that the class is an entity. This annotation is applied to the entity class.

### Official Documentation

Pivotal Software has provided a pretty extensive documentation on Spring Framework, and it can be found at

- [https://projects.spring.io/spring-boot/](https://projects.spring.io/spring-boot/)
- [http://projects.spring.io/spring-data-jpa/](http://projects.spring.io/spring-data-jpa/)
- [https://spring.io/guides/gs/accessing-data-jpa/](https://spring.io/guides/gs/accessing-data-jpa/)

