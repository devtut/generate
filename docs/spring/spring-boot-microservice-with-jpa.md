---
metaTitle: "Spring Boot - Spring-Boot Microservice with JPA"
description: "Application Class, Book Model, Book Repository, Enabling validation, Loading some test data, Adding the Validator, Gradle Build File"
---

# Spring-Boot Microservice with JPA



## Application Class


```java
package com.mcf7.spring;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class SpringDataMicroServiceApplication {

    public static void main(String[] args) {
        SpringApplication.run(SpringDataMicroServiceApplication.class, args);
    }
}

```



## Book Model


```java
package com.mcf7.spring.domain;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.io.Serializable;

@lombok.Getter
@lombok.Setter
@lombok.EqualsAndHashCode(of = "isbn")
@lombok.ToString(exclude="id")
@Entity
public class Book implements Serializable {

    public Book() {}

    @Id
    @GeneratedValue(strategy= GenerationType.AUTO)
    private long id;

    @NotNull
    @Size(min = 1)
    private String isbn;

    @NotNull
    @Size(min = 1)
    private String title;

    @NotNull
    @Size(min = 1)
    private String author;

    @NotNull
    @Size(min = 1)
    private String description;
}

```

Just a note since a few things are going on here I wanted to break them down real quick.

All of the annotations with @lombok are generating some of our class's boiler plate

```java
@lombok.Getter  //Creates getter methods for our variables

@lombok.Setter  //Creates setter methods four our variables

@lombok.EqualsAndHashCode(of = "isbn") //Creates Equals and Hashcode methods based off of the isbn variable

@lombok.ToString(exclude="id") //Creates a toString method based off of every variable except id

```

We also leveraged Validation in this Object

```java
@NotNull  //This specifies that when validation is called this element shouldn't be null

@Size(min = 1)  //This specifies that when validation is called this String shouldn't be smaller than 1

```



## Book Repository


```java
package com.mcf7.spring.domain;

import org.springframework.data.repository.PagingAndSortingRepository;

public interface BookRepository extends PagingAndSortingRepository<Book, Long> {
}

```

Basic Spring Repository pattern, except we enabled a Paging and Sorting Repository for extra features like.... paging and sorting :)



## Enabling validation


```java
package com.mcf7.spring.domain;

import org.springframework.validation.Errors;
import org.springframework.validation.Validator;


public class BeforeCreateBookValidator implements Validator{
    public boolean supports(Class<?> clazz) {
        return Book.class.equals(clazz);
    }

    public void validate(Object target, Errors errors) {
        errors.reject("rejected");
    }
}

```



## Loading some test data


```java
package com.mcf7.spring.domain;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

@Component
public class DatabaseLoader implements CommandLineRunner {
    private final BookRepository repository;

    @Autowired
    public DatabaseLoader(BookRepository repository) {
        this.repository = repository;
    }

    public void run(String... Strings) throws Exception {
        Book book1 = new Book();
        book1.setIsbn("6515616168418510");
        book1.setTitle("SuperAwesomeTitle");
        book1.setAuthor("MCF7");
        book1.setDescription("This Book is super epic!");
        repository.save(book1);
    }
}

```

Just loading up some test data ideally this should be added only under a development profile.



## Adding the Validator


```java
package com.mcf7.spring.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.data.rest.core.event.ValidatingRepositoryEventListener;
import org.springframework.data.rest.webmvc.config.RepositoryRestConfigurerAdapter;
import org.springframework.validation.Validator;
import org.springframework.validation.beanvalidation.LocalValidatorFactoryBean;

@Configuration
public class RestValidationConfiguration extends RepositoryRestConfigurerAdapter {

    @Bean
    @Primary
    /**
     * Create a validator to use in bean validation - primary to be able to autowire without qualifier
     */
    Validator validator() {
        return new LocalValidatorFactoryBean();
    }


    @Override
    public void configureValidatingRepositoryEventListener(ValidatingRepositoryEventListener validatingListener) {
        Validator validator = validator();
        //bean validation always before save and create
        validatingListener.addValidator("beforeCreate", validator);
        validatingListener.addValidator("beforeSave", validator);
    }
}

```



## Gradle Build File


```java
buildscript {
    repositories {
        jcenter()
    }
    dependencies {
        classpath 'io.spring.gradle:dependency-management-plugin:0.5.4.RELEASE'
    }
}

apply plugin: 'io.spring.dependency-management'
apply plugin: 'idea'
apply plugin: 'java'

dependencyManagement {
    imports {
        mavenBom 'io.spring.platform:platform-bom:2.0.5.RELEASE'
    }
}

sourceCompatibility = 1.8
targetCompatibility = 1.8

repositories {
    mavenCentral()
}

dependencies {
    compile 'org.springframework.boot:spring-boot-starter-web'
    compile 'org.springframework.boot:spring-boot-starter-data-jpa'
    compile 'org.springframework.boot:spring-boot-starter-data-rest'
    compile 'org.springframework.data:spring-data-rest-hal-browser'
    compile 'org.projectlombok:lombok:1.16.6'
    compile 'org.springframework.boot:spring-boot-starter-validation'
    compile 'org.springframework.boot:spring-boot-actuator'

    runtime 'com.h2database:h2'

    testCompile 'org.springframework.boot:spring-boot-starter-test'
    testCompile 'org.springframework.restdocs:spring-restdocs-mockmvc'
}

```

