---
metaTitle: "Spring Boot - Spring Boot + JPA + REST"
description: "Spring Boot Startup, Domain Object, Repository Interface, Maven Configuration"
---

# Spring Boot + JPA + REST



## Spring Boot Startup


```java
package com.example;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class Application {

   //main entry point
    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }
    
}

```



## Domain Object


```java
package com.example.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;

//simple person object with JPA annotations

@Entity
public class Person {

    @Id
    @GeneratedValue(strategy=GenerationType.AUTO)
    private Long id;
    
    @Column
    private String firstName;
    
    @Column
    private String lastName;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getFirstName() {
        return firstName;
    }

    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }
    
}

```



## Repository Interface


```java
package com.example.domain;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;
import org.springframework.data.rest.core.annotation.RestResource;

//annotation exposes the 
@RepositoryRestResource(path="/person")
public interface PersonRepository extends JpaRepository<Person,Long> {

    //the method below allows us to expose a search query, customize the endpoint name, and specify a parameter name
    //the exposed URL is GET /person/search/byLastName?lastname=
    @RestResource(path="/byLastName")
    Iterable<Person> findByLastName(@Param("lastName") String lastName);

    //the methods below are examples on to prevent an operation from being exposed.  
    //For example DELETE; the methods are overridden and then annotated with RestResouce(exported=false) to make sure that no one can DELETE entities via REST
    @Override
    @RestResource(exported=false)
    default void delete(Long id) { }

    @Override
    @RestResource(exported=false)
    default void delete(Person entity) { }

    @Override
    @RestResource(exported=false)
    default void delete(Iterable<? extends Person> entities) { }

    @Override
    @RestResource(exported=false)
    default void deleteAll() { }

    @Override
    @RestResource(exported=false)
    default void deleteAllInBatch() { }

    @Override
    @RestResource(exported=false)
    default void deleteInBatch(Iterable<Person> arg0) { }


}

```



## Maven Configuration


```java
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>
    <parent>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-parent</artifactId>
        <version>1.4.0.RELEASE</version>
    </parent>
    <groupId>com.example</groupId>
    <artifactId>spring-boot-data-jpa-rest</artifactId>
    <version>0.0.1-SNAPSHOT</version>
    <name>spring-boot-data-jpa-rest</name>
    <build>
        <plugins>
            <plugin>
                <artifactId>maven-compiler-plugin</artifactId>
                <configuration>
                    <source>1.8</source>
                    <target>1.8</target>
                </configuration>
            </plugin>
            <plugin>
                <groupId>org.springframework.boot</groupId>
                <artifactId>spring-boot-maven-plugin</artifactId>
            </plugin>
        </plugins>
    </build>
    <dependencies>
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-data-rest</artifactId>
        </dependency>
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-data-jpa</artifactId>
        </dependency>
        <dependency>
            <groupId>com.h2database</groupId>
            <artifactId>h2</artifactId>
        </dependency
    </dependencies>
</project>

```



#### Remarks


This example uses Spring Boot, Spring Data JPA and Spring Data REST to expose a simple JPA-managed domain object via REST.  The example responds with the HAL JSON format and exposes a url accessible on `/person`.  The maven configuration includes a H2 in-memory database to support a quick standup.

