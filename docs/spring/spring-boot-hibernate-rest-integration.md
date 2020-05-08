---
metaTitle: "Spring Boot - Spring Boot- Hibernate-REST Integration"
description: "Add REST support, Add Hibernate support"
---

# Spring Boot- Hibernate-REST Integration



## Add REST support


1. Add **spring-boot-starter-web** dependency to pom.xml. You may skip **version** tag, if you are using **spring-boot-starter-parent** as the parent of your **pom.xml** ([reference](http://docs.spring.io/spring-boot/docs/current/reference/htmlsingle/#getting-started-first-application-dependencies)).

```

   <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-web</artifactId>
    </dependency>

```


<li>
Add REST controller to desired package, for example to **com.example.myproject.web.rest** ([reference](http://docs.spring.io/spring-boot/docs/current/reference/htmlsingle/#getting-started-first-application-code)):

```java
package com.example.myproject.web.rest;

import java.util.Map;
import java.util.HashMap;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletRequest;

@RestController
public class VersionController {
    @RequestMapping("/api/version")
    public ResponseEntity get() {
        final Map<String, String> responseParams = new HashMap();
        responseParams.put("requestStatus", "OK");
        responseParams.put("version", "0.1-SNAPSHOT");

        return ResponseEntity.ok().body(responseParams.build());
    }
}    

```


</li>
<li>
Start Spring Boot application ([reference](http://docs.spring.io/spring-boot/docs/current/reference/htmlsingle/#getting-started-first-application-run)).
</li>
<li>
Your controller is accessible at the address **[http://localhost:8080/api/version](http://localhost:8080/api/version)**.
</li>



## Add Hibernate support


1. Add **spring-boot-starter-data-jpa** dependency to pom.xml. You may skip **version** tag, if you are using **spring-boot-starter-parent** as the parent of your **pom.xml**. The dependency below brings Hibernate and everything related to JPA to your project ([reference](http://docs.spring.io/spring-boot/docs/current/reference/htmlsingle/#boot-features-embedded-database-support)).

```

   <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-data-jpa</artifactId>
    </dependency>

```


<li>
Add database driver to **pom.xml**. This one below is for H2 database ([reference](http://docs.spring.io/spring-boot/docs/current/reference/htmlsingle/#boot-features-embedded-database-support)).

```java
<dependency>
    <groupId>com.h2database</groupId>
    <artifactId>h2</artifactId>
</dependency>

```


</li>
<li>
Enable debug logging for Hibernate in **application.properties**
logging.level.org.hibernate.SQL = debug
</li>

or in **application.yml**

```java
logging:
  level:
    org.hibernate.SQL: debug

```


<li>
Add entity class to desired package under **${project.home}/src/main/java/**, for example under **com.example.myproject.domain** ([reference](http://docs.spring.io/spring-boot/docs/current/reference/htmlsingle/#boot-features-entity-classes)):

```java
package com.example.myproject.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import java.io.Serializable;

@Entity
public class City implements Serializable {

    @Id
    @GeneratedValue
    public Long id;

    @Column(nullable = false)
    public String name;
}

```


</li>
<li>
Add **import.sql** to **${project.home}/src/main/resources/**. Put **INSERT** statements into the file. This file will be used for database schema population on each start of the app ([reference](http://docs.spring.io/spring-boot/docs/current/reference/htmlsingle/#howto-initialize-a-database-using-hibernate)):

```java
insert into city(name) values ('Brisbane');

insert into city(name) values ('Melbourne');

```


</li>
<li>
Add Repository class to desired package under **${project.home}/src/main/java/**, for example under **com.example.myproject.service** ([reference](http://docs.spring.io/spring-boot/docs/current/reference/htmlsingle/#boot-features-spring-data-jpa-repositories)):

```java
package com.example.myproject.service;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import java.io.Serializable;

import com.example.myproject.domain.City;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.Repository;

interface CityRepository extends Repository<City, Long> {

    Page<City> findAll(Pageable pageable);

    Page<City> findByName(String name);
}

```


</li>

Basically that's it! At this point you already can access the database using the methods of **com.example.myproject.service.CityRepository**.

