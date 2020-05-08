---
metaTitle: "Spring Boot - Spring Boot + Spring Data Elasticsearch"
description: "Spring boot and spring data elasticsearch integration, Spring Boot and Spring Data Elasticsearch integration"
---

# Spring Boot + Spring Data Elasticsearch


[Spring Data Elasticsearch](https://github.com/spring-projects/spring-data-elasticsearch) is a [Spring Data](http://projects.spring.io/spring-data/) implementation for [Elasticsearch](https://github.com/elastic/elasticsearch) which provides integration with the [Elasticsearch](https://github.com/elastic/elasticsearch) search engine.



## Spring boot and spring data elasticsearch integration


In this example we are going to see a maven based spring boot application which integrates spring-data-elasticsearch. Here, we will do the followings and see the respective code segments.

- Insert a `Greeting(id, username, message)` item on elasticsearch.
- Get all items from elasticsearch
- Update a specific item.
- Delete a specific item.
- Get a specific item by id.
- Get a specific item by username.

**Project configuration file (pom.xml)**

```java
<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>

    <groupId>org.springdataes</groupId>
    <artifactId>springdataes</artifactId>
    <version>1.0-SNAPSHOT</version>
    <packaging>jar</packaging>

    <parent>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-parent</artifactId>
        <version>1.5.6.RELEASE</version>
    </parent>

    <dependencies>
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-web</artifactId>
        </dependency>

        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-data-elasticsearch</artifactId>
        </dependency>
    </dependencies>

    <build>
        <plugins>
            <plugin>
                <groupId>org.springframework.boot</groupId>
                <artifactId>spring-boot-maven-plugin</artifactId>
                <executions>
                    <execution>
                        <goals>
                            <goal>repackage</goal>
                        </goals>
                    </execution>
                </executions>
            </plugin>
        </plugins>
    </build>
</project>

```

We will use [Spring Boot](https://mvnrepository.com/artifact/org.springframework.boot/spring-boot-starter/1.5.6.RELEASE) of version `1.5.6.RELEASE` and [Spring Data Elasticsearch](https://github.com/spring-projects/spring-data-elasticsearch) of that respective version. For this project, we need to run [elasticsearch-2.4.5](https://www.elastic.co/downloads/past-releases/elasticsearch-2-4-5) to test our apis.

**Properties file**

We will put the project properties file (named `applications.properties`) in `resources` folder which contains:

```java
elasticsearch.clustername = elasticsearch
elasticsearch.host = localhost
elasticsearch.port = 9300

```

We will use the default cluster name, host and port. By default, `9300` port is used as transport port and `9200` port is known as http port. To see the default cluster name hit [http://localhost:9200/](http://localhost:9200/).

**Main Class(Application.java)**

```java
package org.springdataes;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class Application {
    public static void main(String []args) {
        SpringApplication.run(Application.class, args);
    }
}

```

`@SpringBootApplication` is a combination of `@Configuration`, `@EnableAutoConfiguration`, `@EnableWebMvc` and `@ComponentScan` annotations. The `main()` method uses Spring Boot’s `SpringApplication.run()` method to launch an application. There we don't need any xml configuration, this application is pure java spring application.

**Elasticsearch Configuration Class(ElasticsearchConfig.java)**

```java
package org.springdataes.config;

import org.elasticsearch.client.Client;
import org.elasticsearch.client.transport.TransportClient;
import org.elasticsearch.common.settings.Settings;
import org.elasticsearch.common.transport.InetSocketTransportAddress;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.data.elasticsearch.core.ElasticsearchOperations;
import org.springframework.data.elasticsearch.core.ElasticsearchTemplate;
import org.springframework.data.elasticsearch.repository.config.EnableElasticsearchRepositories;

import java.net.InetAddress;

@Configuration
@PropertySource(value = "classpath:applications.properties")
@EnableElasticsearchRepositories(basePackages = "org.springdataes.dao")
public class ElasticsearchConfig {
    @Value("${elasticsearch.host}")
    private String EsHost;

    @Value("${elasticsearch.port}")
    private int EsPort;

    @Value("${elasticsearch.clustername}")
    private String EsClusterName;

    @Bean
    public Client client() throws Exception {
        Settings esSettings = Settings.settingsBuilder()
                .put("cluster.name", EsClusterName)
                .build();

        return TransportClient.builder()
                .settings(esSettings)
                .build()
                .addTransportAddress(new InetSocketTransportAddress(InetAddress.getByName(EsHost), EsPort));
    }

    @Bean
    public ElasticsearchOperations elasticsearchTemplate() throws Exception {
        return new ElasticsearchTemplate(client());
    }
}

```

`ElasticsearchConfig` class configures elasticsearch to this project and make a connection with elasticsearch. Here, `@PropertySource` is used to read the `application.properties` file where we store the cluster name, elasticsearch host and port. `@EnableElasticsearchRepositories` is used to enable Elasticsearch repositories that Will scan the packages of the annotated configuration class for Spring Data repositories by default. `@Value` is used here for reading the properties from the `application.properties` file.

The `Client()` method creates a transport connection with elasticsearch.
The configuration above sets up an Embedded Elasticsearch Server which is used by the `ElasticsearchTemplate`. The `ElasticsearchTemplate` bean uses the `Elasticsearch Client` and provides a custom layer for manipulating data in Elasticsearch.

**Model Class(Greeting.java)**

```java
package org.springdataes.model;

import org.springframework.data.annotation.Id;
import org.springframework.data.elasticsearch.annotations.Document;
import java.io.Serializable;

@Document(indexName = "index", type = "greetings")
public class Greeting implements Serializable{

    @Id
    private String id;

    private String username;

    private String message;

    public Greeting() {
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }
}

```

Here we have annotated our `Greeting` data objects with a @Document annotation that we can also use to determine index settings like name, numbers of shards or number of replicas. One of the attributes of the class needs to be an `id`, either by annotating it with `@Id` or using one of the automatically found names `id` or `documentId`. Here, `id` field value will be auto-generated, if we don't set any value of `id` field.

**Elasticsearch Repository Class(GreetingRepository.class)**

```java
package org.springdataes.dao;

import org.springdataes.model.Greeting;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;
import java.util.List;

public interface GreetingRepository extends ElasticsearchRepository<Greeting, String> {
    List<Greeting> findByUsername(String username);
}

```

Here, We have extended `ElasticsearchRepository` which provide us many of apis that we don't need to define externally. This is the base repository class for `elasticsearch` based domain classes. Since it extends `Spring` based repository classes, we get the benefit of avoiding boilerplate code required to implement data access layers for various persistence stores.

Here we have declared a method `findByUsername(String username)` which will convert to a match query that matches with username with the `username` field of `Greeting` objects and returns the list of results.

**Services(GreetingService.java)**

```java
package org.springdataes.service;

import org.springdataes.model.Greeting;
import java.util.List;

public interface GreetingService {
    List<Greeting> getAll();
    Greeting findOne(String id);
    Greeting create(Greeting greeting);
    Greeting update(Greeting greeting);
    List<Greeting> getGreetingByUsername(String username);
    void delete(String id);
}

```

**Service Bean(GreetingServiceBean.java)**

```java
package org.springdataes.service;

import com.google.common.collect.Lists;
import org.springdataes.dao.GreetingRepository;
import org.springdataes.model.Greeting;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class GreetingServiceBean implements GreetingService {

    @Autowired
    private GreetingRepository repository;

    @Override
    public List<Greeting> getAll() {
        return Lists.newArrayList(repository.findAll());
    }

    @Override
    public Greeting findOne(String id) {
        return repository.findOne(id);
    }

    @Override
    public Greeting create(Greeting greeting) {
        return repository.save(greeting);
    }

    @Override
    public Greeting update(Greeting greeting) {
        Greeting persitedGreeting = repository.findOne(greeting.getId());
        if(persitedGreeting == null) {
            return null;
        }
        return repository.save(greeting);
    }

    @Override
    public List<Greeting> getGreetingByUsername(String username) {
        return repository.findByUsername(username);
    }

    @Override
    public void delete(String id) {
        repository.delete(id);
    }
}

```

In above class, we have `@Autowired` the `GreetingRepository`. We can simply call the `CRUDRepository` methods and the method we have declared in repository class with the `GreetingRepository` object.

In `getAll()` method, you may find a line `Lists.newArrayList(repository.findAll())`. We have done this to convert `repository.findAll()` to `List<>` item as it returns a `Iterable` List.

**Controller Class(GreetingController.java)**

```java
package org.springdataes.controller;

import org.springdataes.model.Greeting;
import org.springdataes.service.GreetingService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api")
public class GreetingController {

    @Autowired
    private GreetingService greetingService;

    @ResponseBody
    @RequestMapping(value = "/greetings", method = RequestMethod.GET)
    public ResponseEntity<List<Greeting>> getAll() {
        return new ResponseEntity<List<Greeting>>(greetingService.getAll(), HttpStatus.OK);
    }

    @ResponseBody
    @RequestMapping(value = "/greetings", method = RequestMethod.POST)
    public ResponseEntity<Greeting> insertGreeting(@RequestBody Greeting greeting) {
        return new ResponseEntity<Greeting>(greetingService.create(greeting), HttpStatus.CREATED);
    }

    @ResponseBody
    @RequestMapping(value = "/greetings", method = RequestMethod.PUT)
    public ResponseEntity<Greeting> updateGreeting(@RequestBody Greeting greeting) {
        return new ResponseEntity<Greeting>(greetingService.update(greeting), HttpStatus.MOVED_PERMANENTLY);
    }

    @ResponseBody
    @RequestMapping(value = "/greetings/{id}", method = RequestMethod.DELETE)
    public ResponseEntity<Greeting> deleteGreeting(@PathVariable("id") String idd) {
        greetingService.delete(idd);
        return new ResponseEntity<Greeting>(HttpStatus.NO_CONTENT);
    }

    @ResponseBody
    @RequestMapping(value = "/greetings{id}", method = RequestMethod.POST)
    public ResponseEntity<Greeting> getOne(@PathVariable("id") String idd) {
        return new ResponseEntity<Greeting>(greetingService.findOne(idd), HttpStatus.OK);
    }

    @ResponseBody
    @RequestMapping(value = "/greetings/{name}", method = RequestMethod.GET)
    public ResponseEntity<List<Greeting>> getByUserName(@PathVariable("name") String name) {
        return new ResponseEntity<List<Greeting>>(greetingService.getGreetingByUsername(name), HttpStatus.OK);
    }
}

```

**Build**

To build this maven application run

```java
mvn clean install

```

Above command first remove all the files in the `target` folder and build the project. After building the project we will get the executable .jar file which is named `springdataes-1.0-SNAPSHOT.jar`. We can run the main class(`Application.java`) to start the process or simply executing the above jar by typing:

```java
java -jar springdataes-1.0-SNAPSHOT.jar

```

**Checking the APIs**

For inserting a Greeting item in elasticsearch, execute the below command

```java
curl -H "Content-Type: application/json" -X POST -d '{"username":"sunkuet02","message": "this is a message"}' http://localhost:8080/api/greetings

```

You should get the below result like

```java
{"id":"AV2ddRxBcuirs1TrVgHH","username":"sunkuet02","message":"this is a message"}

```

You can also check the get api by executing:

```java
curl -H "Content-Type: application/json" -X GET http://localhost:8080/api/greetings

```

You should get

```java
[{"id":"AV2ddRxBcuirs1TrVgHH","username":"sunkuet02","message":"this is a message"}]

```

You can check other apis by following the above processes.

**Official Documentations:**

- [https://projects.spring.io/spring-boot/](https://projects.spring.io/spring-boot/)
- [https://projects.spring.io/spring-data-elasticsearch/](https://projects.spring.io/spring-data-elasticsearch/)



## Spring Boot and Spring Data Elasticsearch integration


In this example we are going to implement spring-data-elasticsearch project to store  POJO in elasticsearch. We will see a sample maven project which does the followings:

- Insert a `Greeting(id, username, message)` item on elasticsearch.
- Get All Greeting items which have been inserted.
- Update a Greeting item.
- Delete a Greeting item.
- Get a Greeting item by id.
- Get all Greeting items by username.

