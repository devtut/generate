---
metaTitle: "Spring Boot - Caching with Redis Using Spring Boot for MongoDB"
description: "Why Caching?, The Basic System"
---

# Caching with Redis Using Spring Boot for MongoDB



## Why Caching?


Today, performance is one of the most important metrics we need to evaluate when developing a web service/Application. Keeping customers engaged is critical to any product and for this reason, it is extremely important to improve the performances and reduce page load times.

When running a web server that interacts with a database, its operations may become a bottleneck. MongoDB is no exception here, and as our MongoDB database scales up, things can really slow down. This issue can even get worse if the database server is detached from the web server. In such systems, the communication with the database can cause a big overhead.

Luckily, we can use a method called caching to speed things up. In this example, we’ll introduce this method and see how we can use it to enhance the performance of our application using Spring Cache, Spring Data, and Redis.



## The Basic System


As the first step, we’ll build a basic web server that stores data in MongoDB. For this demonstration, we’ll name it “fast Library”. The server will have two basic operations:

`POST /book`: This endpoint will receive the title, the author, and the content of the book, and create a book entry in the database.

`GET /book/ {title}`: This endpoint will get a title and return its content. We assume that titles uniquely identify books (thus, there won’t be two books with the same title). A better alternative would be, of course, to use an ID. However, to keep things simple, we’ll simply use the title.

This is a simple library system, but we’ll add more advanced abilities later.

Now, let’s create the project using Spring Tool Suite (build using eclipse) and spring starter Project

[<img src="http://i.stack.imgur.com/WS90q.jpg" alt="enter image description here" />](http://i.stack.imgur.com/WS90q.jpg)

We are building our project using Java and to build we are using maven, select values and click on next

[<img src="http://i.stack.imgur.com/qufTj.jpg" alt="enter image description here" />](http://i.stack.imgur.com/qufTj.jpg)

Select MongoDB, Redis from NOSQL and Web from the web module and click on finish. We are using Lombok for auto generation of Setters and getters of model values so we need to add the Lombok dependency to the POM

[<img src="http://i.stack.imgur.com/NQkCE.jpg" alt="enter image description here" />](http://i.stack.imgur.com/NQkCE.jpg)
[<img src="http://i.stack.imgur.com/Lao11.jpg" alt="enter image description here" />](http://i.stack.imgur.com/Lao11.jpg)

MongoDbRedisCacheApplication.java contains the main method which is used to run Spring Boot Application add

Create model class Book which contains id, book title, author, description and annotate with @Data to generate automatic setters and getters from jar project lombok

```java
package com.example;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.index.Indexed;
import lombok.Data;
@Data
public class Book {
    @Id
    private String id;
    @Indexed
    private String title;
    private String author;
    private String description;
}

```

Spring Data creates all basic CRUD Operations for us automatically so let’s create BookRepository.Java which finds book by title and deletes book

```java
package com.example;
import org.springframework.data.mongodb.repository.MongoRepository;
public interface BookRepository  extends MongoRepository<Book, String>
{
    Book findByTitle(String title);
    void delete(String title);
}

```

Let’s create webservicesController which saves data to MongoDB and retrieve data by idTitle(@PathVariable String title).

```java
package com.example;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;
@RestController
public class WebServicesController {
    @Autowired
    BookRepository repository;
    @Autowired
    MongoTemplate mongoTemplate;
    @RequestMapping(value = "/book", method = RequestMethod.POST)
    public Book saveBook(Book book) 
    {
        return repository.save(book);
    }
    @RequestMapping(value = "/book/{title}", method = RequestMethod.GET)
    public Book findBookByTitle(@PathVariable String title) 
    {
        Book insertedBook = repository.findByTitle(title);
        return insertedBook;
    }
}

```

**Adding the Cache**
So far we’ve created a basic library web service, but it’s not astonishingly fast at all. In this section, we’ll try to optimize the findBookByTitle () method by caching the results.

To get a better idea of how we’ll achieve this goal, let’s go back to the example of the people sitting in a traditional library. Let’s say they want to find the book with a certain title. First of all, they’ll look around the table to see if they already brought it there. If they have, that’s great! They just had a cache hit that is finding an item in the cache. If they haven’t found it, they had a cache miss, meaning they didn’t find the item in the cache. In the case of a missing item, they’ll have to look for the book in the library. When they find it, they’ll keep it on their table or insert it into the cache.

In our example, we’ll follow exactly the same algorithm for the findBookByTitle () method. When asked for a book with a certain title, we’ll look for it in the cache. If not found, we’ll look for it in the main storage, that is our MongoDB database.

**Using Redis**

Adding spring-boot-data-redis to our class path will allow spring boot to perform its magic. It will create all necessary operations by auto configuring

Let’s now annotate the method with below line to cache and let spring boot do its magic

```java
@Cacheable (value = "book", key = "#title")

```

To delete from the cache when a record is deleted just annotate with below line in BookRepository and let Spring Boot handle cache deletion for us.

```java
@CacheEvict (value = "book", key = "#title")

```

To update the data we need to add below line to the method and let spring boot handle

```java
@CachePut(value = "book", key = "#title")

```

You can find full Project code at [GitHub](https://github.com/rajadileepkolli/POC/tree/master/MongoDBRedisIntegration)

