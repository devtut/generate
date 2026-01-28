---
metaTitle: "Spring Boot - Testing in Spring Boot"
description: "How to Test a Simple Spring Boot Application, Loading different yaml [or properties] file or override some properties"
---

# Testing in Spring Boot



## How to Test a Simple Spring Boot Application


We have a sample Spring boot application which stores user data in MongoDB and we are using Rest services to retrieve data

First there is a domain class i.e. POJO

```java
@Document
public class User{
    @Id
    private String id;

    private String name;

}

```

A corresponding repository based on Spring Data MongoDB

```java
public interface UserRepository extends MongoRepository<User, String> {
}

```

Then Our User Controller

```java
@RestController
class UserController {
 
    @Autowired
    private UserRepository repository;
 
    @RequestMapping("/users")
    List<User> users() {
        return repository.findAll();
    }
 
    @RequestMapping(value = "/Users/{id}", method = RequestMethod.DELETE)
    @ResponseStatus(HttpStatus.NO_CONTENT)
    void delete(@PathVariable("id") String id) {
        repository.delete(id);
    }
 
    // more controller methods
}

```

And finally our Spring Boot Application

```java
@SpringBootApplication
public class Application {
    public static void main(String args[]){
     SpringApplication.run(Application.class, args);
    }
}

```

If, let’s say that John Cena, The Rock and TripleHHH were the only three users in the database, a request to /users would give the following response:

```java
$ curl localhost:8080/users
[{"name":"John Cena","id":"1"},{"name":"The Rock","id":"2"},{"name":"TripleHHH","id":"3"}]

```

Now to test the code we will verify that the application work

```java
@RunWith(SpringJUnit4ClassRunner.class)   // 1
@SpringApplicationConfiguration(classes = Application.class)   // 2
@WebAppConfiguration   // 3
@IntegrationTest("server.port:0")   // 4
public class UserControllerTest {

    @Autowired   // 5
    UserRepository repository;

    User cena;
    User rock;
    User tripleHHH;

    @Value("${local.server.port}")   // 6
    int port;

    @Before
    public void setUp() {
        // 7
        cena = new User("John Cena");
        rock = new User("The Rock");
        tripleHHH = new User("TripleHH");

        // 8
        repository.deleteAll();
        repository.save(Arrays.asList(cena, rock, tripleHHH));

        // 9
        RestAssured.port = port;
    }

    // 10
    @Test
    public void testFetchCena() {
        String cenaId = cena.getId();

        when().
                get("/Users/{id}", cenaId).
        then().
                statusCode(HttpStatus.SC_OK).
                body("name", Matchers.is("John Cena")).
                body("id", Matchers.is(cenaId));
    }

    @Test
    public void testFetchAll() {
        when().
                get("/users").
        then().
                statusCode(HttpStatus.SC_OK).
                body("name", Matchers.hasItems("John Cena", "The Rock", "TripleHHH"));
    }

    @Test
    public void testDeletetripleHHH() {
        String tripleHHHId = tripleHHH.getId();

        when()
                .delete("/Users/{id}", tripleHHHId).
        then().
                statusCode(HttpStatus.SC_NO_CONTENT);
    }
}

```

Explanation

1. Like any other Spring based test, we need the `SpringJUnit4ClassRunner` so that an application context is created.
1. The `@SpringApplicationConfiguration` annotation is similar to the `@ContextConfiguration` annotation in that it is used to specify which application context(s) that should be used in the test. Additionally, it will trigger logic for reading Spring Boot specific configurations, properties, and so on.
1. `@WebAppConfiguration` must be present in order to tell Spring that a `WebApplicationContext` should be loaded for the test. It also provides an attribute for specifying the path to the root of the web application.
1. `@IntegrationTest` is used to tell Spring Boot that the embedded web server should be started. By providing colon- or equals-separated name-value pair(s), any environment variable can be overridden. In this example, the `"server.port:0"` will override the server’s default port setting. Normally, the server would start using the specified port number, but the value 0 has a special meaning. When specified as 0, it tells Spring Boot to scan the ports on the host environment and start the server on a random, available port. That is useful if we have different services occupying different ports on the development machines and the build server that could potentially collide with the application port, in which case the application will not start. Secondly, if we create multiple integration tests with different application contexts, they may also collide if the tests are running concurrently.
1. We have access to the application context and can use autowiring to inject any Spring bean.
1. The `@Value("${local.server.port}”)` will be resolved to the actual port number that is used.
1. We create some entities that we can use for validation.
1. The MongoDB database is cleared and re-initialized for each test so that we always validate against a known state. Since the order of the tests is not defined, chances are that the testFetchAll() test fails if it is executed after the testDeletetripleHHH() test.
1. We instruct [Rest Assured](https://github.com/rest-assured/rest-assured) to use the correct port. It is an open source project that provides a Java DSL for testing restful services
1. Tests are implemented by using Rest Assured. we can implement the tests using the TestRestTemplate or any other http client, but I use Rest Assured because we can write concise documentation using [RestDocs](http://projects.spring.io/spring-restdocs/)



## Loading different yaml [or properties] file or override some properties


When we use  `@SpringApplicationConfiguration` it will use configuration from `application.yml` [properties] which in certain situation is not appropriate. So to override the properties we can use `@TestPropertySource` annotation.

```java
@TestPropertySource(
        properties = {
                "spring.jpa.hibernate.ddl-auto=create-drop",
                "liquibase.enabled=false"
        }
)
@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
public class ApplicationTest{

    // ...

}

```

We can use **properties** attribute of `@TestPropertySource` to override the specific **properties** we want. In above example we are **overriding** property `spring.jpa.hibernate.ddl-auto` to `create-drop`. And `liquibase.enabled` to `false`.

### Loading different yml file

If you want to totally load different **yml** file for test you can use **locations** attribute on `@TestPropertySource`.

```java
@TestPropertySource(locations="classpath:test.yml")
@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(Application.class)
public class ApplicationTest{

    // ...

}

```

### Alternatively options

**Option 1:**

You can also load different **yml** file my placing a **yml** file on **`test > resource`** directory

**Option 2:**

Using `@ActiveProfiles` annotation

```java
@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(classes = Application.class)
@ActiveProfiles("somename")
public class MyIntTest{
}

```

You can see we are using `@ActiveProfiles` annotation and we are passing the **somename** as the value.

Create a file called **`application-somename.yml`** and and the test will load this file.

