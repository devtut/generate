---
metaTitle: "Spring Boot - REST Services"
description: "Creating a REST-Service, Creating a Rest Service with JERSEY and Spring Boot, Consuming a REST API with RestTemplate (GET)"
---

# REST Services




## Creating a REST-Service


1. Create project using STS (Spring Starter Project) or Spring Initializr (at [https://start.spring.io](https://start.spring.io) ).
1. Add a Web Dependency in your pom.xml:

```

   <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-web</artifactId>
    </dependency>

```

or type **web** in `Search for dependencies` search box, add web dependency and download zipped project.

1. Create a Domain Class (i.e. User)

```

public class User {
    
        private Long id;
    
        private String userName;
    
        private String password;
    
        private String email;
    
        private String firstName;
    
        private String lastName;
    
        public Long getId() {
            return id;
        }
    
        public void setId(Long id) {
            this.id = id;
        }
    
        public String getUserName() {
            return userName;
        }
    
        public void setUserName(String userName) {
            this.userName = userName;
        }
    
        public String getPassword() {
            return password;
        }
    
        public void setPassword(String password) {
            this.password = password;
        }
    
        public String getEmail() {
            return email;
        }
    
        public void setEmail(String email) {
            this.email = email;
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
    
        @Override
        public String toString() {
            return "User [id=" + id + ", userName=" + userName + ", password=" + password + ", email=" + email
                    + ", firstName=" + firstName + ", lastName=" + lastName + "]";
        }
    
        public User(Long id, String userName, String password, String email, String firstName, String lastName) {
            super();
            this.id = id;
            this.userName = userName;
            this.password = password;
            this.email = email;
            this.firstName = firstName;
            this.lastName = lastName;
        }
    
        public User() {}
    }

```


1. Create UserController class and add `@Controller`, `@RequestMapping` annotations

```

   @Controller
    @RequestMapping(value = "api")
    public class UserController {
    }

```


1. Define static List users variable to simulate database and add 2 users to the list

```

   private static List<User> users = new ArrayList<User>();

    public UserController() {
        User u1 = new User(1L, "shijazi", "password", "shijazi88@gmail.com", "Safwan", "Hijazi");
        User u2 = new User(2L, "test", "password", "test@gmail.com", "test", "test");
        users.add(u1);
        users.add(u2);
    }

```


1. Create new method to return all users in static list (getAllUsers)

```

   @RequestMapping(value = "users", method = RequestMethod.GET)
    public @ResponseBody List<User> getAllUsers() {
         return users;
    }

```


<li>
Run the application [by `mvn clean install spring-boot:run`] and call this URL `http://localhost:8080/api/users`
</li>
<li>
We can annotate the class with `@RestController`, and in this case we can remove the ResponseBody from all methods in this class, `(@RestController = @Controller + ResponseBody)`, one more point we can control the return http code if we use `ResponseEntity`, we will implement same previous functions but using `@RestController` and `ResponseEntity`
</li>

```java
@RestController
@RequestMapping(value = "api2")
public class UserController2 {

    private static List<User> users = new ArrayList<User>();

    public UserController2() {
        User u1 = new User(1L, "shijazi", "password", "shijazi88@gmail.com", "Safwan", "Hijazi");
        User u2 = new User(2L, "test", "password", "test@gmail.com", "test", "test");
        users.add(u1);
        users.add(u2);
    }
    
    @RequestMapping(value = "users", method = RequestMethod.GET)
    public ResponseEntity<?> getAllUsers() {
       try {
           return new ResponseEntity<>(users, HttpStatus.OK);
       } catch (Exception e) {
           return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
       }
    }
}

```

now try to run the application and call this URL [http://localhost:8080/api2/users](http://localhost:8080/api2/users)



## Creating a Rest Service with JERSEY and Spring Boot


Jersey is one of the many frameworks available to create Rest Services, This example will show you how to create Rest Services using Jersey and Spring Boot

### 1.Project Setup

You can create a new project using STS or by using the [Spring Initializr](https://start.spring.io/) page. While creating a project, include the following dependencies:

1. Jersey (JAX-RS)
1. Web

### 2.Creating a Controller

Let us create a controller for our Jersey Web Service

```java
@Path("/Welcome")
@Component
public class MyController {
    @GET
    public String welcomeUser(@QueryParam("user") String user){
        return "Welcome "+user;
    }    
}

```

`@Path("/Welcome")` annotation indicates to the framework that this controller should respond to the URI path /Welcome

`@QueryParam("user")` annotation indicates to the framework that we are expecting one query parameter with the name `user`

### 3.Wiring Jersey Configurations

Let us now configure Jersey Framework with Spring Boot:
Create a class, rather a spring component which extends `org.glassfish.jersey.server.ResourceConfig`:

```java
@Component
@ApplicationPath("/MyRestService")
public class JerseyConfig extends ResourceConfig {
    /**
     * Register all the Controller classes in this method 
     * to be available for jersey framework
     */
    public JerseyConfig() {
        register(MyController.class);
    }

}

```

`@ApplicationPath("/MyRestService")` indicates to the framework that only requests directed to the path `/MyRestService` are meant to be handled by the jersey framework, other requests should still continue to be handeld by spring framework.

It is a good idea to annotate the configuration class with `@ApplicationPath`, otherwise all the requests will be handled by Jersey and we will not be able to bypass it and let a spring controller handle it if required.

### 4.Done

Start the application and fire a sample URL like (Assuming you have configured spring boot to run on port 8080):

`http://localhost:8080/MyRestService/Welcome?user=User`

You should see a message in your browser like:

**Welcome User**

And you are done with your Jersey Web Service with Spring Boot



## Consuming a REST API with RestTemplate (GET)


To consume a REST API with `RestTemplate`, create a Spring boot project with the Spring boot initialzr and make sure the **Web** dependency is added:

```java
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-web</artifactId>
</dependency>

```

Once [you've set up your project](https://stackoverflow.com/documentation/spring-boot/829/introduction-to-spring-boot/2804/installation-or-setup#t=201608010634522359805), create a `RestTemplate` bean. You can do this within the main class that has already been generated, or within a separate configuration class (a class annotated with `@Configuration`):

```java
@Bean
public RestTemplate restTemplate() {
    return new RestTemplate();
}

```

After that, create a domain class, similar to how you should do when [creating a REST service](https://stackoverflow.com/documentation/spring-boot/1920/rest-services/6268/creating-a-rest-service).

```java
public class User {
    private Long id;
    private String username;
    private String firstname;
    private String lastname;

    public Long getId() {
        return id;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getFirstname() {
        return firstname;
    }

    public void setFirstname(String firstname) {
        this.firstname = firstname;
    }

    public String getLastname() {
        return lastname;
    }

    public void setLastname(String lastname) {
        this.lastname = lastname;
    }
}

```

In your client, autowire the `RestTemplate`:

```java
@Autowired
private RestTemplate restTemplate;

```

To consume a REST API that is returning a single user, you can now use:

```java
String url = "http://example.org/path/to/api";
User response = restTemplate.getForObject(url, User.class);

```

Consuming a REST API that is returning a list or array of users, you have two options. Either consume it as an array:

```java
String url = "http://example.org/path/to/api";
User[] response = restTemplate.getForObject(url, User[].class);

```

Or consume it using the `ParameterizedTypeReference`:

```java
String url = "http://example.org/path/to/api";
ResponseEntity<List<User>> response = restTemplate.exchange(url, HttpMethod.GET, null, new ParameterizedTypeReference<List<User>>() {});
List<User> data = response.getBody();

```

Be aware, when using `ParameterizedTypeReference`, you'll have to use the more advanced `RestTemplate.exchange()` method and you'll have to create a subclass of it. In the example above, an anonymous class is used.



#### Parameters


|Annotation|Column
|---|---|---|---|---|---|---|---|---|---
|@Controller|Indicates that an annotated class is a "Controller" (web controller).
|@RequestMapping|Annotation for mapping web requests onto specific handler classes (if we  used with class) and/or handler methods (if we used with methods).
|method = RequestMethod.GET|Type of HTTP request methods
|ResponseBody|Annotation that indicates a method return value should be bound to the web response body
|@RestController|@Controller + ResponseBody
|@ResponseEntity|Extension of HttpEntity that adds a HttpStatus status code, we can control the return http code

