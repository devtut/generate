---
metaTitle: "Spring - ApplicationContext Configuration"
description: "Autowiring, Bootstrapping the ApplicationContext, Java Configuration, Xml Configuration"
---

# ApplicationContext Configuration



## Autowiring


Autowiring is done using a **sterotype** annotation to specify what classes are going to be beans in the `ApplicationContext`, and using the `Autowired` and `Value` annotations to specify bean dependencies.  The unique part of autowiring is that there is no external `ApplicationContext` definition, as it is all done within the classes that are the beans themselves.

```java
@Component // The annotation that specifies to include this as a bean
           // in the ApplicationContext
class Book {
    
    @Autowired // The annotation that wires the below defined Author
               // instance into this bean
    Author author;

    String title = "It";

    Author getAuthor() { return author; }
    String getTitle() { return title; }
}

@Component // The annotation that specifies to include
           // this as a bean in the ApplicationContext
class Author {
    String firstName = "Steven";
    String lastName = "King";

    String getFirstName() { return firstName; }
    String getLastName() { return lastName; }
}

```



## Bootstrapping the ApplicationContext


### Java Config

The configuration class needs only to be a class that is on the classpath of your application and visible to your applications main class.

```java
class MyApp {
    public static void main(String[] args) throws Exception {
        AnnotationConfigApplicationContext appContext =
            new AnnotationConfigApplicationContext(MyConfig.class);
        
        // ready to retrieve beans from appContext, such as myObject.
    }
}

@Configuration
class MyConfig {
    @Bean
    MyObject myObject() {
        // ...configure myObject...
    }

    // ...define more beans...
}

```

### Xml Config

The configuration xml file needs only be on the classpath of your application.

```java
class MyApp {
    public static void main(String[] args) throws Exception {
        ClassPathXmlApplicationContext appContext =
            new ClassPathXmlApplicationContext("applicationContext.xml");
        
        // ready to retrieve beans from appContext, such as myObject.
    }
}

<?xml version="1.0" encoding="UTF-8"?>
<!-- applicationContext.xml -->
<beans xmlns="http://www.springframework.org/schema/beans"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="http://www.springframework.org/schema/beans
                        http://www.springframework.org/schema/beans/spring-beans.xsd">
    <bean id="myObject" class="com.example.MyObject">
        <!-- ...configure myObject... -->
    </bean>

    <!-- ...define more beans... -->
</beans>

```

### Autowiring

Autowiring needs to know which base packages to scan for annotated beans (`@Component`).  This is specified via the `#scan(String...)` method.

```java
class MyApp {
    public static void main(String[] args) throws Exception {
        AnnotationConfigApplicationContext appContext =
            new AnnotationConfigApplicationContext();
        appContext.scan("com.example");
        appContext.refresh();
        
        // ready to retrieve beans from appContext, such as myObject.
    }
}

// assume this class is in the com.example package.
@Component
class MyObject {
    // ...myObject definition...
}

```



## Java Configuration


Java configuration is typically done by applying the `@Configuration` annotation to a class to suggest that a class contains bean definitions.  The bean definitions are specified by applying the `@Bean` annotation to a method that returns an object.

```java
@Configuration // This annotation tells the ApplicationContext that this class
               // contains bean definitions.
class AppConfig {
    /**
     * An Author created with the default constructor
     * setting no properties
     */
    @Bean // This annotation marks a method that defines a bean 
    Author author1() {
        return new Author();
    }

    /**
     * An Author created with the constructor that initializes the 
     * name fields
     */
    @Bean
    Author author2() {
        return new Author("Steven", "King");
    }

    /**
     * An Author created with the default constructor, but  
     * then uses the property setters to specify name fields
     */
    @Bean
    Author author3() {
        Author author = new Author();
        author.setFirstName("George");
        author.setLastName("Martin");
        return author;
    }

    /**
     * A Book created referring to author2 (created above) via
     * a constructor argument.  The dependency is fulfilled by
     * invoking the method as plain Java.
     */
    @Bean
    Book book1() {
        return new Book(author2(), "It");
    }

    /**
     * A Book created referring to author3 (created above) via
     * a property setter.  The dependency is fulfilled by
     * invoking the method as plain Java.
     */
    @Bean
    Book book2() {
        Book book = new Book();
        book.setAuthor(author3());
        book.setTitle("A Game of Thrones");
        return book;
    }
}

```

```java
// The classes that are being initialized and wired above...
class Book { // assume package org.springframework.example
    Author author;
    String title;
    
    Book() {} // default constructor
    Book(Author author, String title) {
        this.author = author;
        this.title= title;
    }

    Author getAuthor() { return author; }
    String getTitle() { return title; }

    void setAuthor(Author author) {
        this.author = author;
    }

    void setTitle(String title) {
        this.title= title;
    }
}

class Author { // assume package org.springframework.example
    String firstName;
    String lastName;

    Author() {} // default constructor
    Author(String firstName, String lastName) {
        this.firstName = firstName;
        this.lastName = lastName;
    }

    String getFirstName() { return firstName; }
    String getLastName() { return lastName; }

    void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    void setLastName(String lastName) {
        this.lastName = lastName;
    }
}

```



## Xml Configuration


Xml configuration is typically done by defining beans within an xml file, using Spring's specific `beans` schema.  Under the root `beans` element, typical bean definition would be done using the `bean` subelement.

```java
<?xml version="1.0" encoding="UTF-8"?>
<beans xmlns="http://www.springframework.org/schema/beans"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="http://www.springframework.org/schema/beans
                        http://www.springframework.org/schema/beans/spring-beans.xsd">
    <!-- An Author created with the default constructor
         setting no properties -->
    <bean id="author1" class="org.springframework.example.Author" />
    
    <!-- An Author created with the constructor that initializes the 
         name fields -->
    <bean id="author2" class="org.springframework.example.Author">
        <constructor-arg index="0" value="Steven" />
        <constructor-arg index="1" value="King" />
    </bean>

    <!-- An Author created with the default constructor, but  
         then uses the property setters to specify name fields -->
    <bean id="author3" class="org.springframework.example.Author">
        <property name="firstName" value="George" />
        <property name="lastName" value="Martin" />
    </bean>

    <!-- A Book created referring to author2 (created above) via
         a constructor argument -->
    <bean id="book1" class="org.springframework.example.Book">
        <constructor-arg index="0" ref="author2" />
        <constructor-arg index="1" value="It" />
    </bean>

    <!-- A Book created referring to author3 (created above) via
         a property setter -->
    <bean id="book1" class="org.springframework.example.Book">
        <property name="author" ref="author3" />
        <property name="title" value="A Game of Thrones" />
    </bean>
</beans>

```

```java
// The classes that are being initialized and wired above...
class Book { // assume package org.springframework.example
    Author author;
    String title;
    
    Book() {} // default constructor
    Book(Author author, String title) {
        this.author = author;
        this.title= title;
    }

    Author getAuthor() { return author; }
    String getTitle() { return title; }

    void setAuthor(Author author) {
        this.author = author;
    }

    void setTitle(String title) {
        this.title= title;
    }
}

class Author { // assume package org.springframework.example
    String firstName;
    String lastName;

    Author() {} // default constructor
    Author(String firstName, String lastName) {
        this.firstName = firstName;
        this.lastName = lastName;
    }

    String getFirstName() { return firstName; }
    String getLastName() { return lastName; }

    void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    void setLastName(String lastName) {
        this.lastName = lastName;
    }
}

```



#### Remarks


Spring has made it so that configuring an `ApplicationContext` is extremely flexible.  There are numerous ways to apply each type of configuration, and they can all be mixed and matched together nicely.

**Java configuration** is a form of **explicit** configuration.  A `@Configuration` annotated class is used to specify the beans that will be a part of the `ApplicationContext`, as well as define and wire the dependencies of each bean.

**Xml configuration** is a form of **explicit** configuration.  A specific xml schema is used to define the beans that will be a part of the `ApplicationContext`.  This same schema is used to define and wire the dependencies of each bean.

**Autowiring** is a form of **automatic** configuration.  Certain annotations are used in class definitions to establish what beans will be a part of the `ApplicationContext`, and other annotations are used to wire the dependencies of these beans.

