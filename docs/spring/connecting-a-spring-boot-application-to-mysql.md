---
metaTitle: "Spring Boot - Connecting a spring-boot application to MySQL"
description: "Spring-boot sample using MySQL"
---

# Connecting a spring-boot application to MySQL


We know that spring-boot by default runs using H2 database. In this article, we will see how to tweak the default configuration to work with MySQL database.



## Spring-boot sample using MySQL


We will follow the [official guide for spring-boot and spring-data-jpa](https://spring.io/guides/gs/accessing-data-jpa/). We will be building the application using gradle.

<li>
**Create the gradle build file**
build.gradle

```java
buildscript {
    repositories {
        mavenCentral()
    }
    dependencies {
        classpath("org.springframework.boot:spring-boot-gradle-plugin:1.4.3.RELEASE")
    }
}

apply plugin: 'java'
apply plugin: 'eclipse'
apply plugin: 'idea'
apply plugin: 'org.springframework.boot'

jar {
    baseName = 'gs-accessing-data-jpa'
    version =  '0.1.0'
}

repositories {
    mavenCentral()
    maven { url "https://repository.jboss.org/nexus/content/repositories/releases" }
}

sourceCompatibility = 1.8
targetCompatibility = 1.8

dependencies {
    compile("org.springframework.boot:spring-boot-starter-data-jpa")
    runtime('mysql:mysql-connector-java')
    testCompile("junit:junit")
}

```


</li>

<li>
**Create the customer entity**
src/main/java/hello/Customer.java

```java
@Entity
public class Customer {

    @Id
    @GeneratedValue(strategy=GenerationType.AUTO)
    private Long id;
    private String firstName;
    private String lastName;

    protected Customer() {}

    public Customer(String firstName, String lastName) {
        this.firstName = firstName;
        this.lastName = lastName;
    }

    @Override
    public String toString() {
        return String.format(
                "Customer[id=%d, firstName='%s', lastName='%s']",
                id, firstName, lastName);
    }
}

```


</li>

<li>
**Create Repositories**
src/main/java/hello/CustomerRepository.java

```java
import java.util.List;
import org.springframework.data.repository.CrudRepository;

public interface CustomerRepository extends CrudRepository<Customer, Long> {
    List<Customer> findByLastName(String lastName);
}

```


</li>
<li>
**Create application.properties file**

```java
################### DataSource Configuration ##########################
jdbc.driverClassName=com.mysql.jdbc.Driver
jdbc.url=jdbc:mysql://localhost:3306/your_database_name
jdbc.username=username
jdbc.password=password

init-db=false

################### Hibernate Configuration ##########################

hibernate.dialect=org.hibernate.dialect.MySQLDialect
hibernate.show_sql=true
hibernate.hbm2ddl.auto=update

```


</li>
<li>
**Create the PersistenceConfig.java file**
</li>

> 
<p>In step 5, we will be defining how the datasource will be loaded and
how our application connects to MySQL. The above snippet is the bare
minimum configuration we need to connect to MySQL. Here we provide two
beans:</p>


```java
@Configuration
@EnableTransactionManagement
@EnableJpaRepositories(basePackages="hello")
public class PersistenceConfig 
{
    @Autowired
    private Environment env;
 
    @Bean
    public LocalContainerEntityManagerFactoryBean entityManagerFactory()
    {
        LocalContainerEntityManagerFactoryBean factory = new LocalContainerEntityManagerFactoryBean();
 
        HibernateJpaVendorAdapter vendorAdapter = new HibernateJpaVendorAdapter();
        vendorAdapter.setGenerateDdl(Boolean.TRUE);
        vendorAdapter.setShowSql(Boolean.TRUE);
 
        factory.setDataSource(dataSource());
        factory.setJpaVendorAdapter(vendorAdapter);
        factory.setPackagesToScan("hello");
 
        Properties jpaProperties = new Properties();
        jpaProperties.put("hibernate.hbm2ddl.auto", env.getProperty("hibernate.hbm2ddl.auto"));
        factory.setJpaProperties(jpaProperties);
 
        factory.afterPropertiesSet();
        factory.setLoadTimeWeaver(new InstrumentationLoadTimeWeaver());
        return factory;
    }
     
    @Bean
    public DataSource dataSource()
    {
        DriverManagerDataSource dataSource = new DriverManagerDataSource();
        dataSource.setDriverClassName(env.getProperty("jdbc.driverClassName"));
        dataSource.setUrl(env.getProperty("jdbc.url"));
        dataSource.setUsername(env.getProperty("jdbc.username"));
        dataSource.setPassword(env.getProperty("jdbc.password"));
        return dataSource;
    }
      
}

```


<li>****LocalContainerEntityManagerFactoryBean****
This gives us an handle over the EntityManagerFactory configurations and allows us to do customizations. It also allows us to inject the PersistenceContext in our components as below:</li>

```java
@PersistenceContext
private EntityManager em;

```


<li>****DataSource****
Here we return an instance of the `DriverManagerDataSource`. It is a simple implementation of the standard JDBC DataSource interface, configuring a plain old JDBC Driver via bean properties, and returning a new Connection for every getConnection call. Note that I recommend to use this strictly for testing purposes as there are better alternatives like `BasicDataSource` available. Refer [here](https://mytechrepo.wordpress.com/2015/02/17/drivermanagerdatasource-vs-basicdatasource/) for more details</li>

<li>
**Create an Application class**
src/main/java/hello/Application.java

```java
 @SpringBootApplication
 public class Application {

     private static final Logger log = LoggerFactory.getLogger(Application.class);
 
     @Autowired
     private CustomerRepository repository;
 
     public static void main(String[] args) {
         SpringApplication.run(TestCoreApplication.class, args);
     }
 
     @Bean
     public CommandLineRunner demo() {
         return (args) -> {
             // save a couple of customers
             repository.save(new Customer("Jack", "Bauer"));
             repository.save(new Customer("Chloe", "O'Brian"));
             repository.save(new Customer("Kim", "Bauer"));
             repository.save(new Customer("David", "Palmer"));
             repository.save(new Customer("Michelle", "Dessler"));
 
             // fetch all customers
             log.info("Customers found with findAll():");
             log.info("-------------------------------");
             for (Customer customer : repository.findAll()) {
                 log.info(customer.toString());
             }
             log.info("");
 
             // fetch an individual customer by ID
             Customer customer = repository.findOne(1L);
             log.info("Customer found with findOne(1L):");
             log.info("--------------------------------");
             log.info(customer.toString());
             log.info("");
 
             // fetch customers by last name
             log.info("Customer found with findByLastName('Bauer'):");
             log.info("--------------------------------------------");
             for (Customer bauer : repository.findByLastName("Bauer")) {
                 log.info(bauer.toString());
         }
         log.info("");
     };
 }

```


}
</li>
<li>
**Running the application**
</li>

**If you are using an IDE like STS**, you can simply right click your project -> Run As -> Gradle (STS) Build...
In the tasks list, type bootRun and Run.

**If you are using gradle on command line**, you can simply run the application as follows:

```java
./gradlew bootRun

```

You should see something like this:

```java
== Customers found with findAll():
Customer[id=1, firstName='Jack', lastName='Bauer']
Customer[id=2, firstName='Chloe', lastName='O'Brian']
Customer[id=3, firstName='Kim', lastName='Bauer']
Customer[id=4, firstName='David', lastName='Palmer']
Customer[id=5, firstName='Michelle', lastName='Dessler']

== Customer found with findOne(1L):
Customer[id=1, firstName='Jack', lastName='Bauer']

== Customer found with findByLastName('Bauer'):
Customer[id=1, firstName='Jack', lastName='Bauer']
Customer[id=3, firstName='Kim', lastName='Bauer']

```



#### Remarks


As a pre-requisite, make sure that MySQL is already running on port 3306 and has your database created.

