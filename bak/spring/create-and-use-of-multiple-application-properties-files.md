---
metaTitle: "Spring Boot - Create and Use of multiple application.properties files"
description: "Dev and Prod environment using different datasources, Set the right spring-profile by building the application automatically (maven)"
---

# Create and Use of multiple application.properties files



## Dev and Prod environment using different datasources


After succesfully setup Spring-Boot application all the configuration is handled in an application.properties file. You will find the file at `src/main/resources/`.

Normally there is a need to have a database behind the application. For development its good to have a setup of `dev` and a `prod` environments. Using multiple `application.properties` files you can tell Spring-Boot with which environment the application should start.

A good example is to configure two databases. One for `dev` and one for `productive`.

For the `dev` environment you can use an in-memory database like `H2`.
Create a new file in `src/main/resources/` directory named `application-dev.properties`. Inside the file there is the configuration of the in-memory database:

```java
spring.datasource.url=jdbc:h2:mem:test
spring.datasource.driverClassName=org.h2.Driver
spring.datasource.username=sa
spring.datasource.password=

```

For the `prod` environment we will connect to a "real" database for example `postgreSQL`.
Create a new file in `src/main/resources/` directory named `application-prod.properties`. Inside the file there is the configuration of the `postgreSQL` database:

```java
spring.datasource.url= jdbc:postgresql://localhost:5432/yourDB
spring.datasource.username=postgres
spring.datasource.password=secret

```

In your default `application.properties` file you are now able to set which profile is activated and used by Spring-Boot. Just set one attribute inside:

```java
spring.profiles.active=dev

```

or

```java
spring.profiles.active=prod

```

Important is that the part after `-` in `application-dev.properties` is the identifier of the file.

Now you are able to start Spring-Boot application in develop or production mode by just changing the identifier. An in-Memory database will startup or the connection to a "real" database. Sure there are also much more use cases to have multiple property files.



## Set the right spring-profile by building the application automatically (maven)


By creating multiple properties files for the different environments or use cases, its sometimes hard to manually change the `active.profile` value to the right one. But there is a way to set the `active.profile` in the `application.properties` file while building the application by using `maven-profiles`.

Let's say there are three environments property files in our application:

**`application-dev.properties`:**

```java
spring.profiles.active=dev
server.port=8081

```

**`application-test.properties`:**

```java
spring.profiles.active=test
server.port=8082

```

**`application-prod.properties`.**

```java
spring.profiles.active=prod
server.port=8083

```

Those three files just differ in port and active profile name.

In the main `application.properties` file we set our spring profile using a [maven variable](https://docs.spring.io/spring-boot/docs/current/reference/html/howto-properties-and-configuration.html#howto-automatic-expansion-maven):

**`application.properties`.**

```java
spring.profiles.active=@profileActive@

```

After that we just have to add the maven profiles in our `pom.xml` We will set profiles for all three environments:

```

<profiles>
        <profile>
            <id>dev</id>
            <activation>
                <activeByDefault>true</activeByDefault>
            </activation>
            <properties>
                <build.profile.id>dev</build.profile.id>
                <profileActive>dev</profileActive>
            </properties>
        </profile>
        <profile>
            <id>test</id>
            <properties>
                <build.profile.id>test</build.profile.id>
                <profileActive>test</profileActive>
            </properties>
        </profile>
        <profile>
            <id>prod</id>
            <properties>
                <build.profile.id>prod</build.profile.id>
                <profileActive>prod</profileActive>
            </properties>
        </profile>
    </profiles>

```

You are now able to build the application with maven. If you dont set any maven profile, its building the default one (in this example it's dev). For specify one you have to use a maven keyword. The keyword to set a profile in maven is `-P` directly followed by the name of the profile: `mvn clean install -Ptest`.

Now, you are also able to create custom builds and save those in your `IDE` for faster builds.

**Examples:**

`mvn clean install -Ptest`

```

 .   ____          _            __ _ _
 /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
 \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
  '  |____| .__|_| |_|_| |_\__, | / / / /
 =========|_|==============|___/=/_/_/_/
 :: Spring Boot ::        (v1.5.3.RELEASE)

2017-06-06 11:24:44.885  INFO 6328 --- [           main] com.demo.SpringBlobApplicationTests      : Starting SpringApplicationTests on KB242 with PID 6328 (started by me in C:\DATA\Workspaces\spring-demo)
2017-06-06 11:24:44.886  INFO 6328 --- [           main] com.demo.SpringApplicationTests      : The following profiles are active: test

```

`mvn clean install -Pprod`

```

 .   ____          _            __ _ _
 /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
 \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
  '  |____| .__|_| |_|_| |_\__, | / / / /
 =========|_|==============|___/=/_/_/_/
 :: Spring Boot ::        (v1.5.3.RELEASE)

2017-06-06 14:43:31.067  INFO 6932 --- [           main] com.demo.SpringBlobApplicationTests      : Starting SpringApplicationTests on KB242 with PID 6328 (started by me in C:\DATA\Workspaces\spring-demo)
2017-06-06 14:43:31.069  INFO 6932 --- [           main] com.demo.SpringApplicationTests      : The following profiles are active: prod

```

