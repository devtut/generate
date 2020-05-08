---
metaTitle: "Spring Boot - Spring-Boot + JDBC"
description: "schema.sql file, First JdbcTemplate Boot App, data.sql"
---

# Spring-Boot + JDBC


Spring Boot can be used to build and persist a SQL Relational DataBase. You can choose to connect to an H2 in memory DataBase using Spring Boot, or perhaps choose to connect to MySql DataBase, it's completely your choice. If you want to conduct CRUD operations against your DB you can do it using JdbcTemplate bean, this bean will automatically bean be provided by Spring Boot. Spring Boot will help you by providing auto configuration of some commonly used beans related to JDBC.



## schema.sql file


```java
CREATE SCHEMA IF NOT EXISTS `backgammon`;
USE `backgammon`;

DROP TABLE IF EXISTS `user_in_game_room`;
DROP TABLE IF EXISTS `game_users`;
DROP TABLE IF EXISTS `user_in_game_room`;

CREATE TABLE `game_users`
(
    `user_id` BIGINT NOT NULL AUTO_INCREMENT,
    `first_name` VARCHAR(255) NOT NULL,
    `last_name` VARCHAR(255) NOT NULL,
    `email` VARCHAR(255) NOT NULL UNIQUE,
    `user_name` VARCHAR(255) NOT NULL UNIQUE,
    `password` VARCHAR(255) NOT NULL,
    `role` VARCHAR(255) NOT NULL,
    `last_updated_date` DATETIME NOT NULL,
    `last_updated_by` BIGINT NOT NULL,
    `created_date` DATETIME NOT NULL,
    `created_by` BIGINT NOT NULL,
    PRIMARY KEY(`user_id`)
);

DROP TABLE IF EXISTS `game_rooms`;

CREATE TABLE `game_rooms`
(
    `game_room_id` BIGINT NOT NULL AUTO_INCREMENT,
    `name` VARCHAR(255) NOT NULL,
    `private` BIT(1) NOT NULL,
    `white` BIGINT DEFAULT NULL,
    `black` BIGINT DEFAULT NULL,
    `opened_by` BIGINT NOT NULL,
    `speed` BIT(3) NOT NULL,
    `last_updated_date` DATETIME NOT NULL,
    `last_updated_by` BIGINT NOT NULL,
    `created_date` DATETIME NOT NULL,
    `created_by` BIGINT NOT NULL,
    `token` VARCHAR(255) AS (SHA1(CONCAT(`name`, "This is a qwe secret 123", `created_by`, `created_date`))),
    PRIMARY KEY(`game_room_id`)
);

CREATE TABLE `user_in_game_room`
(
    `user_id` BIGINT NOT NULL,
    `game_room_id` BIGINT NOT NULL,
    `last_updated_date` DATETIME NOT NULL,
    `last_updated_by` BIGINT NOT NULL,
    `created_date` DATETIME NOT NULL,
    `created_by` BIGINT NOT NULL,
    PRIMARY KEY(`user_id`, `game_room_id`),
    FOREIGN KEY (`user_id`) REFERENCES `game_users`(`user_id`),
    FOREIGN KEY (`game_room_id`) REFERENCES `game_rooms`(`game_room_id`)
);

```



## First JdbcTemplate Boot App


```java
@SpringBootApplication
@RestController
public class SpringBootJdbcApplication {

    @Autowired
    private JdbcTemplate template;
    
    @RequestMapping("/cars")
    public List<Map<String,Object>> stocks(){
        return template.queryForList("select * from car");
    }
    
    public static void main(String[] args) {
        SpringApplication.run(SpringBootJdbcApplication.class, args);
    }
}

```



## data.sql


```java
insert into game_users values(..., ..., ..., ...);
insert into game_users values(..., ..., ..., ...);
insert into game_users values(..., ..., ..., ...);

```



#### Remarks


In order to get started, in your sts eclipse go to new --> Spring Starter Project --> fill in your Maven coordinates --> and add the next dependencies:

Under SQL tab --> add JDBC + add MySql (if MySql is your choice).

For Mysql you'll also need to add the MySql Java Connector.

In you Spring Boot application.properties file (you Spring Boot configuration file) you'll need to configure your Data Source credentials to MySql DB:

1. spring.datasource.url
1. spring.datasource.username
1. spring.datasource.password
1. spring.datasource.driver-class-name

for example:

```java
spring.datasource.url=jdbc:mysql://localhost/test
spring.datasource.username=dbuser
spring.datasource.password=dbpass
spring.datasource.driver-class-name=com.mysql.jdbc.Driver

```

Under the resources folder add the next two files:

<li>
schema.sql --> every time you run your application Spring Boot will run this file, inside it you suppose to write your DB schema, define tables and their relationships.
</li>
<li>
data.sql --> every time you run your application Spring Boot will run this file, inside it, you suppose to write data that will be inserted into your table as an initial initialization.
</li>

Spring Boot will provide you JdbcTemplate bean automatically so you can instantly can you use it like this:

```java
@Autowired
private JdbcTemplate template;

```

without any other configurations.

