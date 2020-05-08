---
metaTitle: "Spring - Task Execution and Scheduling"
description: "Cron expression, Enable Scheduling, Fixed delay, Fixed Rate"
---

# Task Execution and Scheduling




## Cron expression


A Cron expression consists of six sequential fields -

```java
second, minute, hour, day of month, month, day(s) of week

```

and is declared as follows

```java
@Scheduled(cron = "* * * * * *")

```

We can also set the [timezone](https://docs.oracle.com/cd/B13866_04/webconf.904/b10877/timezone.htm) as -

```java
@Scheduled(cron="* * * * * *", zone="Europe/Istanbul")

```

**Notes: -**

```java
syntax            means                example                explanation
------------------------------------------------------------------------------------
*                 match any            "* * * * * *"          do always
*/x               every x              "*/5 * * * * *"        do every five seconds
?                 no specification     "0 0 0 25 12 ?"        do every Christmas Day

```

**Example: -**

```java
syntax                        means
------------------------------------------------------------------------------------
"0 0 * * * *"                 the top of every hour of every day.
"*/10 * * * * *"              every ten seconds.
"0 0 8-10 * * *"              8, 9 and 10 o'clock of every day.
"0 0/30 8-10 * * *"           8:00, 8:30, 9:00, 9:30 and 10 o'clock every day.
"0 0 9-17 * * MON-FRI"        on the hour nine-to-five weekdays
"0 0 0 25 12 ?"               every Christmas Day at midnight

```

A method declared with `@Scheduled()` is called explicitly for every matching case.

If we want some code to be executed when a cron expression is met, then we have to specify it in the annotation:

```java
@Component
public class MyScheduler{    
    
    @Scheduled(cron="*/5 * * * * MON-FRI")
    public void doSomething() {
        // this will execute on weekdays
    }
}

```

If we want to print current time in our console for every after 5 seconds -

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.text.SimpleDateFormat;
import java.util.Date;


@Component
public class Scheduler {

    private static final Logger log = LoggerFactory.getLogger(Scheduler.class);
    private static final SimpleDateFormat dateFormat = new SimpleDateFormat("HH:mm:ss");

    @Scheduled(cron = "*/5 * * * * *")
    public void currentTime() {
        log.info("Current Time      = {}", dateFormat.format(new Date()));
    }

}

```

**Example using XML configuration:**

Example class:

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.text.SimpleDateFormat;
import java.util.Date;


@Component("schedulerBean")
public class Scheduler {

    private static final Logger log = LoggerFactory.getLogger(Scheduler.class);
    private static final SimpleDateFormat dateFormat = new SimpleDateFormat("HH:mm:ss");

    public void currentTime() {
        log.info("Current Time      = {}", dateFormat.format(new Date()));
    }

}  

```

Example XML(task-context.xml):

```

 <?xml version="1.0" encoding="UTF-8"?>
<beans xmlns="http://www.springframework.org/schema/beans"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
    xmlns:task="http://www.springframework.org/schema/task"
    xsi:schemaLocation="http://www.springframework.org/schema/beans 
        http://www.springframework.org/schema/beans/spring-beans-4.1.xsd
        http://www.springframework.org/schema/task 
        http://www.springframework.org/schema/task/spring-task-4.1.xsd">


    <task:scheduled-tasks scheduler="scheduledTasks">
        <task:scheduled ref="schedulerBean" method="currentTime" cron="*/5 * * * * MON-FRI" />
    </task:scheduled-tasks>

    <task:scheduler id="scheduledTasks" />

</beans>

```



## Enable Scheduling


Spring provides a useful task scheduling support. To enable it, just annotate any of your [`@Configuration`](http://docs.spring.io/spring-framework/docs/4.0.4.RELEASE/javadoc-api/org/springframework/context/annotation/Configuration.html) classes with [`@EnableScheduling`](http://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/scheduling/annotation/EnableScheduling.html):

```

@Configuration
 @EnableScheduling
 public class MyConfig {

     // Here it goes your configuration
 }

```



## Fixed delay


If we want some code to be executed periodically after the execution which was before is finished, we should use fixed delay (measured in milliseconds):

```java
@Component
public class MyScheduler{    
    
    @Scheduled(fixedDelay=5000)
    public void doSomething() {
        // this will execute periodically, after the one before finishes
    }
}

```



## Fixed Rate


If we want something to be executed periodically, this code will be triggered once per the value in milliseconds we specify:

```java
@Component
public class MyScheduler{    
    
    @Scheduled(fixedRate=5000)
    public void doSomething() {
        // this will execute periodically
    }
}

```

