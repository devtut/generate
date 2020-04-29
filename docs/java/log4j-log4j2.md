---
metaTitle: "log4j / log4j2"
description: "How to get Log4j, Setting up property file, Properties-File to log to DB, How to use Log4j in Java code, Basic log4j2.xml configuration file, Migrating from log4j 1.x to 2.x, Filter Logoutput by level (log4j 1.x)"
---

# log4j / log4j2


[Apache Log4j](https://en.wikipedia.org/wiki/Log4j) is a Java-based logging utility, it is one of several Java logging frameworks. This topic is to show how to setup and configure Log4j in Java with detailed examples on all of its possible aspects of usage.



## How to get Log4j


**Current version (log4j2)**

**Using Maven:**<br />
Add the following dependency to your `POM.xml` file:

```java
<dependencies>
  <dependency>
    <groupId>org.apache.logging.log4j</groupId>
    <artifactId>log4j-api</artifactId>
    <version>2.6.2</version>
  </dependency>
  <dependency>
    <groupId>org.apache.logging.log4j</groupId>
    <artifactId>log4j-core</artifactId>
    <version>2.6.2</version>
  </dependency>
</dependencies>

```

**Using Ivy:**

```java
<dependencies>
  <dependency org="org.apache.logging.log4j" name="log4j-api" rev="2.6.2" />
  <dependency org="org.apache.logging.log4j" name="log4j-core" rev="2.6.2" />
</dependencies>

```

**Using Gradle:**

```java
dependencies {
  compile group: 'org.apache.logging.log4j', name: 'log4j-api', version: '2.6.2'
  compile group: 'org.apache.logging.log4j', name: 'log4j-core', version: '2.6.2'
}

```

**Getting log4j 1.x**

**Note:** Log4j 1.x has reached End-of-Life (EOL) (see Remarks).

**Using Maven:**

Declare this dependency in the `POM.xml` file:

```java
<dependency>
    <groupId>log4j</groupId>
    <artifactId>log4j</artifactId>
    <version>1.2.17</version>
</dependency>

```

**Using Ivy:**

```java
<dependency org="log4j" name="log4j" rev="1.2.17"/>

```

**Usign Gradle:**

```java
compile group: 'log4j', name: 'log4j', version: '1.2.17'

```

**Using Buildr:**

```java
'log4j:log4j:jar:1.2.17'

```

**Adding manually in path build:**

Download from Log4j [website project](http://logging.apache.org/log4j/1.2/download.html)



## Setting up property file


Log4j gives you posibility to log data into console and file at same time.
Create a `log4j.properties` file and put inside this basic configuration:

```java
# Root logger option
log4j.rootLogger=DEBUG, stdout, file

# Redirect log messages to console
log4j.appender.stdout=org.apache.log4j.ConsoleAppender
log4j.appender.stdout.Target=System.out
log4j.appender.stdout.layout=org.apache.log4j.PatternLayout
log4j.appender.stdout.layout.ConversionPattern=%d{yyyy-MM-dd HH:mm:ss} %-5p %c{1}:%L - %m%n

# Redirect log messages to a log file, support file rolling.
log4j.appender.file=org.apache.log4j.RollingFileAppender
log4j.appender.file.File=C:\\log4j-application.log
log4j.appender.file.MaxFileSize=5MB
log4j.appender.file.MaxBackupIndex=10
log4j.appender.file.layout=org.apache.log4j.PatternLayout
log4j.appender.file.layout.ConversionPattern=%d{yyyy-MM-dd HH:mm:ss} %-5p %c{1}:%L - %m%n

```

If you are using maven, put this propertie file in path:

```java
/ProjectFolder/src/java/resources

```



## Properties-File to log to DB


For this example to work you'll need a JDBC driver compatible to the system the database is running on. An opensource one that allows you to connect to DB2 databases on an IBM System i can be found here: [JT400](http://jt400.sourceforge.net/)

Even though this example is DB2 specific, it works for almost every other system if you exchange the driver and adapt the JDBC URL.

```java
# Root logger option
log4j.rootLogger= ERROR, DB

# Redirect log messages to a DB2
# Define the DB appender   
log4j.appender.DB=org.apache.log4j.jdbc.JDBCAppender

# Set JDBC URL (!!! adapt to your target system !!!)
log4j.appender.DB.URL=jdbc:as400://10.10.10.1:446/DATABASENAME;naming=system;errors=full;

# Set Database Driver (!!! adapt to your target system !!!)
log4j.appender.DB.driver=com.ibm.as400.access.AS400JDBCDriver

# Set database user name and password
log4j.appender.DB.user=USER
log4j.appender.DB.password=PASSWORD

# Set the SQL statement to be executed.
log4j.appender.DB.sql=INSERT INTO DB.TABLENAME VALUES('%d{yyyy-MM-dd}','%d{HH:mm:ss}','%C','%p','%m')

# Define the layout for file appender
log4j.appender.DB.layout=org.apache.log4j.PatternLayout

```



## How to use Log4j in Java code


First need to create a `final static logger` object:

```java
final static Logger logger = Logger.getLogger(classname.class);

```

Then, call logging methods:

```java
//logs an error message
logger.info("Information about some param: " + parameter); // Note that this line could throw a NullPointerException!

//in order to improve performance, it is advised to use the `isXXXEnabled()` Methods
if( logger.isInfoEnabled() ){
    logger.info("Information about some param: " + parameter);
}

// In log4j2 parameter substitution is preferable due to readability and performance
// The parameter substitution only takes place if info level is active which obsoletes the use of isXXXEnabled().
logger.info("Information about some param: {}" , parameter);

//logs an exception
logger.error("Information about some error: ", exception);

```



## Basic log4j2.xml configuration file


```java
<?xml version="1.0" encoding="UTF-8"?>
<Configuration>
  <Appenders>
    <Console name="STDOUT" target="SYSTEM_OUT">
      <PatternLayout pattern="%d %-5p [%t] %C{2} %m%n"/>
    </Console>
  </Appenders>
  <Loggers>
    <Root level="debug">
      <AppenderRef ref="STDOUT"/>
    </Root>
  </Loggers>
</Configuration>

```

This is a basic log4j2.xml configuration which has a console appender and a root logger. The pattern layout specifies which pattern should be used for logging the statements. <br>
In order to debug the loading of log4j2.xml you can add the attribute `status = <WARN | DEBUG | ERROR | FATAL | TRACE | INFO>` in the configuration tag of your log4j2.xml. <br>
You can also add a monitor interval so that it loads the configuration again after the specified interval period. The monitor interval can be added to the configuration tag as follows: `monitorInterval = 30`. It means that the config will be loaded every 30 seconds.



## Migrating from log4j 1.x to 2.x


If you want to migrate from existing log4j 1.x in your project to log4j 2.x then remove all existing log4j 1.x dependencies and add the following dependency:

**Log4j 1.x API Bridge**

**Maven Build**

```java
<dependencies>
  <dependency>
    <groupId>org.apache.logging.log4j</groupId>
    <artifactId>log4j-1.2-api</artifactId>
    <version>2.6.2</version>
  </dependency>
</dependencies>

```

**Ivy Build**

```java
<dependencies>
  <dependency org="org.apache.logging.log4j" name="log4j-1.2-api" rev="2.6.2" />
</dependencies>

```

**Gradle Build**

```java
dependencies {
  compile group: 'org.apache.logging.log4j', name: 'log4j-1.2-api', version: '2.6.2'
}

```

**Apache Commons Logging Bridge**
If your project is using Apache Commons Logging which use log4j 1.x and you want to migrate it to log4j 2.x then add the following dependencies:

**Maven Build**

```java
<dependencies>
  <dependency>
    <groupId>org.apache.logging.log4j</groupId>
    <artifactId>log4j-jcl</artifactId>
    <version>2.6.2</version>
  </dependency>
</dependencies>

```

**Ivy Build**

```java
<dependencies>
  <dependency org="org.apache.logging.log4j" name="log4j-jcl" rev="2.6.2" />
</dependencies>

```

**Gradle Build**

```java
dependencies {
  compile group: 'org.apache.logging.log4j', name: 'log4j-jcl', version: '2.6.2'
}

```

Note: Do not remove any existing dependencies of Apache commons logging

Reference: [https://logging.apache.org/log4j/2.x/maven-artifacts.html](https://logging.apache.org/log4j/2.x/maven-artifacts.html)



## Filter Logoutput by level (log4j 1.x)


You can use a filter to log only messages "lower" than e.g. `ERROR` level. **But the filter is not supported by PropertyConfigurator. So you must change to XML config to use it**. See [log4j-Wiki about filters](http://wiki.apache.org/logging-log4j/LogToAppenderByLevel?highlight=%28filter%29).

Example "specific level"

```java
<appender name="info-out" class="org.apache.log4j.FileAppender"> 
            <param name="File" value="info.log"/> 
            <layout class="org.apache.log4j.PatternLayout"> 
                    <param name="ConversionPattern" value="%m%n"/> 
            </layout> 
            <filter class="org.apache.log4j.varia.LevelMatchFilter">
                    <param name="LevelToMatch" value="info" />
                    <param name="AcceptOnMatch" value="true"/>
            </filter>
            <filter class="org.apache.log4j.varia.DenyAllFilter" />
</appender> 

```

Or "Level range"

```java
<appender name="info-out" class="org.apache.log4j.FileAppender"> 
            <param name="File" value="info.log"/> 
            <layout class="org.apache.log4j.PatternLayout"> 
                    <param name="ConversionPattern" value="%m%n"/> 
            </layout> 
            <filter class="org.apache.log4j.varia.LevelRangeFilter">
                    <param name="LevelMax" value="info"/>
                    <param name="LevelMin" value="info"/>
                    <param name="AcceptOnMatch" value="true"/>
            </filter>
</appender>

```



#### Syntax


- Logger.debug("text to log"); // Logging debugging info
- Logger.info("text to log");  // Logging common info
- Logger.error("text to log"); // Logging error info
- Logger.warn("text to log");  // Logging warnings
- Logger.trace("text to log"); // Logging trace info
- Logger.fatal("text to log"); // Logging fatal errors
- Log4j2 usage with parameter logging:
- Logger.debug("Debug params {} {} {}", param1, param2, param3); // Logging debug with parameters
- Logger.info("Info params {} {} {}", param1, param2, param3); // Logging info with parameters
- Logger.error("Error params {} {} {}", param1, param2, param3); // Logging error with parameters
- Logger.warn("Warn params {} {} {}", param1, param2, param3); // Logging warnings with parameters
- Logger.trace("Trace params {} {} {}", param1, param2, param3); // Logging trace with parameters
- Logger.fatal("Fatal params {} {} {}", param1, param2, param3); // Logging fatal with parameters
- Logger.error("Caught Exception: ", ex ); // Logging exception with message and stacktrace (will automatically be appended)



#### Remarks


### End of Life for Log4j 1 reached

> 
On August 5, 2015 the Logging Services Project Management Committee announced that Log4j 1.x had reached end of life. For complete text of the announcement please see the Apache Blog. **Users of Log4j 1 are recommended to upgrade to Apache Log4j 2**.


From: [http://logging.apache.org/log4j/1.2/](http://logging.apache.org/log4j/1.2/)

