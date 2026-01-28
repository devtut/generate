---
metaTitle: "Spring Boot - Fully-Responsive Spring Boot Web Application with JHipster"
description: "Create Spring Boot App using jHipster on Mac OS"
---

# Fully-Responsive Spring Boot Web Application with JHipster



## Create Spring Boot App using jHipster on Mac OS


jHipster allows you to bootstrap a Spring Boot web application with a REST API back-end and a AngularJS and Twitter Bootstrap front-end.

More on jHipster here:  [jHipster Documentation](https://jhipster.github.io/)

**Install brew:**

```

/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

```

View additional info on how to install brew here: [Install Brew](http://brew.sh/)

**Install Gradle**

Gradle is a dependency management and build system.

```

brew install gradle

```

**Install Git**

Git is a version control tool

```

brew install git

```

**Install NodeJS**

NodeJS gives you access to npm, the node package manager which is needed to install other tools.

```

brew install node

```

**Install Yeoman**

Yeoman is a generator

```

npm install -g yo

```

**Install Bower**

Bower is a dependency management tool

```

npm install -g bower

```

**Install Gulp**

Gulp is a task runner

```

npm install -g gulp

```

**Install jHipster Yeoman Generator**

This is the jHipster generator

```

npm install -g generator-jhipster

```

**Create an Application**

Open a Terminal window.

Navigate to the root directory where you will keep your projects. Create an empty directory in which you will create your application

```

mkdir myapplication

```

Go to that directory

```

cd myapplication/

```

To generate your application, type

```

yo jhipster

```

**You will be prompted with the following questions**

**Which type of application would you like to create?**

Your type of application depends on whether you wish to use a microservices architecture or not. A full explanation on microservices is available here, if unsure use the default “Monolithic application”.

Choose **Monolithic application** by default if you are not sure

**What is your default Java package name?**

Your Java application will use this as its root package.

**Which type of authentication would you like to use?**

Use basic session-based **Spring Security** by default if you are not sure

**Which type of database would you like to use?**

Which development database would you like to use?

This is the database you will use with your “development” profile. You can either use:

Use H2 by default if you are not sure

H2, running in-memory. This is the easiest way to use JHipster, but your data will be lost when you restart your server.

**Do you want to use Hibernate 2nd level cache?**

Hibernate is the JPA provider used by JHipster. For performance reasons, we highly recommend you to use a cache, and to tune it according to your application’s needs. If you choose to do so, you can use either ehcache (local cache) or Hazelcast (distributed cache, for use in a clustered environnement)

**Do you want to use a search engine in your application?**
Elasticsearch will be configured using Spring Data Elasticsearch. You can find more information on our Elasticsearch guide.

Choose no if you are not sure

**Do you want to use clustered HTTP sessions?**

By default, JHipster uses a HTTP session only for storing Spring Security’s authentication and autorisations information. Of course, you can choose to put more data in your HTTP sessions. Using HTTP sessions will cause issues if you are running in a cluster, especially if you don’t use a load balancer with “sticky sessions”. If you want to replicate your sessions inside your cluster, choose this option to have Hazelcast configured.

Choose no if you are not sure

**Do you want to use WebSockets?**
Websockets can be enabled using Spring Websocket. We also provide a complete sample to show you how to use the framework efficiently.

Choose no if you are not sure

**Would you like to use Maven or Gradle?**
You can build your generated Java application either with Maven or Gradle. Maven is more stable and more mature. Gradle is more flexible, easier to extend, and more hype.

Choose **Gradle** if you are not sure

Would you like to use the LibSass stylesheet preprocessor for your CSS?
Node-sass a great solution to simplify designing CSS. To be used efficiently, you will need to run a Gulp server, which will be configured automatically.

Choose no if you are not sure

Would you like to enable translation support with Angular Translate?
By default JHipster provides excellent internationalization support, both on the client side with Angular Translate and on the server side. However, internationalization adds a little overhead, and is a little bit more complex to manage, so you can choose not to install this feature.

Choose no if you are not sure

Which testing frameworks would you like to use?
By default JHipster provide Java unit/integration testing (using Spring’s JUnit support) and JavaScript unit testing (using Karma.js). As an option, you can also add support for:

Choose none if you are not sure. You will have access to junit and Karma by default.

