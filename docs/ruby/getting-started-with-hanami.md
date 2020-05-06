---
metaTitle: "Ruby - Getting started with Hanami"
description: "About Hanami, How to install Hanami?, How to start the server?"
---

# Getting started with Hanami


My mission here is to contribute with the community to help new people who wants to learn about this amazing framework - Hanami.

**But how it is going to work ?**

Short and easygoing tutorials showing with examples about Hanami and following the next tutorials we will see how to test our application and build a simple REST API.

**Let's start!**



## About Hanami


Besides Hanami be a lightweight and fast framework one of the points that most call attention is the **Clean Architecture** concept where shows to us that the framework is not our application as Robert Martin said before.

Hanami arquitecture design offer to us the use of **Container**, in each Container we have  our application independently of the framework. This means that we can grab our code and put it into a Rails framework for example.

**Hanami is a MVC Framework ?**

The MVC's frameworks idea is to build one structure following the Model -> Controller -> View. Hanami follows the Model | Controller -> View -> Template. The result is an application more uncopled, following **SOLID** principles, and much cleaner.

**- Important links.**

Hanami [http://hanamirb.org/](http://hanamirb.org/)

Robert Martin - Clean Arquitecture [https://www.youtube.com/watch?v=WpkDN78P884](https://www.youtube.com/watch?v=WpkDN78P884)

Clean Arquitecture [https://8thlight.com/blog/uncle-bob/2012/08/13/the-clean-architecture.html](https://8thlight.com/blog/uncle-bob/2012/08/13/the-clean-architecture.html)

SOLID Principles  [http://practicingruby.com/articles/solid-design-principles](http://practicingruby.com/articles/solid-design-principles)



## How to install Hanami?


- **Step 1:** Installing the Hanami gem.

```

 $ gem install hanami

```


<li>
**Step 2**: Generate a new project setting [RSpec](https://github.com/rspec/rspec) as testing framework.
Open up a command line or terminal. To generate a new hanami application, use hanami new followed by the name of your app and the rspec test param.
</li>

```

$ hanami new "myapp" --test=rspec 

```

Obs. By default Hanami sets [Minitest](https://github.com/seattlerb/minitest) as testing framework.

This will create a hanami application called myapp in a myapp directory and install the gem dependencies that are already mentioned in Gemfile using bundle install.

To switch to this directory, use the cd command, which stands for change directory.

```ruby
$ cd my_app
$ bundle install

```

The myapp directory has a number of auto-generated files and folders that make up the structure of a Hanami application. Following is a list of files and folders that are created by default:

<li>
**Gemfile** defines our Rubygems dependencies (using Bundler).
</li>
<li>
**Rakefile** describes our Rake tasks.
</li>
<li>
**apps** contains one or more web applications compatible with Rack. Here we can find the first generated Hanami application called Web. It's the place where we find our controllers, views, routes and templates.
</li>
<li>
**config** contains configuration files.
</li>
<li>
**config.ru** is for Rack servers.
</li>
<li>
**db** contains our database schema and migrations.
</li>
<li>
<p>**lib** contains our business logic and domain model, including entities
and repositories.</p>
</li>
<li>
**public** will contain compiled static assets.
</li>
<li>
**spec** contains our tests.
</li>

- **Important links.**

Hanami gem [https://github.com/hanami/hanami](https://github.com/hanami/hanami)

Hanami official Getting Started [http://hanamirb.org/guides/getting-started/](http://hanamirb.org/guides/getting-started/)



## How to start the server?


<li>
**Step 1:** To start the server just type the command bellow then you'll see the start page.

```ruby
 $ bundle exec hanami server

```


</li>

[<img src="https://i.stack.imgur.com/RCBWx.png" alt="enter image description here" />](https://i.stack.imgur.com/RCBWx.png)

