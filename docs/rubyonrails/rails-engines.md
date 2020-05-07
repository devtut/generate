---
metaTitle: "Ruby on Rails - Rails -Engines"
description: "Famous examples are"
---

# Rails -Engines


Engines can be considered miniature applications that provide functionality to their host applications. A Rails application is actually just a "supercharged" engine, with the Rails::Application class inheriting a lot of its behavior from Rails::Engine.

Engines are the reusable rails applications/plugins. It works like a Gem. Famous engines are Device, Spree gems which can be integrated with rails applications easily.



## Famous examples are


Generating simple blog engine

```ruby
rails plugin new [engine name] --mountable

```

Famous engines examples are

[Device](https://github.com/plataformatec/devise) (authentication gem for rails)

[Spree](https://github.com/spree/spree) (Ecommerce)



#### Syntax


- `rails plugin new [engine name] --mountable`



#### Parameters


|Parameters|Purpose
|---|---|---|---|---|---|---|---|---|---
|**--mountable**|option tells the generator that you want to create a "mountable" and namespace-isolated engine
|**--full**|option tells the generator that you want to create an engine, including a skeleton structure



#### Remarks


Engines are very good options for creating reusable plugin for rails application

