---
metaTitle: "Ruby on Rails - Tools for Ruby on Rails code optimization and cleanup"
description: "If you want to keep your code maintainable, secure and optimized, look at some gems for code optimization and cleanup :"
---

# Tools for Ruby on Rails code optimization and cleanup


Keeping your code clean and organized while developing a large Rails application can be quite a challenge, even for an experienced developer. Fortunately, there is a whole category of gems that make this job much easier.



## If you want to keep your code maintainable, secure and optimized, look at some gems for code optimization and cleanup :


[**Bullet**](https://github.com/flyerhzm/bullet)

This one particularly blew my mind. The bullet gem helps you kill all the N+1 queries, as well as unnecessarily eager loaded relations. Once you install it and start visiting various routes in development, alert boxes with warnings indicating database queries that need to be optimized will pop out. It works right out of the box and is extremely helpful for optimizing your application.

[**Rails Best Practices**](https://github.com/railsbp/rails_best_practices)

Static code analyzer for finding Rails specific code smells. It offers a variety of suggestions; use scope access, restrict auto-generated routes, add database indexes, etc. Nevertheless, it contains lots of nice suggestions that will give you a better perspective on how to re-factor your code and learn some best practices.

[**Rubocop**](https://github.com/bbatsov/rubocop#cops)

A Ruby static code analyzer which you can use to check if your code complies with the Ruby community code guidelines. The gem reports style violations through the command line, with lots of useful code refactoring goodies such as useless variable assignment, redundant use of Object#to_s in interpolation or even unused method argument.

A good thing is that it's highly configurable, since the analyzer can be quite irritating if you're not following the Ruby style guide 100% (i.e. you have lots of trailing whitespaces or you double quote your strings even when not interpolating, etc.).

It's divided into 4 sub-analyzers (called cops): Style, Lint, Metrics and Rails.

