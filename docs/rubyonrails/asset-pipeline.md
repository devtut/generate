---
metaTitle: "Ruby on Rails - Asset Pipeline"
description: "Manifest Files and Directives, Rake tasks, Basic Usage"
---

# Asset Pipeline


The asset pipeline provides a framework to concatenate and minify or compress JavaScript and CSS assets. It also adds the ability to write these assets in other languages and pre-processors such as CoffeeScript, Sass and ERB. It allows assets in your application to be automatically combined with assets from other gems. For example, jquery-rails includes a copy of jquery.js and enables AJAX features in Rails.



## Manifest Files and Directives


In the `assets` initalizer (`config/initializers/assets.rb`) are a few files explicitly defined to be precompiled.

```ruby
# Precompile additional assets.
# application.coffee, application.scss, and all non-JS/CSS in app/assets folder are already added.
# Rails.application.config.assets.precompile += %w( search.js )

```

In this example the `application.coffee` and `application.scss` are so called 'Manifest Files'. This files should be used to include other JavaScript or CSS assets. The following command are available:

- `require <path>`: The `require` directive functions similar to Ruby's own `require`. It provides a way to declare a dependency on a file in your path and ensures it's only loaded once before the source file.
- `require_directory <path>`: requires all the files inside a single directory. It's similar to `path/*` since it does not follow nested directories.
- `require_tree <path>`: requires all the nested files in a directory. Its glob equivalent is `path/**/*`.
- `require_self`: causes the body of the current file to be inserted before any subsequent `require` directives. Useful in CSS files, where it's common for the index file to contain global styles that need to be defined before other dependencies are loaded.
- `stub <path>`: remove a file from being included
<li>`depend_on <path>`: Allows you to state a dependency on a file without including it.
This is used for caching purposes. Any changes made to the dependency file will invalidate the cache of the source file.</li>

An `application.scss` file could look like:

```ruby
/*
 *= require bootstrap
 *= require_directory .
 *= require_self
 */

```

Another example is the `application.coffee` file. Here with including `jquery` and `Turbolinks`:

```ruby
#= require jquery2
#= require jquery_ujs
#= require turbolinks
#= require_tree .

```

If you don't use CoffeeScript, but plain JavaScript, the syntax would be:

```ruby
//= require jquery2
//= require jquery_ujs
//= require turbolinks
//= require_tree .

```



## Rake tasks


By default `sprockets-rails` is shipped with the following rake tasks:

- `assets:clean[keep]`: Remove old compiled assets
- `assets:clobber`: Remove compiled assets
- `assets:environment`: Load asset compile environment
- `assets:precompile`: Compile all the assets named in `config.assets.precompile`



## Basic Usage


There are two basic ways that the asset pipeline is used:

<li>
When running a server in development mode, it automatically pre-processes and prepares your assets on-the-fly.
</li>
<li>
In production mode, you’ll probably use it to pre-process, versionize, and compress and compile your assets. You can do so by running the following command:
`bundle exec rake assets:precompile`
</li>

