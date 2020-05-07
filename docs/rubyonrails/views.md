---
metaTitle: "Ruby on Rails - Views"
description: "Structure, Partials, AssetTagHelper, Replace HTML code in Views, HAML - an alternative way to use in your views"
---

# Views




## Structure


As Rails follows the M**V**C pattern `Views` are where your "templates" are for your actions.

Let's say you have a controller `articles_controller.rb`. For this controller you would have a folder in views called `app/views/articles`:

```ruby
app
|-- controllers
|   '-- articles_controller.rb
|
'-- views
    '-- articles
    |   |- index.html.erb
    |   |- edit.html.erb
    |   |- show.html.erb
    |   |- new.html.erb
    |   '- _partial_view.html.erb
    |
    '-- [...]

```

This structure allows you to have a folder for each controller. When calling an action in your controller the appropriate view will be rendered automatically.

```ruby
// articles_controller.rb
class ArticlesController < ActionController::Base
  def show
  end
end

// show.html.erb
<h1>My show view</h1>

```



## Partials


Partial templates (partials) are a way of breaking the rendering process into more manageable chunks. Partials allow you to extract pieces of code from your templates to separate files and also reuse them throughout your templates.

To **create** a partial, create a new file that begins with an underscore: `_form.html.erb`

To **render** a partial as part of a view, use the render method within the view:
`<%= render "form" %>`

- Note, the underscore is left out when rendering
- A partial has to be rendered using its path if located in a different folder

To **pass** a variable into the partial as a local variable, use this notation:

`<%= render :partial => 'form', locals: { post: @post } %>`

Partials are also useful when you need to **reuse** exactly the same code (**DRY philosophy**).

For example, to reuse `<head>` code, create a partial named `_html_header.html.erb`,  enter your `<head>` code to be reused, and render the partial whenever needed by: `<%= render 'html_header' %>`.

### Object Partials

Objects that respond to `to_partial_path` can also be rendered, as in `<%= render @post %>`. By default, for ActiveRecord models, this will be something like `posts/post`, so by actually rendering `@post`, the file `views/posts/_post.html.erb` will be rendered.

A local named `post` will be automatically assigned. In the end, `<%= render @post %>` is a short hand for `<%= render 'posts/post', post: @post %>`.

Collections of objects that respond to `to_partial_path` can also be provided, such as `<%= render @posts %>`. Each item will be rendered consecutively.

### Global Partials

To create a global partial that can be used anywhere without referencing its exact path, the partial has to be located in the `views/application` path. The previous example has been modified below to illustrate this feature.

For example, this is a path to a global partial `app/views/application/_html_header.html.erb:`

To render this global partial anywhere, use `<%= render 'html_header' %>`



## AssetTagHelper


To let rails automatically and correctly link assets (css/js/images) in most cases you want to use built in helpers. ([Official documentation](http://guides.rubyonrails.org/action_view_overview.html#assettaghelper))

### Image helpers

### **image_path**

This returns the path to an image asset in `app/assets/images`.

```ruby
image_path("edit.png") # => /assets/edit.png

```

### **image_url**

This returns the full URL to an image asset in `app/assets/images`.

```ruby
image_url("edit.png") # => http://www.example.com/assets/edit.png

```

### **image_tag**

Use this helper if you want to include an `<img src="" />`-tag with the source set.

```ruby
image_tag("icon.png") # => <img src="/assets/icon.png" alt="Icon" />

```

### JavaScript helpers

### **javascript_include_tag**

If you want to include a JavaScript-file in your view.

```ruby
javascript_include_tag "application" # => <script src="/assets/application.js"></script>

```

### **javascript_path**

This returns the path of your JavaScript-file.

```ruby
javascript_path "application" # => /assets/application.js

```

### **javascript_url**

This returns the full URL of your JavaScript-file.

```ruby
javascript_url "application" # => http://www.example.com/assets/application.js

```

### Stylesheet helpers

### **stylesheet_link_tag**

If you want to include a CSS-file in your view.

```ruby
stylesheet_link_tag "application" # => <link href="/assets/application.css" media="screen" rel="stylesheet" />

```

### **stylesheet_path**

This returns the path of you stylesheet asset.

```ruby
stylesheet_path "application" # => /assets/application.css

```

### **stylesheet_url**

This returns the full URL of you stylesheet asset.

```ruby
stylesheet_url "application" # => http://www.example.com/assets/application.css

```

### Example usage

When creating a new rails app you will automatically have two of these helpers in `app/views/layouts/application.html.erb`

```ruby
<%= stylesheet_link_tag    'application', media: 'all', 'data-turbolinks-track': 'reload' %>
<%= javascript_include_tag 'application', 'data-turbolinks-track': 'reload' %>

```

This outputs:

```ruby
// CSS
<link rel="stylesheet" media="all" href="/assets/application.self-e19d4b856cacba4f6fb0e5aa82a1ba9aa4ad616f0213a1259650b281d9cf6b20.css?body=1" data-turbolinks-track="reload" />
// JavaScript
<script src="/assets/application.self-619d9bf310b8eb258c67de7af745cafbf2a98f6d4c7bb6db8e1b00aed89eb0b1.js?body=1" data-turbolinks-track="reload"></script>

```



## Replace HTML code in Views


If you ever wanted to determine the html content to be printed on a page during run time then, rails has a very good solution for that. It has something called the **content_for** which allows us to pass a block to a rails view. Please check the below example,

**Declare content_for**

```ruby
<div>
  <%= yield :header %>
</div>

<% content_for :header do %>
 <ul>
   <li>Line Item 1</li>
   <li>Line Item 2</li>
 </ul>
<% end %>

```



## HAML - an alternative way to use in your views


HAML (HTML abstraction markup language) is a beautiful and elegant way to describe and design the HTML of your views. Instead of opening- and closing tags, HAML uses indentation for the structure of your pages. Basically, if something should be placed within another element, you just indent it by using one tab stop. Tabs and white space are important in HAML, so be sure that you always use the same amount of tabs.

**Examples:**

```ruby
#myview.html.erb
<h1><%= @the_title %></h1>
<p>This is my form</p>
<%= render "form" %>

```

And in HAML:

```ruby
#myview.html.haml
%h1= @the_title
%p
    This is my form
= render 'form'

```

You see, the structure of the layout is much clearer than using HTML and ERB.

**Installation**

Just install the gem using

```ruby
gem install haml

```

and add the gem to the Gemfile

```ruby
gem "haml"

```

For using HAML instead of HTML/ERB, just replace the file extensions of your views from `something.html.erb` to `something.html.haml`.

**Quick tipps**

Common elements like divs can be written in a short way

HTML

```ruby
<div class="myclass">My Text</div>

```

HAML

```ruby
%div.myclass

```

HAML, shorthand

```ruby
.myclass

```

**Attributes**

HTML

```ruby
<p class="myclass" id="myid">My paragraph</p>

```

HAML

```ruby
%p{:class => "myclass", :id => "myid"} My paragraph

```

**Inserting ruby code**

You can insert ruby code with the = and - signs.

```ruby
= link_to "Home", home_path

```

Code starting with **=** will be executed and embedded into the document.

Code starting with **-** will be executed, but not inserted into the document.

**Full documentation**

HAML is very easy to start with, but is also very complex, so that I'll recommend [reading the documentation](http://haml.info/docs/yardoc/file.REFERENCE.html#using_haml).

