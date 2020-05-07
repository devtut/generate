---
metaTitle: "Ruby on Rails - Nested form in Ruby on Rails"
description: "How to setup a nested form in Ruby on Rails"
---

# Nested form in Ruby on Rails




## How to setup a nested form in Ruby on Rails


The first to thing to have: a model that contains a `has_many` relation with another model.

```ruby
class Project < ApplicationRecord
  has_many :todos
end

class Todo < ApplicationRecord
  belongs_to :project
end

```

In `ProjectsController`:

```ruby
class ProjectsController < ApplicationController
  def new
    @project = Project.new
  end
end

```

In a nested form, you can create child objects with a parent object at the same time.

```ruby
<%= nested_form_for @project do |f| %>
  <%= f.label :name %>
  <%= f.text_field :name %>

  <% # Now comes the part for `Todo` object %>
  <%= f.fields_for :todo do |todo_field| %>
    <%= todo_field.label :name %>
    <%= todo_field.text_field :name %>
  <% end %>
<% end %>

```

As we initialized `@project` with `Project.new` to have something for creating a new `Project` object, same way for creating a `Todo` object, we have to have something like this, and there are multiple ways to do so:

<li>
In `Projectscontroller`, in `new` method, you can write: `@todo = @project.todos.build` or `@todo = @project.todos.new` to instantiate a new `Todo` object.
</li>
<li>
You can also do this in view: `<%= f.fields_for :todos, @project.todos.build %>`
</li>

For strong params, you can include them in the following way:

```ruby
def project_params
  params.require(:project).permit(:name, todo_attributes: [:name])
end

```

Since, the `Todo` objects will be created through the creation of a `Project` object, so you have to specify this thing in `Project` model by adding the following line:

```ruby
accepts_nested_attributes_for :todos

```

