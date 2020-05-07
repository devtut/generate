---
metaTitle: "Ruby on Rails - Form Helpers"
description: "Creating a search form, Helpers for form elements, Dropdown, Create a form"
---

# Form Helpers


Rails provides view helpers for generating form markup.



## Creating a search form


To create a search form, enter the following code

```ruby
<%= form_tag("/search", method: "get") do %>
  <%= label_tag(:q, "Search for:") %>
  <%= text_field_tag(:q) %>
  <%= submit_tag("Search") %>
<% end %>

```


- `form_tag`: This is the default helper for creating a form. It's first parameter, `/search` is the action and the second parameter specifies the HTTP method. For search forms, it is important to always use the method `get`
- `label_tag`: This helper creates an html `<label>` tag.
- `text_field_tag`: This will create an input element with type `text`
- `submit_tag`: This creates an input element with type `submit`



## Helpers for form elements


### Checkboxes

```ruby
<%= check_box_tag(:pet_dog) %>
<%= label_tag(:pet_dog, "I own a dog") %>
<%= check_box_tag(:pet_cat) %>
<%= label_tag(:pet_cat, "I own a cat") %>

```

This will generate the following html

```ruby
<input id="pet_dog" name="pet_dog" type="checkbox" value="1" />
<label for="pet_dog">I own a dog</label>
<input id="pet_cat" name="pet_cat" type="checkbox" value="1" />
<label for="pet_cat">I own a cat</label>

```

### Radio Buttons

```ruby
<%= radio_button_tag(:age, "child") %>
<%= label_tag(:age_child, "I am younger than 18") %>
<%= radio_button_tag(:age, "adult") %>
<%= label_tag(:age_adult, "I'm over 18") %>

```

This generates the following HTML

```ruby
<input id="age_child" name="age" type="radio" value="child" />
<label for="age_child">I am younger than 18</label>
<input id="age_adult" name="age" type="radio" value="adult" />
<label for="age_adult">I'm over 18</label>

```

### Text Area

To create a larger text box, it is recommended to use the `text_area_tag`

```ruby
<%= text_area_tag(:message, "This is a longer text field", size: "25x6") %>

```

This will create the following HTML

```ruby
<textarea id="message" name="message" cols="25" rows="6">This is a longer text field</textarea>

```

### Number Field

This will create an `input<type="number">` element

```ruby
<%= number_field :product, :rating %>

```

To specify a range of values, we can use the `in:` option

```ruby
<%= number_field :product, :rating, in: 1..10 %>

```

### Password Field

Sometimes you want the characters typed by the user to be masked. This will generate an `<input type="password">`

```ruby
<%= password_field_tag(:password) %>

```

### Email Field

This will create an `<input type="email">`

```ruby
<%= email_field(:user, :email) %>

```

### Telephone Field

This will create an `<input type="tel">`.

```ruby
<%= telephone_field :user, :phone %>

```

### Date Helpers

<li>
`input[type="date"]`

```ruby
<%= date_field(:user, :reservation) %>

```


</li>
<li>
`input[type="week"]`

```ruby
 <%= week_field(:user, :reservation) %>

```


</li>
<li>
`input[type="year"]`

```ruby
 <%= year_field(:user, :reservation) %>

```


</li>
<li>
`input[type="time"]`

```ruby
 <%= time_field(:user, :check_in) %>

```


</li>



## Dropdown


Standard example:
@models = Model.all
select_tag "models", options_from_collection_for_select(@models, "id", "name"), {}

This will generate the following HTML:
David

The last argument are options, which accepts the following:
{
multiple: false,
disabled: false,
include_blank: false,
prompt: false
}

More examples can be found:
[http://apidock.com/rails/ActionView/Helpers/FormTagHelper/select_tag](http://apidock.com/rails/ActionView/Helpers/FormTagHelper/select_tag)



## Create a form


You can create a form using the `form_tag` helper

```ruby
<%= form_tag do %>
  Form contents
<% end %>

```

This creates the following HTML

```ruby
<form accept-charset="UTF-8" action="/" method="post">
  <input name="utf8" type="hidden" value="&#x2713;" />
  <input name="authenticity_token" type="hidden" value="J7CBxfHalt49OSHp27hblqK20c9PgwJ108nDHX/8Cts=" />
  Form contents
</form>

```

This form tag has created a `hidden` input field. This is necessary, because forms cannot be successfully submitted without it.

The second input field, named `authenticity_token` adds protection against `cross-site request forgery`.



#### Remarks


- The date input types including `date`, `datetime`, `datetime-local`, `time`, `month` and `week` do not work in FireFox.
- `input<type="telephone">` only works with Safari 8.
- `input<type="email">` does not work on Safari

