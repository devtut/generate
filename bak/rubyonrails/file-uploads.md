---
metaTitle: "Ruby on Rails - File Uploads"
description: "Single file upload using Carrierwave, Nested model - multiple uploads"
---

# File Uploads



## Single file upload using Carrierwave


Start using File Uploads in Rails is quite simple, first thing you have to do is to choice plugin for managing uploads. The most common onces are **Carrierwave** and **Paperclip**. Both are similar in functionality and rich in documentation on

Let's have an look on example with simple avatar upload image with Carrierwave

After `bundle install` Carrierwave, type in console

```ruby
$ rails generate uploader ProfileUploader

```

This will create an config file located at **/app/uploaders/profile_uploader.rb**

Here you can set up storage (i.e local or cloud), apply extensions for image manipulations (i.e. generting thumbs via MiniMagick) and set server-side extension white list

Next, create new migration with string tipe for user_pic and mount uploader for it in **user.rb** model.

```ruby
mount_uploader :user_pic, ProfileUploader

```

Next, display an form to upload avatar (may be an edit view for the user)

```ruby
<% form_for @user, html: { multipart: true } do |f| %>
    <%= f.file_field :user_pic, accept: 'image/png, image/jpg' %>
    <%= f.submit "update profile pic", class: "btn" %>
<% end %>

```

Make sure to include { multipart: true } in order form can process uploads. Accept is an optional to set client-side extension white-list.

To display an avatar, simply do

```ruby
<%= image_tag @user.user_pic.url %>

```



## Nested model - multiple uploads


If you want to create multiple uploads, first thing you might want to do is create new model and set up relations

Let's say you want an multiple images for the Product model. Create an new model and make it `belongs_to` your parent model

```ruby
rails g model ProductPhoto

#product.rb
has_many :product_photos, dependent: :destroy
accepts_nested_attributes_for :product_photos

#product_photo.rb
belongs_to :product
mount_uploader :image_url, ProductPhotoUploader # make sure to include uploader (Carrierwave example)

```

**accepts_nested_attributes_for** is must, because it allow us to create nested form, so we can upload new file, change product name and set price from an single form

Next, create form in a view (edit/create)

```

   <%= form_for @product, html: { multipart: true } do |product|%>

        <%= product.text_field :price # just normal type of field %>

        <%= product.fields_for :product_photos do |photo| # nested fields %>
            <%= photo.file_field :image, :multiple => true, name: "product_photos[image_url][]" %>
        <% end %>
        <%= p.submit "Update", class: "btn" %>
    <% end %>

```

Controller is nothing special, if you don't want to create an new one, just make an new one inside your product controller

```

 # create an action
  def upload_file
    printer = Product.find_by_id(params[:id])
    @product_photo = printer.prodcut_photos.create(photo_params)
  end

  # strong params
  private
    def photo_params
      params.require(:product_photos).permit(:image)
    end

```

Display all images in a view

```

   <% @product.product_photos.each do |i| %>
        <%= image_tag i.image.url, class: 'img-rounded' %>
    <% end %>

```

