---
metaTitle: "Ruby on Rails - Add Admin Panel"
description: "So here are few screen shots from the admin panel using rails_admin gem."
---

# Add Admin Panel




## So here are few screen shots from the admin panel using rails_admin gem.


As you can see the layout of this gem is very catching and user friendly.

[<img src="https://i.stack.imgur.com/xuqoG.png" alt="enter image description here" />](https://i.stack.imgur.com/xuqoG.png)

[<img src="https://i.stack.imgur.com/SQKX2.png" alt="enter image description here" />](https://i.stack.imgur.com/SQKX2.png)

[<img src="https://i.stack.imgur.com/KrU7B.png" alt="enter image description here" />](https://i.stack.imgur.com/KrU7B.png)



#### Syntax


1. Open gem file and writer gem 'rails_admin', '~> 1.0'
1. bundle install
1. rails g rails_admin:install
1. it will ask you about the admin route if you want to go with the default press Enter.
<li>Now go app/config/initializers/rails_admin.rb and paste this code:     config.authorize_with do
redirect_to main_app.root_path unless current_user.try(:admin?)
end
This code will allow only admin user to access the yoursite.com/admin other users will be redirected to the rootpath.</li>
1. For more details checkout the documentation of this gem. [https://github.com/sferik/rails_admin/wiki](https://github.com/sferik/rails_admin/wiki)



#### Remarks


Use it if you want to have Admin to your website otherwise there is no need for this. It is more easy and powerful than active_admin gem. You can add this at any stage after creating users and don't forget to make any user admin before the 4th step. Use cancan for granting roles.

