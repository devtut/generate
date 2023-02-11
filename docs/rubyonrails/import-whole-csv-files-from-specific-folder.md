---
metaTitle: "Ruby on Rails - Import whole CSV files from specific folder"
description: "Uploads CSV from console command"
---

# Import whole CSV files from specific folder


In this example, lets say we have many product CSV files in a folder. Each CSV file need to upload our database from our console write a command. Run the following command in a new or existing project to create this model.



## Uploads CSV from console command


Terminal Commands:

```ruby
rails g model Product name:string quantity:integer price:decimal{12,2}
rake db:migrate

```

Let's create a controller.

Terminal Commands:

```ruby
rails g controller Products

```

Controller Code:

```ruby
class HistoriesController < ApplicationController
    def create
        file = Dir.glob("#{Rails.root}/public/products/**/*.csv") #=> This folder directory where read the CSV files
        file.each do |file|
            Product.import(file)
        end
    end
end 

```

Model:

```ruby
class Product< ApplicationRecord
  def self.import(file)
      CSV.foreach(file.path, headers: true) do |row|
          Product.create! row.to_hash
      end
  end
end

```

routes.rb

```ruby
resources :products

```

app/config/application.rb

```ruby
require 'csv'

```

Now open your development `console` & `run`

```ruby
=> ProductsController.new.create #=> Uploads your whole CSV files from your folder directory

```

