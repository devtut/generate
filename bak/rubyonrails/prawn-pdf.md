---
metaTitle: "Ruby on Rails - Prawn PDF"
description: "Advanced Example, Basic Example"
---

# Prawn PDF



## Advanced Example


This is the advanced approach with example

```ruby
class FundsController < ApplicationController

  def index
    @funds = Fund.all_funds(current_user)
  end

  def show
    @fund = Fund.find(params[:id])
    respond_to do |format|
      format.html
      format.pdf do
        pdf = FundsPdf.new(@fund, view_context)
        send_data pdf.render, filename: 
        "fund_#{@fund.created_at.strftime("%d/%m/%Y")}.pdf",
        type: "application/pdf"
      end
    end
  end
end  

```

I above code we have this line `FundsPdf.new(@fund, view_context)`. Here we are initializing FundsPdf class with @fund instance and view_context to use helper methods in FundsPdf. FundsPdf wuld look like this

```

class FundPdf < Prawn::Document

  def initialize(fund, view)
    super()
    @fund = fund
    @view = view
    upper_half
    lower_half
  end

  def upper_half
    logopath =  "#{Rails.root}/app/assets/images/logo.png"
    image logopath, :width => 197, :height => 91
    move_down 10
    draw_text "Receipt", :at => [220, 575], size: 22
    move_down 80
    text "Hello #{@invoice.customer.profile.first_name.capitalize},"
  end

  def thanks_message
    move_down 15
    text "Thank you for your order.Print this receipt as 
    confirmation of your order.",
    :indent_paragraphs => 40, :size => 13
  end
end 

```

This is one of the best approach to generate PDF with classes using Prawn gem.



## Basic Example


You need to add Gem and PDF MIME:Type inside mime_types.rb as we need to notify rails about PDF mime type.

After that we can generate Pdf with Prawn in following basic ways

### This is the basic assignment

```ruby
pdf = Prawn::Document.new
pdf.text "Hello World"
pdf.render_file "assignment.pdf"

```

### We can do it with Implicit Block

```ruby
Prawn::Document.generate("implicit.pdf") do
 text "Hello World"
end

```

### With Explicit Block

```ruby
Prawn::Document.generate("explicit.pdf") do |pdf|
 pdf.text "Hello World"
end

```

