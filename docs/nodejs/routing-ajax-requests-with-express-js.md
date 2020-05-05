---
metaTitle: "Node.js - Routing ajax requests with Express.JS"
description: "A simple implementation of AJAX"
---

# Routing ajax requests with Express.JS



## A simple implementation of AJAX


You should have the basic express-generator template

In app.js, add(you can add it anywhere after `var app = express.app()`):

```js
app.post(function(req, res, next){
    next();
});

```

Now in your index.js file (or its respective match), add:

```js
router.get('/ajax', function(req, res){
    res.render('ajax', {title: 'An Ajax Example', quote: "AJAX is great!"});
});
router.post('/ajax', function(req, res){
    res.render('ajax', {title: 'An Ajax Example', quote: req.body.quote});
});

```

Create an `ajax.jade` / `ajax.pug` or `ajax.ejs` file in `/views` directory, add:

For Jade/PugJS:

```js
extends layout
script(src="http://code.jquery.com/jquery-3.1.0.min.js")
script(src="/magic.js")
h1 Quote: !{quote}
form(method="post" id="changeQuote")
    input(type='text', placeholder='Set quote of the day', name='quote')
    input(type="submit", value="Save")

```

For EJS:

```js
<script src="http://code.jquery.com/jquery-3.1.0.min.js"></script>
<script src="/magic.js"></script>
<h1>Quote: <%=quote%> </h1>
<form method="post" id="changeQuote">
    <input type="text" placeholder="Set quote of the day" name="quote"/>
    <input type="submit" value="Save">
</form>

```

Now, create a file in `/public` called `magic.js`

```js
$(document).ready(function(){
    $("form#changeQuote").on('submit', function(e){
        e.preventDefault();
        var data = $('input[name=quote]').val();
        $.ajax({
            type: 'post',
            url: '/ajax',
            data: data,
            dataType: 'text'
        })
        .done(function(data){
            $('h1').html(data.quote);
        });
    });
});

```

And there you have it!
When you click Save the quote will change!

