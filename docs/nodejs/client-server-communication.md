---
metaTitle: "Client-server communication"
description: "/w Express, jQuery and Jade"
---

# Client-server communication



## /w Express, jQuery and Jade


```js
//'client.jade'    

//a button is placed down; similar in HTML
button(type='button', id='send_by_button') Modify data
    
    #modify Lorem ipsum Sender
    
    //loading jQuery; it can be done from an online source as well
    script(src='./js/jquery-2.2.0.min.js')

    //AJAX request using jQuery
    script
        $(function () {
            $('#send_by_button').click(function (e) {
                e.preventDefault();
                
                //test: the text within brackets should appear when clicking on said button
                //window.alert('You clicked on me.  - jQuery');
        
                //a variable and a JSON initialized in the code
                var predeclared = "Katamori";
                var data = {
                    Title: "Name_SenderTest",
                    Nick: predeclared,
                    FirstName: "Zoltan",
                    Surname: "Schmidt"
                };
                
                //an AJAX request with given parameters
                $.ajax({
                    type: 'POST',
                    data: JSON.stringify(data),
                    contentType: 'application/json',
                    url: 'http://localhost:7776/domaintest',

                    //on success, received data is used as 'data' function input
                    success: function (data) {
                        window.alert('Request sent; data received.');
        
                        var jsonstr = JSON.stringify(data);
                        var jsonobj = JSON.parse(jsonstr);
                        
                        //if the 'nick' member of the JSON does not equal to the predeclared string (as it was initialized), then the backend script was executed, meaning that communication has been established
                        if(data.Nick != predeclared){
                            document.getElementById("modify").innerHTML = "JSON changed!\n" + jsonstr;
                        };
        
                    }
                });
            });
            });

//'domaintest_route.js'

var express = require('express');
var router = express.Router();

//an Express router listening to GET requests - in this case, it's empty, meaning that nothing is displayed when you reach 'localhost/domaintest'
router.get('/', function(req, res, next) {
});

//same for POST requests - notice, how the AJAX request above was defined as POST 
router.post('/', function(req, res) {
    res.setHeader('Content-Type', 'application/json');

    //content generated here
    var some_json = {
        Title: "Test",
        Item: "Crate"
    };

    var result = JSON.stringify(some_json);

    //content got 'client.jade'
    var sent_data = req.body;
    sent_data.Nick = "ttony33";

    res.send(sent_data);

});


module.exports = router;

```

//based on a personally used gist: [https://gist.github.com/Katamori/5c9850f02e4baf6e9896](https://gist.github.com/Katamori/5c9850f02e4baf6e9896)

