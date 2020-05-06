---
metaTitle: "jQuery - Ajax"
description: "Handling HTTP Response Codes with $.ajax(), Using Ajax to Submit a Form, All in one examples, Ajax File Uploads, Ajax Abort a Call or Request, Sending JSON data"
---

# Ajax



## Handling HTTP Response Codes with $.ajax()


In addition to `.done`, `.fail` and `.always` promise callbacks, which are triggered based on whether the request was successful or not, there is the option to trigger a function when a specific [HTTP Status Code](https://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html) is returned from the server. This can be done using the `statusCode` parameter.

```js
$.ajax({
    type: {POST or GET or PUT etc.},
    url:  {server.url},
    data: {someData: true},
    statusCode: {
        404: function(responseObject, textStatus, jqXHR) {
            // No content found (404)
            // This code will be executed if the server returns a 404 response
        },
        503: function(responseObject, textStatus, errorThrown) {
            // Service Unavailable (503)
            // This code will be executed if the server returns a 503 response
        }           
    }
})
.done(function(data){
    alert(data);
})
.fail(function(jqXHR, textStatus){
    alert('Something went wrong: ' + textStatus);
})
.always(function(jqXHR, textStatus) {
   alert('Ajax request was finished')
});

```

As official jQuery documentation states:

> 
<p>If the request is successful, the status code functions take the same
parameters as the success callback; if it results in an error
(including 3xx redirect), they take the same parameters as the `error`
callback.</p>




## Using Ajax to Submit a Form


Sometimes you may have a form and want to submit it using ajax.

Suppose you have this simple form -

```js
<form id="ajax_form" action="form_action.php">
  <label for="name">Name :</label>
  <input name="name" id="name" type="text" />
  <label for="name">Email :</label>
  <input name="email" id="email" type="text" />
  <input type="submit" value="Submit" />
</form>

```

The following jQuery code can be used (within a `$(document).ready` call) -

```js
$('#ajax_form').submit(function(event){
  event.preventDefault();
  var $form = $(this);

  $.ajax({
      type: 'POST',
      url: $form.attr('action'),
      data: $form.serialize(),
      success: function(data) {
        // Do something with the response
      },
      error: function(error) {
        // Do something with the error
      }
  });
});

```

**Explanation**

- `var $form = $(this)` - the form, cached for reuse
<li>`$('#ajax_form').submit(function(event){` - When the form with ID
"ajax_form" is submitted run this function and pass the event as a
parameter.</li>
- `event.preventDefault();` - Prevent the form from submitting normally (Alternatively  we can use `return false` after the `ajax({});` statement, which will have the same effect)
- `url: $form.attr('action'),` - Get the value of the form's "action" attribute and use it for the "url" property.
- `data: $form.serialize(),` - Converts the inputs within the form into a string suitable for sending to the server.  In this case it will return something like "name=Bob&email=bob@bobsemailaddress.com"



## All in one examples


**Ajax Get:**

**Solution 1:**

```js
$.get('url.html', function(data){
    $('#update-box').html(data);
});

```

**Solution 2:**

```

$.ajax({
     type: 'GET',
     url: 'url.php',  
 }).done(function(data){
     $('#update-box').html(data);
 }).fail(function(jqXHR, textStatus){
     alert('Error occured: ' + textStatus);
 });

```

**Ajax Load:** Another ajax get method created for simplcity

```js
$('#update-box').load('url.html');

```

.load can also be called with additional data. The data part can be provided as string or object.

```js
$('#update-box').load('url.php', {data: "something"});
$('#update-box').load('url.php', "data=something");

```

If .load is called with a callback method, the request to the server will be a post

```js
$('#update-box').load('url.php', {data: "something"}, function(resolve){
    //do something
});

```

**Ajax Post:**

**Solution 1:**

```js
$.post('url.php', 
    {date1Name: data1Value, date2Name: data2Value},  //data to be posted
    function(data){
        $('#update-box').html(data);
    }
);

```

**Solution 2:**

```js
$.ajax({
    type: 'Post',
    url: 'url.php',  
    data: {date1Name: data1Value, date2Name: data2Value}  //data to be posted
}).done(function(data){
    $('#update-box').html(data);
}).fail(function(jqXHR, textStatus){
    alert('Error occured: ' + textStatus);
});

```

**Ajax Post JSON:**

```js
var postData = {
    Name: name,
    Address: address,
    Phone: phone
};

$.ajax({
     type: "POST",
     url: "url.php",
     dataType: "json",
     data: JSON.stringfy(postData),
     success: function (data) {
         //here variable data is in JSON format
     }
 });

```

**Ajax Get JSON:**

**Solution 1:**

```js
$.getJSON('url.php', function(data){
    //here variable data is in JSON format
});

```

**Solution 2:**

```

$.ajax({
      type: "Get",
      url: "url.php",
      dataType: "json",
      data: JSON.stringfy(postData),
      success: function (data) {
          //here variable data is in JSON format
      },    
      error: function(jqXHR, textStatus){
          alert('Error occured: ' + textStatus);
      }
  });

```



## Ajax File Uploads


### 1. A Simple Complete Example

We could use this sample code to upload the files selected by the user every time a new file selection is made.

```js
<input type="file" id="file-input" multiple>

```

 

```js
var files;
var fdata = new FormData();
$("#file-input").on("change", function (e) {
    files = this.files;

    $.each(files, function (i, file) {
        fdata.append("file" + i, file);
    });

    fdata.append("FullName", "John Doe");
    fdata.append("Gender", "Male");
    fdata.append("Age", "24");

    $.ajax({
        url: "/Test/Url",
        type: "post",
        data: fdata, //add the FormData object to the data parameter
        processData: false, //tell jquery not to process data
        contentType: false, //tell jquery not to set content-type
        success: function (response, status, jqxhr) {
            //handle success
        },
        error: function (jqxhr, status, errorMessage) {
            //handle error
        }
    });
});

```

Now let's break this down and inspect it part by part.

### **2. Working With File Inputs**

This [MDN Document ( Using files from web applications )](https://developer.mozilla.org/en/docs/Using_files_from_web_applications) is a good read about various methods on how to handle file inputs. Some of these methods will also be used in this example.

Before we get to uploading files, we first need to give the user a way to select the files they want to upload. For this purpose we will use a `file input`. The `multiple` property allows for selecting more than one files, you can remove it if you want the user to select one file at a time.

```js
<input type="file" id="file-input" multiple>

```

We will be using input's `change event` to capture the files.

```js
var files;
$("#file-input").on("change", function(e){
    files = this.files;
});

```

Inside the handler function, we access the files through the files property of our input. This gives us a [FileList](https://developer.mozilla.org/en-US/docs/Web/API/FileList), which is an array like object.

### 3. Creating and Filling the FormData

In order to upload files with Ajax we are going to use [FormData](https://developer.mozilla.org/en-US/docs/Web/API/FormData/FormData).

```js
var fdata = new FormData();

```

[FileList](https://developer.mozilla.org/en-US/docs/Web/API/FileList) we have obtained in the previous step is an array like object and can be iterated using various methods including [for loop](https://developer.mozilla.org/en-US/docs/Web/API/FileList#Example), [for...of loop](https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Statements/for...of) and [jQuery.each](http://api.jquery.com/jquery.each/). We will be sticking with the jQuery in this example.

```js
$.each(files, function(i, file) {
  //...
});

```

We will be using the [append method](https://developer.mozilla.org/en-US/docs/Web/API/FormData/append) of FormData to add the files into our formdata object.

```js
$.each(files, function(i, file) {
  fdata.append("file" + i, file);
});

```

We can also add other data we want to send the same way. Let's say we want to send some personal information we have received from the user along with the files. We could add this this information into our formdata object.

```js
fdata.append("FullName", "John Doe");
fdata.append("Gender", "Male");
fdata.append("Age", "24");
//...

```

### 4. Sending the Files With Ajax

```js
$.ajax({
    url: "/Test/Url",
    type: "post",
    data: fdata, //add the FormData object to the data parameter
    processData: false, //tell jquery not to process data
    contentType: false, //tell jquery not to set content-type
    success: function (response, status, jqxhr) {
        //handle success
    },
    error: function (jqxhr, status, errorMessage) {
        //handle error
    }
});

```

We set `processData` and `contentType` properties to `false`. This is done so that the files can be send to the server and be processed by the server correctly.



## Ajax Abort a Call or Request


```js
var xhr = $.ajax({
    type: "POST",
    url: "some.php",
    data: "name=John&location=Boston",
    success: function(msg){
       alert( "Data Saved: " + msg );
    }
});

```

//kill the request

```js
xhr.abort()

```



## Sending JSON data


jQuery makes handling jSON **responses** painless, but a bit more work is required when a given request wishes you to **send** data in JSON format:

```

$.ajax("/json-consuming-route", {
      data: JSON.stringify({author: {name: "Bullwinkle J. Moose", 
                                     email: "bullwinkle@example.com"} }),
      method: "POST",
      contentType: "application/json"
   });

```

Observe that we're specifying [the correct `contentType`](http://stackoverflow.com/questions/477816/what-is-the-correct-json-content-type/477819#477819) for the data we're sending; this is a good practice in general and may be required by the API you're posting to - but it **also** has the side-effect of instructing jQuery not to perform the default conversion of `%20` to `+`, which it would do if `contentType` was left at the default value of `application/x-www-form-urlencoded`. If for some reason you must leave contentType set to the default, be sure to set `processData` to false to prevent this.

The call to [`JSON.stringify`](http://stackoverflow.com/documentation/javascript/416/json/1385/serializing-a-simple-json-value#t=201607210507006821455) could be avoided here, but using it allows us to provide the data in the form of a JavaScript object (thus avoiding embarrassing JSON syntax errors such as failing to quote property names).



#### Syntax


- var jqXHR = $.ajax( url [,settings] )
- var jqXHR = $.ajax( [settings] )
- jqXHR.done(function( data, textStatus, jqXHR ) {});
- jqXHR.fail(function( jqXHR, textStatus, errorThrown ) {});
- jqXHR.always(function( jqXHR ) {});



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|url|Specifies the URL to which the request will be sent
|settings|an object containing numerous values that affect the behavior of the request
|type|The HTTP method to be used for the request
|data|Data to be sent by the request
|success|A callback function to be called if the request succeeds
|error|A callback to handle error
|statusCode|An object of numeric HTTP codes and functions to be called when the response has the corresponding code
|dataType|The type of data that you're expecting back from the server
|contentType|Content type of the data to sent to the server. Default is "application/x-www-form-urlencoded; charset=UTF-8"
|context|Specifies the context to be used inside callbacks, usually `this` which refers to the current target.



#### Remarks


AJAX stands for **A**synchronous **J**avaScript **a**nd **X**ML. AJAX allows a webpage to perform an asynchronous HTTP (AJAX) request to the server and receive a response, without needing to reload the entire page.

