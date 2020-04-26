# Flask


Flask is a Python micro web framework used to run major websites including Pintrest, Twilio, and  Linkedin. This topic explains and demonstrates the variety of features Flask offers for both front and back end web development.



## Files and Templates


Instead of typing our HTML markup into the return statements, we can use the `render_template()` function:

```
from flask import Flask
from flask import render_template
app = Flask(__name__)

@app.route(&quot;/about&quot;)
def about():
    return render_template(&quot;about-us.html&quot;)

if __name__ == &quot;__main__&quot;:
    app.run(host=&quot;0.0.0.0&quot;, port=80, debug=True)

```

This will use our template file `about-us.html`. To ensure our application can find this file we must organize our directory in the following format:

```
- application.py
/templates
    - about-us.html
    - login-form.html
/static
    /styles
        - about-style.css
        - login-style.css
    /scripts
        - about-script.js
        - login-script.js

```

Most importantly, references to these files in the HTML must look like this:

`<link rel=&quot;stylesheet&quot; type=&quot;text/css&quot;, href=&quot;{{url_for('static', filename='styles/about-style.css')}}&quot;>`

which will direct the application to look for `about-style.css` in the styles folder under the static folder. The same format of path applies to all references to images, styles, scripts, or files.



## The basics


The following example is an example of a basic server:

```
# Imports the Flask class
from flask import Flask
# Creates an app and checks if its the main or imported
app = Flask(__name__)

# Specifies what URL triggers hello_world()
@app.route('/')
# The function run on the index route
def hello_world():
    # Returns the text to be displayed
    return &quot;Hello World!&quot;

# If this script isn't an import
if __name__ == &quot;__main__&quot;:
    # Run the app until stopped
    app.run()

```

Running this script (with all the right dependencies installed) should start up a local server. The host is `127.0.0.1` commonly known as **localhost**. This server by default runs on port **5000**. To access your webserver, open a web browser and enter the URL `localhost:5000` or `127.0.0.1:5000` (no difference). Currently, only your computer can access the webserver.

`app.run()` has three parameters, **host**, **port**, and **debug**. The host is by default `127.0.0.1`, but setting this to `0.0.0.0` will make your web server accessible from any device on your network using your private IP address in the URL. the port is by default 5000 but if the parameter is set to port `80`, users will not need to specify a port number as browsers use port 80 by default. As for the debug option, during the development process (never in production) it helps to set this parameter to True, as your server will restart when changes made to your Flask project.

```
if __name__ == &quot;__main__&quot;:
    app.run(host=&quot;0.0.0.0&quot;, port=80, debug=True)

```



## Routing URLs


With Flask, URL routing is traditionally done using decorators. These decorators can be used for static routing, as well as routing URLs with parameters. For the following example, imagine this Flask script is running the website `www.example.com`.

```
@app.route(&quot;/&quot;)
def index():
    return &quot;You went to www.example.com&quot;

@app.route(&quot;/about&quot;)
def about():
    return &quot;You went to www.example.com/about&quot;

@app.route(&quot;/users/guido-van-rossum&quot;)
    return &quot;You went to www.example.com/guido-van-rossum&quot;

```

With that last route, you can see that given a URL with /users/ and the profile name, we could return a profile. Since it would be horribly inefficient and messy to include a `@app.route()` for every user, Flask offers to take parameters from the URL:

```
@app.route(&quot;/users/<username>&quot;)
def profile(username):
    return &quot;Welcome to the profile of &quot; + username

cities = [&quot;OMAHA&quot;, &quot;MELBOURNE&quot;, &quot;NEPAL&quot;, &quot;STUTTGART&quot;, &quot;LIMA&quot;, &quot;CAIRO&quot;, &quot;SHANGHAI&quot;]

@app.route(&quot;/stores/locations/<city>&quot;)
def storefronts(city):
    if city in cities:
        return &quot;Yes! We are located in &quot; + city
    else:
        return &quot;No. We are not located in &quot; + city

```



## HTTP Methods


The two most common HTTP methods are **GET** and **POST**. Flask can run different code from the same URL dependent on the HTTP method used. For example, in a web service with accounts, it is most convenient to route the sign in page and the sign in process through the same URL. A GET request, the same that is made when you open a URL in your browser should show the login form, while a POST request (carrying login data) should be processed separately. A route is also created to handle the DELETE and PUT HTTP method.

```
@app.route(&quot;/login&quot;, methods=[&quot;GET&quot;])
def login_form():
    return &quot;This is the login form&quot;
@app.route(&quot;/login&quot;, methods=[&quot;POST&quot;])
def login_auth():
    return &quot;Processing your data&quot;
@app.route(&quot;/login&quot;, methods=[&quot;DELETE&quot;, &quot;PUT&quot;])
def deny():
    return &quot;This method is not allowed&quot;

```

To simplify the code a bit, we can import the `request` package from flask.

```
from flask import request

@app.route(&quot;/login&quot;, methods=[&quot;GET&quot;, &quot;POST&quot;, &quot;DELETE&quot;, &quot;PUT&quot;])
def login():
    if request.method == &quot;DELETE&quot; or request.method == &quot;PUT&quot;:
        return &quot;This method is not allowed&quot;
    elif request.method == &quot;GET&quot;:
        return &quot;This is the login forum&quot;
    elif request.method == &quot;POST&quot;:
        return &quot;Processing your data&quot;

```

To retrieve data from the POST request, we must use the `request` package:

```
from flask import request
@app.route(&quot;/login&quot;, methods=[&quot;GET&quot;, &quot;POST&quot;, &quot;DELETE&quot;, &quot;PUT&quot;])
def login():
    if request.method == &quot;DELETE&quot; or request.method == &quot;PUT&quot;:
        return &quot;This method is not allowed&quot;
    elif request.method == &quot;GET&quot;:
        return &quot;This is the login forum&quot;
    elif request.method == &quot;POST&quot;:
        return &quot;Username was &quot; + request.form[&quot;username&quot;] + &quot; and password was &quot; + request.form[&quot;password&quot;]

```



## Jinja Templating


Similar to Meteor.js, Flask integrates well with front end templating services. Flask uses by  default Jinja Templating. Templates allow small snippets of code to be used in the HTML file such as conditionals or loops.

When we render a template, any parameters beyond the template file name are passed into the HTML templating service. The following route will pass the username and joined date (from a function somewhere else) into the HTML.

```
@app.route(&quot;/users/<username>)
def profile(username):
    joinedDate = get_joined_date(username) # This function's code is irrelevant
    awards = get_awards(username) # This function's code is irrelevant
    # The joinDate is a string and awards is an array of strings
    return render_template(&quot;profile.html&quot;, username=username, joinDate=joinDate, awards=awards)

```

When this template is rendered, it can use the variables passed to it from the `render_template()` function. Here are the contents of `profile.html`:

```
<!DOCTYPE html>
<html>
    <head>
        # if username
            <title>Profile of {{ username }}</title>
        # else
            <title>No User Found</title>
        # endif
    <head>
    <body>
        {% if username %}
            <h1>{{ username }} joined on the date {{ date }}</h1>
            {% if len(awards) > 0 %}
                <h3>{{ username }} has the following awards:</h3>
                <ul>
                {% for award in awards %}
                    <li>{{award}}</li>
                {% endfor %}
                </ul>
            {% else %}
                <h3>{{ username }} has no awards</h3>
            {% endif %}
        {% else %}
            <h1>No user was found under that username</h1>
        {% endif %}
        {# This is a comment and doesn't affect the output #}
    </body>
</html>

```

The following delimiters are used for different interpretations:

- `{% ... %}` denotes a statement
- `{{ ... }}` denotes an expression where a template is outputted
- `{# ... #}` denotes a comment (not included in template output)
- `{# ... ##` implies the rest of the line should be interpreted as a statement



## The Request Object


The `request` object provides information on the request that was made to the route. To utilize this object, it must be imported from the flask module:

```
from flask import request

```

### URL Parameters

In previous examples `request.method` and `request.form` were used, however we can also use the `request.args` property to retrieve a dictionary of the keys/values in the URL parameters.

```
@app.route(&quot;/api/users/<username>&quot;)
def user_api(username):
    try:
        token = request.args.get(&quot;key&quot;)
        if key == &quot;pA55w0Rd&quot;:
            if isUser(username): # The code of this method is irrelevant
                joined = joinDate(username) # The code of this method is irrelevant
                return &quot;User &quot; + username + &quot; joined on &quot; + joined
            else:
                return &quot;User not found&quot;
        else:
            return &quot;Incorrect key&quot;
    # If there is no key parameter
    except KeyError:
        return &quot;No key provided&quot;

```

To correctly authenticate in this context, the following URL would be needed (replacing the username with any username:

`www.example.com/api/users/guido-van-rossum?key=pa55w0Rd`

### File Uploads

If a file upload was part of the submitted form in a POST request, the files can be handled using the `request` object:

```
@app.route(&quot;/upload&quot;, methods=[&quot;POST&quot;])
def upload_file():
    f = request.files[&quot;wordlist-upload&quot;]
    f.save(&quot;/var/www/uploads/&quot; + f.filename) # Store with the original filename

```

### Cookies

The request may also include cookies in a dictionary similar to the URL parameters.

```
@app.route(&quot;/home&quot;)
def home():
    try:
        username = request.cookies.get(&quot;username&quot;)
        return &quot;Your stored username is &quot; + username
    except KeyError:
        return &quot;No username cookies was found&quot;)

```



#### Syntax


- @app.route(&quot;/urlpath&quot;, methods=[&quot;GET&quot;, &quot;POST&quot;, &quot;DELETE&quot;, &quot;PUTS&quot;, &quot;HEAD&quot;, &quot;OPTIONS&quot;])
- @app.route(&quot;/urlpath/<param>&quot;, methods=[&quot;GET&quot;, &quot;POST&quot;, &quot;DELETE&quot;, &quot;PUTS&quot;, &quot;HEAD&quot;, &quot;OPTIONS&quot;])

