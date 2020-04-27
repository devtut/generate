---
metaTitle: "Flask"
description: "Files and Templates, The basics, Routing URLs, HTTP Methods, Jinja Templating, The Request Object"
---

# Flask


Flask is a Python micro web framework used to run major websites including Pintrest, Twilio, and  Linkedin. This topic explains and demonstrates the variety of features Flask offers for both front and back end web development.



## Files and Templates


Instead of typing our HTML markup into the return statements, we can use the `render_template()` function:

```
from flask import Flask
from flask import render_template
app = Flask(__name__)

@app.route("/about")
def about():
    return render_template("about-us.html")

if __name__ == "__main__":
    app.run(host="0.0.0.0", port=80, debug=True)

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

