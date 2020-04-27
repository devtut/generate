# Django


Django is a high-level Python Web framework that encourages rapid development and clean, pragmatic design. Built by experienced developers, it takes care of much of the hassle of Web development, so you can focus on writing your app without needing to reinvent the wheel. It’s free and open source.



## Hello World with Django


Make a simple `Hello World` Example using your django.

let's make sure that you have django installed on your PC first.

open a terminal and type: python -c "import django"<br />
-->if no error comes that means django is already installed.

Now lets create a project in django. For that write below command on terminal:<br />
django-admin startproject HelloWorld

Above command will create a directory named HelloWorld.<br />
Directory structure will be like:<br />
HelloWorld<br />
|--helloworld<br />
|  |--**init**.py<br />
|  |--settings.py<br />
|  |--urls.py<br />
|  |--wsgi.py<br />
|--manage.py

**Writing Views** (Reference from django documentation)

A view function, or view for short, is simply a Python function that takes a Web request and returns a Web response. This response can be the HTML contents of a Web page or anything.Documentation says we can write views function any where but its better to write in views.py placed in our project directory.

Here's a view that returns a hello world message.(views.py)

```
from django.http import HttpResponse

define helloWorld(request):
    return HttpResponse("Hello World!! Django Welcomes You.")

```

let's understand the code, step by step.

<li>
First, we import the class HttpResponse from the django.http module.
</li>
<li>
Next, we define a function called helloWorld. This is the view function. Each view function takes an HttpRequest object as its first parameter, which is typically named request.
Note that the name of the view function doesn’t matter; it doesn’t have to be named in a certain way in order for Django to recognise it. we called it helloWorld here, so that, it will be clear what it does.
</li>
<li>
The view returns an HttpResponse object that contains the generated response. Each view function is responsible for returning an HttpResponse object.
</li>

[For more info on django views click here](https://docs.djangoproject.com/en/1.11/topics/http/views/)

**Mapping URLs to views**<br />
To display this view at a particular URL, you’ll need to create a URLconf;

Before that let's understand how django processes requests.

- Django determines the root URLconf module to use.
- Django loads that Python module and looks for the variable urlpatterns. This should be a Python list of django.conf.urls.url() instances.
- Django runs through each URL pattern, in order, and stops at the first one that matches the requested URL.
- Once one of the regexes matches, Django imports and calls the given view, which is a simple Python function.

Here’s how our URLconf look alike:

```
from django.conf.urls import url
from . import views #import the views.py from current directory
    
urlpatterns = [
   url(r'^helloworld/$', views.helloWorld),
]

```

[For more info on django Urls click here](https://docs.djangoproject.com/en/1.11/topics/http/urls/)

Now change directory to HelloWorld and write below command on terminal.<br />
python manage.py runserver

by default the server will be run at 127.0.0.1:8000

Open your browser and type 127.0.0.1:8000/helloworld/. The page will show you "Hello World!! Django Welcomes You."

