---
metaTitle Python Requests Post
description Simple Post, Form Encoded Data, File Upload, Responses, Authentication, Proxies
---

# Python Requests Post


Documentation for the Python Requests module in the context of the HTTP POST method and its corresponding Requests function



## Simple Post


```
from requests import post

foo = post('http://httpbin.org/post', data = {'key':'value'})

```

Will perform a simple HTTP POST operation. Posted data can be inmost formats, however key value pairs are most prevalent.

**Headers**

Headers can be viewed:

```
print(foo.headers)

```

An example response:

```
{'Content-Length': '439', 'X-Processed-Time': '0.000802993774414', 'X-Powered-By': 'Flask', 'Server': 'meinheld/0.6.1', 'Connection': 'keep-alive', 'Via': '1.1 vegur', 'Access-Control-Allow-Credentials': 'true', 'Date': 'Sun, 21 May 2017 20:56:05 GMT', 'Access-Control-Allow-Origin': '*', 'Content-Type': 'application/json'}

```

Headers can also be prepared before post:

```
headers = {'Cache-Control':'max-age=0',
        'Upgrade-Insecure-Requests':'1',
        'User-Agent':'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
        'Content-Type':'application/x-www-form-urlencoded',
        'Accept':'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
        'Referer':'https://www.groupon.com/signup',
        'Accept-Encoding':'gzip, deflate, br',
        'Accept-Language':'es-ES,es;q=0.8'
        }

 foo = post('http://httpbin.org/post', headers=headers, data = {'key':'value'})

```

**Encoding**

Encoding can be set and viewed in much the same way:

```
 print(foo.encoding)

'utf-8'

foo.encoding = 'ISO-8859-1'

```

**SSL Verification**

Requests by default validates SSL certificates of domains. This can be overridden:

```
foo = post('http://httpbin.org/post', data = {'key':'value'}, verify=False)

```

**Redirection**

Any redirection will be followed (e.g. http to https) this can also be changed:

```
foo = post('http://httpbin.org/post', data = {'key':'value'}, allow_redirects=False)

```

If the post operation has been redirected, this value can be accessed:

```
print(foo.url) 

```

A full history of redirects can be viewed:

```
print(foo.history) 

```



## Form Encoded Data


```
from requests import post

payload = {'key1' : 'value1',
           'key2' : 'value2'
           }

foo = post('http://httpbin.org/post', data=payload)

```

To pass form encoded data with the post operation, data must be structured as dictionary and supplied as the data parameter.

If the data does not want to be form encoded, simply pass a string, or integer to the data parameter.

Supply the dictionary to the json parameter for Requests to format the data automatically:

```
from requests import post

payload = {'key1' : 'value1', 'key2' : 'value2'}

foo = post('http://httpbin.org/post', json=payload)

```



## File Upload


With the Requests module,its is only necessary to provide a file handle as opposed to the contents retrieved with `.read()`:

```
from requests import post

files = {'file' : open('data.txt', 'rb')}

foo = post('http://http.org/post', files=files)

```

Filename, content_type and headers can also be set:

```
files = {'file': ('report.xls', open('report.xls', 'rb'), 'application/vnd.ms-excel', {'Expires': '0'})}

foo = requests.post('http://httpbin.org/post', files=files)

```

Strings can also be sent as a file, as long they are supplied as the `files` parameter.

**Multiple Files**

Multiple files can be supplied in much the same way as one file:

```
multiple_files = [
    ('images', ('foo.png', open('foo.png', 'rb'), 'image/png')),
    ('images', ('bar.png', open('bar.png', 'rb'), 'image/png'))]

foo = post('http://httpbin.org/post', files=multiple_files)

```



## Responses


Response codes can be viewed from a post operation:

```
from requests import post

foo = post('http://httpbin.org/post', data={'data' : 'value'})
print(foo.status_code)

```

**Returned Data**

Accessing data that is returned:

```
foo = post('http://httpbin.org/post', data={'data' : 'value'})
print(foo.text)

```

**Raw Responses**

In the instances where you need to access the underlying urllib3 response.HTTPResponse object, this can be done by the following:

```
foo = post('http://httpbin.org/post', data={'data' : 'value'})
res = foo.raw

print(res.read())

```



## Authentication


**Simple HTTP Authentication**

Simple HTTP Authentication can be achieved with the following:

```
from requests import post

foo = post('http://natas0.natas.labs.overthewire.org', auth=('natas0', 'natas0'))

```

This is technically short hand for the following:

```
from requests import post
from requests.auth import HTTPBasicAuth

foo = post('http://natas0.natas.labs.overthewire.org', auth=HTTPBasicAuth('natas0', 'natas0'))

```

**HTTP Digest Authentication**

HTTP Digest Authentication is done in a very similar way, Requests provides a different object for this:

```
from requests import post
from requests.auth import HTTPDigestAuth

foo = post('http://natas0.natas.labs.overthewire.org', auth=HTTPDigestAuth('natas0', 'natas0'))

```

**Custom Authentication**

In some cases the built in authentication mechanisms may not be enough, imagine this example:

A server is configured to accept authentication if the sender has the correct user-agent string, a certain header value and supplies the correct credentials through HTTP Basic Authentication. To achieve this a custom authentication class should be prepared, subclassing AuthBase, which is the base for Requests authentication implementations:

```
from requests.auth import AuthBase
from requests.auth import _basic_auth_str
from requests._internal_utils import to_native_string

class CustomAuth(AuthBase):

    def __init__(self, secret_header, user_agent , username, password):
        # setup any auth-related data here
        self.secret_header =  secret_header
        self.user_agent = user_agent
        self.username = username
        self.password = password

    def __call__(self, r):
        # modify and return the request
        r.headers['X-Secret'] = self.secret_header
        r.headers['User-Agent'] = self.user_agent
        r.headers['Authorization'] = _basic_auth_str(self.username, self.password)
        
        return r

```

This can then be utilized with the following code:

```
foo = get('http://test.com/admin', auth=CustomAuth('SecretHeader', 'CustomUserAgent', 'user', 'password' ))

```



## Proxies


Each request POST operation can be configured to use network proxies

**HTTP/S Proxies**

```
from requests import post

proxies = {
  'http': 'http://192.168.0.128:3128',
  'https': 'http://192.168.0.127:1080',
   }

foo = requests.post('http://httpbin.org/post', proxies=proxies)

```

HTTP Basic Authentication can be provided in this manner:

```
proxies = {'http': 'http://user:pass@192.168.0.128:312'}
foo = requests.post('http://httpbin.org/post', proxies=proxies)

```

**SOCKS Proxies**

The use of socks proxies requires 3rd party dependencies `requests[socks]`, once installed socks proxies are used in a very similar way to HTTPBasicAuth:

```
proxies = {
'http': 'socks5://user:pass@host:port',
'https': 'socks5://user:pass@host:port'
}

foo = requests.post('http://httpbin.org/post', proxies=proxies)

```

