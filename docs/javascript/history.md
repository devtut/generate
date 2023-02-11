---
metaTitle: "JavaScript - History"
description: "history.pushState(), history.replaceState(), Load a specific URL from the history list"
---

# History



## history.pushState()


Syntax :

```js
history.pushState(state object, title, url)

```

This method allows to ADD histories entries. For more reference, Please have a look on this document : [pushState() method](https://developer.mozilla.org/en-US/docs/Web/API/History_API#The_pushState()_method)

**Example :**

```js
window.history.pushState("http://example.ca", "Sample Title", "/example/path.html");

```

This example inserts a new record into the history, address bar, and page title.

Note this is different from the `history.replaceState()`. Which updates the current history entry, rather than adding a new one.



## history.replaceState()


**Syntax :**

```js
history.replaceState(data, title [, url ])

```

This method modifies the current history entry instead of creating a new one. Mainly used when we want to update URL of the current history entry.

```js
window.history.replaceState("http://example.ca", "Sample Title", "/example/path.html");

```

This example replaces the current history, address bar, and page title.

Note this is different from the `history.pushState()`. Which inserts a new history entry, rather than replacing the current one.



## Load a specific URL from the history list


**go() method**

The go() method loads a specific URL from the history list.
The parameter can either be a number which goes to the URL within the specific position (-1 goes back one page, 1 goes forward one page), or a string. The string must be a partial or full URL, and the function will go to the first URL that matches the string.

> 
Syntax


```js
history.go(number|URL)

```

> 
Example


Click on the button to go back two pages:

```js
<html>
    <head>
    <script type="text/javascript">
        function goBack()
        {
            window.history.go(-2)
        }
    </script>
    </head>
    <body>    
        <input type="button" value="Go back 2 pages" onclick="goBack()" />    
    </body>
</html>

```



#### Syntax


- window.history.pushState(domain, title, path);
- window.history.replaceState(domain, title, path);



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|domain|The domain you want to update to
|title|The title to update to
|path|The path to update to



#### Remarks


The HTML5 History API is not implemented by all browsers and implementations tend to differ between browser vendors. It is currently supported by the following browsers:

- Firefox 4+
- Google Chrome
- Internet Explorer 10+
- Safari 5+
- iOS 4

If you want to find out more about the History API implementations and methods, please refer to [the state of the HTML5 History API](https://github.com/browserstate/history.js/wiki/The-State-of-the-HTML5-History-API#coherence).

