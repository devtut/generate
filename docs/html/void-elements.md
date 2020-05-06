---
metaTitle: "HTML - Void Elements"
description: "Void elements"
---

# Void Elements


Not all HTML tags are of the same structure. While most elements require an opening tag, a closing tag, and contents, some elements - known as void elements - only require an opening tag as they themselves do not contain any elements. This topic explains and demonstrates the proper usage of void elements in HTML



## Void elements


HTML 4.01/XHTML 1.0 Strict includes the following void elements:

- `area` - clickable, defined area in an image
- `base` - specifies a base URL from which all links base
- `br` - line break
- `col` - column in a table [deprecated]
- `hr` - horizontal rule (line)
- `img` - image
- `input` - field where users enter data
- `link` - links an external resource to the document
- `meta` - provides information about the document
- `param` - defines parameters for plugins

HTML 5 standards include all non-deprecated tags from the previous list and

- `command` - represents a command users can invoke [obsolete]
- `keygen` - facilitates public key generation for web certificates [deprecated]
- `source` - specifies media sources for `picture`, `audio`, and `video` elements

The example below does **not** include void elements:

```html
<div>
    <a href="http://stackoverflow.com/">
        <h3>Click here to visit <i>Stack Overflow!</i></h3>
    </a>
    <button onclick="alert('Hello!');">Say Hello!</button>
    <p>My favorite language is <b>HTML</b>. Here are my others:</p>
    <ol>
        <li>CSS</li>
        <li>JavaScript</li>
        <li>PHP</li>
    </ol>
</div>

```

Notice how every element has an opening tag, a closing tag, and text or other elements inside the opening and closing tags. Void tags however, are shown in the example below:

```html
<img src="https://cdn.sstatic.net/Sites/stackoverflow/company/img/logos/so/so-icon.png" />
<br>
<hr>
<input type="number" placeholder="Enter your favorite number">

```

With the exception of the img tag, all of these void elements have only an opening tag. The img tag, unlike any other tag, has a self closing `/` before the greater than sign of the opening tag. It is best practice to have a space before the slash.



#### Remarks


A void element cannot have any content but may have attributes. Void elements are self-closing, so they must not have a closing tag.

[In HTML5](https://www.w3.org/TR/2014/REC-html5-20141028/syntax.html#void-elements), the following elements are void:

- `area`
- `base`
- `br`
- `col`
- `embed`
- `hr`
- `img`
- `input`
- `keygen`
- `link`
- `meta`
- `param`
- `source`
- `track`
- `wbr`

