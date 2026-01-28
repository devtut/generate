---
metaTitle: "HTML - Getting started with HTML"
description: "Hello World"
---

# Getting started with HTML



## Hello World


### Introduction

[HTML](https://en.wikipedia.org/wiki/HTML) (**H**yper**t**ext **M**arkup **L**anguage) uses a markup system composed of elements which represent specific content. **Markup** means that with HTML you declare **what** is presented to a viewer, not **how** it is presented. Visual representations are defined by [Cascading Style Sheets (CSS)](https://en.wikipedia.org/wiki/CSS) and realized by browsers. [Still existing elements that allow for such](https://www.w3.org/TR/html5/obsolete.html#non-conforming-features), like e.g. [`font`](https://www.w3.org/wiki/HTML/Elements/font), "are entirely obsolete, and must not be used by authors"<sup>[1]</sup>.

HTML is sometimes called a programming language but it has no logic, so is a **markup language**. HTML tags provide semantic meaning and machine-readability to the content in the page.

An element usually consists of an opening tag (`<element_name>`), a closing tag (`</element_name>`), which contain the element's name surrounded by angle brackets, and the content in between: `<element_name>...content...</element_name>`

There are some HTML elements that don't have a closing tag or any contents. These are called [void elements](https://stackoverflow.com/documentation/html/1449/void-elements). Void elements include `<img>`, `<meta>`, `<link>` and `<input>`.

Element names can be thought of as descriptive keywords for the content they contain, such as `video`, `audio`, `table`, `footer`.

A HTML page may consist of potentially hundreds of elements which are then read by a web browser, interpreted and rendered into human readable or audible content on the screen.

For this document it is important to note the difference between elements and tags:

**Elements:** `video`, `audio`, `table`, `footer`

**Tags:** `<video>`, `<audio>`, `<table>`, `<footer>`, `</html>`, `</body>`

<br><hr />

### Element insight

Let's break down a tag...

The `<p>` tag represents a common paragraph.

Elements commonly have an opening tag and a closing tag. The opening tag contains the element's name in angle brackets (`<p>`). The closing tag is identical to the opening tag with the addition of a forward slash (`/`) between the opening bracket and the element's name (`</p>`).

Content can then go between these two tags: `<p>This is a simple paragraph.</p>`.

<br /><hr />

### Creating a simple page

The following HTML example creates a simple ["Hello World"](https://en.wikipedia.org/wiki/%22Hello,_World!%22_program) web page.

HTML files can be created using any [text editor](https://en.wikipedia.org/wiki/Text_editor). The files must be saved with a `.html` or `.htm`<sup>[2]</sup> extension in order to be recognized as HTML files.

Once created, this file can be opened in any web browser.

```html
<!DOCTYPE html>
<html lang="en">

    <head>
        <meta charset="UTF-8">
        <title>Hello!</title>
    </head>

    <body>
        <h1>Hello World!</h1>
        <p>This is a simple paragraph.</p>
    </body>

</html>

```

<br /><hr />

### Simple page break down

These are the tags used in the example:

|Tag|Meaning
|---|---|---|---|---|---|---|---|---|---
|`<!DOCTYPE>`|Defines the HTML version used in the document. In this case it is HTML5.<br>See the [doctypes topic](http://stackoverflow.com/documentation/html/806/doctypes) for more information.
|`<html>`|Opens the page. No markup should come after the closing tag (`</html>`). The `lang` attribute declares the primary language of the page using the [ISO language codes](https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes) (`en` for English).<br />See the [Content Language topic](http://stackoverflow.com/documentation/html/737/content-languages#t=201607221637059314928) for more information.
|`<head>`|Opens the head section, which does not appear in the main browser window but mainly contains information **about** the HTML document, called **metadata**. It can also contain imports from external stylesheets and scripts. The closing tag is `</head>`.
|`<meta>`|Gives the browser some metadata about the document. The `charset` attribute declares the [character encoding](https://www.w3.org/International/questions/qa-html-encoding-declarations.en). Modern HTML documents should always use [UTF-8](https://en.wikipedia.org/wiki/UTF-8), even though it is not a requirement. In HTML, the `<meta>` tag does not require a closing tag.<br />See the [Meta topic](http://stackoverflow.com/documentation/html/1264/meta) for more information.
|`<title>`|The title of the page. Text written between this opening and the closing tag (`</title>`) will be displayed on the tab of the page or in the title bar of the browser.
|`<body>`|Opens the part of the document displayed to users, i.e. all the visible or audible content of a page. No content should be added after the closing tag `</body>`.
|`<h1>`|A level 1 heading for the page.<br />See [headings](http://stackoverflow.com/documentation/html/226/headings) for more information.
|`<p>`|Represents a common paragraph of text.

1. ↑ [HTML5, 11.2 Non-conforming features](https://www.w3.org/TR/html5/obsolete.html#non-conforming-features)<br />
2. ↑ `.htm` is inherited from the legacy [DOS](https://en.wikipedia.org/wiki/DOS) three character file extension limit.



#### Remarks


[HTML](https://en.wikipedia.org/wiki/HTML) (**H**yper**t**ext **M**arkup **L**anguage) is an [XML](http://stackoverflow.com/documentation/xml/882/introduction-to-xml#t=201608040152247808936)-compliant system of  annotating documents with 'tags'. It is used specifically to create content for web pages and web applications, which can then be shared over a network.

Apart from text, the current version of HTML supports many different [types of media](https://en.wikipedia.org/wiki/Media_type), including images and videos.

