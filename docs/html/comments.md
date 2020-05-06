---
metaTitle: "HTML - Comments"
description: "Creating comments, Conditional comments for Internet Explorer, Commenting out whitespace between inline elements"
---

# Comments


Similar to other programming, markup, and markdown languages, comments in HTML provide other developers with development specific information without affecting the user interface. Unlike other languages however, HTML comments can be used to specify HTML elements for Internet Explorer only. This topic explains how to write HTML comments, and their functional applications.



## Creating comments


HTML comments can be used to leave notes to yourself or other developers about a specific point in code. They can be initiated with `<!--` and concluded with `-->`, like so:

```html
<!-- I'm an HTML comment! -->

```

They can be incorporated inline within other content:

```html
<h1>This part will be displayed <!-- while this will not be displayed -->.</h1>

```

They can also span multiple lines to provide more information:

```html
<!-- This is a multiline HTML comment.
  Whatever is in here will not be rendered by the browser.
  You can "comment out" entire sections of HTML code.
-->

```

However, they **cannot** appear within another HTML tag, like this:

```html
<h1 <!-- testAttribute="something" -->>This will not work</h1>

```

This produces invalid HTML as the entire `<h1 <!-- testAttribute="something" -->` block would be considered a single start tag `h1` with some other invalid information contained within it, followed by a single `>` closing bracket that does nothing.

For compatibility with tools that try to parse HTML as XML or SGML, the body of your comment should not contain two dashes `--`.



## Conditional comments for Internet Explorer


Conditional comments can be used to customize code for different versions of Microsoft Internet Explorer. For example, different HTML classes, script tags, or stylesheets can be provided. Conditional comments are supported in Internet Explorer versions 5 through 9. Older and newer Internet Explorer versions, and all non-IE browsers, are considered "downlevel" and treat conditional comments as ordinary HTML comments.

<h3>Downlevel-hidden</h3>

Downlevel-hidden comments work by encapsulating the entire content within what appears to be a normal HTML comment. Only IE 5 through 9 will still read it as a conditional comment, and they will hide or display the content accordingly. In other browsers the content will be hidden.

```html
<!--[if IE]>
  Revealed in IE 5 through 9. Commented out and hidden in all other browsers.
<![endif]-->

<!--[if lt IE 8]>
  Revealed only in specified versions of IE 5-9 (here, IE less than 8).
<![endif]-->

<!--[if !IE]>
  Revealed in no browsers. Equivalent to a regular HTML comment.
<![endif]-->

<!--
  For purposes of comparison, this is a regular HTML comment.
-->

```

<h3>Downlevel-revealed</h3>

These are slightly different than downlevel-hidden comments: only the conditional comment itself is contained within the normal comment syntax. Browsers which do not support conditional comments will simply ignore them and display the rest of the content between them.

```html
<!--[if IE]>-->
  The HTML inside this comment is revealed in IE 5-9, and in all other browsers.
<!--<![endif]-->

<!--[if IE 9]>-->
  This is revealed in specified versions of IE 5-9, and in all other browsers.
<!--<![endif]-->

<!--[if !IE]>-->
  This is not revealed in IE 5-9. It's still revealed in other browsers.
<!--<![endif]-->

```



## Commenting out whitespace between inline elements


Inline display elements, usually such as `span` or `a`, will include up to one white-space character before and after them in the document. In order to avoid very long lines in the markup (that are hard to read) and unintentional white-space (which affects formatting), the white-space can be commented out.

```html
<!-- Use an HTML comment to nullify the newline character below: -->
<a href="#">I hope there will be no extra whitespace after this!</a><!--
--><button>Foo</button>

```

Try it without a comment between the inline elements, and there will be one space between them. Sometimes picking up the space character is desired.

Example code:

```html
<!-- Use an HTML comment to nullify the newline character below: -->
<a href="#">I hope there will be no extra whitespace after this!</a><!--
--><button>Foo</button>
<hr>
<!-- Without it, you can notice a small formatting difference: -->
<a href="#">I hope there will be no extra whitespace after this!</a>
<button>Foo</button>

```

Output:

[<img src="http://i.stack.imgur.com/LE9oM.png" alt="enter image description here" />](http://i.stack.imgur.com/LE9oM.png)



#### Syntax


- `<!-- Comment text -->`



#### Remarks


Anything starting with `<!--` and ending with `-->` is a comment. Comments cannot contain two adjacent dashes (`--`), and must end with exactly two dashes (i.e. `--->` is not correct).

Comments are not visible on a web page and cannot be styled with CSS. They can be used by the page's developer to make notes within the HTML, or to hide certain content during development.

For dynamic or interactive pages, hiding and showing content is done with JavaScript and CSS rather than with HTML comments.

JavaScript can be used to get the content of HTML comment nodes and these nodes can be dynamically created, added and removed from the document but this will not affect how the page is displayed.

Since HTML comments are part of the page's source code, they are downloaded to the browser along with the rest of the page. The source code can typically be viewed using the web browser's menu option to "View Source" or "View Page Source."

