---
metaTitle: "HTML - Marking up computer code"
description: "Block with <pre> and <code>, Inline with <code>"
---

# Marking up computer code



## Block with <pre> and <code>


If the formatting (white space, new lines, indentation) of the code matters, use the `pre` element in combination with the `code` element:

```html
<pre>
    <code>
    x = 42
    if x == 42:
        print "x is …          … 42"
    </code>
</pre>

```

You still have to escape characters with special meaning in HTML (like `<` with `&lt;`), so for **displaying** a block of HTML code (`<p>This is a paragraph.</p>`), it could look like this:

```html
<pre>
    <code>
    &lt;p>This is a paragraph.&lt;/p>
    </code>
</pre>

```



## Inline with <code>


If a sentence contains computer code (for example, the name of an HTML element), use the `code` element to mark it up:

```html
<p>The <code>a</code> element creates a hyperlink.</p>

```



#### Syntax


- `<pre>Formatted text</pre>`
- `<code>Inline Code</code>`



#### Remarks


The `code` element should be used for any kind of "string that a computer would recognize" ([HTML5](https://www.w3.org/TR/2014/REC-html5-20141028/text-level-semantics.html#the-code-element)), for example:

- source code
- terms from markup/programming languages (element names, function names, etc.)
- file names

### Related elements

For variables, the [`var` element](https://www.w3.org/TR/2014/REC-html5-20141028/text-level-semantics.html#the-var-element) can be used.

For computer output, the [`samp` element](https://www.w3.org/TR/2014/REC-html5-20141028/text-level-semantics.html#the-samp-element) can be used.

For user input, the [`kbd` element](https://www.w3.org/TR/2014/REC-html5-20141028/text-level-semantics.html#the-kbd-element) can be used.

