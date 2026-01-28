---
metaTitle: "HTML - Text Formatting"
description: "Highlighting, Bold, Italic, and Underline, Abbreviation, Inserted, Deleted, or Stricken, Superscript and Subscript"
---

# Text Formatting


While most HTML tags are used to create elements, HTML also provides in-text formatting tags to apply specific text-related styles to portions of text. This topic includes examples of HTML text formatting such as  highlighting, bolding, underlining, subscript, and stricken text.



## Highlighting


The `<mark>` element is new in HTML5 and is used to mark or highlight text in a document "due to its relevance in another context".[<sup>1</sup>](https://www.w3.org/TR/2014/REC-html5-20141028/text-level-semantics.html#the-mark-element)

The most common example would be in the results of a search were the user has entered a search query and results are shown highlighting the desired query.

```html
<p>Here is some content from an article that contains the <mark>searched query</mark>
that we are looking for. Highlighting the text will make it easier for the user to 
find what they are looking for.</p>

```

Output:

[<img src="http://i.stack.imgur.com/XWNfJ.png" alt="Black text on yellow background is a common default style for the mark element" />](http://i.stack.imgur.com/XWNfJ.png)

A common standard formatting is black text on a yellow background, but this can be changed with CSS.



## Bold, Italic, and Underline


### Bold Text

To bold text, use the `<strong>` or `<b>` tags:

```html
<strong>Bold Text Here</strong>

```

or

```html
<b>Bold Text Here</b>

```

What’s the difference? Semantics. `<strong>` is used to indicate that the text is fundamentally or semantically **important** to the surrounding text, while `<b>` indicates no such importance and simply represents text that should be bolded.

If you were to use `<b>` a text-to-speech program would not say the word(s) any differently than any of the other words around it - you are simply drawing attention to them without adding any additional importance. By using `<strong>`, though, the same program would want to speak those word(s) with a different tone of voice to convey that the text is important in some way.

### Italic Text

To italicize text, use the `<em>` or `<i>` tags:

```html
<em>Italicized Text Here</em>

```

or

```html
<i>Italicized Text Here</i>

```

What’s the difference? Semantics. `<em>` is used to indicate that the text should have extra emphasis that should be stressed, while `<i>` simply represents text which should be set off from the normal text around it.

For example, if you wanted to stress the action inside a sentence, one might do so by emphasizing it in italics via `<em>`: "Would you just **submit** the edit already?"

But if you were identifying a book or newspaper that you would normally italicize stylistically, you would simply use `<i>`: "I was forced to read *Romeo and Juliet* in high school.

### Underlined Text

While the `<u>` element itself was deprecated in HTMl 4, it was reintroduced with alternate semantic meaning in HTML 5 - to represent an unarticulated, non-textual annotation. You might use such a rendering to indicate misspelled text on the page, or for a Chinese proper name mark.

```html
<p>This paragraph contains some <u>mispelled</u> text.</p>

```



## Abbreviation


To mark some expression as an abbreviation, use `<abbr>` tag:

```html
<p>I like to write <abbr title="Hypertext Markup Language">HTML</abbr>!</p>

```

If present, the `title` attribute is used to present the full description of such abbreviation.



## Inserted, Deleted, or Stricken


To mark text as inserted, use the `<ins>` tag:

```html
<ins>New Text</ins>

```

To mark text as deleted, use the `<del>` tag:

```html
<del>Deleted Text</del>

```

To strike through text, use the `<s>` tag:

```html
<s>Struck-through text here</s>

```



## Superscript and Subscript


To offset text either upward or downward you can use the tags `<sup>` and `<sub>`.

To create superscript:

```html
<sup>superscript here</sup>

```

To create subscript:

```html
<sub>subscript here</sub>

```



#### Syntax


- `<abbr>Abbreviation</abbr>`
- `<b>Bold Text</b>`
- `<del>Deleted Text</del>`
- `<em>Emphasized Text</em>`
- `<i>Italic Text</i>`
- `<ins>Inserted Text</ins>`
- `<mark>Marked (or Highlighted) Text</mark>`
- `<s>Stricken Text</s>`
- `<strong>Strong Text</strong>`
- `<sub>Subscript Text</sub>`
- `<sup>Superscript Text</sup>`
- `<u>Underlined Text</u>`

