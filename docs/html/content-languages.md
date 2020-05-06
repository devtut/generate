---
metaTitle: "HTML - Content Languages"
description: "Base Document Language, Element Language, Elements with Multiple Languages, Handling Attributes with Different Languages, Regional URLs"
---

# Content Languages




## Base Document Language


It’s a good practice to declare the primary language of the document in the `html` element:

```html
<html lang="en">

```

If no other `lang` attribute is specified in the document, it means that **everything** (i.e., element content and attribute text values) is in that language.

If the document contains parts in other languages, these parts should get their own `lang` attributes to "overwrite" the language declaration.



## Element Language


The `lang` attribute is used to specify the language of element content and attribute text values:

```html
<p lang="en">The content of this element is in English.</p>

```

```html
<p lang="en" title="The value of this attribute is also in English.">The content of this element is in English.</p>

```

The language declaration gets inherited:

```html
<div lang="en">
  <p>This element contains English content.</p>
  <p title="This attribute, too.">Same with this element.</p>
</div>

```



## Elements with Multiple Languages


You can "overwrite" a language declaration:

```html
<p lang="en">This English sentence contains the German word <span lang="de">Hallo</span>.</p>

```



## Handling Attributes with Different Languages


You can "overwrite" a parent element's language declaration by introducing any element apart from `applet`, `base`, `basefont`, `br`, `frame`, `frameset`, `hr`, `iframe`, `meta`, `param`, `script` (of HTML 4.0) with an own [`lang`](https://www.w3.org/TR/html5/dom.html#the-lang-and-xml:lang-attributes) attribute:

```html
<p lang="en" title="An English paragraph">
    <span lang="de" title="A German sentence">Hallo Welt!</span>
</p>

```



## Regional URLs


It is possible to add the attribute [`hreflang`](https://www.w3.org/TR/2014/REC-html5-20141028/links.html#attr-hyperlink-hreflang) to the elements `<a>` and `<area>` that create hyperlinks. Such it specifies the language of the linked resource. The language defined must be a valid [BCP 47](https://en.wikipedia.org/wiki/BCP_47)<sup>[1]</sup> language tag.

```html
<p>
    <a href="example.org" hreflang="en">example.org</a> is one of IANA's example domains.
</p>

```


1. ↑ IETF Network Working Group: RFC 5646 [**Tags for Identifying Languages**](http://www.ietf.org/rfc/bcp/bcp47.txt), IETF, September 2009



#### Syntax


<li>
`<element lang="language_code">`   <!-- Language code has to be in the format [[ISO 639-1](](https://en.wikipedia.org/wiki/ISO_639-1)[https://en.wikipedia.org/wiki/ISO_639-1](https://en.wikipedia.org/wiki/ISO_639-1) [)](https://en.wikipedia.org/wiki/ISO_639-1) -->
</li>



#### Remarks


The value of the `lang` attribute must be a valid **BCP 47 language tag** or the **empty string** (if the language is unknown).

The [BCP 47](http://www.ietf.org/rfc/bcp/bcp47.txt) language tags are listed in the [IANA Language Subtag Registry](http://www.iana.org/assignments/language-subtag-registry).

### Accessibility

The relevant WCAG 2.0 Success Criteria are:

- [3.1.1 Language of Page](https://www.w3.org/TR/2008/REC-WCAG20-20081211/#meaning-doc-lang-id)
- [3.1.2 Language of Parts](https://www.w3.org/TR/2008/REC-WCAG20-20081211/#meaning-other-lang-id)

The related WCAG 2.0 Techniques are:

- [H57: Using language attributes on the html element](https://www.w3.org/TR/WCAG20-TECHS/H57.html)
- [H58: Using language attributes to identify changes in the human language](https://www.w3.org/TR/WCAG20-TECHS/H58.html)

