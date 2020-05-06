---
metaTitle: "HTML - Marking-up Quotes"
description: "Inline with <q>, Block with <blockquote>"
---

# Marking-up Quotes



## Inline with <q>


The **`q` element** can be used for a quote that is part of a sentence:

```html
<p>She wrote <q>The answer is 42.</q> and everyone agreed.</p>

```

### Quotation marks

Quotation marks should not be added. User agents should (in HTML 4.01) resp. must (in HTML 4.0) render them automatically.

Quotation marks must not be added. User agents will render them automatically.

### Source URL (`cite` attribute)

The **`cite` attribute** can be used to reference the URL of the quoted source:

```html
<p>She wrote <q cite="http://example.com/blog/hello-world">The answer is 42.</q> and everyone agreed.</p>

```

Note that browsers typically don’t show this URL, so if the source is relevant, you should add a hyperlink (`a` element) in addition.



## Block with <blockquote>


The **`blockquote` element** can be used for a (block-level) quote:

```html
<blockquote>
  <p>The answer is 42.</p>
</blockquote>

```

### Source URL (`cite` attribute)

The **`cite` attribute** can be used to reference the URL of the quoted source:

```html
<blockquote cite="http://example.com/blog/hello-world">
  <p>The answer is 42.</p>
</blockquote>

```

Note that browsers typically don’t show this URL, so if the source is relevant, you should add a hyperlink (`a` element) in addition (see the section **Citation/Attribution** about where to place this link).

### Citation/Attribution

The citation/attribution should not be part of the `blockquote` element:

```html
<blockquote cite="http://example.com/blog/hello-world">
  <p>The answer is 42.</p>
</blockquote>
<p>Source: <cite><a href="http://example.com/blog/hello-world" rel="external">Hello World</a></cite></p>

```

You can add a `div` element to group the quote and the citation, but it exists no way to associate them semantically.

The **`cite` element** can be used for the reference of the quoted source (but not for the author name).

The citation/attribution (e.g., the hyperlink giving the source URL) can be inside the `blockquote`, but in that case it must be within a `cite` element (for in-text attributions) or a `footer` element:

```html
<blockquote cite="http://example.com/blog/hello-world">
  <p>The answer is 42.</p>
  <footer>
    <p>Source: <cite><a href="http://example.com/blog/hello-world" rel="external">Hello World</a></cite></p>
  </footer>
</blockquote>

```

The **`cite` element** can be used for the reference of the quoted source, or for the name of the quote’s author.



#### Remarks


`cite` and `blockquote` elements should not be used for the purpose of representing a conversation, transcripts of conversations, dialogues in scripts, records of instant messages and other situations in which different players take turns in the speech.

