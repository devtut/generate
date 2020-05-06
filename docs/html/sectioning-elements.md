---
metaTitle: "HTML - Sectioning Elements"
description: "Nav Element, Article Element, Main Element, Section Element, Header Element, Footer Element"
---

# Sectioning Elements



## Nav Element


The `<nav>` element is primarily intended to be used for sections that contain **main navigation blocks** for the website, this can include links to other parts of the web page **(e.g. anchors for a table of contents)** or other pages entirely.

### Inline items

The following will display an inline set of hyperlinks.

```html
<nav>
    <a href="https://google.com">Google</a>
    <a href="https://www.yahoo.com">Yahoo!</a>
    <a href="https://www.bing.com">Bing</a>
</nav>

```

### Use list items when needed

If the content represents a list of items, use a list item to show this and enhance the user experience.

Note the `role="navigation"`, **more on this below.**

```html
<nav role="navigation">
    <ul>
        <li><a href="https://google.com">Google</a></li>
        <li><a href="https://www.yahoo.com">Yahoo!</a></li>
        <li><a href="https://www.bing.com">Bing</a></li>
    </ul>
</nav>

```

### Avoid unnecessary usage

`<footer>` elements may have a list of links to other parts of the site (FAQ, T&C, etc.). The footer element alone is sufficient in this case, you don't **need** to further wrap your links with a `<nav>` element in the `<footer>`.

```html
<!-- the <nav> is not required in the <footer> -->
<footer>
    <nav>
        <a href="#">...</a>
    </nav>
</footer>

<!-- The footer alone is sufficient -->
<footer>
    <a href="#">...</a>
</footer>

```

> 
**Notes:**
<ul>
- [`<main>` element](http://docs-beta.stackexchange.com/documentation/html/311/sectioning-elements/1093/main-element#t=201607161859442758976) descendants are not allowed within a `<nav>`
</ul>
Adding a `role="navigation"` [ARIA role](https://www.w3.org/TR/html-aria/) to the `<nav>` element is advised to aid user agents that don't support HTML5 and also to provide more context for those that do.

```html
<nav role="navigation"><!-- ... --></nav>

```




> 
**Screen Readers:**  **(software that allows blind or visually impaired users to navigate the site)**
User agents like screen readers will interpret the `<nav>` element differently depending on their requirements.
<ul>
- It could give the `<nav>` element a higher priority when rendering the page
- It could delay the rendering of the element
<li>It could adapt the page in a specific way to tailor for the user's needs<br />
**example:** make the text links within the `<nav>` elements larger for someone who's visually impaired.</li>
</ul>


[Click here to read the official HTML5 Specification for the `<nav>` element](https://www.w3.org/TR/html5/sections.html#the-nav-element)



## Article Element


The `<article>` element contains **self-contained content** like articles, blog posts, user comments or an interactive widget that could be distributed outside the context of the page, for example by RSS.

- When article elements are nested, the contents of the inner article node should be related to the outer article element.

A blog (`section`) with multiple posts (`article`), and comments (`article`) might look something like this.

```html
<section>
    <!-- Each individual blog post is an <article> -->
    <article>
        <header>
            <h1>Blog Post</h1>
            <time datetime="2016-03-13">13th March 2016</time>
        </header>

        <p>The article element represents a self contained article or document.</p>
        <p>The section element represents a grouping of content.</p>

        <section>
            <h2>Comments <small>relating to "Blog Post"</small></h2>

            <!-- Related comment is also a self-contained article -->
            <article id="user-comment-1">
                <p>Excellent!</p>
                <footer><p>...</p><time>...</time></footer>
            </article>
        </section>
    </article>

    <!-- ./repeat: <article> -->

</section>

<!-- Content unrelated to the blog or posts should be outside the section. -->
<footer>
    <p>This content should be unrelated to the blog.</p>
</footer>

```

### Avoid unnecessary usage

When the main content of the page (excluding headers, footers, navigation bars, etc.) is simply one group of elements. You can omit the `<article>` in favour of the `<main>` element.

```html
<article>
    <p>This doesn't make sense, this article has no real `context`.</p>
</article>

```

Instead, replace the article with a [`<main>`](http://docs-beta.stackexchange.com/documentation/html/311/sectioning-elements/1093/main-element#t=201607161859442758976) element to indicate this is the `main` content for this page.

```html
<main>
    <p>I'm the main content, I don't need to belong to an article.</p>
</main>

```

If you use another element, ensure you specify the [`<main>` ARIA role](https://www.w3.org/TR/html5/dom.html#index-aria-main) for correct interpretation and rendering across multiple devices and non HTML5 browsers.

```html
<section role="main">
    <p>This section is the main content of this page.</p>
</section>

```

> 
**Notes:**
<ul>
- [`<main>` element](http://docs-beta.stackexchange.com/documentation/html/311/sectioning-elements/1093/main-element#t=201607161859442758976) descendants are not allowed within a `<article>`
</ul>


[Click here to read the official HTML5 Specification for the `<article>` element](https://www.w3.org/TR/html5/sections.html#the-article-element)



## Main Element


The `<main>` element contains the **main content** for your web page. This content is unique to the individual page, and should not appear elsewhere on the site. Repeating content like headers, footers, navigation, logos, etc., is placed outside the element.

- The `<main>` element should only ever be used at most **once** on a single page.
- The `<main>` element must not be included as a descendant of an `article`, `aside`, `footer`, `header` or `nav` element.

In the following example, we're displaying a **single blog post** (and related information like references and comments).

```html
<body>
    <header>
        <nav>...</nav>
    </header>

    <main>
        <h1>Individual Blog Post</h1>
        <p>An introduction for the post.</p>

        <article>
            <h2>References</h2>
            <p>...</p>
        </article>

        <article>
            <h2>Comments</h2> ...
        </article>
    </main>

    <footer>...</footer>
</body>

```


<li>
The blog post is contained within the `<main>` element to indicate this is the main content for this page (and therefore, unique across the website).
</li>
<li>
The `<header>` and `<footer>` tags are **siblings** to the `<main>` element.
</li>

> 
**Notes:**
The HTML5 specification recognizes the `<main>` element as a **grouping** element, and not a **sectioning** element.
<ul>
- [ARIA role attributes](https://www.w3.org/TR/html-aria/): [`main`](https://www.w3.org/TR/html5/dom.html#index-aria-main) **(default)**, [`presentation`](https://www.w3.org/TR/html5/dom.html#index-aria-presentation)
</ul>
Adding a `role="main"` [ARIA role](https://www.w3.org/TR/html5/dom.html#index-aria-main) attribute to **other elements** intended to be used as main content is advised to aid user agents that don't support HTML5 and also to provide more context for those that do.
The `<main>` element by default has the main role, and so does not need to be provided.


[Click here to read the official HTML5 Specification for the `<main>` element](https://www.w3.org/TR/html5/grouping-content.html#the-main-element)



## Section Element


The `<section>` element represents a generic section to thematically group content. Every section, typically, should be able to be identified with a heading element as a child of the `section`.

- You can use the `<section>` element within an `<article>` and vice-versa.
- Every section should have a **theme** (a heading element identifying this region)
<li>Don't use the `<section>` element as a general styling 'container'.<br />
If you need a container to apply styling, use a `<div>` instead.</li>

In the following example, we're displaying a **single blog post** with multiple chapters each chapter is a section **(a set of thematically grouped content, which can be identified by the heading elements in each section)**.

```html
<article>
    <header>
        <h2>Blog Post</h2>
    </header>
    <p>An introduction for the post.</p>
    <section>
        <h3>Chapter 1</h3>
        <p>...</p>
    </section>
    <section>
        <h3>Chapter 2</h3>
        <p>...</p>
    </section>
    <section>
        <h3>Comments</h3> ...
    </section>
</article>

```

> 
**Notes:**
Developers should use the **article** element when it makes sense to syndicate the contents of the element.


[Click here to read the official HTML5 Specification for the `<main>` element](https://www.w3.org/TR/html5/grouping-content.html#the-section-element)



## Header Element


The `<header>` element represents introductory content for its nearest ancestor sectioning content or sectioning root element. A `<header>` typically contains a group of introductory or navigational aids.

> 
**Note:** The header element is not sectioning content; it doesn’t introduce a new section.


### Examples:

```html
<header>
  <p>Welcome to...</p>
  <h1>Voidwars!</h1>
</header>

```

In this example, the [`<article>`](http://stackoverflow.com/documentation/html/311/sectioning-elements/1092/article-element#t=201609212322171848834) has a `<header>`.

```html
<article>
  <header>
    <h1>Flexbox: The definitive guide</h1>
  </header>
  <p>The guide about Flexbox was supposed to be here, but it turned out Wes wasn’t a Flexbox expert either.</p>
</article>

```

[W3C Proposed Recommendation](https://www.w3.org/TR/html51/sections.html#the-header-element)



## Footer Element


The `<footer>` element contains the footer part of the page.

Here is an example for `<footer>` element that contain `p` paragraph tag.

```html
<footer>
    <p>All rights reserved</p>
</footer>

```



#### Remarks


The HTML5 standards does not list the main element as a sectioning element.

