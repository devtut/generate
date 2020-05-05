---
metaTitle: "RMarkdown and knitr presentation"
description: "Adding a footer to an ioslides presentation, Rstudio example"
---

# RMarkdown and knitr presentation



## Adding a footer to an ioslides presentation


Adding a footer is not natively possible. Luckily, we can make use of jQuery and CSS to add a footer to the slides of an ioslides presentation rendered with knitr.
First of all we have to include the jQuery plugin. This is done by the line

```r
<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.2/jquery.min.js"></script>

```

Now we can use jQuery to alter the DOM (**document object model**) of our presentation. In other words: we alter the HTML structure of the document.
As soon as the presentation is loaded (`$(document).ready(function() { ... })`), we select all slides, that do not have the class attributes `.title-slide`, `.backdrop`, or `.segue` and add the tag `<footer></footer>` right before each slide is 'closed' (so before `</slide>`). The attribute `label` carries the content that will be displayed later on.

All we have to do now is to layout our footer with CSS:

After each `<footer>` (`footer::after`):

- display the content of the attribute `label`
- use font size 12
- position the footer (20 pixels from the bottom of the slide and 60 pxs from the left)

(the other properties can be ignored but might have to be modified if the presentation uses a different style template).

```r
---
title: "Adding a footer to presentaion slides"
author: "Martin Schmelzer"
date: "26 Juli 2016"
output: ioslides_presentation
---

```r{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```r

<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.2/jquery.min.js"></script>

<script>
    $(document).ready(function() {
      $('slide:not(.title-slide, .backdrop, .segue)').append('<footer label=\"My amazing footer!\"></footer>');    
    })
</script>

<style>
  footer:after {
    content: attr(label);
    font-size: 12pt;
    position: absolute;
    bottom: 20px;
    left: 60px;
    line-height: 1.9;
  }
</style>


## Slide 1

This is slide 1.

## Slide 2

This is slide 2

# Test

## Slide 3

And slide 3.

```

The result will look like this:

[<img src="http://i.stack.imgur.com/ioNUE.png" alt="enter image description here" />](http://i.stack.imgur.com/ioNUE.png)



## Rstudio example


This is a script saved as .Rmd, on the contrary of r scripts saved as .R.

To knit the script, either use the `render` function or use the shortcut button in Rstudio.

```r
--- 
title: "Rstudio exemple of a rmd file"
author: 'stack user'
date: "22 July 2016"
output: html_document
---

The header is used to define the general parameters and the metadata.

## R Markdown

This is an R Markdown document.
It is a script written in markdown with the possibility to insert chunk of R code in it.
To insert R code, it needs to be encapsulated into inverted quote.

Like that for a long piece of code:

```r{r cars}
summary(cars)
```r

And like ``r cat("that")`` for small piece of code.

## Including Plots

You can also embed plots, for example:

```r{r echo=FALSE}
plot(pressure)
```r

```



#### Syntax


<li>Header:
<ul>
- YAML format, used when the script is compile to define general parameter and metadata



#### Parameters


|Parameter|definition
|---|---|---|---
|title|the title of the document
|author|The author of the document
|date|The date of the document: Can be "`r format(Sys.time(), '%d %B, %Y')`"
|author|The author of the document
|output|The output format of the document: at least 10 format available. For html document, `html_output`. For PDF document, `pdf_document`, ..



#### Remarks


### Sub options parameters:

|sub-option|description|html|pdf|word|odt|rtf|md|github|ioslides|slidy|beamer
|---|---|---|---|---|---|---|---|---|---|---|---|---
|citation_package|The LaTeX package to process citations, natbib, biblatex or none||X||||X||||X
|code_folding|Let readers to toggle the display of R code, "none", "hide", or "show"|X|||||||||
|colortheme|Beamer color theme to use||||||||||X
|css|CSS file to use to style document|X|||||||X|X|
|dev|Graphics device to use for figure output (e.g. "png")|X|X||||X|X|X|X|X
|duration|Add a countdown timer (in minutes) to footer of slides|||||||||X|
|fig_caption|Should figures be rendered with captions?|X|X|X|X||||X|X|X
|fig_height, fig_width|Default figure height and width (in inches) for document|X|X|X|X|X|X|X|X|X|X
|highlight|Syntax highlighting: "tango", "pygments", "kate","zenburn", "textmate"|X|X|X||||||X|X
|includes|File of content to place in document (in_header, before_body, after_body)|X|X||X||X|X|X|X|X
|incremental|Should bullets appear one at a time (on presenter mouse clicks)?||||||||X|X|X
|keep_md|Save a copy of .md file that contains knitr output|X||X|X|X|||X|X|
|keep_tex|Save a copy of .tex file that contains knitr output||X||||||||X
|latex_engine|Engine to render latex, or ""pdflatex", "xelatex", lualatex"||X||||||||X
|lib_dir|Directory of dependency files to use (Bootstrap, MathJax, etc.)|X|||||||X|X|
|mathjax|Set to local or a URL to use a local/URL version of MathJax to render|X|||||||X|X|
|md_extensions|Markdown extensions to add to default definition or R Markdown|X|X|X|X|X|X|X|X|X|X
|number_sections|Add section numbering to headers|X|X||||||||
||||||||||||
|pandoc_args|Additional arguments to pass to Pandoc|X|X|X|X|X|X|X|X|X|X
|preserve_yaml|Preserve YAML front matter in final document?||||||X||||
|reference_docx|docx file whose styles should be copied when producing docx output|||X|||||||
|self_contained|Embed dependencies into the doc|X|||||||X|X|
|slide_level|The lowest heading level that defines individual slides||||||||||X
|smaller|Use the smaller font size in the presentation?||||||||X||
|smart|Convert straight quotes to curly, dashes to em-dashes, ... to ellipses, etc.|X|||||||X|X|
|template|Pandoc template to use when rendering file|X|X||X|||||X|X
||||||||||||
|theme|Bootswatch or Beamer theme to use for page|X|||||||||X
|toc|Add a table of contents at start of document|X|X|X||X|X|X|||X
|toc_depth|The lowest level of headings to add to table of contents|X|X|X||X|X|X|||
|toc_float|Float the table of contents to the left of the main content|X|||||||||

