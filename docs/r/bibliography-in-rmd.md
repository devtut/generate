---
metaTitle: "R - Bibliography in RMD"
description: "Specifying a bibliography and cite authors, Inline references, Citation styles"
---

# Bibliography in RMD



## Specifying a bibliography and cite authors


The most important part of your RMD file is the YAML header. For writing an academic paper, I suggest to use PDF output, numbered sections and a table of content (toc).

```r
---
title: "Writing an academic paper in R"
author: "Author"
date: "Date"
output:
  pdf_document:
    number_sections: yes
toc: yes
bibliography: bibliography.bib
---

```

In this example, our file `bibliography.bib` looks like this:

```r
@ARTICLE{Meyer2000,  
  AUTHOR="Bernd Meyer",  
  TITLE="A constraint-based framework for diagrammatic reasoning",  
  JOURNAL="Applied Artificial Intelligence",  
  VOLUME= "14",  
  ISSUE = "4",  
  PAGES= "327--344",  
  YEAR=2000  
}

```

To cite an author mentioned in your .bib file write `@` and the bibkey, e.g. `Meyer2000`.

```r
# Introduction

`@Meyer2000` results in @Meyer2000.

`@Meyer2000 [p. 328]` results in @Meyer2000 [p. 328]

`[@Meyer2000]` results in [@Meyer2000]

`[-@Meyer2000]` results in [-@Meyer2000]

# Summary

# References

```

Rendering the RMD file via RStudio (Ctrl+Shift+K) or via console `rmarkdown::render("<path-to-your-RMD-file">)` results in the following output:

[<img src="https://i.stack.imgur.com/aldv1.jpg" alt="enter image description here" />](https://i.stack.imgur.com/aldv1.jpg)



## Inline references


If you have no *.bib file, you can use a references field in the document’s YAML metadata. This should include an array of YAML-encoded references, for example:

```r
---
title: "Writing an academic paper in R"
author: "Author"
date: "Date"
output:
  pdf_document:
    number_sections: yes
toc: yes
references:
  - id: Meyer2000
    title: A Constraint-Based Framework for Diagrammatic Reasoning
    author:
    - family: Meyer
      given: Bernd
    volume: 14
    issue: 4
    publisher: Applied Artificial Intelligence
    page: 327-344
    type: article-journal
    issued:
      year: 2000
---

# Introduction

`@Meyer2000` results in @Meyer2000.

`@Meyer2000 [p. 328]` results in @Meyer2000 [p. 328]

`[@Meyer2000]` results in [@Meyer2000]

`[-@Meyer2000]` results in [-@Meyer2000]

# Summary

# References

```

Rendering this file results in the same output as in example "Specifying a bibliography".



## Citation styles


By default, `pandoc` will use a Chicago author-date format for citations and references. To use another style, you will need to specify a CSL 1.0 style file in the csl metadata field. In the following a often used citation style, the elsevier style, is presented (download at [https://github.com/citation-style-language/styles](https://github.com/citation-style-language/styles) ). The style-file has to be stored in the same directory as the RMD file OR the absolute path to the file has to be submitted.

To use another style then the default one, the following code is used:

```r
---
title: "Writing an academic paper in R"
author: "Author"
date: "Date"
output:
  pdf_document:
    number_sections: yes
toc: yes
bibliography: bibliography.bib
csl: elsevier-harvard.csl
---

# Introduction

`@Meyer2000` results in @Meyer2000.

`@Meyer2000 [p. 328]` results in @Meyer2000 [p. 328]

`[@Meyer2000]` results in [@Meyer2000]

`[-@Meyer2000]` results in [-@Meyer2000]

# Summary

# Reference

```

[<img src="https://i.stack.imgur.com/pBnr2.jpg" alt="enter image description here" />](https://i.stack.imgur.com/pBnr2.jpg)

Notice the differences to the output of example "Specifying a bibliography and cite authors"



#### Parameters


|Parameter in YAML header|Detail
|---|---|---
|`toc`|table of contents
|`number_sections`|numbering the sections automatically
|`bibliography`|path to the bibliography file
|`csl`|path to the style file



#### Remarks


<li>
The purpose of this documentation is integrating an academic bibliography in a RMD file.
</li>
<li>
To use the documentation given above, you have to install `rmarkdown` in R via `install.packages("rmarkdown")`.
</li>
<li>
<p>Sometimes Rmarkdown removes the hyperlinks of the citations. The solution for this is adding the following code to your YAML header:
`link-citations: true`</p>
</li>
<li>
The bibliography may have any of these formats:
</li>

|Format|File extension
|---|---|---
|MODS|.mods
|BibLaTeX|.bib
|BibTeX|.bibtex
|RIS|.ris
|EndNote|.enl
|EndNote XML|.xml
|ISI|.wos
|MEDLINE|.medline
|Copac|.copac
|JSON citeproc|.json

