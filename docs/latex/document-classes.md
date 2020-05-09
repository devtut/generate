---
metaTitle: "LaTex - Document classes"
description: "Article, Beamer, Defining the document class"
---

# Document classes



## Article


```latex
\documentclass{article}

```

### When to use the article class ?

For articles in scientific journals, presentations, short reports, program documentation, invitations, ... <sup>[1](https://en.wikibooks.org/wiki/LaTeX/Document_Structure#Document_classes)</sup>

### What are the specificities of this class ?

An article doesn't contain chapters or parts. It can be divided in sections, subsections and paragraphs etc.

By default, the title is shown at the top of the first page, and not on a separate title page.

### Simple example

```latex
\documentclass{article}

\title{Hello world}
\author{Me    }
\date{\today}

\begin{document}

\maketitle

Hello, World!
\end{document}

```



## Beamer


```latex
\documentclass{beamer}

```

### When to use the beamer class ?

For presentation slides.

### What are the specificities of this class ?

The output is landscape-oriented. The document is separated in "frames" (slides).

### Simple example

Following example was adapted from : [sharelatex.com/learn/Beamer](https://www.sharelatex.com/learn/Beamer)

```latex
\documentclass{beamer}
 
\usepackage[utf8]{inputenc}
 
\title{Sample title}
\author{Me}
\date{\today}
 
\begin{document}
     
\frame{\titlepage}
 
\begin{frame}
\frametitle{Sample frame title}
This is a text in first frame. This is a text in first frame. This is a text in first frame.
\end{frame}
 
\end{document}

```



## Defining the document class


The very first line in each of your LaTeX  programs should do this. It should follow the form `\documentclass{...}`. What you put within the curly braces is very important. Some document classes give you extra commands to use, others use a different format, and all have specific parameters you can input (described in the parameters section).



#### Syntax


- \documentclass{...}



#### Remarks


This topic aims to explain the different types of document and their specificities.

A good way to organize it would be 1 example per type

