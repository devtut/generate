---
metaTitle: "LaTex - Text Formatting"
description: "Emphazise Text, Strike through text, Bold text"
---

# Text Formatting



## Emphazise Text


In order to emphasize text the command `\emph` can be used which usually displays the text in an italics font:

```latex
This is some text with \emph{emphasized words}.

```



## Strike through text


The command `\sout` of the package `ulem` strikes through a text:

```latex
\sout{This text is striked through}

```

The package `ulem` redefines the command `\emph`. When you do not want to have this behavior you can use the package `ulem` with the option `normalem`:

```latex
\usepackage[normalem]{ulem}

```



## Bold text


In order to typeset text in bold, use `\textbf`:

```latex
\textbf{This text is typeset in bold.}

```

