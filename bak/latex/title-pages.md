---
metaTitle: "LaTex - Title Pages"
description: "Standard report titlepage"
---

# Title Pages



## Standard report titlepage


```latex
\documentclass{report}

\begin{document}

\title{I want to be a Wombat}
\author{Carl Capybara}
\maketitle

\end{document}

```

This will create a title page with no other content:

[<img src="http://i.stack.imgur.com/7hogM.png" alt="Rendered result" />](http://i.stack.imgur.com/7hogM.png)



#### Remarks


`\title{<title>}`, `\author{<author>}` and `\date{<date}` internally store the content.

`\maketitle` produces a standard title page with the previously defined values.

