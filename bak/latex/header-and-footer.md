---
metaTitle: "LaTex - Header and Footer"
description: "Using fancyhdr and titleps packages, Page number as CurrPage/TotalPages in footer"
---

# Header and Footer




## Using fancyhdr and titleps packages


```latex
\documentclass[12pt]{article}

\usepackage{titleps}
\usepackage{fancyhdr}
\usepackage{graphicx}
\usepackage{lipsum} % for dummy text

\pagestyle{myheadings}
\pagestyle{fancy}
\fancyhf{}

\setlength{\headheight}{30pt}

\renewcommand{\headrulewidth}{4pt}
\renewcommand{\footrulewidth}{2pt}

\fancyhead[L]{\includegraphics[width=1cm]{example-image-a}}
\fancyhead[C]{}
\fancyhead[R]{\rightmark}
\fancyfoot[L]{ABC}
\fancyfoot[C]{\textcopyright xyz}
\fancyfoot[R]{\thepage}



\begin{document}

\section{First section}
\subsection{One}
 \lipsum[1-3]
\subsection{Two} 
 \lipsum[4-6]
 
\end{document}

```

[<img src="https://i.stack.imgur.com/g8Td4.png" alt="enter image description here" />](https://i.stack.imgur.com/g8Td4.png)

[<img src="https://i.stack.imgur.com/Ey216.png" alt="enter image description here" />](https://i.stack.imgur.com/Ey216.png)



## Page number as CurrPage/TotalPages in footer


```latex
\documentclass[12pt]{article}

\usepackage{lastpage}
\usepackage{fancyhdr}
\usepackage{graphicx}
\usepackage{lipsum} % for dummy text

\pagestyle{myheadings}
\pagestyle{fancy}
\fancyhf{}

\setlength{\headheight}{30pt}

\renewcommand{\headrulewidth}{1pt}
\renewcommand{\footrulewidth}{2pt}

\lhead{\includegraphics[width=1cm]{example-image-a}}
\rhead{}

\lfoot{ABC}
\rfoot{\thepage/\pageref{LastPage}}

\begin{document}

\section{First section}
\subsection{One}
 \lipsum[1-3]
 
\end{document}

```

[<img src="https://i.stack.imgur.com/437E0.png" alt="enter image description here" />](https://i.stack.imgur.com/437E0.png)

