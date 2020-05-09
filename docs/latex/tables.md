---
metaTitle: "LaTex - Tables"
description: "The tabular environment, Coloring Table"
---

# Tables



## The tabular environment


The `tabular` environment is the most basic way to create a table in LaTeX and doesn't require any other packages.

```latex
\begin{tabular}{|lcr||}
  left aligned column & center column & right column \\
  \hline
  text & text & text \\
  text & text & text \\
\end{tabular}

```

[<img src="https://i.stack.imgur.com/BNwOp.png" alt="Rendered result" />](https://i.stack.imgur.com/BNwOp.png)

The parameter (`|lcr||` in the example) is called the **table specification** and tells LaTeX how many columns there are and how they are supposed to be formatted. Each letter represents a single column. Possible values are:

|Character|Meaning
|---|---|---|---|---|---|---|---|---|---
|l|left aligned column
|c|centered column
|r|right aligned column
|p{'width'} e.g. `p{5cm}`|paragraph column with defined width
|| (pipe character)|vertical line
||| (2 pipes)|2 vertical lines

Cells are seperated by the `&` character. A row is ended by 2 back slashes `\\`.

Horizontal lines can be inserted by using the `\hline` command.

Tables are always formatted to be wide enough to include all the content. If a table is to big, LaTeX will print `overfull hbox` warnings. Possible solutions include using the `p{'width'}` specifier or other packages like `tabularx`.

A table with column headings spanning over several columns can be created using the command `\multicolumn{cols}{pos}{text}`.

```latex
\begin{center}
\begin{tabular}{|c|c|c|c|}
\hline
&\multicolumn{3}{|c|}{Income Groups}\\
\cline{2-4}
City&Lower&Middle&Higher\\
\hline
City-1& 11 & 21 & 13\\
City-2& 21 & 31 &41\\
\hline
\end{tabular}
\end{center}

```

[<img src="https://i.stack.imgur.com/EEHSO.jpg" alt="Table with multicolumn headings" />](https://i.stack.imgur.com/EEHSO.jpg)

Note that the command `\multicolumn` has three mandatory arguments: the first argument specifies the number of columns over which the heading spans; the second argument specifies the position of the heading`(l,c,r)`; and the third argument is the text for heading. The command `\cline{2-4}` specifies the the starting column(here, 2) and ending column(here, 4) over which a line is to be drawn.



## Coloring Table


To make the table more readable, following are the ways to color it:

1. Rows
1. Columns
1. Lines
1. Cells

**Coloring Rows**

Use `\rowcolor` (provided by [`colortbl`](http://ctan.org/pkg/colortbl); also loaded by [`xcolor`](http://ctan.org/pkg/xcolor) under the `[table]` package option). Example:

```latex
\documentclass{article}
\usepackage[table]{xcolor}

\begin{document}

\begin{tabular}{ | l | l | l | }
  \rowcolor{green}
  A & B & C \\
  \rowcolor{red}
  D & E & F \\
  G & H & I \\
  \rowcolor{blue}
  J & K & L
\end{tabular}

\end{document}

```

[<img src="https://i.stack.imgur.com/LodcX.png" alt="enter image description here" />](https://i.stack.imgur.com/LodcX.png)

**Coloring Columns**

Columns can be colored using following ways:

<li>
Defining column color property outside the table tag using `\newcolumntype`:

```latex
 \newcolumntype{a}{ >{\columncolor{yellow}} c }

```


</li>
<li>
Defining column color property inside the table parameters

```latex
 \begin{tabular}{ | >{\columncolor{red}} c | l | l }

```


</li>

Example:

```latex
\documentclass{article}
\usepackage[table]{xcolor}

\newcolumntype{a}{>{\columncolor{yellow}}c}
\newcolumntype{b}{>{\columncolor{green}}c}

\begin{document}

\begin{tabular}{ a | >{\columncolor{red}}c | l | b }
  \hline
  A & B & C & D \\
  E & F & G & H \\
  \hline
\end{tabular}

\end{document}

```

[<img src="https://i.stack.imgur.com/761ty.png" alt="enter image description here" />](https://i.stack.imgur.com/761ty.png)

**Coloring Lines**

Use `\arrayrulecolor`. Example:

```latex
\documentclass{article}
\usepackage[table]{xcolor}

\arrayrulecolor{blue}

\begin{document}

\begin{tabular}{ | l | l | l | }
  \hline
  A & B & C \\
  \hline
  D & E & F\\
  \hline
  G & H & I \\
  \hline
\end{tabular}

\end{document}

```

[<img src="https://i.stack.imgur.com/oiQjR.png" alt="enter image description here" />](https://i.stack.imgur.com/oiQjR.png)

**Coloring Cells**

Use `\cellcolor`. Example:

```latex
\documentclass{article}
\usepackage[table]{xcolor}

\begin{document}

\begin{tabular}{ | l | l | l | }
  \hline
  A & B & C \\
  \hline
  D & E & \cellcolor{green}F \\
  \hline
  G & H & I \\
  \hline
\end{tabular}

\end{document}

```

[<img src="https://i.stack.imgur.com/z7zN2.png" alt="enter image description here" />](https://i.stack.imgur.com/z7zN2.png)

We can define our own colors too using package `colortbl`. Following are the tags examples:

```

   \definecolor{Gray}{gray}{0.85}
    \columncolor[RGB]{230, 242, 255}}
    \columncolor[HTML]{AAACED}

```

