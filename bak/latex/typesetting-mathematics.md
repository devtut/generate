---
metaTitle: "LaTex - Typesetting mathematics"
description: "Basic Equations, Finding Symbols, Packages available for use, Good Commands to Know, Creating New Symbols, Matrices"
---

# Typesetting mathematics




## Basic Equations


**Simple, Inline Equations**

You can do a simple inline equation by using `$an equation here$`.

For example, you might do

```latex
$\lim\limits_{n \to \infty} \frac{1}{2^n} i\bar z$ 

```

which, if we put a little fake text around it, gives

[<img src="http://i.stack.imgur.com/LKDlH.png" alt="enter image description here" />](http://i.stack.imgur.com/LKDlH.png)

**Numbered, Centered Equations**

When writing papers or other documents, it is sometimes preferable to have your equations centered and numbered, as opposed to in-line. Then, use the `\begin{equation}` and `\end{equation}` commands.

For example, if we use the code

```latex
\begin{equation}
\lim\limits_{n \to \infty} \frac{1}{2^n} i\bar z
\end{equation}

```

And add a little text around it, we get

[<img src="http://i.stack.imgur.com/puEgK.png" alt="enter image description here" />](http://i.stack.imgur.com/puEgK.png)

You can remove the numbering of the equation by using `\begin{equation*}` and `\end{equation*}`.

For example, if we use the code

```latex
\begin{equation*}
\lim\limits_{n \to \infty} \frac{1}{2^n} i\bar z
\end{equation*}

```

and add a little text around it, we get

[<img src="http://i.stack.imgur.com/rwFqS.png" alt="enter image description here" />](http://i.stack.imgur.com/rwFqS.png)

(though it should be noted you have to use the `amsmath` package for this).



## Finding Symbols


Sometimes, it can be difficult to find the mathematical symbol you need. There are several options here. The first (and quickest) is to use [Detexify](http://detexify.kirelabs.org/classify.html), where you draw the symbol you'd like, and it tries to find what you want, like as shown below:

[<img src="http://i.stack.imgur.com/2NSao.png" alt="enter image description here" />](http://i.stack.imgur.com/2NSao.png)

Another option is to use the comprehensive LaTeX symbols list, which can be found [here](http://mirrors.ibiblio.org/CTAN/info/symbols/comprehensive/symbols-a4.pdf). If you are using the package `unicode-math` [this list](http://ctan.math.washington.edu/tex-archive/macros/latex/contrib/unicode-math/unimath-symbols.pdf) of all supported symbols can be helpful. Another option is [this website](http://web.ift.uib.no/Teori/KURS/WRK/TeX/symALL.html), which has common math symbols.



## Packages available for use


While standard LaTeX is all that is needed for most simple mathematical formulae and equations, sometimes more symbols and tools are needed. There are multiple packages available that will enhance your equations and provide you with more to work with. Three of the main packages are described below. Remember, to load a package, type `\usepackage{package}` in your document preamble.

**`amsmath`**

The `amsmath` package is an incredibly useful package. It is used to allow your equations to be centered but not numbered, as in `\begin{equation*}`, it is used to create matrices (as described below) and it introduces many other useful commands, such as `\overset` and `\underset`, described below. The `amsmath` package documentation can be found [here](ftp://ftp.ams.org/pub/tex/doc/amsmath/amsldoc.pdf).

**`mathtools`**

The `mathtools` package builds off of the `amsmath` package, adding further useful symbols and tools. It automatically loads the `amsmath` package, so you do not need to load both in your document preamble. The `mathtools` documentation can be found [here](http://texdoc.net/texmf-dist/doc/latex/mathtools/mathtools.pdf).

**`amssymb`**

The `amssymb` package provides many extra symbols that can be very handy for more complex equations. The `amssymb` documentation can be found [here](http://texdoc.net/texmf-dist/doc/fonts/amsfonts/amssymb.pdf).

**Font packages**

There are also various fonts you can use for your equations, as described on [this question](http://tex.stackexchange.com/questions/58098/what-are-all-the-font-styles-i-can-use-in-math-mode/58124#58124) on the TeX stack exchange, for TeX, LaTeX, and friends.

[This paper](ftp://ftp.ams.org/pub/tex/doc/amsmath/short-math-guide.pdf) is a concise explanation of the different features provided by some packages as well as standard LaTeX; it is very helpful.



## Good Commands to Know


Some of the most common commands include:

- **Fractions and Square Roots:** For fractions, use `\frac {numerator}{denominator}`. For square roots, use `\sqrt[root]{number}`.
- **Greek letters:** use the commands given in the table below:

[<img src="http://i.stack.imgur.com/vKdIS.png" alt="enter image description here" />](http://i.stack.imgur.com/vKdIS.png)

- **Operators:** `\leq` gives the less than or equal to symbol, `\geq` gives the greater than or equal to symbol, `\neq` gives the not equal symbol, `\sum` gives the summation symbol, `\partial` gives the partial derivative symbol, `\nabla` gives the Laplacian operator, `\times` gives the cross product or multiplication symbol, `\cdot` gives the dot product or multiplication symbol, and `\int` gives the integral symbol.
- **Arrows:** `\rightarrow` and `\leftarrow` give right and left arrows, respectively.
- **Percents:** If typing % in LaTeX, it is important to include a backslash, `\%` as the percent symbol is normally used for comments.
- **Superscripts and Subscripts:** To do a superscript, you can type `x^2`, or, for longer superscripts, `x^{2x}`. To do a subscript, you can type `x_a`, or, for longer subscripts, `x_{ab}`.
- **Bold:** Use `\boldmath{...}` to make your math symbols bold. Other options are given at [this TeX.SX question](http://tex.stackexchange.com/questions/595/how-can-i-get-bold-math-symbols). Math symbols are automatically italicized; if you don't want this to be true, make your equation text as described below.
- **Infinity:** To write infinity, use the command `\infty`.
- **Moving items over or under another:** First, for math operators only, there is an alternate method. You can type the math operator, say `\int`, and then use the `\limits` command. An example is `\int\limits_{\infty}` or `\int\limits^{\infty}`. Then, for normal cases, you can do `\overset{top}{normal}` or `\underset{bottom}{normal}`. This can be very useful for doing vectors. For example, you might do `\overset{\rightarrow}{x}` The `amsmath` package is need for `overset` and `underset`.
- **Curly Braces:** Because curly braces are used in commands, it is necessary to type `\{` or `\}` to get curly braces.
- **Text:** To include text in equations, type `\usepackage{amsmath}` in the preamble, and then type `\text{...}`.
- **Space:** To add space in your equations, type `\quad` between the two items you want to separate (for example, you might have `$2x \quad cos`).



## Creating New Symbols


Let's say you cannot find the symbol you need anywhere. You can create a custom symbol. For example, the code

```latex
\documentclass{article}
\usepackage{graphicx,amsmath,amssymb}
\DeclareRobustCommand{\diamondtimes}{%
  \mathbin{\text{\rotatebox[origin=c]{45}{$\boxplus$}}}%
}

\begin{document}
$a\diamondtimes b$
\end{document}

```

creates and calls a symbol, giving

[<img src="http://i.stack.imgur.com/8W5Fr.png" alt="enter image description here" />](http://i.stack.imgur.com/8W5Fr.png)

This is a simpler example; it merely has to rotate an already existent symbol. However, you can create more complex symbols.

This section is in the process of being expanded.



## Matrices


**Matrices**

You must always use the `amsmath` package if you are going to use the following commands. There are four main types of matrix, as shown in the code below:

```latex
\begin{matrix} 
    a & b \\
    c & d 
\end{matrix}
\quad
\begin{pmatrix} 
   a & b \\
   c & d 
\end{pmatrix}
\quad
\begin{bmatrix} 
    a & b \\
    c & d 
\end{bmatrix}
\quad
\begin{vmatrix} 
    a & b \\
    c & d 
\end{vmatrix}
\quad
\begin{Vmatrix} 
    a & b \\
    c & d 
\end{Vmatrix}

```

This code produces

[<img src="https://i.stack.imgur.com/vQQ2o.png" alt="enter image description here" />](https://i.stack.imgur.com/vQQ2o.png)

There are a couple important things to note about this:

1. It is important you put your matrix within the `equation`, `equation*`, or `$...$` environment - the `bmatrix` command is not a math environment on its own.
1. The construction of the matrix is actually fairly simple. For each row, you create each element (say `x_{11}`), then put a `&`, and then write the next element. For multiple rows, at the end of each row put `\\` (you do not have to do this for the last row). It is fairly similar to a table in this.



#### Syntax


- \begin{equation} ... \end{equation}
- text $ ... $ text
- \usepackage{amsmath} ... \begin{equation*} ... \end{equation*}



#### Remarks


Here are some basic ideas to make sure your code doesn't break on you and your equations look better:

1. Make sure all brackets, curly braces, dollar signs, and `\begin{}` `\end{}` commands are matching. This is something where one small mistake can mess your whole piece of code up in a big way.
1. If you get errors, make sure you have the proper package loaded (for example, don't use the `\begin{equation*}` command without the `amsmath` package).
1. Never, ever, **ever** use double dollar signs (`$$an equation here$$`) instead of `\begin{equation}`.
1. Never use math mode as a way to make your text italic.
1. Completely stuck? Try [TeX.SX](http://tex.stackexchange.com/), a site for answering questions about TeX, LaTeX, and related languages.

Good luck!

