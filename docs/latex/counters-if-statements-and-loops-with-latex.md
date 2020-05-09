---
metaTitle: "LaTex - Counters, if statements and loops with latex"
description: "Operations with counters, Counter declaration, initialization and printing to pdf, If statements, Loops - repeating things, Using loops in Tikz"
---

# Counters, if statements and loops with latex




## Operations with counters


This example shows how to use mathematical operations with counters. It may be useful for loops in latex.

**Addition:** `\addtocounter{num}{n}`

this command adds `n` to `num`, where `num` is a counter and `n` is a positive integer.

**Subtraction:** `\addtocounter{num}{-n}`

this command subtracts `n` from `num`, where `num` is a counter and `n` is a positive integer.

**Multiplication:** `\multiply\value{num} by n`

this command multiply `num` by `n`, where `num` is a counter and `n` is an integer.

**Division** `\divide\value{num} by n`

this command divides `num` by `n` and gets the integer part of the quotient (`num` is a counter and `n` is an integer)

```latex
\documentclass{article}
    \begin{document}
    \newcounter{num}
    \setcounter{num}{3}
    \addtocounter{num}{10}
    \thenum\\%prints 13
    \addtocounter{num}{-3}
    \thenum\\%prints 10
    \stepcounter{num}
    \thenum\\%prints 11
    \multiply\value{num} by \value{num}
    \thenum\\%prints 121
    \multiply\value{num} by 2
    \thenum\\%prints 242
    \divide\value{num} by 60
    \thenum%prints 4
\end{document}

```

`\newcommand{num}` declares counter. `\setcounter{num}{3}` sets num value to 3.

`\addtocounter{num}{10}` adds 10 to num.

`\addtocounter{num}{-3}` subtract 3 from num.

`\stepcounter{num}` adds 1 to num

`\multiply\value{num} by \value{num}` squares num.

`\multiply\value{num} by 2` doubles num.

`\divide\value{num} by 60` divides num by 60 and gets the integer part.

The result of the code: 13\\10\\11\\121\\242\\4

(\\ symbolizes new line)

intcalc package adds some other integer operations e.g. mod, pow, sng, abs, inv ...

[intcalc_package.pdf](http://ctan.mackichan.com/macros/latex/contrib/oberdiek/intcalc.pdf)



## Counter declaration, initialization and printing to pdf


It is possible to use integer variables with latex. To create a new variable we need the `\newcounter{name}` command, where `name` is the name of the new counter. The `name` must contain only letters. This command creates a new one with name `\thename`. With this command we can print `name` variable onto the paper. The initial value of `name` is 0. To give value to "name" we can use `\setcounter{name}{n}` where n is an integer. `\value{name}` is a function which returns with the value of `name`.

```latex
\documentclass{article}
\begin{document}
\newcounter{num}                    %new counter, initial value is 0
\thenum                             %print 0
\setcounter{num}{3}                 %set num to 3
\thenum                             %print 3
\newcounter{number}
\setcounter{number}{\value{num}}    %set number to value of num
\thenumber                          %print 3

Latex provides some other formats to print a number.

Other types of printing:

\arabic{num}\\
\Roman{num}\\ %→ I, II, III, IV, . . . (num = 1, 2, 3, . . . )
\roman{num}\\ %→ i, ii, iii, iv, . . . (num = 1, 2, 3, . . . )
\Alph{num}\\  %→ A, B, C, D, . . . (num = 1, 2, 3, . . . , 26)
\alph{num}\\  %→ a, b, c, d, . . . (num = 1, 2, 3, . . . , 26)
\fnsymbol{num}\\ %→ ∗, †, ‡, §, ¶, k, ∗∗, ††, ‡‡ (num = 1, 2, 3, . . . , 9)
\end{document}

```

[<img src="https://i.stack.imgur.com/sroou.png" alt="enter image description here" />](https://i.stack.imgur.com/sroou.png)



## If statements


In latex we can use built-in commands to execute code whether the conditions are true or not.

**Comparing two integers:** `\ifnum\value{num}>n {A} \else {B}\fi`

This code executes A if num>n else B. We can substitute > with < and =.

**If a number is odd:** `\ifodd\value{num} {A}\else {B}\fi`

If num is odd then it executes A else B.

**If with condition:** `\ifthenelse{condition}{A}{B}`

We have to load ifthen package to use this command. If condition are true then it executes A else B.

It is possible to create complex condition with `\( \)`, `\AND`, `\OR`, `\NOT`.

**For example:** `\ifthenelse{\(\NOT 4<2 \OR 4>11\)\AND\isodd{4}}{A}{B}`

This piece of code writes down "B" on the page. `\NOT 4<2` is true and `4>11` is false. If we connect a false and a true statement with "OR" then the result is true. So `\(\NOT 4<2 \OR 4>11\)` is true. `\isodd{4}` is false because 4 is even. A false and a true statement connected with "AND" is false, so the output is B.

An example code:

```latex
\documentclass{article}
\usepackage{ifthen}
\begin{document}
    \newcounter{num}
    \setcounter{num}{10}

    If num$>$100 then the next sentence will be "Num is large." else "Num is small."

    Num is \ifnum \value{num}>100 {large} \else {small}.

    If num is odd then the next sentence will begin with "Odd" if not then with "Even"

    \ifodd \value{num} {Odd} \else {Even} numbers are cool.

    If (num$>$3 and (1$<$0 or num$=$10)) is true then the next sentence will be "True." else "False."

    \ifthenelse{\value{num}>3\AND\(1<0 \OR \value{num}=10\)}{True.}{False.}


\end{document}

```

[<img src="https://i.stack.imgur.com/3P1FT.png" alt="result" />](https://i.stack.imgur.com/3P1FT.png)



## Loops - repeating things


We can create loops in latex. They are similar but not as customizable as loops in other programming languages. One alternative to use loops are @loops. If we use a command which includes "@" in its name, we must be put it between `\makeatletter` and `\makeatother`. It is not allowed to use them in a macro which describes a new definition.

Wrong:

```latex
\def\is#1#2{\makeatletter\@ifstar{#1}{#2}\makeatother

```

Right:

```latex
\makeatletter\def\is#1#2{\@ifstar{#1}{#2}}\makeatother

```

**@for loop:** `\@for\command:={list}\do{commands}`

**Example**:

```latex
\makeatletter
\@for\sun:={rising,setting}\do{The sun is \sun.}
\makeatother

```

It creates the following text: The sun is rising. The sun is setting.

**@whilenum loop:** `\@whilenum condition\do{commands}`

**Example**:

```latex
\makeatletter
\newcounter{int}
\@whilenum\value{int}<10\do
{\stepcounter{int}\ifthenelse{\isodd{\value{int}}}{\theint}{}}
\makeatother

```

This code writes odd numbers from 1 to 9.

**"loop repeat" loop:** `\loop {commands} \ifnum condition \repeat`

Executes commands till condition is true.

**Example**

```latex
\setcounter{int}{1}
\loop
\theint
\addtocounter{int}{2}
\ifnum \value{int}<10
\repeat

```

This code does the same as @whilenum loop.

An example code:

```latex
\documentclass{article}
\usepackage{ifthen}
\usepackage{amsmath} %\text{} command needs this package
\begin{document}
    Demonstration of @for loop:

    \makeatletter
    \@for\sun:={rising,setting}\do{The sun is \sun. }
    \makeatother

    \newcounter{int}

    @whilenum loop:
    
    \setcounter{int}{0}
    \makeatletter
    \@whilenum\value{int}<20\do
    {\stepcounter{int}\ifthenelse{\isodd{\value{int}}}{\theint\text{ }}{}}
    \makeatother
    
    "loop repeat" loop:
    
    \setcounter{int}{1}
    \loop
    \theint
    \text{ }\addtocounter{int}{2}\ifnum\value{int}<20
    \repeat
\end{document}

```

[<img src="https://i.stack.imgur.com/e7aBf.png" alt="result" />](https://i.stack.imgur.com/e7aBf.png)



## Using loops in Tikz


Loops are useful in Tikz.

The following code draws a clock without numbers:

```latex
\documentclass{article}
\usepackage{ifthen}
\usepackage{intcalc}
\usepackage{tikz}
\newcounter{num}

\begin{document}       
\begin{tikzpicture}
    \makeatletter
    \setcounter{num}{1}
    \newcounter{angle}
    \draw (0,0) circle (3cm);
    \@whilenum\value{num}<13\do{
    \setcounter{angle}{360}
    \multiply\value{angle} by \value{num}
    \divide\value{angle} by 12
    \ifnum \intcalcMod{\value{num}}{3}=0{
    \draw[line width=4pt] (\theangle:2cm) -- (\theangle:3cm);    }\else
    {
    \draw[line width=1pt] (\theangle:2.3cm) -- (\theangle:3cm);
    }\fi
    \addtocounter{num}{1}
    }
    \makeatother
\end{tikzpicture}
\end{document}

```

The result:

[<img src="https://i.stack.imgur.com/4VGfD.png" alt="clock" />](https://i.stack.imgur.com/4VGfD.png)

