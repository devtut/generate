---
metaTitle: "LaTex - Creating posters using beamer"
description: "Orientation and size, Basic outline of a beamer poster, Full example of beamer poster"
---

# Creating posters using beamer


Creating a poster using beamerposter package is very similar to creating a single frame. Put the content in columns. Within each column, separate the content using blocks.



## Orientation and size


While adding the beamerposter package, provide the required parameters.

`\usepackage[orientation=landscape,size=a1]{beamerposter}`

You can also customize the size of the poster.

`\usepackage[orientation=portrait,size=custom,height=110,width=80,scale=1.4]{beamerposter}`

The height and width dimensions here, are in cms. The `scale` is used for the font size.



## Basic outline of a beamer poster


In landscape orientation

```latex
\documentclass[final,t]{beamer}
\mode<presentation>
  {
  \usetheme{Berlin}
  }

\usepackage[orientation=landscape,size=a1,scale=1,debug]{beamerposter}
\usepackage{lipsum} % for dummy text


\title[]{\huge Awesome title}
\author[]{\large \textbf{Author Name1} \and Author Name2 \and Author Name3}
\institute[]{\Large Dept of XYZ, ABC Institute}
\date{}

\begin{document}

\begin{frame}
\maketitle
\begin{columns}[t]
  \begin{column}{.32\linewidth}
 
  \begin{block}{Some heading}
  \lipsum[1]
  \end{block}     

  \begin{block}{Some heading}
  \lipsum[1]
  \end{block}
  
  \begin{block}{Some heading}
  \lipsum[1]
  \end{block}
  
  
  \end{column}

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  \begin{column}{.32\linewidth}
 
  \begin{block}{Some heading}
  \lipsum[1]
  \end{block}     

  \begin{block}{Some heading}
  \lipsum[1]
  \end{block}
  
  \begin{block}{Some heading}
  \lipsum[1]
  \end{block}
  
  
  \end{column}

  
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  \begin{column}{.32\linewidth}
 
  \begin{block}{Some heading}
  \lipsum[1]
  \end{block}     

  \begin{block}{Some heading}
  \lipsum[1]
  \end{block}
  
  \begin{block}{Some heading}
  \lipsum[1]
  \end{block}
  
  
  \end{column}
  \end{columns}
    
\end{frame}

\end{document}

```

[<img src="https://i.stack.imgur.com/TerHq.png" alt="enter image description here" />](https://i.stack.imgur.com/TerHq.png)

In portrait orientation

```latex
\documentclass[final,t]{beamer}
\mode<presentation>
  {
  \usetheme{Berlin}
  }

\usepackage[orientation=portrait,size=a1,scale=1,debug]{beamerposter}
\usepackage{lipsum} % for dummy text


\title[]{\huge Awesome title}
\author[]{\large \textbf{Author Name1} \and Author Name2 \and Author Name3}
\institute[]{\Large Dept of XYZ, ABC Institute}
\date{}

\begin{document}

\begin{frame}
\maketitle
\begin{columns}[t]
  \begin{column}{.45\linewidth}
 
  \begin{block}{Some heading}
  \lipsum[1]
  \end{block}     

  \begin{block}{Some heading}
  \lipsum[1]
  \end{block}
  
  \begin{block}{Some heading}
  \lipsum[1]
  \end{block}
  
  \begin{block}{Some heading}
  \lipsum[1]
  \end{block}
  
  \end{column}

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  \begin{column}{.45\linewidth}
 
  \begin{block}{Some heading}
  \lipsum[1]
  \end{block}     

  \begin{block}{Some heading}
  \lipsum[1]
  \end{block}
  
  \begin{block}{Some heading}
  \lipsum[1]
  \end{block}
  
  \begin{block}{Some heading}
  \lipsum[1]
  \end{block}
  
  \end{column}
  \end{columns}
    
\end{frame}

\end{document}

```

[<img src="https://i.stack.imgur.com/Ookzl.png" alt="enter image description here" />](https://i.stack.imgur.com/Ookzl.png)



## Full example of beamer poster


```latex
\documentclass[final,t]{beamer}
\mode<presentation>
  {
  \usetheme{Berlin}
  }

\usepackage[orientation=landscape,size=a1,scale=1,debug]{beamerposter}
\usepackage{lipsum} % for dummy text
\usepackage{graphicx} % for dummy image
\usepackage{tikz} % for tikzpicture
\usepackage{pgfplots} % for plot
\usetikzlibrary{arrows,shapes,positioning}


\title[]{\huge Awesome title}
\author[]{\large \textbf{Author Name1} \and Author Name2 \and Author Name3}
\institute[]{\Large Dept of XYZ, ABC Institute}
\date{}

\begin{document}

\begin{frame}
\maketitle
\begin{columns}[t]

\begin{column}{.32\linewidth}
 
 
  \begin{block}{First paragraph}
  \lipsum[1]
  \end{block}
  
  \begin{block}{First figure}
  Some text describing figure
  \begin{center}
   \begin{figure}
    \includegraphics[scale=0.7]{example-image-a}
    \caption{First figure caption}
   \end{figure}

  \end{center}

  
  \end{block}
  
    
  \begin{block}{First list}
  \begin{itemize}
   \item Nam dui ligula, fringilla a, euismod sodales, sollicitudin vel, wisi.
   \item Morbi auctor lorem non justo.
   \item Nam lacus libero, pretium at, lobortis vitae, ultricies et, tellus.
   \item Donec aliquet, tortor sed accumsan bibendum, erat ligula aliquet magna, vitae ornare odio metus a mi.
   \item Morbi ac orci et nisl hendrerit mollis.
   \item Suspendisse ut massa.
   \item Cras nec ante.
   \item Pellentesque a nulla.
   \item Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus.
   \item Aliquam tincidunt urna.
   \item Nulla ullamcorper vestibulum turpis.
   \item Pellentesque cursus luctus mauris.
  \end{itemize}

  \end{block}
  
  
   \end{column}
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  \begin{column}{.32\linewidth}
 
  \begin{block}{Second list}
  \begin{enumerate}
   \item Nulla malesuada porttitor diam.
   \item Donec felis erat, congue non, volutpat at, tincidunt tristique, libero.
   \item Vivamus viverra fermentum felis.
   \item Donec nonummy pellentesque ante.
   \item Phasellus adipiscing semper elit.
   \item Proin fermentum massa ac quam.
   \item Sed diam turpis, molestie vitae, placerat a, molestie nec, leo.
   \item Maecenas lacinia.
   \item Nam ipsum ligula, eleifend at, accumsan nec, suscipit a, ipsum.
  \end{enumerate}

  \end{block}
  
  \begin{block}{First split block}
    \begin{columns}
      \begin{column}{0.5\linewidth}
      
      \begin{center}
      \begin{figure}
       \includegraphics[width=0.55\linewidth]{example-image-b}
       \caption{Second figure caption}
      \end{figure}
    \end{center}
    \begin{center}
      \begin{figure}
       \includegraphics[width=0.55\linewidth]{example-image-c}
       \caption{Third figure caption}
      \end{figure}
    \end{center}
    
   \end{column}
   
   \begin{column}{0.5\linewidth}
   
   Morbi blandit ligula feugiat magna.
   \begin{itemize}
   \item Nunc eleifend consequat lorem.
   \item Sed lacinia nulla vitae enim.
   \item Pellentesque tincidunt purus vel magna.
   \item Integer non enim.
   \item Praesent euismod nunc eu purus.
   \item Donec bibendum quam in tellus.
   \item Nullam cursus pulvinar lectus.
   \item Donec et mi.
   \item Nam vulputate metus eu enim.
   \item Vestibulum pellentesque felis eu massa.
   \end{itemize}

  \end{column}
  \end{columns}
  
  \end{block}
  

  \begin{block}{First tikz picture}
  Morbi blandit ligula feugiat magna. Nunc eleifend consequat lorem. Sed lacinia nulla vitae enim. Pellentesque
tincidunt purus vel magna.

  \begin{center}
  \begin{figure}
    
  \begin{tikzpicture}
 
      % Definitions
      \pgfmathsetmacro{\b}{75}
      \pgfmathsetmacro{\a}{15}
      \pgfmathsetmacro{\R}{2}
      \pgfmathsetmacro{\r}{1}
      \pgfmathsetmacro{\P}{\R*tan(\b)}
      \pgfmathsetmacro{\Q}{\R/cos(\b)}
      \pgfmathsetmacro{\p}{\r/tan(\a)}
      \pgfmathsetmacro{\q}{\r/sin(\a)}

      % Pulleys
      
      % big pulley
      \draw (0,0) circle (\R) ;
      \fill[left color=gray!80, right color=gray!60, middle
        color=white] (0,0) circle (\R) ;
      \draw[thick, white] (0,0) circle (.8*\R);
      \shade[ball color=white] (0,0) circle (.3) node[left,xshift=-5] {$P$};

      % small pulley
      \draw (\Q+\q-.3, 0) circle (\r);
      \fill[left color=gray!80, right color=gray!60, middle
        color=white] (\Q+\q-.3, 0) circle (\r) ;
      \draw[thick, white] (\Q+\q-.3,0) circle (.8*\r);
      \shade[ball color=white] (\Q+\q-.3,0) circle (.15) 
      node[right, xshift=2] {$Q$};

      % belt and point labels
      \begin{scope}[ultra thick]
        \draw (\b:\R) arc (\b:360-\b:\R) ;
        \draw (\b:\R) -- ( \P, 0 ); 
        \draw (-\b:\R) -- ( \P, 0 );
        \draw (\Q-.3,0) -- + (\a:\p)  arc (105:-105:\r) ;
        \draw (\Q-.3,0) -- + (-\a:\p);
        %\draw (\b:\R) arc (\b:360-\b:\r) ;
      \end{scope}
   
      \draw (0,0) -- (\b:\R) node[midway, above,sloped] {$R$} node[above] {$A$};
      \draw (-\b:\R)--(0,0) ;
      \draw (\Q+\q-.3,0) -- +(105:\r) node[midway,above, sloped] {$r$}
        node[above] {$E$};
      \draw (\Q+\q-.3,0) -- +(-105:\r) node[below] {$D$};
      \node[below] at (-\b:\R) {$B$};
      \node[below] at (\Q-.3,0) {$C$};

      % center line
      \draw[dash pattern=on5pt off3pt] (0,0) -- (\Q+\q-.3,0);

      % angle label
      \node[fill=white] at (0.73*\Q, 0) {$\theta$} ;
      \draw (\Q-1.8,0) arc (180:195:1.5);
      \draw (\Q-1.8,0) arc (180:165:1.5);

  \end{tikzpicture}
  \caption{First tikz picture caption}
  \end{figure}
  \end{center}
 
 Donec bibendum quam in tellus. Nullam cursus pulvinar lectus. Donec et mi. Nam vulputate metus eu enim. Vestibulum pellentesque felis eu massa.
  \end{block}
  
  \end{column}

  
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  \begin{column}{.32\linewidth}
 
  \begin{block}{Second paragraph}
  Fusce mauris. Vestibulum luctus nibh at lectus. Sed bibendum, nulla a faucibus semper, leo velit ultricies tellus, ac venenatis arcu wisi vel nisl. Vestibulum diam. Aliquam pellentesque, augue quis sagittis posuere, turpis lacus congue quam, in hendrerit risus eros eget felis. Maecenas eget erat in sapien mattis porttitor. Vestibulum porttitor. Nulla facilisi. Sed a turpis eu lacus commodo facilisis. Morbi fringilla, wisi in dignissim interdum, justo lectus sagittis dui, et vehicula libero dui cursus dui.
  \end{block}     
     
  \begin{block}{First table}
  \begin{center}
  \begin{tabular}{lrrrrrr}
  \hline
  & AAA & BBB & CCC & DDD & EEE & FFF\\ \hline
  XXX & 1 & 2 & 3 & 4 & 5 & 6 \\
  YYY & 1 & 2 & 3 & 4 & 5 & 6 \\
  ZZZ & 1 & 2 & 3 & 4 & 5 & 6 \\
  
  \hline
  \end{tabular}

  \end{center}
  \end{block}
  
  \begin{block}{First plot}
  \begin{center}
  \begin{figure}

    \begin{tikzpicture}
    \begin{axis}[
    width=0.7\linewidth,
    max space between ticks=50,
    minor x tick num=2,
    minor y tick num=1,
    tick style={semithick,color=black},
    xlabel=Value,
    ylabel=Time (sec),
    xtick={0, 50, 100, 150},
    ytick={0, 2, 4, 6, 8}]
    
    \addplot[smooth, blue, mark=*] coordinates { (1,1.48) (2,1.48) (4,1.48) (8,1.48) (16,1.49) (32,1.49) (64,1.49) (128,1.85) (136,5.87) (138,6.84) (139,7.46)};
    \end{axis}
    \end{tikzpicture}

      
  \caption{First plot caption}
  \end{figure}
  \end{center}
  \end{block}
  
  \begin{block}{Third paragraph}
   Mauris tempor ligula sed lacus. Duis cursus enim ut augue. Cras ac magna. Cras nulla. Nulla egestas. Curabitur a leo. Quisque egestas wisi eget nunc.
  \end{block}

  \begin{block}{Third list}
   \begin{enumerate}
    \item Nam feugiat lacus vel est.
    \item Curabitur consectetuer.
   \end{enumerate}

  \end{block}


  \end{column}
  \end{columns}
    
\end{frame}

\end{document}

```

[<img src="https://i.stack.imgur.com/XDJs7.png" alt="enter image description here" />](https://i.stack.imgur.com/XDJs7.png)

