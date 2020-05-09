---
metaTitle: "LaTex - Presentation with beamer package"
description: "Simple one author title slide, Multiple author and affiliation title slide"
---

# Presentation with beamer package




## Simple one author title slide


`\documentclass{beamer}`

`\mode<presentation>`

`\usetheme{AnnArbor}`

`\usecolortheme{seahorse}`

`\title[Short topic]{Awesome long topic}`

`\author[Name]{Full name}`

`\institute[Institute short form]{Full name of institute}`

`\date{\today}`

`\begin{document}`

`\maketitle`

`\end{document}`

[<img src="https://i.stack.imgur.com/uYKhO.png" alt="enter image description here" />](https://i.stack.imgur.com/uYKhO.png)



## Multiple author and affiliation title slide


`\documentclass[compress]{beamer}`

`\mode\<presentation>`

`\title[]{ABCDE for analysis of PQRS systems}`

`\author[] {`

```

     AA AAAA \inst{1}
         
      \and BB BBBB \inst{1}

      \and CC CCCC \inst{1}

      \and DD DDDD \inst{1} 

      \and EE EEEE\inst{2}

      \and FF FFFF\inst{3}

      \and GG GGGG \inst{3}}

```

`\institute[]`

```

 {

   \inst{1}%
   Department of UV, Univ. of XYZ

   \and

   \inst{2}%
   Department of MN, Univ. of XYZ 

   \and

   \inst{3}
   Advanced Centre for PQR

  }

```

`\date[]{\today}`

`\begin{document}`

`\begin{frame}`

`\titlepage`

`\end{frame}`

`\end{document}`

[<img src="https://i.stack.imgur.com/EkbE5.png" alt="enter image description here" />](https://i.stack.imgur.com/EkbE5.png)



#### Parameters


|theme|AnnArbor
|---|---|---|---|---|---|---|---|---|---
|color theme|seahoarse



#### Remarks


For other themes and colors you can visit
[here](http://deic.uab.es/%7Eiblanes/beamer_gallery/index_by_theme_and_color.html)

