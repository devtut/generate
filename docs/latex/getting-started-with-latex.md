---
metaTitle: "LaTex - Getting started with latex"
description: "LaTeX Editors, Installation and Setup"
---

# Getting started with latex



## LaTeX Editors


While you can create LaTeX documents using any editor and compiling using the console, there exist several plugins for widely used editors to simplify creating LaTeX documents, and there are specialized LaTeX editors. An [exhaustive list of LaTeX editors](http://tex.stackexchange.com/questions/339/latex-editors-ides) is available on [TeX.SE](http://tex.stackexchange.com/) (the StackExchange site, dedicated to TeX, LaTeX & Friends).

The most widely used editors, according to this list, are:

- The [Emacs](http://www.gnu.org/software/emacs/emacs.html) editor with the [AUCTeX](http://www.gnu.org/software/auctex/) extension.
- The [Vim](http://www.vim.org/) editor with the [LaTeX-suite](http://vim-latex.sourceforge.net/) plugin.
- [Texmaker](http://www.xm1math.net/texmaker/) – a specialized LaTeX IDE.
- [TeXstudio](http://texstudio.sourceforge.net/) – another LaTeX IDE.
- [TeXworks](https://github.com/TeXworks/texworks/releases) – one more LaTeX IDE.

While experienced users of Emacs or Vim may want to stick to their editor (whose plugins provide a host of functionality unavailable elsewhere), a specialized IDE might be easier to install/use for beginners. The last three on the list have a preview function where one can see the results of the compilation of the document.

Additionally, there are online LaTeX tools that can be of use to beginners or people that must collaborate, e.g. [ShareLaTeX](https://www.sharelatex.com/) and [Overleaf](https://www.overleaf.com/).



## Installation and Setup


You can choose between major distributions of LaTeX:

- [TeX Live](https://www.tug.org/texlive/) (Windows, Linux, and OS X), the standard, cross-platform distribution.
- [MacTeX](http://tug.org/mactex/) (Mac) A packaged version of TeX Live made for OS X with some Mac-specific tools
- [MiKTeX](http://miktex.org/) (Windows) A separate distribution entirely that

All distributions are more or less equivalent in an ideal world. TeX Live has the advantage of being available on all platforms and thus has much better community support. MiKTeX can take advantage of Windows-specific features. For licensing reasons, MiKTeX will also distribute a few packages that TeX Live will not.

In all cases, the full install is recommended.  Specifically, using MiKTeX's download-on-command feature will hang/crash many editors.

### Installation

### Windows (TeXLive)

1. Download the most recent TeXLive `install-tl-windows.exe` from their [website](https://www.tug.org/texlive/).
1. Run `install-tl-windows.exe` and follow the instructions.

### Windows (MiKTeX)

1. Download the most recent MiKTeX installer from their [website](http://miktex.org/download).
1. Run the installer and follow the instructions.

### Mac OS X (TeXLive)

1. Download the most recent MacTeX from their [website](http://www.tug.org/mactex/mactex-download.html).
1. Run `MacTeX.pkg` and follow the instructions.

### Linux (TeXLive)

Linux users have two options:

1. Install via your distribution's package manager (usually several releases behind)
1. Install from upstream (released yearly, updated often)

### Using Package Managers

- Arch Linux: `pacman -S texlive-most`
- Debian/Ubuntu/Mint: `apt-get install texlive-full`
- Fedora: `yum install texlive`

Note that using this method means that you will be dependent on that package's maintainer for the distribution for updates.  These packages will often be several releases behind the most recent distribution, often meaning critical updates will be missing.  It's almost always best to install from upstream. Also note that the distribution's package manager will probably not recognize the direct installation and could try to install it when one installs other related support packages.

### Installing from Upstream

<li>
Download the most recent TeXLive `install-tl-unx.tar.gz` from their [website](https://www.tug.org/texlive/).
</li>
<li>
Extract the files from the archive with `tar -zxvf install-tl-unx.tar.gz`.
</li>
<li>
Change into the downloaded folder with `cd install-tl-unx`.
</li>
<li>
Run `./install-tl` and follow the instructions.
TeXLive should now be installed under `/usr/local/texlive/YEAR/`, where `YEAR` is the four digit year (e.g. `2016`). In this way, it is possible to have multiple TeXLive versions alongside each other and switch between them by changing your PATH variable.
Open this folder and check the `bin` folder. It should contain a subfolder, which (depending on your platform) will be something like `i386-linux` or `x86_64-linux`.
</li>
<li>
Add the TeX Live binary folder to your path with

```latex
EXPORT PATH=/usr/local/texlive/YEAR/bin/PLATFORM:$PATH

```


where `YEAR` is the four digit year (e.g. `2016`), and `PLATFORM` is your platform (e.g. `x86_64-linux`).
</li>

### Test Installation

The LaTeX installation is now complete. To test it, create a new file with your favorite text editor, name it `test.tex` and add the following content:

Now, open the console or terminal, navigate to the folder where you saved `test.tex` and run

(Note that your editor may have facilities to run this for you.)

This creates several new files, including `test.pdf`. This is the output document, and looks like this:

[<img src="http://i.stack.imgur.com/fIs4k.png" alt="Resulting PDF file from compiling `test.tex" />](http://i.stack.imgur.com/fIs4k.png)

Congratulations, you have successfully installed LaTeX, and created your first LaTeX document!



#### Remarks


### LaTeX

### What is LaTeX?

LaTeX (pronounced **lay-tech** or **lah-tekh**) is a markup language for typesetting documents similar to how HTML is one for web sites.

LaTeX has advantages over What-You-See-Is-What-You-Get (WYSIWYG) editors such as Microsoft Word because with LaTeX you provide the content, and LaTeX takes care of the layout. Separation of content from typesetting results in documents that are consistently and beautifully formatted. Furthermore, because LaTeX markup is of plain text format (unlike more complex file formats produced by WYSIWYG editors, like `.docx`), LaTeX files are lightweight and can be easily kept under [version control](https://en.wikipedia.org/wiki/Version_control).

LaTeX documents are typically compiled to PDF files so that consistency in layout is retained across different viewers, and for printing.

LaTeX is especially popular in academic writing due to its rich support for typesetting equations, cross-referencing figures and tables, and citations and bibliographies.

