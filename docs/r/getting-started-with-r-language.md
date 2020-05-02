---
metaTitle: "Getting started with R Language"
description: "Installing R, Hello World!, Getting Help, Interactive mode and R scripts"
---

# Getting started with R Language



## Installing R


You might wish to install [RStudio](https://www.rstudio.com/) after you have installed R. RStudio is a development environment for R that simplifies many programming tasks.

### Windows only:

[Visual Studio](https://www.visualstudio.com/) (starting from version 2015 Update 3) now features a development environment for R called [R Tools](https://www.visualstudio.com/vs/rtvs/), that includes a live interpreter, IntelliSense, and a debugging module. If you choose this method, you won't have to install R as specified in the following section.

### For Windows

1. Go to the [CRAN](https://cran.r-project.org/bin/windows/base/) website, click on download R for Windows, and download the latest version of R.
1. Right-click the installer file and RUN as administrator.
1. Select the operational language for installation.
1. Follow the instructions for installation.

### For OSX / macOS

### Alternative 1

(0. Ensure [XQuartz](https://www.xquartz.org/) is installed )

1. Go to the [CRAN](https://cran.r-project.org/bin/macosx/) website and download the latest version of R.
1. Open the disk image and run the installer.
1. Follow the instructions for installation.

This will install both R and the R-MacGUI. It will put the GUI in the /Applications/ Folder as R.app where it can either be double-clicked or dragged to the Doc. When a new version is released, the (re)-installation process will overwrite R.app but prior major versions of R will be maintained. The actual R code will be in the /Library/Frameworks/R.Framework/Versions/ directory. Using R within RStudio is also possible and would be using the same R code with a different GUI.

### Alternative 2

1. Install homebrew (the missing package manager for macOS) by following the instructions on [https://brew.sh/](https://brew.sh/)
1. `brew install R`

Those choosing the second method should be aware that the maintainer of the Mac fork advises against it, and will not respond to questions about difficulties on the R-SIG-Mac Mailing List.

### For Debian, Ubuntu and derivatives

You can get the version of R corresponding to your distro via `apt-get`. However, this version will frequently be quite far behind the most recent version available on CRAN. You can add CRAN to your list of recognized "sources".

```r
sudo apt-get install r-base

```

You can get a more recent version directly from CRAN by adding CRAN to your sources list.  Follow the [directions](http://cran.us.r-project.org/bin/linux/ubuntu/README.html) from CRAN for more details. Note in particular the need to also execute this so that you can use `install.packages()`. Linux packages are usually distributed as source files and need compilation:

```r
sudo apt-get install r-base-dev

```

### For Red Hat and Fedora

```r
sudo dnf install R

```

### For Archlinux

R is directly available in the `Extra` package repo.

More info on using R under Archlinux can be found on the [ArchWiki R page](https://wiki.archlinux.org/index.php/R).



## Hello World!


```r
"Hello World!"

```

Also, check out [the detailed discussion of how, when, whether and why to print a string](http://stackoverflow.com/documentation/r/5541/reading-and-writing-text-strings/1221/printing-and-displaying-text#t=201608181943557669031).



## Getting Help


You can use function `help()` or `?` to access documentations and search for help in R. For even more general searches, you can use `help.search()` or `??`.

```r
#For help on the help function of R
help()

#For help on the paste function
help(paste)    #OR
help("paste")  #OR
?paste         #OR
?"paste"

```

Visit [https://www.r-project.org/help.html](https://www.r-project.org/help.html) for additional information



## Interactive mode and R scripts


### The interactive mode

The most basic way to use R is the **interactive** mode. You type commands and immediately get the result from R.

### Using R as a calculator

Start R by typing `R` at the command prompt of your operating system or by executing `RGui` on Windows. Below you can see a screenshot of an interactive R session on Linux:

[<img src="https://i.stack.imgur.com/uHqS3.png" alt="Screenshot of a linux terminal executing R" />](https://i.stack.imgur.com/uHqS3.png)

This is RGui on Windows, the most basic working environment for R under Windows:
[<img src="https://i.stack.imgur.com/2hsAR.png" alt="Screenshot of a RGui terminal on Windows" />](https://i.stack.imgur.com/2hsAR.png)

After the `>` sign, expressions can be typed in. Once an expression is typed, the result is shown by R. In the screenshot above, R is used as a calculator: Type

```r
1+1

```

to immediately see the result, `2`. The leading `[1]` indicates that R returns a vector. In this case, the vector contains only one number (2).

### The first plot

R can be used to generate plots. The following example uses the data set `PlantGrowth`, which comes as an example data set along with R

Type int the following all lines into the R prompt which do not start with `##`. Lines starting with `##` are meant to document the result which R will return.

```r
data(PlantGrowth)
str(PlantGrowth)
## 'data.frame':    30 obs. of  2 variables:
## $ weight: num  4.17 5.58 5.18 6.11 4.5 4.61 5.17 4.53 5.33 5.14 ...
## $ group : Factor w/ 3 levels "ctrl","trt1",..: 1 1 1 1 1 1 1 1 1 1 ...
anova(lm(weight ~ group, data = PlantGrowth))
## Analysis of Variance Table
## 
## Response: weight
##           Df  Sum Sq Mean Sq F value  Pr(>F)  
## group      2  3.7663  1.8832  4.8461 0.01591 *
## Residuals 27 10.4921  0.3886                  
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
boxplot(weight ~ group, data = PlantGrowth, ylab = "Dry weight")

```

The following plot is created:

[<img src="https://i.stack.imgur.com/fssq4.png" alt="Boxplot, created from the PlantGrowth example data set" />](https://i.stack.imgur.com/fssq4.png)

`data(PlantGrowth)` loads the example data set `PlantGrowth`, which is records of dry masses of plants which were subject to two different treatment conditions or no treatment at all (control group). The data set is made available under the name `PlantGrowth`. Such a name is also called a [Variable](http://stackoverflow.com/documentation/r/9013/variables#t=201703202048433892161).

To load your own data, the following two documentation pages might be helpful:

- [Reading and writing tabular data in plain-text files (CSV, TSV, etc.)](http://stackoverflow.com/documentation/r/481/reading-and-writing-tabular-data-in-plain-text-files-csv-tsv-etc#t=201703202036309470083)
- [I/O for foreign tables (Excel, SAS, SPSS, Stata)](http://stackoverflow.com/documentation/r/5536/i-o-for-foreign-tables-excel-sas-spss-stata#t=201703202035257131862)

`str(PlantGrowth)` shows information about the data set which was loaded. The output indicates that `PlantGrowth` is a `data.frame`, which is R's name for a table. The `data.frame` contains of two columns and 30 rows. In this case, each row corresponds to one plant. Details of the two columns are shown in the lines starting with `$`: The first column is called `weight` and contains
numbers (`num`, the dry weight of the respective plant). The second column, `group`, contains the treatment that the plant was subjected to. This is categorial data, which is called `factor` in R.
[Read more information about data frames](http://stackoverflow.com/documentation/r/438/data-frames#t=201703202042442838867).

To compare the dry masses of the three different groups, a one-way ANOVA is performed using `anova(lm( ... ))`. `weight ~ group` means "Compare the values of the column `weight`, grouping by the values of the column `group`". This is called a [Formula](http://stackoverflow.com/documentation/r/1061/formula#t=201703202051107370356) in R.
`data = ...` specifies the name of the table where the data can be found.

The result shows, among others, that there exists a significant difference (Column `Pr(>F)`), `p = 0.01591`) between some of the three groups. Post-hoc tests, like Tukey's Test, must be performed to determine which groups' means differ significantly.

`boxplot(...)` creates a box plot of the data.  where the values to be plotted come from. `weight ~ group` means: "Plot the values of the column weight **versus** the values of the column `group`. `ylab = ...` specifies the label of the y axis. More information: [Base plotting](http://stackoverflow.com/documentation/r/1377/base-plotting#t=201703202053421042418)

Type `q()` or <kbd>Ctrl</kbd>-<kbd>D</kbd> to exit from the R session.

### R scripts

To document your research, it is favourable to save the commands you use for calculation in a file. For that effect, you can create **R scripts**. An R script is a simple text file, containing R commands.

Create a text file with the name `plants.R`, and fill it with the following text, where some commands are familiar from the code block above:

```r
data(PlantGrowth)

anova(lm(weight ~ group, data = PlantGrowth))

png("plant_boxplot.png", width = 400, height = 300)
boxplot(weight ~ group, data = PlantGrowth, ylab = "Dry weight")
dev.off()    

```

Execute the script by typing into your terminal (The terminal of your operating system, **not** an interactive R session like in the previous section!)

```r
R --no-save <plant.R >plant_result.txt

```

The file `plant_result.txt` contains the results of your calculation, as if you had typed them into the interactive R prompt. Thereby, your calculations are documented.

The new commands `png` and `dev.off` are used for saving the boxplot to disk. The two commands must enclose the plotting command, as shown in the example above. `png("FILENAME", width = ..., height = ...)` opens a new PNG file with the specified file name, width and height in pixels. `dev.off()` will finish plotting and saves the plot to disk. No output is saved until `dev.off()` is called.



#### Remarks


### Editing R Docs on Stack Overflow

See the [documentation guidelines](http://stackoverflow.com/documentation/r/5410/meta-documentation-guidelines#t=201608111658158162781) for general rules when creating documentation.

### A few features of R that immigrants from other language may find unusual

- Unlike other languages variables in R need not require type declaration.
<li>The same variable can be assigned  different data types at different
instances of time, if required.</li>
- Indexing of atomic vectors and lists starts from 1, not 0.
- R `arrays` (and the special case of matrices) have a `dim` attribute that sets them apart from R's "atomic vectors" which have no attributes.
- A list in R allows you to gather a variety of objects under one name (that is, the name of the list) in an ordered way. These objects can be **matrices**, **vectors**, **data frames**, **even other lists**, etc. It is not even required that these objects are related to each other in any way.
- [Recycling](http://stackoverflow.com/documentation/r/5649/recycling#t=201702071755160741896)
- [Missing values](http://stackoverflow.com/documentation/r/3388)

