---
metaTitle: "I/O for foreign tables (Excel, SAS, SPSS, Stata)"
description: "Importing data with rio, Read and write Stata, SPSS and SAS files, Importing Excel files, Import or Export of Feather file"
---

# I/O for foreign tables (Excel, SAS, SPSS, Stata)




## Importing data with rio


A very simple way to import data from many common file formats is with [rio](https://cran.r-project.org/web/packages/rio/index.html). This package provides a function `import()` that wraps many commonly used data import functions, thereby providing a standard interface. It works simply by passing a file name or URL to `import()`:

```r
import("example.csv")       # comma-separated values
import("example.tsv")       # tab-separated values
import("example.dta")       # Stata
import("example.sav")       # SPSS
import("example.sas7bdat")  # SAS
import("example.xlsx")      # Excel

```

`import()` can also read from compressed directories, URLs (HTTP or HTTPS), and the clipboard.
A comprehensive list of all supported file formats is available on the [rio package github repository](https://github.com/leeper/rio).

It is even possible to specify some further parameters related to the specific file format you are trying to read, passing them directly within the `import()` function:

```r
import("example.csv", format = ",") #for csv file where comma is used as separator
import("example.csv", format = ";") #for csv file where semicolon is used as separator

```



## Read and write Stata, SPSS and SAS files


The packages `foreign` and `haven` can be used to import and export files from a variety of other statistical packages like Stata, SPSS and SAS and related software. There is a `read` function for each of the supported data types to import the files.

```r
# loading the packages
library(foreign)
library(haven)
library(readstata13)
library(Hmisc)

```

Some examples for the most common data types:

```r
# reading Stata files with `foreign`
read.dta("path\to\your\data")
# reading Stata files with `haven`
read_dta("path\to\your\data")

```

The `foreign` package can read in stata (.dta) files for versions of Stata 7-12. According to the development page, the `read.dta` is more or less frozen and will not be updated for reading in versions 13+. For more recent versions of Stata, you can use either the `readstata13` package or `haven`. For `readstata13`, the files are

```r
# reading recent Stata (13+) files with `readstata13`
read.dta13("path\to\your\data")

```

For reading in SPSS and SAS files

```r
# reading SPSS files with `foreign`
read.spss("path\to\your\data.sav", to.data.frame = TRUE)
# reading SPSS files with `haven`
read_spss("path\to\your\data.sav")
read_sav("path\to\your\data.sav")
read_por("path\to\your\data.por")

# reading SAS files with `foreign`
read.ssd("path\to\your\data")
# reading SAS files with `haven`
read_sas("path\to\your\data")
# reading native SAS files with `Hmisc`
sas.get("path\to\your\data")   #requires access to saslib 
# Reading SA XPORT format ( *.XPT ) files
sasxport.get("path\to\your\data.xpt")  # does not require access to SAS executable

```

The `SAScii` package provides functions that will accept SAS SET import code and construct a text file that can be processed with `read.fwf`. It has proved very robust for import of large public-released datasets. Support is at [https://github.com/ajdamico/SAScii](https://github.com/ajdamico/SAScii)

To export data frames to other statistical packages you can use the write functions `write.foreign()`. This will write 2 files, one containing the data and one containing instructions the other package needs to read the data.

```r
# writing to Stata, SPSS or SAS files with `foreign`
write.foreign(dataframe, datafile, codefile,
              package = c("SPSS", "Stata", "SAS"), ...)
write.foreign(dataframe, "path\to\data\file", "path\to\instruction\file", package = "Stata")

# writing to Stata files with `foreign`
write.dta(dataframe, "file", version = 7L,
          convert.dates = TRUE, tz = "GMT",
          convert.factors = c("labels", "string", "numeric", "codes"))

# writing to Stata files with `haven`
write_dta(dataframe, "path\to\your\data")

# writing to Stata files with `readstata13`
save.dta13(dataframe, file, data.label = NULL, time.stamp = TRUE,
  convert.factors = TRUE, convert.dates = TRUE, tz = "GMT",
  add.rownames = FALSE, compress = FALSE, version = 117,
  convert.underscore = FALSE)

# writing to SPSS files with `haven`
write_sav(dataframe, "path\to\your\data")

```

File stored by the SPSS can also be read with `read.spss` in this way:

```

foreign::read.spss('data.sav', to.data.frame=TRUE, use.value.labels=FALSE, 
                     use.missings=TRUE, reencode='UTF-8')
# to.data.frame if TRUE: return a data frame
# use.value.labels if TRUE: convert variables with value labels into R factors with those levels
# use.missings if TRUE: information on user-defined missing values will used to set the corresponding values to NA.
# reencode character strings will be re-encoded to the current locale. The default, NA, means to do so in a UTF-8 locale, only.

```



## Importing Excel files


There are several R packages to read excel files, each of which using different languages or resources, as summarized in the following table:

|R package|Uses
|------
|xlsx|Java
|XLconnect|Java
|openxlsx|C++
|readxl|C++
|RODBC|ODBC
|gdata|Perl

For the packages that use Java or ODBC it is important to know details about your system because you may have compatibility issues depending on your R version and OS. For instance, if you are using R 64 bits then you also must have Java 64 bits to use `xlsx` or `XLconnect`.

Some examples of reading excel files with each package are provided below. Note that many of the packages have the same or very similar function names. Therefore, it is useful to state the package explicitly, like `package::function`. The package `openxlsx` requires prior installation of RTools.

### Reading excel files with the xlsx package

```r
library(xlsx)

```

The index or name of the sheet is required to import.

```r
xlsx::read.xlsx("Book1.xlsx", sheetIndex=1)

xlsx::read.xlsx("Book1.xlsx", sheetName="Sheet1")

```

### Reading Excel files with the XLconnect package

```r
library(XLConnect)
wb <- XLConnect::loadWorkbook("Book1.xlsx")

# Either, if Book1.xlsx has a sheet called "Sheet1":
sheet1 <- XLConnect::readWorksheet(wb, "Sheet1")
# Or, more generally, just get the first sheet in Book1.xlsx:
sheet1 <- XLConnect::readWorksheet(wb, getSheets(wb)[1])

```

`XLConnect` automatically imports the pre-defined Excel cell-styles embedded in `Book1.xlsx`. This is useful when you wish to format your workbook object and export a perfectly formatted Excel document. Firstly, you will need to create the desired cell formats in `Book1.xlsx` and save them, for example, as `myHeader`, `myBody` and `myPcts`. Then, after loading the workbook in `R` (see above):

```r
Headerstyle <- XLConnect::getCellStyle(wb, "myHeader")
Bodystyle <- XLConnect::getCellStyle(wb, "myBody")
Pctsstyle <- XLConnect::getCellStyle(wb, "myPcts")

```

The cell styles are now saved in your `R` environment. In order to assign the cell styles to certain ranges of your data, you need to define the range and then assign the style:

```r
Headerrange <- expand.grid(row = 1, col = 1:8)
Bodyrange <- expand.grid(row = 2:6, col = c(1:5, 8))
Pctrange <- expand.grid(row = 2:6, col = c(6, 7))

XLConnect::setCellStyle(wb, sheet = "sheet1", row = Headerrange$row,
             col = Headerrange$col, cellstyle = Headerstyle)
XLConnect::setCellStyle(wb, sheet = "sheet1", row = Bodyrange$row,
             col = Bodyrange$col, cellstyle = Bodystyle)
XLConnect::setCellStyle(wb, sheet = "sheet1", row = Pctrange$row,
             col = Pctrange$col, cellstyle = Pctsstyle)

```

Note that `XLConnect` is easy, but can become extremely slow in formatting. A much faster, but more cumbersome formatting option is offered by `openxlsx`.

### Reading excel files with the openxlsx package

Excel files can be imported with package `openxlsx`

```r
library(openxlsx)

openxlsx::read.xlsx("spreadsheet1.xlsx", colNames=TRUE, rowNames=TRUE)

#colNames: If TRUE, the first row of data will be used as column names.
#rowNames: If TRUE, first column of data will be used as row names.

```

The sheet, which should be read into R can be selected either by providing its position in the `sheet` argument:

```r
openxlsx::read.xlsx("spreadsheet1.xlsx", sheet = 1)

```

or by declaring its name:

```r
openxlsx::read.xlsx("spreadsheet1.xlsx", sheet = "Sheet1")

```

Additionally, `openxlsx` can detect date columns in a read sheet. In order to allow automatic detection of dates, an argument `detectDates` should be set to `TRUE`:

```r
openxlsx::read.xlsx("spreadsheet1.xlsx", sheet = "Sheet1", detectDates= TRUE)

```

### Reading excel files with the readxl package

Excel files can be imported as a data frame into `R` using the `readxl` package.

```r
library(readxl)

```

It can read both `.xls` and `.xlsx` files.

```r
readxl::read_excel("spreadsheet1.xls")
readxl::read_excel("spreadsheet2.xlsx")

```

The sheet to be imported can be specified by number or name.

```r
readxl::read_excel("spreadsheet.xls", sheet = 1)
readxl::read_excel("spreadsheet.xls", sheet = "summary")

```

The argument `col_names = TRUE` sets the first row as the column names.

```

readxl::read_excel("spreadsheet.xls", sheet = 1, col_names = TRUE)

```

The argument `col_types` can be used to specify the column types in the data as a vector.

```r
readxl::read_excel("spreadsheet.xls", sheet = 1, col_names = TRUE,
                   col_types = c("text", "date", "numeric", "numeric"))

```

### Reading excel files with the RODBC package

Excel files can be read using the ODBC Excel Driver that interfaces with Windows' Access Database Engine (ACE), formerly JET. With the RODBC package, R can connect to this driver and directly query workbooks. Worksheets are assumed to maintain column headers in first row with data in organized columns of similar types. **NOTE:** This approach is limited to only Windows/PC machines as JET/ACE are installed .dll files and not available on other operating systems.

```r
library(RODBC)

xlconn <- odbcDriverConnect('Driver={Microsoft Excel Driver (*.xls, *.xlsx, *.xlsm, *.xlsb)};
                             DBQ=C:\\Path\\To\\Workbook.xlsx')

df <- sqlQuery(xlconn, "SELECT * FROM [SheetName$]")
close(xlconn)

```

Connecting with an SQL engine in this approach, Excel worksheets can be queried similar to database tables including `JOIN` and `UNION` operations. Syntax follows the JET/ACE SQL dialect. **NOTE:** Only data access DML statements, specifically `SELECT` can be run on workbooks, considered not updateable queries.

```r
joindf <-  sqlQuery(xlconn, "SELECT t1.*, t2.* FROM [Sheet1$] t1
                             INNER JOIN [Sheet2$] t2
                             ON t1.[ID] = t2.[ID]")

uniondf <-  sqlQuery(xlconn, "SELECT * FROM [Sheet1$]
                              UNION  
                              SELECT * FROM [Sheet2$]")

```

Even other workbooks can be queried from the same ODBC channel pointing to a current workbook:

```r
otherwkbkdf <- sqlQuery(xlconn, "SELECT * FROM 
                                 [Excel 12.0 Xml;HDR=Yes;
                                 Database=C:\\Path\\To\\Other\\Workbook.xlsx].[Sheet1$];")

```

### Reading excel files with the gdata package

**example here**



## Import or Export of Feather file


[Feather](https://github.com/wesm/feather) is an implementation of [Apache Arrow](https://arrow.apache.org/) designed to store data frames in a language agnostic manner while maintaining metadata (e.g. date classes), increasing interoperability between Python and R. Reading a feather file will produce a tibble, not a standard data.frame.

```r
library(feather)

path <- "filename.feather"
df <- mtcars

write_feather(df, path)

df2 <- read_feather(path)

head(df2)
##  A tibble: 6 x 11
##     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1  21.0     6   160   110  3.90 2.620 16.46     0     1     4     4
## 2  21.0     6   160   110  3.90 2.875 17.02     0     1     4     4
## 3  22.8     4   108    93  3.85 2.320 18.61     1     1     4     1
## 4  21.4     6   258   110  3.08 3.215 19.44     1     0     3     1
## 5  18.7     8   360   175  3.15 3.440 17.02     0     0     3     2
## 6  18.1     6   225   105  2.76 3.460 20.22     1     0     3     1

head(df)
##                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
## Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
## Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
## Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
## Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
## Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
## Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1

```

The current documentation contains this warning:

> 
Note to users: Feather should be treated as alpha software. In particular, the file format is likely to evolve over the coming year. Do not use Feather for long-term data storage.


