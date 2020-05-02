---
metaTitle: "Input and output"
description: "Reading and writing data frames"
---

# Input and output



## Reading and writing data frames


[Data frames](http://stackoverflow.com/documentation/r/438/data-frames#t=201608181818154952835) are R's tabular data structure. They can be written to or read from in a variety of ways.

This example illustrates a couple common situations. See the links at the end for other resources.

### Writing

**Before making the example data below, make sure you're in a folder you want to write to. Run `getwd()` to verify the folder you're in and read `?setwd` if you need to change folders.**

```r
set.seed(1)
for (i in 1:3) 
  write.table(
    data.frame(id = 1:2, v = sample(letters, 2)), 
    file = sprintf("file201%s.csv", i)
  )

```

Now, we have three similarly-formatted CSV files on disk.

### Reading

We have three similarly-formatted files (from the last section) to read in. Since these files are related, we should store them together after reading in, in a `list`:

```r
file_names = c("file2011.csv", "file2012.csv", "file2013.csv")
file_contents = lapply(setNames(file_names, file_names), read.table)

# $file2011.csv
#   id v
# 1  1 g
# 2  2 j
# 
# $file2012.csv
#   id v
# 1  1 o
# 2  2 w
# 
# $file2013.csv
#   id v
# 1  1 f
# 2  2 w

```

To work with this list of files, first examine the structure with `str(file_contents)`, then read about stacking the list with `?rbind` or iterating over the list with `?lapply`.

### Further resources

Check out `?read.table` and `?write.table` to extend this example. Also:

- [R binary formats (for tables and other objects)](http://stackoverflow.com/documentation/r/5540/i-o-for-rs-binary-format#t=201608181926177335259)
<li>[Plain-text table formats](http://stackoverflow.com/documentation/r/481/i-o-for-plain-text-tables-csv-tsv-etc#t=20160818180444087839)
<ul>
- comma-delimited CSVs
- tab-delimited TSVs
- Fixed-width formats

- Feather

- SAS
- SPSS
- Stata
- Excel

- MySQL
- SQLite
- PostgreSQL



#### Remarks


To construct file paths, for reading or writing, use `file.path`.

Use `dir` to see what files are in a directory.

