---
metaTitle: "Standardize analyses by writing standalone R scripts"
description: "The basic structure of standalone R program and how to call it, Using littler to execute R scripts"
---

# Standardize analyses by writing standalone R scripts


If you want to routinely apply an R analysis to a lot of separate data files, or provide a repeatable analysis method to other people, an executable R script is a user-friendly way to do so. Instead of you or your user having to call `R` and execute your script inside R via `source(.)`  or a function call, your user may simply call the script itself as if it was a program.



## The basic structure of standalone R program and how to call it


### The first standalone R script

Standalone R scripts are not executed by the program `R` (`R.exe` under Windows), but by a program called `Rscript` (`Rscript.exe`), which is included in your `R` installation by default.

To hint at this fact, standalone R scripts start with a special line called **Shebang line**, which holds the following content: `#!/usr/bin/env Rscript`. Under Windows, an additional measure is needed, which is detailled later.

The following simple standalone R script saves a histogram under the file name "hist.png" from numbers it receives as input:

```r
#!/usr/bin/env Rscript

# User message (\n = end the line)
cat("Input numbers, separated by space:\n")
# Read user input as one string (n=1 -> Read only one line)
input <- readLines(file('stdin'), n=1)
# Split the string at each space (\\s == any space)
input <- strsplit(input, "\\s")[[1]]
# convert the obtained vector of strings to numbers
input <- as.numeric(input)

# Open the output picture file
png("hist.png",width=400, height=300)
# Draw the histogram
hist(input)
# Close the output file
dev.off()

```

You can see several key elements of a standalone R script. In the first line, you see the Shebang line. Followed by that, `cat("....\n")` is used to print a message to the user. Use `file("stdin")` whenever you want to specify "User input on console" as a data origin. This can be used instead of a file name in several data reading functions (`scan`, `read.table`, `read.csv`,...). After the user input is converted from strings to numbers, the plotting begins. There, it can be seen, that plotting commands which are meant to be written to a file must be enclosed in two commands. These are in this case `png(.)` and `dev.off()`. The first function depends on the desired output file format (other common choices being `jpeg(.)` and `pdf(.)`). The second function, `dev.off()` is always required. It writes the plot to the file and ends the plotting process.

### Preparing a standalone R script

### Linux/Mac

The standalone script's file must first be made executable. This can happen by right-clicking the file, opening "Properties" in the opening menu and checking the "Executable" checkbox in the "Permissions" tab. Alternatively, the command

```r
chmod +x PATH/TO/SCRIPT/SCRIPTNAME.R

```

can be called in a Terminal.

### Windows

For each standalone script, a batch file must be written with the following contents:

```r
"C:\Program Files\R-XXXXXXX\bin\Rscript.exe" "%~dp0\XXXXXXX.R" %*

```

A batch file is a normal text file, but which has a `*.bat` extension except a `*.txt` extension. Create it using a text editor like `notepad` (not `Word`) or similar and put the file name into quotation marks `"FILENAME.bat"`)  in the save dialog. To edit an existing batch file, right-click on it and select "Edit".

You have to adapt the code shown above everywhere `XXX...` is written:

- Insert the correct folder where your R installation resides
- Insert the correct name of your script and place it into the same directory as this batch file.

Explanation of the elements in the code: The first part `"C:\...\Rscript.exe"` tells Windows where to find the `Rscript.exe` program. The second part `"%~dp0\XXX.R"` tells `Rscript` to execute the R script you've written which resides in the same folder as the batch file (`%~dp0` stands for the batch file folder). Finally, `%*` forwards any command line arguments you give to the batch file to the R script.

If you double-click on the batch file, the R script is executed. If you drag files on the batch file, the corresponding file names are given to the R script as command line arguments.



## Using littler to execute R scripts


[littler](http://dirk.eddelbuettel.com/code/littler.html) (pronounced **little r**) ([cran](https://cran.r-project.org/web/packages/littler/index.html)) provides, besides other features, two possibilities to run R scripts from the command line with littler's `r` command (when one works with Linux or MacOS).

### Installing littler

### From R:

```r
install.packages("littler")

```

The path of `r` is printed in the terminal, like

```r
You could link to the 'r' binary installed in
'/home/*USER*/R/x86_64-pc-linux-gnu-library/3.4/littler/bin/r'
from '/usr/local/bin' in order to use 'r' for scripting.

```

To be able to call `r` from the system's command line, a symlink is needed:

```r
ln -s /home/*USER*/R/x86_64-pc-linux-gnu-library/3.4/littler/bin/r /usr/local/bin/r

```

### Using apt-get (Debian, Ubuntu):

```r
sudo apt-get install littler

```

### Using littler with standard .r scripts

With `r` from `littler` it is possible to execute standalone R scripts without any changes to the script.
Example script:

```r
# User message (\n = end the line)
cat("Input numbers, separated by space:\n")
# Read user input as one string (n=1 -> Read only one line)
input <- readLines(file('stdin'), n=1)
# Split the string at each space (\\s == any space)
input <- strsplit(input, "\\s")[[1]]
# convert the obtained vector of strings to numbers
input <- as.numeric(input)

# Open the output picture file
png("hist.png",width=400, height=300)
# Draw the histogram
hist(input)
# Close the output file
dev.off()

```

Note that no shebang is at the top of the scripts. When saved as for example `hist.r`, it is directly callable from the system command:

```r
r hist.r

```

### Using littler on **shebanged** scripts

It is also possible to create executable R scripts with littler, with the use of the shebang

```r
#!/usr/bin/env r

```

at the top of the script. The corresponding R script has to be made executable with `chmod +X /path/to/script.r` and is directly callable from the system terminal.



#### Remarks


To represent the standard input-/output channels, use the functions `file("stdin")` (input from terminal or other program via pipe), `stdout()` (standard output) and `stderr()` (standard error). Note that while there is the function `stdin()`, it can not be used when supplying a ready-made script to R, because it will read the next lines of that script instead of user input.

