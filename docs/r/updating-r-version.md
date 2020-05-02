---
metaTitle: "Updating R version"
description: "Installing from R Website, Updating from within R using installr Package, Deciding on the old packages, Updating Packages, Check R Version"
---

# Updating R version


Installing or Updating your Software will give access to new features and bug fixes. Updating your R installation can be done in a couple of ways. One Simple way is go to [R website](https://cran.r-project.org/) and download the latest version for your system.



## Installing from R Website


To get the latest release go to [https://cran.r-project.org/](https://cran.r-project.org/) and download the file for your operating system. Open the downloaded file and follow the on-screen installation steps. All the settings can be left on default unless you want to change a certain behaviour.



## Updating from within R using installr Package


You can also update R from within R by using a handy package called **installr**.

Open R Console (NOT RStudio, this doesn't work from RStudio) and run the following code to install the package and initiate update.

```r
install.packages("installr")
library("installr")
updateR()

```

[<img src="https://i.stack.imgur.com/UMl3T.png" alt="enter image description here" />](https://i.stack.imgur.com/UMl3T.png)



## Deciding on the old packages


Once the installation is finished click the Finish button.

Now it asks if you want to copy your packages fro the older version of R to Newer version of R. Once you choose yes all the package are copied to the newer version of R.

[<img src="https://i.stack.imgur.com/ytDqh.png" alt="enter image description here" />](https://i.stack.imgur.com/ytDqh.png)

After that you can choose if you still want to keep the old packages or delete.

[<img src="https://i.stack.imgur.com/zK6L9.png" alt="enter image description here" />](https://i.stack.imgur.com/zK6L9.png)

You can even move your Rprofile.site from older version to keep all your customised settings.

[<img src="https://i.stack.imgur.com/ffK8W.png" alt="enter image description here" />](https://i.stack.imgur.com/ffK8W.png)



## Updating Packages


You can update your installed packages once the updating of R is done.

[<img src="https://i.stack.imgur.com/rQbIt.png" alt="enter image description here" />](https://i.stack.imgur.com/rQbIt.png)

Once its done Restart R and enjoy exploring.



## Check R Version


You can check R Version using the console

```r
version

```

