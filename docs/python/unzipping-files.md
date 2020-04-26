# Unzipping Files


To extract or uncompress a tarball, ZIP, or gzip file, Python's tarfile, zipfile, and gzip modules are provided respectively. Python's tarfile module provides the `TarFile.extractall(path==.=, members=None)` function for extracting from a tarball file. Python's zipfile module provides the `ZipFile.extractall([path[, members[, pwd]]])` function for extracting or unzipping ZIP compressed files. Finally, Python's gzip module provides the GzipFile class for decompressing.



## Using Python ZipFile.extractall() to decompress a ZIP file


```
file_unzip = 'filename.zip'
unzip = zipfile.ZipFile(file_unzip, 'r')
unzip.extractall()
unzip.close()

```



## Using Python TarFile.extractall() to decompress a tarball


```
file_untar = 'filename.tar.gz'
untar = tarfile.TarFile(file_untar)
untar.extractall()
untar.close()

```

