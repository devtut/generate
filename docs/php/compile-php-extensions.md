---
metaTitle: "Compile PHP Extensions"
description: "Compiling on Linux"
---

# Compile PHP Extensions



## Compiling on Linux


To compile a PHP extension in a typical Linux environment, there are a few pre-requisites:

- Basic Unix skills (being able to operate "make" and a C compiler)
- An ANSI C compiler
- The source code for the PHP extension you want to compile

Generally there are two ways to compile a PHP extension. You can **statically** compile the extension into the PHP binary, or compile it as a **shared** module loaded by your PHP binary at startup. Shared modules are more likely since they allow you to add or remove extensions without rebuilding the entire PHP binary. This example focuses on the shared option.

If you installed PHP via your package manager (`apt-get install`, `yum install`, etc..) you will need to install the `-dev` package for PHP, which will include the necessary PHP header files and phpize script for the build environment to work. The package might be named something like `php5-dev` or `php7-dev`, but be sure to use your package manager to search for the appropriate name using your distro's repositories. They can differ.

If you built PHP from source the header files most likely already exist on your system (**usually** in `/usr/include` or `/usr/local/include`).

### Steps to compile

After you check to make sure you have all the prerequisites, necessary to compile, in place you can head over to [pecl.php.net](http://pecl.php.net), select an extension you wish to compile, and download the tar ball.

1. Unpack the tar ball (e.g. `tar xfvz yaml-2.0.0RC8.tgz`)
1. Enter the directory where the archive was unpacked and run `phpize`
1. You should now see a newly created `.configure` script if all went well, run that `./configure`
1. Now you will need to run `make`, which will compile the extension
1. Finally, `make install` will copy the compiled extension binary to your extension directory

The `make install` step will typically provide the installation path for you where the extension was copied. This is **usually** in `/usr/lib/`, for example it might be something like `/usr/lib/php5/20131226/yaml.so`. But this depends on your configuration of PHP (i.e. `--with-prefix`) and specific API version. The API number is included in the path to keep extensions built for different API versions in separate locations.

### Loading the Extension in PHP

To load the extension in PHP, find your loaded php.ini file for the appropriate SAPI, and add the line `extension=yaml.so` then restart PHP. Change `yaml.so` to the name of the actual extension you installed, of course.

For a Zend extension you do need to provide the full path to the shared object file. However, for normal PHP extensions this path derived from the [`extension_dir`](http://php.net/ini.core#ini.extension-dir) directive in your loaded configuration, or from the `$PATH` environment during initial setup.

