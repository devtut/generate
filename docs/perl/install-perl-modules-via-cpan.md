---
metaTitle: "Perl - Install Perl modules via CPAN"
description: "Installing modules manually, cpanminus, the lightweight configuration-free replacement for cpan, Run Perl CPAN in your terminal (Mac and Linux) or command prompt (Windows)"
---

# Install Perl modules via CPAN




## Installing modules manually


If you don't have permissions to install perl modules, you may still install them manually, indicating a custom path where you've got writing permissions.

Fist, download and unzip module archive:

```perl
wget module.tar.gz
tar -xzf module.tar.gz
cd module

```

Then, if the module distribution contains a `Makefile.PL` file, run:

```perl
perl Makefile.PL INSTALL_BASE=$HOME/perl
make
make test
make install

```

or if you have `Build.PL` file instead of a `Makefile.PL`:

```perl
perl Build.PL --install_base $HOME/perl
perl Build
perl Build test
perl Build install

```

You also have to include the module path in `PERL5LIB` environment variable in order to use it in your code:

```perl
export PERL5LIB=$HOME/perl

```



## cpanminus, the lightweight configuration-free replacement for cpan


**Usage**

To install a module (assuming `cpanm` is already installed):

```perl
cpanm Data::Section

```

`cpanm` ("cpanminus") strives to be less verbose than `cpan` but still captures all of the installation information in a log file in case it is needed. It also handles many "interactive questions" for you, whereas `cpan` doesn't.

`cpanm` is also popular for installing dependencies of a project from, e.g., GitHub.  Typical use is to first `cd` into the project's root, then run

```perl
cpanm --installdeps .

```

With `--installdeps` it will:

<li>Scan and install **configure_requires** dependencies from either
<ul>
1. META.json
1. META.yml (if META.json is missing)
</ul>
</li>
1. Build the project (equivalent to `perl Build.PL`), generating MYMETA files
<li>Scan and install **requires** dependencies from either
<ul>
1. MYMETA.json
1. MYMETA.yml (if MYMETA.json is missing)
</ul>
</li>

- MYMETA.json
- MYMETA.yml (if MYMETA.json is missing)

To specify the file 'some.cpanfile', containing the dependencies, run:

```perl
cpanm --installdeps --cpanfile some.cpanfile .

```

**`cpanm` Installation**

There are [several ways to install it](https://metacpan.org/pod/App::cpanminus#INSTALLATION). Here's installation via `cpan`:

```perl
cpan App::cpanminus

```

**`cpanm` Configuration**

There is **no** config file for `cpanm`.  Rather, it relies on the following environment variables for its configuration:

<li>`PERL_CPANM_OPT` (General cpanm command line options)
<ul>
- `export PERL_CPANM_OPT="--prompt"` # in .bashrc, to enable prompting, e.g.
- `setenv PERL_CPANM_OPT "--prompt"` # in .tcshrc



## Run Perl CPAN in your terminal (Mac and Linux) or command prompt (Windows)


### Command line

You can use `cpan` to install modules directly from the command line:

```perl
cpan install DBI

```

This would be followed by possibly many pages of output describing exactly what it is doing to install the module. Depending on the modules being installed, it may pause and ask you questions.

### Interactive Shell

You can also enter a "shell" thus:

```perl
perl -MCPAN -e "shell"

```

It will produce output as below:

```perl
Terminal does not support AddHistory.

cpan shell -- CPAN exploration and modules installation (v2.00)
Enter 'h' for help.

cpan[1]>

```

Then you can install the modules which you want by the easy command `install <module>`.

Example: `cpan[1]>` `install DBI`

After installing successfully, type `exit` to quit.

