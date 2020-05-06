---
metaTitle: "Perl - Installation of Perl"
description: "Linux, OS X, Windows"
---

# Installation of Perl


I'm going to begin this with the process in Ubuntu, then in OS X and finally in Windows. I haven't tested it on all perl versions, but it should be a similar process.

Use [Perlbrew](https://perlbrew.pl) if you like to switch easily beween different versions of Perl.

I want to state that this tutorial is about Perl in it's open-source version. There are other versions like `activeperl` which its advantages and disadvantages, that are not part of this tutorial.



## Linux


There is more than one way to do it:

<li>
Using the package manager:

```perl
sudo apt install perl

```


</li>
<li>
Installing from source:

```perl
wget http://www.cpan.org/src/5.0/perl-version.tar.gz
tar -xzf perl-version.tar.gz
cd perl-version
./Configure -de
make
make test
make install

```


</li>
<li>
Installing in your $home directory (not sudo needed) with [Perlbrew](https://perlbrew.pl):

```perl
wget -O - https://install.perlbrew.pl | bash

```


See also [Perlbrew](http://stackoverflow.com/documentation/perl/9144/perlbrew#t=20170309214354041427)
</li>



## OS X


There are several options:

<li>
[Perlbrew](https://perlbrew.pl/):

```perl
# You need to install Command Line Tools for Xcode  
curl -L https://install.perlbrew.pl | bash

```


</li>
<li>
[Perlbrew](https://perlbrew.pl/) with thread support:

```perl
# You need to install Command Line Tools for Xcode  
curl -L https://install.perlbrew.pl | bash

```


After the install of perlbrew, if you want to install Perl with thread support, just run:

```perl
perlbrew install -v perl-5.26.0 -Dusethreads

```


</li>
<li>
From source:

```perl
tar -xzf perl-version.tar.gz
cd perl-version
./Configure -de
make
make test
make install

```


</li>



## Windows


- As we said before, we go with the open-source version. For Windows you can choose `strawberry` or `DWIM`. Here we cover the `strawberry` version, since `DWIM` is based on it. The easy way here is installing from the [official executable](http://strawberryperl.com/).

See also [berrybrew](https://github.com/dnmfarrell/berrybrew) - the perlbrew for Windows Strawberry Perl

