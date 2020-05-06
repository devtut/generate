---
metaTitle: "Perl - Perlbrew"
description: "Setup perlbrew for the first time"
---

# Perlbrew


Perlbrew is a tool to manage multiple perl installations in your `$HOME` directory.



## Setup perlbrew for the first time


### Create setup script `~/.perlbrew.sh`:

```perl
# Reset any environment variables that could confuse `perlbrew`:
export PERL_LOCAL_LIB_ROOT=
export PERL_MB_OPT=
export PERL_MM_OPT=

# decide where you want to install perlbrew:
export PERLBREW_ROOT=~/perlbrew
[[ -f "$PERLBREW_ROOT/etc/bashrc" ]] && source "$PERLBREW_ROOT/etc/bashrc"

```

### Create installation script `install_perlbrew.sh`:

```perl
source ~/.perlbrew.sh
curl -L https://install.perlbrew.pl | bash
source "$PERLBREW_ROOT/etc/bashrc"

# Decide which version you would like to install: 
version=perl-5.24.1
perlbrew install "$version"
perlbrew install-cpanm
perlbrew switch  "$version"

```

### Run installation script:

```perl
./install_perlbrew.sh

```

### Add to the end of your `~/.bashrc`

```perl
[[ -f ~/.perlbrew.sh ]] && source ~/.perlbrew.sh

```

### Source `~/.bashrc`:

```perl
source ~/.bashrc

```



#### Remarks


See also

- [Official homepage for perlbrew](https://perlbrew.pl/)
- [CPAN documentation for perlbrew](https://metacpan.org/pod/perlbrew)

