---
metaTitle: "Contributing to the PHP Core"
description: "Setting up a basic development environment"
---

# Contributing to the PHP Core



## Setting up a basic development environment


PHP's source code is hosted on [GitHub](https://github.com/php/php-src). To build from source you will first need to check out a working copy of the code.

```php
mkdir /usr/local/src/php-7.0/
cd /usr/local/src/php-7.0/
git clone -b PHP-7.0 https://github.com/php/php-src .

```

If you want to add a feature, it's best to create your own branch.

```php
git checkout -b my_private_branch

```

Finally, configure and build PHP

```php
./buildconf
./configure
make
make test
make install

```

If configuration fails due to missing dependencies, you will need to use your operating system's package management system to install them (e.g. `yum`, `apt`, etc.) or download and compile them from source.



#### Remarks


PHP is an open source project, and as such, anyone is able to contribute to it. Broadly speaking, there are two ways to contribute to the PHP core:

- Bug fixing
- Feature additions

Before contributing, however, it is important to understand how PHP versions are managed and released so that bug fixes and feature requests can target the correct PHP version. The developed changes can be submitted as a pull request to the [PHP Github repository](https://github.com/php/php-src#pull-requests). Useful information for developers can be found on the ["Get Involved" section of the PHP.net site](https://secure.php.net/get-involved.php) and the [#externals forum](https://externals.io/).

### Contributing with Bug Fixes

For those looking to begin contributing to the core, it's generally easier to start with bug fixing. This helps to gain familiarity with PHP's internals before attempting to make more complex modifications to the core that a feature would require.

With respect to the version management process, bug fixes should target the lowest affected, **whilst still supported** PHP version. It's this version that bug fixing pull requests should target. From there, an internals member can merge the fix into the correct branch and then merge it upwards to later PHP versions as necessary.

For those looking to get started on resolving bugs, a list of bug reports can be found at [bugs.php.net](http://bugs.php.net).

### Contributing with Feature Additions

PHP follows an RFC process when introducing new features and making important changes to the language. RFCs are voted on by members of php.net, and must achieve either a simple majority (50% + 1) or a super majority (2/3 + 1) of the total votes. A super majority is required if the change affects the language itself (such as introducing a new syntax), otherwise only a simple majority is required.

Before RFCs can be put to vote, they must undergo a discussion period of at least 2 weeks on the official PHP mailing list. Once this period has finished, and there are no open issues with the RFC, it can then be moved into voting, which must last at least 1 week.

If a user would like to revive a previously rejected RFC, then they can do so only under one of the following two circumstances:

- 6 months has passed from the previous vote
- The author(s) make substantial enough changes to the RFC that would likely affect the outcome of the vote should the RFC be put to vote again.

The people who have the privilege to vote will either be contributors to PHP itself (and so have php.net accounts), or be representatives of the PHP community. These representatives are chosen by those with php.net accounts, and will either be lead developers of PHP-based projects or regular participants to internals discussions.

When submitting new ideas for proposal, it is almost always required for the proposer to write, at the very least, a proof-of-concept patch. This is because without an implementation, the suggestion simply becomes another feature request that is unlikely to be fulfilled in the near future.

A thorough how-to of this process can be found at the official [How To Create an RFC](https://wiki.php.net/rfc/howto) page.

### Releases

Major PHP versions have no set release cycle, and so they may be released at the discretion of the internals team (whenever they see fit for a new major release). Minor versions, on the other hand, are released annually.

Prior to every release in PHP (major, minor, or patch), a series of release candidates (RCs) are made available. PHP does not use an RC as other projects do (i.e. if an RC has not problems found with it, then make it as the next final release). Instead, it uses them as a form of final betas, where typically a set number of RCs are decided before the final release is made.

### Versioning

PHP generally attempts to follow semantic versioning where possible. As such, backwards compatibility (BC) should be maintained in minor and patch versions of the language. Features and changes that preserve BC should target minor versions (not patch versions). If a feature or change has the potential to break BC, then they should aim to target the next major PHP version (**X**.y.z) instead.

Each minor PHP version (x.**Y**.z) has two years of general support (so-called "active support") for all types of bug fixes. An additional year on top of that is added for security support, where only security-related fixes are applied. After the three years is up, support for that version of PHP is dropped completely. A list of [currently supported PHP versions can be found at php.net](http://php.net/supported-versions.php).

