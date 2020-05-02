---
metaTitle: ".mailmap file: Associating contributor and email aliases"
description: "Merge contributers by aliases to show commit count in shortlog."
---

# .mailmap file: Associating contributor and email aliases



## Merge contributers by aliases to show commit count in shortlog.


When contributors add to a project from different machines or operating systems, it may happen that they use different email addresses or names for this, which will fragment contributor lists and statistics.

Running `git shortlog -sn` to get a list of contributors and the number of commits by them could result in the following output:

```git
Patrick Rothfuss 871
Elizabeth Moon 762
E. Moon 184
Rothfuss, Patrick 90

```

This fragmentation/disassociation may be adjusted by providing a plain text file `.mailmap`, containing email mappings.

All names and email addresses listed in one line will be associated to the first named entity respectively.

For the example above, a mapping could look like this:

```git
Patrick Rothfuss <fussy@kingkiller.com> Rothfuss, Patrick <fussy@kingkiller.com>
Elizabeth Moon <emoon@marines.mil> E. Moon <emoon@scifi.org>

```

Once this file exists in the project's root, running `git shortlog -sn` again will result in a condensed list:

```git
Patrick Rothfuss 961
Elizabeth Moon 946

```



#### Syntax


<li># Only replace email addresses<br />
<primary@example.org> <alias@example.org></li>
<li># Replace name by email address<br />
Contributor <primary@example.org></li>
<li># Merge multiple aliases under one name and email<br />
# Note this will not associate 'Other <alias2@example.org>'.<br />
Contributor <primary@example.org> <alias1@example.org> Contributor <alias2@example.org></li>



#### Remarks


A `.mailmap` file may be created in any text editor and is just a plain text file containing optional contributor names, primary email addresses, and their aliases. it has to be placed in the project's root, next to the `.git` directory.

Keep in mind that this just modifies the visual output of commands like `git shortlog` or `git log --use-mailmap`. This will **not** rewrite commit history or prevent commits with varying names and/or email addresses.

To prevent commits based on information such as email addresses, you should use [git hooks](http://stackoverflow.com/documentation/git/1330/hooks) instead.

