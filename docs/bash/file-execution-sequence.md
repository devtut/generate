---
metaTitle: "File execution sequence"
description: ".profile vs .bash_profile (and .bash_login)"
---

# File execution sequence


`.bash_profile`, `.bash_login`, `.bashrc`, and `.profile` all do pretty much the same thing: set up and define functions, variables, and the sorts.

The main difference is that `.bashrc` is called at the opening of a non-login but interactive window, and `.bash_profile` and the others are called for a login shell.  Many people have their `.bash_profile` or similar call `.bashrc` anyway.



## .profile vs .bash_profile (and .bash_login)


`.profile` is read by most shells on startup, including bash. However, `.bash_profile` is used for configurations specific to bash.  For general initialization code, put it in `.profile`. If it's specific to bash, use `.bash_profile`.

`.profile` isn't actually designed for bash specifically, `.bash_profile` is though instead. (`.profile` is for Bourne and other similar shells, which bash is based off) Bash will fall back to `.profile` if `.bash_profile` isn't found.

`.bash_login` is a fallback for `.bash_profile`, if it isn't found. Generally best to use `.bash_profile` or `.profile` instead.



#### Remarks


Other files of note are:

<li>
`/etc/profile`, for system-wide (not user specific) initialization code.
</li>
<li>
`.bash_logout`, triggered when logging out (think cleanup stuff)
</li>
<li>
`.inputrc`, similar to `.bashrc` but for readline.
</li>

