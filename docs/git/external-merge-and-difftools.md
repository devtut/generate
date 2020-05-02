---
metaTitle: "External merge and difftools"
description: "Setting up Beyond Compare, Setting up KDiff3 as merge tool, Setting up KDiff3 as diff tool, Setting up an IntelliJ IDE as merge tool (Windows), Setting up an IntelliJ IDE as diff tool (Windows)"
---

# External merge and difftools



## Setting up Beyond Compare


You can set the path to `bcomp.exe`

```git
git config --global difftool.bc3.path 'c:\Program Files (x86)\Beyond Compare 3\bcomp.exe'

```

and configure `bc3` as default

```git
git config --global diff.tool bc3

```



## Setting up KDiff3 as merge tool


The following should be added to your global `.gitconfig` file

```git
[merge]
    tool = kdiff3
[mergetool "kdiff3"]
    path = D:/Program Files (x86)/KDiff3/kdiff3.exe
    keepBackup = false
    keepbackup = false
    trustExitCode = false

```

Remember to set the `path` property to point to the directory where you have installed KDiff3



## Setting up KDiff3 as diff tool


```git
[diff]
    tool = kdiff3
    guitool = kdiff3
[difftool "kdiff3"]
    path = D:/Program Files (x86)/KDiff3/kdiff3.exe
    cmd = \"D:/Program Files (x86)/KDiff3/kdiff3.exe\" \"$LOCAL\" \"$REMOTE\"

```



## Setting up an IntelliJ IDE as merge tool (Windows)


```git
[merge]
    tool = intellij
[mergetool "intellij"]
    cmd = cmd \"/C D:\\workspace\\tools\\symlink\\idea\\bin\\idea.bat merge $(cd $(dirname "$LOCAL") && pwd)/$(basename "$LOCAL") $(cd $(dirname "$REMOTE") && pwd)/$(basename "$REMOTE") $(cd $(dirname "$BASE") && pwd)/$(basename "$BASE") $(cd $(dirname "$MERGED") && pwd)/$(basename "$MERGED")\"
    keepBackup = false
    keepbackup = false
    trustExitCode = true

```

The one gotcha here is that this `cmd` property does not accept any weird characters in the path. If your IDE's install location has weird characters in it (e.g. it's installed in `Program Files (x86)`, you'll have to create a symlink



## Setting up an IntelliJ IDE as diff tool (Windows)


```git
[diff]
    tool = intellij
    guitool = intellij
[difftool "intellij"]
    path = D:/Program Files (x86)/JetBrains/IntelliJ IDEA 2016.2/bin/idea.bat
    cmd = cmd \"/C D:\\workspace\\tools\\symlink\\idea\\bin\\idea.bat diff $(cd $(dirname "$LOCAL") && pwd)/$(basename "$LOCAL") $(cd $(dirname "$REMOTE") && pwd)/$(basename "$REMOTE")\"

```

The one gotcha here is that this `cmd` property does not accept any weird characters in the path. If your IDE's install location has weird characters in it (e.g. it's installed in `Program Files (x86)`, you'll have to create a symlink

