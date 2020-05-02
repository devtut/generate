---
metaTitle: "Git Branch Name on Bash Ubuntu"
description: "Branch Name in terminal"
---

# Git Branch Name on Bash Ubuntu




## Branch Name in terminal


**What is PS1**

PS1 denotes Prompt String 1. It is the one of the prompt available in Linux/UNIX shell. When you open your terminal, it will display the content defined in PS1 variable in your bash prompt. In order to add branch name to bash prompt we have to edit the PS1 variable(set value of PS1 in ~/.bash_profile).

**Display git branch name**

Add following lines to your ~/.bash_profile

```git
git_branch() {
 git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}
export PS1="\u@\h \[\033[32m\]\w\[\033[33m\]\$(git_branch)\[\033[00m\] $ "

```

This git_branch function will find the branch name we are on.
Once we are done with this changes we can nevigate to the git repo on the terminal and will be able to see the branch name.

