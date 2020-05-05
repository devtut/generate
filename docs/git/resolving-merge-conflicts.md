---
metaTitle: "Git - Resolving merge conflicts"
description: "Manual Resolution"
---

# Resolving merge conflicts



## Manual Resolution


While performing a `git merge` you may find that git reports a "merge conflict" error. It will report to you which files have conflicts, and you will need to resolve the conflicts.

A `git status` at any point will help you see what still needs editing with a helpful message like

```git
On branch master
You have unmerged paths.
  (fix conflicts and run "git commit")

Unmerged paths:
  (use "git add <file>..." to mark resolution)

    both modified:      index.html

no changes added to commit (use "git add" and/or "git commit -a")

```

Git leaves markers in the files to tell you where the conflict arose:

```git
<<<<<<<<< HEAD: index.html #indicates the state of your current branch
<div id="footer">contact : email@somedomain.com</div>
========= #indicates break between conflicts
<div id="footer">
please contact us at email@somedomain.com
</div>
>>>>>>>>> iss2: index.html #indicates the state of the other branch (iss2)

```

In order to resolve the conflicts, you must edit the area between the <<<<<< and >>>>>>> markers appropriately, remove the status lines (the <<<<<<<, >>>>>>>, and ======== lines) completely. Then `git add index.html` to mark it resolved and `git commit` to finish the merge.

