---
metaTitle: "Show"
description: "Overview"
---

# Show



## Overview


`git show` shows various Git objects.

### For commits:

Shows the commit message and a diff of the changes introduced.

|Command|Description
|------
|`git show`|shows the previous commit
|`git show @~3`|shows the 3rd-from-last commit

### For trees and blobs:

Shows the tree or blob.

|Command|Description
|------
|`git show @~3:`|shows the project root directory as it was 3 commits ago (a tree)
|`git show @~3:src/program.js`|shows `src/program.js` as it was 3 commits ago (a blob)
|`git show @:a.txt @:b.txt`|shows `a.txt` concatenated with `b.txt` from current commit

### For tags:

Shows the tag message and the referenced object.



#### Syntax


- git show [options] <object>...



#### Remarks


Shows various Git objects.

- For commits, shows the commit message and diff
- For tags, shows the tag message and referenced object

