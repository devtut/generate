---
metaTitle: "Select keyword"
description: "Select keyword can be used for getting input argument in a menu format"
---

# Select keyword


Select keyword can be used for getting input argument in a menu format.



## Select keyword can be used for getting input argument in a menu format


Suppose you want the `user` to `select` keywords from a menu, we can create a script similar to

```bash
#!/usr/bin/env bash

select os in "linux" "windows" "mac"
do
    echo "${os}"
    break
done

```

Explanation:
Here `select` keyword is used to loop through a list of items that will be presented at the command prompt for a user to pick from. Notice the `break` keyword for breaking out of the loop once the user makes a choice. Otherwise, the loop will be endless!

Results:
Upon running this script, a menu of these items will be displayed and the user will be prompted for a selection. Upon selection, the value will be displayed, returning back to command prompt.

```bash
>bash select_menu.sh
1) linux
2) windows
3) mac
#? 3
mac
>

```

