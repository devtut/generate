---
metaTitle: "Keyboard shortcuts"
description: "Editing Shortcuts, Recall Shortcuts, Job Control, Macros, Custome Key Bindings"
---

# Keyboard shortcuts



## Editing Shortcuts


|Shortcut|Description
|------
|<kbd>Ctrl</kbd> + <kbd>a</kbd>|move to the beginning of the line
|<kbd>Ctrl</kbd> + <kbd>e</kbd>|move to the end of the line
|<kbd>Ctrl</kbd> + <kbd>k</kbd>|Kill the text from the current cursor position to the end of the line.
|<kbd>Ctrl</kbd> + <kbd>u</kbd>|Kill the text from the current cursor position to the beginning of the line
|<kbd>Ctrl</kbd> + <kbd>w</kbd>|Kill the word behind the current cursor position
|<kbd>Alt</kbd> + <kbd>b</kbd>|move backward one word
|<kbd>Alt</kbd> + <kbd>f</kbd>|move forward one word
|<kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>e</kbd>|shell expand line
|<kbd>Ctrl</kbd> + <kbd>y</kbd>|Yank the most recently killed text back into the buffer at the cursor.
|<kbd>Alt</kbd> + <kbd>y</kbd>|Rotate through killed text. You can only do this if the prior command is <kbd>Ctrl</kbd> + <kbd>y</kbd> or <kbd>Alt</kbd> + <kbd>y</kbd>.

Killing text will delete text, but save it so that the user can reinsert it by yanking. Similar to cut and paste except that the text is placed on a kill ring which allows for storing more than one set of text to be yanked back on to the command line.

You can find out more in the [emacs manual](http://www.gnu.org/software/emacs/manual/html_mono/elisp.html#The-Kill-Ring).



## Recall Shortcuts


|Shortcut|Description
|------
|<kbd>Ctrl</kbd> + <kbd>r</kbd>|search the history backwards
|<kbd>Ctrl</kbd> + <kbd>p</kbd>|previous command in history
|<kbd>Ctrl</kbd> + <kbd>n</kbd>|next command in history
|<kbd>Ctrl</kbd> + <kbd>g</kbd>|quit history searching mode
|<kbd>Alt</kbd> + <kbd>.</kbd>|use the last word of the previous command
||repeat to get the last word of the previous + 1 command
|<kbd>Alt</kbd> + <kbd>n</kbd> <kbd>Alt</kbd> + <kbd>.</kbd>|use the nth word of the previous command
|<kbd>!!</kbd> + <kbd>Return</kbd>|execute the last command again (useful when you forgot sudo: `sudo !!`)



## Job Control


|Shortcut|Description
|------
|<kbd>Ctrl</kbd> + <kbd>c</kbd>|Stop the current job
|<kbd>Ctrl</kbd> + <kbd>z</kbd>|Suspend the current job (send a SIGTSTP signal)



## Macros


|Shortcut|Description
|------
|<kbd>Ctrl</kbd> + <kbd>x</kbd>, <kbd>(</kbd>|start recording a macro
|<kbd>Ctrl</kbd> + <kbd>x</kbd>, <kbd>)</kbd>|stop recording a macro
|<kbd>Ctrl</kbd> + <kbd>x</kbd>, <kbd>e</kbd>|execute the last recorded macro



## Custome Key Bindings


With the `bind` command it is possible to define custom key bindings.

The next example bind an <kbd>Alt</kbd> + <kbd>w</kbd> to `>/dev/null 2>&1`:

```bash
bind '"\ew"':"\" >/dev/null 2>&1\""

```

If you want to execute the line immediately add `\C-m` (<kbd>Enter</kbd>) to it:

```bash
bind '"\ew"':"\" >/dev/null 2>&1\C-m\""

```



#### Remarks


`bind -P` show all configured shortcuts.

