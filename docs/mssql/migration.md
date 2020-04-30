---
metaTitle: "Migration"
description: "How to generate migration scripts"
---

# Migration



## How to generate migration scripts


1. Click <kbd>Right Mouse</kbd> on Database you want to migrate then -> `Tasks` -> `Generate Scripts...`

[<img src="http://i.stack.imgur.com/Ip4P0.png" alt="enter image description here" />](http://i.stack.imgur.com/Ip4P0.png)

1. Wizard will open click `Next` then chose objects you want to migrate and click `Next` again, then click `Advanced` scroll a bit down and in `Types of data to script` choose `Schema and data` (unless you want only structures)

[<img src="http://i.stack.imgur.com/SOjkp.png" alt="enter image description here" />](http://i.stack.imgur.com/SOjkp.png)

<li>
Click couple more times `Next` and `Finish` and you should have your database scripted in `.sql` file.
</li>
<li>
run `.sql` file on your new server, and you should be done.
</li>

