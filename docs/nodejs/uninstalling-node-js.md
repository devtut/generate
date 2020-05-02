---
metaTitle: "Uninstalling Node.js"
description: "Completely uninstall Node.js on Mac OSX, Uninstall Node.js on Windows"
---

# Uninstalling Node.js



## Completely uninstall Node.js on Mac OSX


In Terminal on your Mac operating system, enter the following 2 commands:

```js
lsbom -f -l -s -pf /var/db/receipts/org.nodejs.pkg.bom | while read f; do  sudo rm /usr/local/${f}; done

sudo rm -rf /usr/local/lib/node /usr/local/lib/node_modules /var/db/receipts/org.nodejs.*

```



## Uninstall Node.js on Windows


To uninstall Node.js on Windows, use Add or Remove Programs like this:

1. Open `Add or Remove Programs` from the start menu.
1. Search for `Node.js`.

**Windows 10:**

1. Click Node.js.
1. Click Uninstall.
1. Click the new Uninstall button.

**Windows 7-8.1:**

1. Click the Uninstall button under Node.js.

