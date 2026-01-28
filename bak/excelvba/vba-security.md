---
metaTitle: "Excel VBA - VBA Security"
description: "Password Protect your VBA"
---

# VBA Security



## Password Protect your VBA


Sometimes you have sensitive information in your VBA (e.g., passwords) that you don't want users to have access to. You can achieve basic security on this information by password-protecting your VBA project.

Follow these steps:

1. Open your Visual Basic Editor (Alt + F11)
1. Navigate to Tools -> VBAProject Properties...
1. Navigate to the Protection tab
1. Check off the "Lock project for viewing" checkbox
1. Enter your desired password in the Password and Confirm Password textboxes

Now when someone wants to access your code within an Office application, they will first need to enter the password. Be aware, however, that even a strong VBA project password is trivial to break.

