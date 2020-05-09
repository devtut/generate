---
metaTitle: "AngularJS - Angular Project - Directory Structure"
description: "Directory Structure"
---

# Angular Project - Directory Structure



## Directory Structure


A common question among new Angular programmers - "What should be the structure of the project?". A good structure helps toward a scalable application development. When we start a project we have two choices, **Sort By Type** (left) and **Sort By Feature** (right). The second is better, especially in large applications, the project becomes a lot easier to manage.

[<img src="http://i.stack.imgur.com/TTloJ.jpg" alt="enter image description here" />](http://i.stack.imgur.com/TTloJ.jpg)

### **Sort By Type** (left)

The application is organized by the files' type.

- **Advantage** - Good for small apps, for programmers only starting to use Angular, and is easy to convert to the second method.
- **Disadvantage** - Even for small apps it starts to get more difficult to find a specific file. For instance, a view and it's controller are in two seperate folders.

### **Sort By Feature** (right)

The suggested organizing method where the filed are sorted by features' type.

All of the layout views and controllers go in the layout folder, the admin content goes in the admin folder, and so on.

- **Advantage** -  When looking for a section of code determining a certain feature it's all located in one folder.
- **Disadvantage** - Services are a bit different as they “service” many features.

You can read more about it on [Angular Structure: Refactoring for Growth](https://johnpapa.net/angular-growth-structure/)

The suggested file structure combining both of the aforementioned methods:

[<img src="http://i.stack.imgur.com/nxXRu.png" alt="enter image description here" />](http://i.stack.imgur.com/nxXRu.png)

**Credit to:** [Angular Style Guide](https://github.com/mgechev/angularjs-style-guide#directory-structure)

