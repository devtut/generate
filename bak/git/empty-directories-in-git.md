---
metaTitle: "Git - Empty directories in Git"
description: "Git doesn't track directories"
---

# Empty directories in Git



## Git doesn't track directories


Assume you've initialized a project with the following directory structure:

Then you add everything so you've created so far and commit:

Git will only track the file app.js.

Assume you added a build step to your application and rely on the "build" directory to be there as the output directory (and you don't want to make it a setup instruction every developer has to follow), a **convention** is to include a ".gitkeep" file inside the directory and let Git track that file.

Then add this new file:

Git will now track the file build/.gitkeep file and therefore the build folder will be made available on checkout.

Again, this is just a convention and not a Git feature.

