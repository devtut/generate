---
metaTitle: "Angular 2 - Update typings"
description: "Update typings when: typings WARN deprecated"
---

# Update typings



## Update typings when: typings WARN deprecated


Warning message:

`typings WARN deprecated 10/25/2016: "registry:dt/jasmine#2.5.0+20161003201800" is deprecated (updated, replaced or removed)`

Update the reference with:

`npm run typings -- install dt~jasmine --save --global`

Replace [jazmine] for any library that is throwing warning

