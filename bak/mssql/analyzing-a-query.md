---
metaTitle: "Microsoft SQL Server - Analyzing a Query"
description: "Scan vs Seek"
---

# Analyzing a Query



## Scan vs Seek


When viewing an execution plan, you may see that SQL Server decided to do a Seek or a Scan.

A Seek occurs when SQL Server knows where it needs to go and only grab specific items.  This typically occurs when good filters on put in a query, such as `where name = 'Foo'`.

A Scan is when SQL Server doesn't know exactly where all of the data it needs is, or decided that the Scan would be more efficient than a Seek if enough of the data is selected.

Seeks are typically faster since they are only grabbing a sub-section of the data, whereas Scans are selecting a majority of the data.

