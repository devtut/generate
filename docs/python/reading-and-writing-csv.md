---
metaTitle: "Reading and Writing CSV"
description: "Writing a TSV file, Using pandas"
---

# Reading and Writing CSV



## Writing a TSV file


### Python

```
import csv

with open('/tmp/output.tsv', 'wt') as out_file:
    tsv_writer = csv.writer(out_file, delimiter='\t')
    tsv_writer.writerow(['name', 'field'])
    tsv_writer.writerow(['Dijkstra', 'Computer Science'])
    tsv_writer.writerow(['Shelah', 'Math'])
    tsv_writer.writerow(['Aumann', 'Economic Sciences'])

```

### Output file

```
$ cat /tmp/output.tsv

name    field
Dijkstra    Computer Science
Shelah    Math
Aumann    Economic Sciences

```



## Using pandas


Write a CSV file from a `dict` or a `DataFrame`.

```
import pandas as pd

d = {'a': (1, 101), 'b': (2, 202), 'c': (3, 303)}
pd.DataFrame.from_dict(d, orient="index")
df.to_csv("data.csv")

```

Read a CSV file as a `DataFrame` and convert it to a `dict`:

```
df = pd.read_csv("data.csv")
d = df.to_dict()

```

