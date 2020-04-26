# Pillow



## Read Image File


```
from PIL import Image

im = Image.open(&quot;Image.bmp&quot;)

```



## Convert files to JPEG


```
from __future__ import print_function
import os, sys
from PIL import Image

for infile in sys.argv[1:]:
    f, e = os.path.splitext(infile)
    outfile = f + &quot;.jpg&quot;
    if infile != outfile:
        try:
            Image.open(infile).save(outfile)
        except IOError:
            print(&quot;cannot convert&quot;, infile)

```

