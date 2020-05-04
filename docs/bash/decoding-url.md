---
metaTitle: "Decoding URL"
description: "Simple example, Using printf to decode a string"
---

# Decoding URL




## Simple example


**Encoded URL**

> 
http%3A%2F%2Fwww.foo.com%2Findex.php%3Fid%3Dqwerty


**Use this command to decode the URL**

```bash
echo "http%3A%2F%2Fwww.foo.com%2Findex.php%3Fid%3Dqwerty" | sed -e "s/%\([0-9A-F][0-9A-F]\)/\\\\\x\1/g" | xargs -0 echo -e

```

**Decoded URL (result of command)**

> 
[http://www.foo.com/index.php?id=qwerty](http://www.foo.com/index.php?id=qwerty)




## Using printf to decode a string


```bash
#!bin/bash
    
$ string='Question%20-%20%22how%20do%20I%20decode%20a%20percent%20encoded%20string%3F%22%0AAnswer%20%20%20-%20Use%20printf%20%3A)'
$ printf '%b\n' "${string//%/\\x}"

# the result
Question - "how do I decode a percent encoded string?"
Answer   - Use printf :)

```

