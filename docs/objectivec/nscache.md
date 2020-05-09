---
metaTitle: "Objective-C - NSCache"
description: "NSCache"
---

# NSCache




## NSCache


You use it the same way you would use NSMutableDictionary. The difference is that when NSCache detects excessive memory pressure (i.e. it's caching too many values) it will release some of those values to make room.

If you can recreate those values at runtime (by downloading from the Internet, by doing calculations, whatever) then NSCache may suit your needs. If the data cannot be recreated (e.g. it's user input, it is time-sensitive, etc.) then you should not store it in an NSCache because it will be destroyed there.

