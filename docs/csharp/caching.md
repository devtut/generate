---
metaTitle: "Caching"
description: "MemoryCache"
---

# Caching



## MemoryCache


```cs
//Get instance of cache
using System.Runtime.Caching;

var cache = MemoryCache.Default;

//Check if cache contains an item with
cache.Contains("CacheKey");

//get item from cache
var item = cache.Get("CacheKey");

//get item from cache or add item if not existing
object list = MemoryCache.Default.AddOrGetExisting("CacheKey", "object to be stored", DateTime.Now.AddHours(12));

//note if item not existing the item is added by this method
//but the method returns null

```

