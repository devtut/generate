---
metaTitle: "Android - Bitmap Cache"
description: "Bitmap Cache Using LRU Cache"
---

# Bitmap Cache


Memory efficient bitmap caching:
This is particularly important if your application uses animations as they will be stopped during GC cleanup and make your application appears sluggish to the user.
A cache allows reusing objects which are expensive to create. If you load on object into memory, you can think of this as a cache for the object.Working with bitmap in android is tricky.It is more important to cache the bimap if you are going to use it repeatedly.



## Bitmap Cache Using LRU Cache


**LRU Cache**

The following example code demonstrates a possible implementation of the LruCache class for caching images.

```java
private LruCache<String, Bitmap> mMemoryCache;

```

Here string value is key for bitmap value.

```java
// Get max available VM memory, exceeding this amount will throw an
// OutOfMemory exception. Stored in kilobytes as LruCache takes an
// int in its constructor.
final int maxMemory = (int) (Runtime.getRuntime().maxMemory() / 1024);

// Use 1/8th of the available memory for this memory cache.
final int cacheSize = maxMemory / 8;

mMemoryCache = new LruCache<String, Bitmap>(cacheSize) {
    @Override
    protected int sizeOf(String key, Bitmap bitmap) {
        // The cache size will be measured in kilobytes rather than
        // number of items.
        return bitmap.getByteCount() / 1024;
    }
};

```

**For add bitmap to the memory cache**

```java
public void addBitmapToMemoryCache(String key, Bitmap bitmap) {
if (getBitmapFromMemCache(key) == null) {
        mMemoryCache.put(key, bitmap);
    }    
}

```

**For get bitmap from memory cache**

```java
public Bitmap getBitmapFromMemCache(String key) {
    return mMemoryCache.get(key);
}

```

For loading bitmap into imageview just use **getBitmapFromMemCache("Pass key").**



#### Syntax


- `LruCache<String, Bitmap> mMemoryCache;//declaration of LruCache object.`
- void addBitmapToMemoryCache(String key, Bitmap bitmap){}//declaration of generic method adding bitmap into cache memory
- Bitmap getBitmapFromMemCache(String key){}//declaration of generic method for get bimap from cache.



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|key|key to store bitmap in memory cache
|bitmap|bitmap value which will cache into memory

