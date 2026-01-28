---
metaTitle: "Android - LruCache"
description: "Adding a Bitmap(Resource) to the cache, Initialising the cache, Getting a Bitmap(Resouce) from the cache"
---

# LruCache



## Adding a Bitmap(Resource) to the cache


To add a resource to the cache you must provide a key and the resource. First make sure that the value is not in the cache already

```java
public void addResourceToMemoryCache(String key, Bitmap resource) {
    if (memoryCache.get(key) == null)
        memoryCache.put(key, resource);
}

```



## Initialising the cache


The Lru Cache will store all the added resources (values) for fast access until it reaches a memory limit, in which case it will drop the less used resource (value) to store the new one.

To initialise the Lru cache you need to provide a maximum memory value. This value depends on your application requirements and in how critical the resource is to keep a smooth app usage. A recommended value for an image gallery, for example, would be 1/8 of your maximum available memory.

Also note that the Lru Cache works on a key-value basis. In the following example, the key is a `String` and the value is a `Bitmap`:

```java
int maxMemory = (int) (Runtime.getRuntime().maxMemory() / 1024);
int cacheSize = maxMemory / 8;

LruCache<String, Bitmap> = memoryCache = new LruCache<String, Bitmap>(cacheSize) {
    protected int sizeOf(String key, Bitmap bitmap) {
        return bitmap.getByteCount();
    }
};

```



## Getting a Bitmap(Resouce) from the cache


To get a resource from the cache simply pass the key of your resource (String in this example)

```java
public Bitmap getResourceFromMemoryCache(String key) {
    memoryCache.get(key);
}

```



#### Remarks


You should use Lru Cache in applications where repetitive resource loads would affect a smooth app behaviour. For example a photo gallery with big thumbnails (128x128).

Always be careful with the size of the Lru cache since setting it too high might affect the app.

After the Lru Cache is no longer useful avoid holding any references to it to allow the Garbage Collector to clean it up from memory.

For the best performance remember to load resources like bitmaps using the best practices like selecting a proper inSampleSize before adding it to the Lru cache.

