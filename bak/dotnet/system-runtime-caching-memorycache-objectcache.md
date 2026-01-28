---
metaTitle: ".NET Framework - System.Runtime.Caching.MemoryCache (ObjectCache)"
description: "Adding Item to Cache (Set), System.Runtime.Caching.MemoryCache (ObjectCache)"
---

# System.Runtime.Caching.MemoryCache (ObjectCache)



## Adding Item to Cache (Set)


Set function inserts a cache entry into the cache by using a CacheItem instance to supply the key and value for the cache entry.

This function Overrides `ObjectCache.Set(CacheItem, CacheItemPolicy)`

```dotnet
private static bool SetToCache()
{
    string key = "Cache_Key";
    string value = "Cache_Value";

    //Get a reference to the default MemoryCache instance.
    var cacheContainer = MemoryCache.Default; 

    var policy = new CacheItemPolicy()
    {
        AbsoluteExpiration = DateTimeOffset.Now.AddMinutes(DEFAULT_CACHE_EXPIRATION_MINUTES)
     };
     var itemToCache = new CacheItem(key, value); //Value is of type object.
     cacheContainer.Set(itemToCache, policy);                
}

```



## System.Runtime.Caching.MemoryCache (ObjectCache)


This function gets existing item form cache, and if the item don't exist in cache, it will fetch item based on the valueFetchFactory function.

```

   public static TValue GetExistingOrAdd<TValue>(string key, double minutesForExpiration, Func<TValue> valueFetchFactory)
    {            
        try
        {
            //The Lazy class provides Lazy initialization which will evaluate 
            //the valueFetchFactory only if item is not in the cache.
            var newValue = new Lazy<TValue>(valueFetchFactory);

            //Setup the cache policy if item will be saved back to cache.
            CacheItemPolicy policy = new CacheItemPolicy()
            {
                AbsoluteExpiration = DateTimeOffset.Now.AddMinutes(minutesForExpiration)
            };

            //returns existing item form cache or add the new value if it does not exist.
            var cachedItem = _cacheContainer.AddOrGetExisting(key, newValue, policy) as Lazy<TValue>;

            return (cachedItem ?? newValue).Value;
        }
        catch (Exception excep)
        {
            return default(TValue);
        }
    }

```

