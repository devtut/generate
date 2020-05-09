---
metaTitle: "iOS - Cache online images"
description: "AlamofireImage"
---

# Cache online images




## AlamofireImage


Caching Online Images Using `AlamofireImage`. It works on top of `Alamofire` in Swift.
Install `AlamofireImage` using `cocoapods`

```swift
pod 'AlamofireImage', '~> 3.1'

```

**SetUp:**

1. Import `AlamofireImage` and `Alamofire`
<li>SetUp the Image cache:
`let imageCache = AutoPurgingImageCache( memoryCapacity: 111_111_111, preferredMemoryUsageAfterPurge: 90_000_000)`</li>
1. Making a request and adding the Image to Cache:

```swift
Alamofire.request(self.nameUrl[i]).responseImage { response in
                    if response.result.value != nil {
                        let image = UIImage(data: response.data!, scale: 1.0)!
                        imageCache.add(image, withIdentifier: self.nameUrl[i])
                    }
    }

```


1. Retrieve Images From Cache:

```swift
if let image = imageCache.image(withIdentifier: self.nameUrl[self.a])
        {
            self.localImageView.image = image
        }

```

For more Info follow [this link](https://github.com/Alamofire/AlamofireImage)

