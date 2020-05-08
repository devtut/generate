---
metaTitle: "Android - Universal Image Loader"
description: "Basic usage, Initialize Universal Image Loader"
---

# Universal Image Loader




## Basic usage


<li>
Load an image, decode it into a bitmap, and display the bitmap in an `ImageView` (or any other view which implements the `ImageAware` interface):

```java
ImageLoader.getInstance().displayImage(imageUri, imageView);

```


</li>
<li>
Load an image, decode it into a bitmap, and return the bitmap to a callback:

```java
ImageLoader.getInstance().loadImage(imageUri, new SimpleImageLoadingListener() {
    @Override
    public void onLoadingComplete(String imageUri, View view, Bitmap loadedImage) {
        // Do whatever you want with the bitmap.
    }
});

```


</li>
<li>
Load an image, decode it into a bitmap and return the bitmap synchronously:

```java
Bitmap bmp = ImageLoader.getInstance().loadImageSync(imageUri);

```


</li>



## Initialize Universal Image Loader


<li>
Add the following dependency to the **build.gradle** file:

```java
compile 'com.nostra13.universalimageloader:universal-image-loader:1.9.5'

```


</li>
<li>
Add the following permissions to the **AndroidManifest.xml** file:

```java
<uses-permission android:name="android.permission.INTERNET" />
<uses-permission android:name="android.permission.WRITE_EXTERNAL_STORAGE" />

```


</li>
<li>
Initialize the Universal Image Loader. This must be done before the first usage:

```java
ImageLoaderConfiguration config = new ImageLoaderConfiguration.Builder(this)
    // ...
    .build();
ImageLoader.getInstance().init(config);

```


The full configuration options can be found [here](https://github.com/nostra13/Android-Universal-Image-Loader/wiki/Configuration).
</li>



#### Remarks


Acceptable URI examples:

```java
"http://www.example.com/image.png" // from Web
"file:///mnt/sdcard/image.png" // from SD card
"file:///mnt/sdcard/video.mp4" // from SD card (video thumbnail)
"content://media/external/images/media/13" // from content provider
"content://media/external/video/media/13" // from content provider (video thumbnail)
"assets://image.png" // from assets
"drawable://" + R.drawable.img // from drawables (non-9patch images)

```

