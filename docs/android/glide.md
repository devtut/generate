---
metaTitle: "Android - Glide"
description: "Loading an image, Add Glide to your project, Glide circle transformation (Load image in a circular ImageView), Default transformations, Glide rounded corners image with custom Glide target, Placeholder and Error handling, Preloading images, Handling Glide image load failed, Load image in a circular ImageView without custom transformations"
---

# Glide


**** WARNING This documentation is unmaintained and frequently inaccurate ****

Glide's official documentation is a much better source:

For Glide v4, see [http://bumptech.github.io/glide/](http://bumptech.github.io/glide/).
For Glide v3, see [https://github.com/bumptech/glide/wiki](https://github.com/bumptech/glide/wiki).



## Loading an image


### ImageView

To load an image from a specified URL, Uri, resource id, or any other model into an `ImageView`:

```java
ImageView imageView = (ImageView) findViewById(R.id.imageView);
String yourUrl = "http://www.yoururl.com/image.png";

Glide.with(context)
    .load(yourUrl)
    .into(imageView);

```

For Uris, replace `yourUrl` with your Uri (`content://media/external/images/1`). For Drawables replace `yourUrl` with your resource id (`R.drawable.image`).

### RecyclerView and ListView

In ListView or RecyclerView, you can use exactly the same lines:

```java
@Override
public void onBindViewHolder(RecyclerView.ViewHolder viewHolder, int position) {
    MyViewHolder myViewHolder = (MyViewHolder) viewHolder;
    String currentUrl = myUrls.get(position); 

    Glide.with(context)
        .load(currentUrl)
        .into(myViewHolder.imageView);
}

```

If you don't want to start a load in `onBindViewHolder`, make sure you `clear()` any `ImageView` Glide may be managing before modifying the `ImageView`:

```java
@Override
public void onBindViewHolder(RecyclerView.ViewHolder viewHolder, int position) {
    MyViewHolder myViewHolder = (MyViewHolder) viewHolder;
    String currentUrl = myUrls.get(position); 

    if (TextUtils.isEmpty(currentUrl)) {
        Glide.clear(viewHolder.imageView);
        // Now that the view has been cleared, you can safely set your own resource
        viewHolder.imageView.setImageResource(R.drawable.missing_image);
    } else {
        Glide.with(context)
            .load(currentUrl)
            .into(myViewHolder.imageView);
    }
}

```



## Add Glide to your project


From the [official documentation](https://github.com/bumptech/glide):

With Gradle:

```java
repositories {
  mavenCentral() // jcenter() works as well because it pulls from Maven Central
}

dependencies {
  compile 'com.github.bumptech.glide:glide:4.0.0'
  compile 'com.android.support:support-v4:25.3.1'
  annotationProcessor 'com.github.bumptech.glide:compiler:4.0.0'
}

```

With Maven:

```java
<dependency>
  <groupId>com.github.bumptech.glide</groupId>
  <artifactId>glide</artifactId>
  <version>4.0.0</version>
</dependency>
<dependency>
  <groupId>com.google.android</groupId>
  <artifactId>support-v4</artifactId>
  <version>r7</version>
</dependency>
<dependency>
  <groupId>com.github.bumptech.glide</groupId>
  <artifactId>compiler</artifactId>
  <version>4.0.0</version>
  <optional>true</optional>
</dependency>

```

Depending on your ProGuard (DexGuard) config and usage, you may also need to include the following lines in your proguard.cfg (See [Glide's wiki](https://github.com/bumptech/glide/wiki/Configuration#keeping-a-glidemodule) for more info):

```java
-keep public class * implements com.bumptech.glide.module.GlideModule
-keep public class * extends com.bumptech.glide.AppGlideModule
-keep public enum com.bumptech.glide.load.resource.bitmap.ImageHeaderParser$**     {
  **[] $VALUES;
  public *;
}

# for DexGuard only
-keepresourcexmlelements manifest/application/meta-data@value=GlideModule

```



## Glide circle transformation (Load image in a circular ImageView)


Create a circle image with glide.

```java
public class CircleTransform extends BitmapTransformation {

    public CircleTransform(Context context) {
        super(context);
    }

    @Override protected Bitmap transform(BitmapPool pool, Bitmap toTransform, int outWidth, int outHeight) {
        return circleCrop(pool, toTransform);
    }

    private static Bitmap circleCrop(BitmapPool pool, Bitmap source) {
        if (source == null) return null;

        int size = Math.min(source.getWidth(), source.getHeight());
        int x = (source.getWidth() - size) / 2;
        int y = (source.getHeight() - size) / 2;

        Bitmap squared = Bitmap.createBitmap(source, x, y, size, size);

        Bitmap result = pool.get(size, size, Bitmap.Config.ARGB_8888);
        if (result == null) {
            result = Bitmap.createBitmap(size, size, Bitmap.Config.ARGB_8888);
        }

        Canvas canvas = new Canvas(result);
        Paint paint = new Paint();
        paint.setShader(new BitmapShader(squared, BitmapShader.TileMode.CLAMP, BitmapShader.TileMode.CLAMP));
        paint.setAntiAlias(true);
        float r = size / 2f;
        canvas.drawCircle(r, r, r, paint);
        return result;
    }

    @Override public String getId() {
        return getClass().getName();
    }
}

```

**Usage:**

```java
Glide.with(context)
    .load(yourimageurl)
    .transform(new CircleTransform(context))
    .into(userImageView);

```



## Default transformations


Glide includes two default transformations, fit center and center crop.

**Fit center:**

```java
Glide.with(context)
    .load(yourUrl)
    .fitCenter()
    .into(yourView);

```

Fit center performs the same transformation as Android's [ScaleType.FIT_CENTER](http://developer.android.com/reference/android/widget/ImageView.ScaleType.html).

**Center crop:**

```java
Glide.with(context)
    .load(yourUrl)
    .centerCrop()
    .into(yourView);

```

Center crop performs the same transformation as Android's [ScaleType.CENTER_CROP](https://developer.android.com/reference/android/widget/ImageView.ScaleType.html).

For more information, see [Glide's wiki](https://github.com/bumptech/glide/wiki/Transformations#default-transformations).



## Glide rounded corners image with custom Glide target


First make utility class or use this method in class needed

```java
public class UIUtils {
public static BitmapImageViewTarget getRoundedImageTarget(@NonNull final Context context, @NonNull final ImageView imageView,
                                                        final float radius) {
    return new BitmapImageViewTarget(imageView) {
        @Override
        protected void setResource(final Bitmap resource) {
            RoundedBitmapDrawable circularBitmapDrawable =
                    RoundedBitmapDrawableFactory.create(context.getResources(), resource);
            circularBitmapDrawable.setCornerRadius(radius);
            imageView.setImageDrawable(circularBitmapDrawable);
        }
    };
}

```

Loading image:

```java
Glide.with(context)
     .load(imageUrl)
     .asBitmap()
     .into(UIUtils.getRoundedImageTarget(context, imageView, radius));

```

Because you use asBitmap() the animations will be removed though.
You can use your own animation in this place using the animate() method.

Example with similar fade in to default Glide animation.

```java
Glide.with(context)
     .load(imageUrl)
     .asBitmap()
     .animate(R.anim.abc_fade_in)
     .into(UIUtils.getRoundedImageTarget(context, imageView, radius));

```

Please note this animation is support library private resource - it is unrecommended to use as it can change or even be removed.

Note you also need to have support library to use [RoundedBitmapDrawableFactory](https://developer.android.com/reference/android/support/v4/graphics/drawable/RoundedBitmapDrawableFactory.html)



## Placeholder and Error handling


If you want to add a Drawable be shown during the load, you can add a placeholder:

```java
Glide.with(context)
    .load(yourUrl)
    .placeholder(R.drawable.placeholder)
    .into(imageView);

```

If you want a Drawable to be shown if the load fails for any reason:

```java
Glide.with(context)
    .load(yourUrl)
    .error(R.drawable.error)
    .into(imageView);

```

If you want a Drawable to be shown if you provide a null model (URL, Uri, file path etc):

```java
Glide.with(context)
    .load(maybeNullUrl)
    .fallback(R.drawable.fallback)
    .into(imageView);

```



## Preloading images


To preload remote images and ensure that the image is only downloaded once:

```java
Glide.with(context)
    .load(yourUrl)
    .diskCacheStrategy(DiskCacheStrategy.SOURCE)
    .preload();

```

Then:

```java
Glide.with(context)
    .load(yourUrl)
    .diskCacheStrategy(DiskCacheStrategy.SOURCE) // ALL works here too
    .into(imageView);

```

To preload local images and make sure a transformed copy is in the disk cache (and maybe the memory cache):

```java
Glide.with(context)
    .load(yourFilePathOrUri)
    .fitCenter() // Or whatever transformation you want
    .preload(200, 200); // Or whatever width and height you want

```

Then:

```java
Glide.with(context)
    .load(yourFilePathOrUri)
    .fitCenter() // You must use the same transformation as above
    .override(200, 200) // You must use the same width and height as above
    .into(imageView);

```



## Handling Glide image load failed


```java
Glide
        .with(context)
        .load(currentUrl)
        .into(new BitmapImageViewTarget(profilePicture) {
    @Override
    protected void setResource(Bitmap resource) {
        RoundedBitmapDrawable circularBitmapDrawable =
                RoundedBitmapDrawableFactory.create(context.getResources(), resource);
        circularBitmapDrawable.setCornerRadius(radius);
        imageView.setImageDrawable(circularBitmapDrawable);
    }

    @Override
    public void onLoadFailed(@NonNull Exception e, Drawable errorDrawable) {
        super.onLoadFailed(e, SET_YOUR_DEFAULT_IMAGE);
        Log.e(TAG, e.getMessage(), e);
    }
});

```

Here at `SET_YOUR_DEFAULT_IMAGE` place you can set any default `Drawable`. This image will be shown if Image loading is failed.



## Load image in a circular ImageView without custom transformations


Create a custom BitmapImageViewTarget to load the image into:

```java
public class CircularBitmapImageViewTarget extends BitmapImageViewTarget
{
    private Context context;
    private ImageView imageView;

    public CircularBitmapImageViewTarget(Context context, ImageView imageView)
    {
        super(imageView);
        this.context = context;
        this.imageView = imageView;
    }
    
    @Override
    protected void setResource(Bitmap resource)
    {
        RoundedBitmapDrawable bitmapDrawable = RoundedBitmapDrawableFactory.create(context.getResources(), resource);
        bitmapDrawable.setCircular(true);
        imageView.setImageDrawable(bitmapDrawable);
    }
}

```

Usage:

```java
Glide
    .with(context)
    .load(yourimageidentifier)
    .asBitmap()
    .into(new CircularBitmapImageViewTarget(context, imageView));

```



#### Remarks


**Glide** is a fast and efficient open source media management and image loading framework for Android that wraps media decoding, memory and disk caching, and resource pooling into a simple and easy to use interface.

Glide supports fetching, decoding, and displaying video stills, images, and animated GIFs. Glide includes a flexible API that allows developers to plug in to almost any network stack.<br />
By default Glide uses a custom [HttpUrlConnection](https://developer.android.com/reference/java/net/HttpURLConnection.html) based stack, but also includes utility libraries plug in to [Google's Volley](http://stackoverflow.com/documentation/android/2800/volley#t=201607241026109757042) project or [Square's OkHttp library](http://stackoverflow.com/documentation/android/2172/picasso#t=201607241025472569018) instead.

Glide's primary focus is on making scrolling any kind of a list of images as smooth and fast as possible, but Glide is also effective for almost any case where you need to fetch, resize, and display a remote image.

Source code and further documentation is available on GitHub: [https://github.com/bumptech/glide](https://github.com/bumptech/glide)

