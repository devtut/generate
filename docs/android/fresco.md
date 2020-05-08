---
metaTitle: "Android - Fresco"
description: "Getting Started with Fresco, Using OkHttp 3 with Fresco, JPEG Streaming with Fresco using DraweeController"
---

# Fresco


**Fresco** is a powerful system for displaying images in Android applications.

In Android 4.x and lower, Fresco puts images in a special region of **Android memory** (called ashmem). This lets your application run faster - and suffer the dreaded OutOfMemoryError much less often.

Fresco also supports streaming of JPEGs.



## Getting Started with Fresco


First, add Fresco to your `build.gradle` as shown in the Remarks section:

If you need additional features, like animated GIF or WebP support, you have to add the corresponding [Fresco artifacts](http://frescolib.org/docs/index.html) as well.

Fresco needs to be initialized. You should only do this 1 time, so placing the initialization in your `Application` is a good idea. An example for this would be:

```java
public class MyApplication extends Application {
    @Override
    public void onCreate() {
        super.onCreate();
        Fresco.initialize(this);
    }
}

```

If you want to load remote images from a server, your app needs the internt permission. Simply add it to your `AndroidManifest.xml`:

```java
<uses-permission android:name="android.permission.INTERNET" />

```

Then, add a `SimpleDraweeView` to your XML layout.
Fresco does not support `wrap_content` for image dimensions since you might have multiple images with different dimensions (placeholder image, error image, actual image, ...).

So you can either add a `SimpleDraweeView` with fixed dimensions (or `match_parent`):

```java
<com.facebook.drawee.view.SimpleDraweeView
    android:id="@+id/my_image_view"
    android:layout_width="120dp"
    android:layout_height="120dp"
    fresco:placeholderImage="@drawable/placeholder" />

```

Or supply an **aspect ratio** for your image:

```java
<com.facebook.drawee.view.SimpleDraweeView
    android:id="@+id/my_image_view"
    android:layout_width="120dp"
    android:layout_height="wrap_content"
    fresco:viewAspectRatio="1.33"
    fresco:placeholderImage="@drawable/placeholder" />

```

Finally, you can set your image URI in Java:

```java
SimpleDraweeView draweeView = (SimpleDraweeView) findViewById(R.id.my_image_view);
draweeView.setImageURI("http://yourdomain.com/yourimage.jpg");

```

That's it! You should see your placeholder drawable until the network image has been fetched.



## Using OkHttp 3 with Fresco


First, in addition to the normal Fresco Gradle dependency, you have to add the OkHttp 3 dependency to your `build.gradle`:

```java
compile "com.facebook.fresco:imagepipeline-okhttp3:1.2.0" // Or a newer version.

```

When you initialize Fresco (usually in your custom `Application` implementation), you can now specify your OkHttp client:

```java
OkHttpClient okHttpClient = new OkHttpClient(); // Build on your own OkHttpClient.

Context context = ... // Your Application context.
ImagePipelineConfig config = OkHttpImagePipelineConfigFactory
        .newBuilder(context, okHttpClient)
        .build();
Fresco.initialize(context, config);

```



## JPEG Streaming with Fresco using DraweeController


This example assumes that you have already added Fresco to your app (see [this example](https://stackoverflow.com/documentation/android/5217/fresco/18472/getting-started-with-fresco#t=201708071646034886171)):

```java
SimpleDraweeView img = new SimpleDraweeView(context);
ImageRequest request = ImageRequestBuilder
        .newBuilderWithSource(Uri.parse("http://example.com/image.png"))
        .setProgressiveRenderingEnabled(true) // This is where the magic happens.
        .build();

DraweeController controller = Fresco.newDraweeControllerBuilder()
        .setImageRequest(request)
        .setOldController(img.getController()) // Get the current controller from our SimpleDraweeView.
        .build();

img.setController(controller); // Set the new controller to the SimpleDraweeView to enable progressive JPEGs.

```



#### Remarks


How to set up dependencies in the app level build.gradle file:

```java
dependencies {
    // Your app's other dependencies.
    compile 'com.facebook.fresco:fresco:0.14.1' // Or a newer version if available.
}

```

More information can be found [here](http://frescolib.org/docs/getting-started.html).

