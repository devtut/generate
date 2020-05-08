---
metaTitle: "Android - RenderScript"
description: "Getting Started, Blur a View, Blur an image"
---

# RenderScript


RenderScript is a scripting language that allows you to write high performance graphic rendering and raw computational code. It provides a means of writing performance critical code that the system later compiles to native code for the processor it can run on. This could be the CPU, a multi-core CPU, or even the GPU. Which it ultimately runs on depends on many factors that aren't readily available to the developer, but also depends on what architecture the internal platform compiler supports.



## Getting Started


RenderScript is a framework to allow high performance parallel computation on Android. Scripts you write will be executed across all available processors (e.g. CPU, GPU etc) in parallel allowing you to focus on the task you want to achieve instead of how it is scheduled and executed.

Scripts are written in a C99 based language (C99 being an old version of the C programming language standard). For each Script a Java class is created which allows you to easily interact with RenderScript in your Java code.

### Setting up your project

There exist two different ways to access RenderScript in your app, with the Android Framework libraries or the Support Library. Even if you don't want to target devices before API level 11 you should always use the Support Library implementation because it ensures devices compatibility across many different devices. To use the support library implementation you need to use at least build tools version `18.1.0`!

Now lets setup the build.gradle file of your application:

```java
android {
    compileSdkVersion 24
    buildToolsVersion '24.0.1'

    defaultConfig {
        minSdkVersion 8
        targetSdkVersion 24

        renderscriptTargetApi 18
        renderscriptSupportModeEnabled true
    }
}

```


- `renderscriptTargetApi`: This should be set to the version earliest API level which provides all RenderScript functionality you require.
- `renderscriptSupportModeEnabled`: This enables the use of the Support Library RenderScript implementation.

### How RenderScript works

A typical RenderScript consists of two things: Kernels and Functions. A function is just what it sounds like - it accepts an input, does something with that input and returns an output. A Kernel is where the real power of RenderScript comes from.

A Kernel is a function which is executed against every element inside an `Allocation`. An `Allocation` can be used to pass data like a `Bitmap` or a `byte` array to a `RenderScript` and they are also used to get a result from a Kernel. Kernels can either take one `Allocation` as input and another as output or they can modify the data inside just one `Allocation`.

You can write your one Kernels, but there are also many predefined Kernels which you can use to perform common operations like a Gaussian Image Blur.

As already mentioned for every RenderScript file a class is generated to interact with it. These classes always start with the prefix `ScriptC_` followed by the name of the RenderScript file. For example if your RenderScript file is called `example` then the generated Java class will be called `ScriptC_example`. All predefined Scripts just start with the prefix `Script` - for example the Gaussian Image Blur Script is called `ScriptIntrinsicBlur`.

### Writing your first RenderScript

The following example is based on an example on GitHub. It performs basic image manipulation by modifying the saturation of an image. You can find the source code [**here**](https://github.com/Wrdlbrnft/RenderScriptSample) and check it out if you want to play around with it yourself. Here's a quick gif of what the result is supposed to look like:

<img src="https://i.stack.imgur.com/9dNTF.gif" alt="demo picture" />

### RenderScript Boilerplate

RenderScript files reside in the folder `src/main/rs` in your project. Each file has the file extension `.rs` and has to contain two `#pragma` statements at the top:

```java
#pragma version(1)
#pragma rs java_package_name(your.package.name)

```


<li>
`#pragma version(1)`: This can be used to set the version of RenderScript you are using. Currently there is only version 1.
</li>
<li>
`#pragma rs java_package_name(your.package.name)`: This can be used to set the package name of the Java class generated to interact with this particular RenderScript.
</li>

There is another `#pragma` you should usually set in each of your RenderScript files and it is used to set the floating point precision. You can set the floating point precision to three different levels:

- `#pragma rs_fp_full`: This is the strictest setting with the highest precision and it is also the default value if don't specify anything. You should use this if you require high floating point precision.
- `#pragma rs_fp_relaxed`: This is ensures not quite as high floating point precision, but on some architectures it enables a bunch of optimizations which can cause your scripts to run faster.
- `#pragma rs_fp_imprecise`: This ensures even less precision and should be used if floating point precision does not really matter to your script.

Most scripts can just use `#pragma rs_fp_relaxed` unless you really need high floating point precision.

### Global Variables

Now just like in C code you can define global variables or constants:

```java
const static float3 gMonoMult = {0.299f, 0.587f, 0.114f};

float saturationLevel = 0.0f;

```

The variable `gMonoMult` is of type `float3`. This means it is a vector consisting of 3 float numbers. The other `float` variable called `saturationValue` is not constant, therefore you can set it at runtime to a value you like. You can use variables like this in your Kernels or functions and therefore they are another way to give input to or receive output from your RenderScripts. For each not constant variable a getter and setter method will be generated on the associated Java class.

### Kernels

But now lets get started implementing the Kernel. For the purposes of this example I am not going to explain the math used in the Kernel to modify the saturation of the image, but instead will focus on how to implement a Kernel and and how to use it. At the end of this chapter I will quickly explain what the code in this Kernel is actually doing.

### Kernels in general

Let's take a look at the source code first:

```java
uchar4 __attribute__((kernel)) saturation(uchar4 in) {
    float4 f4 = rsUnpackColor8888(in);
    float3 dotVector = dot(f4.rgb, gMonoMult);
    float3 newColor = mix(dotVector, f4.rgb, saturationLevel);
    return rsPackColorTo8888(newColor);
}

```

As you can see it looks like a normal C function with one exception: The `__attribute__((kernel))` between the return type and method name. This is what tells RenderScript that this method is a Kernel. Another thing you might notice is that this method accepts a `uchar4` parameter and returns another `uchar4` value. `uchar4` is - like the `float3` variable we discussed in the chapter before - a vector. It contains 4 `uchar` values which are just byte values in the range from 0 to 255.

You can access these individual values in many different ways, for example `in.r` would return the byte which corresponds to the red channel of a pixel. We use a `uchar4` since each pixel is made up of 4 values - `r` for red, `g` for green, `b` for blue and `a` for alpha - and you can access them with this shorthand. RenderScript also allows you to take any number of values from a vector and create another vector with them. For example `in.rgb` would return a `uchar3` value which just contains the red, green and blue parts of the pixel without the alpha value.

At runtime RenderScript will call this Kernel method for each pixel of an image which is why the return value and parameter are just one `uchar4` value. RenderScript will run many of these calls in parallel on all available processors which is why RenderScript is so powerful. This also means that you don't have to worry about threading or thread safety, you can just implement whatever you want to do to each pixel and RenderScript takes care of the rest.

When calling a Kernel in Java you supply two `Allocation` variables, one which contains the input data and another one which will receive the output. Your Kernel method will be called for each value in the input `Allocation` and will write the result to the output `Allocation`.

### RenderScript Runtime API methods

In the Kernel above a few methods are used which are provided out of the box.  RenderScript provides many such methods and they are vital for almost anything you are going to do with RenderScript. Among them are methods to do math operations like `sin()` and helper methods like `mix()` which mixes two values according to another values. But there are also methods for more complex operations when dealing with vectors, quaternions and matrices.

The official [**RenderScript Runtime API Reference**](https://developer.android.com/guide/topics/renderscript/reference/overview.html) is the best resource out there if you want to know more about a particular method or are looking for a specific method which performs a common operation like calculating the dot product of a matrix. You can find this documentation [**here**](https://developer.android.com/guide/topics/renderscript/reference/overview.html).

### Kernel Implementation

Now let's take a look at the specifics of what this Kernel is doing. Here's the first line in the Kernel:

```java
float4 f4 = rsUnpackColor8888(in);

```

The first line calls the built in method [**`rsUnpackColor8888()`**](https://developer.android.com/guide/topics/renderscript/reference/rs_convert.html#android_rs:rsUnpackColor8888) which transforms the `uchar4` value to a `float4` values. Each color channel is also transformed to the range `0.0f - 1.0f` where `0.0f` corresponds to a byte value of `0` and `1.0f` to `255`. The main purpose of this is to make all the math in this Kernel a lot simpler.

```java
float3 dotVector = dot(f4.rgb, gMonoMult);

```

This next line uses the built in method [**`dot()`**](https://developer.android.com/guide/topics/renderscript/reference/rs_vector_math.html#android_rs:dot) to calculate the dot product of two vectors. `gMonoMult` is a constant value we defined a few chapters above. Since both vectors need to be of the same length to calculate the dot product and also since we just want to affect the color channels and not the alpha channel of a pixel we use the shorthand `.rgb` to get a new `float3` vector which just contains the red, green and blue color channels. Those of us who still remember from school how the dot product works will quickly notice that the dot product should return just one value and not a vector. Yet in the code above we are assigning the result to a `float3` vector. This is again a feature of RenderScript. When you assign a one dimensional number to a vector all elements in the vector will be set to this value. For example the following snippet will assign `2.0f` to each of the three values in the `float3` vector:

```java
float3 example = 2.0f;

```

So the result of the dot product above is assigned to each element in the `float3` vector above.

Now comes the part in which we actually use the global variable `saturationLevel` to modify the saturation of the image:

```java
float3 newColor = mix(dotVector, f4.rgb, saturationLevel);

```

This uses the built in method [**`mix()`**](https://developer.android.com/guide/topics/renderscript/reference/rs_math.html#android_rs:mix) to mix together the original color with the dot product vector we created above. How they are mixed together is determined by the global `saturationLevel` variable. So a `saturationLevel` of `0.0f` will cause the resulting color to have no part of the original color values and will only consist of values in the `dotVector` which results in a black and white or grayed out image. A value of `1.0f` will cause the resulting color to be completely made up of the original color values and values above `1.0f` will multiply the original colors to make them more bright and intense.

```java
return rsPackColorTo8888(newColor);

```

This is the last part in the Kernel. [**`rsPackColorTo8888()`**](https://developer.android.com/guide/topics/renderscript/reference/rs_convert.html#android_rs:rsPackColorTo8888) transforms the `float3` vector back to a `uchar4` value which is then returned. The resulting byte values are clamped to a range between 0 and 255, so float values higher than `1.0f` will result in a byte value of 255 and values lower than `0.0` will result in a byte value of `0`.

And that is the whole Kernel implementation. Now there is only one part remaining: How to call a Kernel in Java.

### Calling RenderScript in Java

### Basics

As was already mentioned above for each RenderScript file a Java class is generated which allows you to interact with the your scripts. These files have the prefix `ScriptC_` followed by the name of the RenderScript file. To create an instance of these classes you first need an instance of the `RenderScript` class:

```java
final RenderScript renderScript = RenderScript.create(context);

```

The static method `create()` can be used to create a `RenderScript` instance from a `Context`. You can then instantiate the Java class which was generated for your script. If you called the RenderScript file `saturation.rs` then the class will be called `ScriptC_saturation`:

```java
final ScriptC_saturation script = new ScriptC_saturation(renderScript);

```

On this class you can now set the saturation level and call the Kernel. The setter which was generated for the `saturationLevel` variable will have the prefix `set_` followed by the name of the variable:

```java
script.set_saturationLevel(1.0f);

```

There is also a getter prefixed with `get_` which allows you to get the saturation level currently set:

```java
float saturationLevel = script.get_saturationLevel();

```

Kernels you define in your RenderScript are prefixed with `forEach_` followed by the name of the Kernel method. The Kernel we have written expects an input `Allocation` and an output `Allocation` as its parameters:

```java
script.forEach_saturation(inputAllocation, outputAllocation);

```

The input `Allocation` needs to contain the input image, and after the `forEach_saturation` method has finished the output allocation will contain the modified image data.

Once you have an `Allocation` instance you can copy data from and to those `Allocations` by using the methods `copyFrom()` and `copyTo()`. For example you can copy a new image into your input `Allocation by calling:

```java
inputAllocation.copyFrom(inputBitmap);

```

The same way you can retrieve the result image by calling `copyTo()` on the output `Allocation`:

```java
outputAllocation.copyTo(outputBitmap);

```

### Creating Allocation instances

There are many ways to create an `Allocation`. Once you have an `Allocation` instance you can copy new data from and to those `Allocations` with `copyTo()` and `copyFrom()` like explained above, but to create them initially you have to know with what kind of data you are exactly working with. Let's start with the input `Allocation`:

We can use the static method `createFromBitmap()` to quickly create our input `Allocation` from a `Bitmap`:

```java
final Allocation inputAllocation = Allocation.createFromBitmap(renderScript, image);

```

In this example the input image never changes so we never need to modify the input `Allocation` again. We can reuse it each time the `saturationLevel` changes to create a new output `Bitmap`.

Creating the output `Allocation` is a little more complex. First we need to create what's called a `Type`. A `Type` is used to tell an `Allocation` with what kind of data it's dealing with. Usually one uses the `Type.Builder` class to quickly create an appropriate `Type`. Let's take a look at the code first:

```java
final Type outputType = new Type.Builder(renderScript, Element.RGBA_8888(renderScript))
        .setX(inputBitmap.getWidth())
        .setY(inputBitmap.getHeight())
        .create();

```

We are working with a normal 32 bit (or in other words 4 byte) per pixel `Bitmap` with 4 color channels. That's why we are choosing `Element.RGBA_8888` to create the `Type`. Then we use the methods `setX()` and `setY()` to set the width and height of the output image to the same size as the input image. The method `create()` then creates the `Type` with the parameters we specified.

Once we have the correct `Type` we can create the output `Allocation` with the static method `createTyped()`:

```java
final Allocation outputAllocation = Allocation.createTyped(renderScript, outputType);

```

Now we are almost done. We also need an output `Bitmap` in which we can copy the data from the output `Allocation`. To do this we use the static method `createBitmap()` to create a new empty `Bitmap` with the same size and configuration as the input `Bitmap`.

```java
final Bitmap outputBitmap = Bitmap.createBitmap(
        inputBitmap.getWidth(),
        inputBitmap.getHeight(),
        inputBitmap.getConfig()
);

```

And with that we have all the puzzle pieces to execute our RenderScript.

### Full example

Now let's put all this together in one example:

```java
// Create the RenderScript instance
final RenderScript renderScript = RenderScript.create(context);

// Create the input Allocation 
final Allocation inputAllocation = Allocation.createFromBitmap(renderScript, inputBitmap);

// Create the output Type.
final Type outputType = new Type.Builder(renderScript, Element.RGBA_8888(renderScript))
        .setX(inputBitmap.getWidth())
        .setY(inputBitmap.getHeight())
        .create();

// And use the Type to create am output Allocation
final Allocation outputAllocation = Allocation.createTyped(renderScript, outputType);

// Create an empty output Bitmap from the input Bitmap
final Bitmap outputBitmap = Bitmap.createBitmap(
        inputBitmap.getWidth(),
        inputBitmap.getHeight(),
        inputBitmap.getConfig()
);

// Create an instance of our script
final ScriptC_saturation script = new ScriptC_saturation(renderScript);

// Set the saturation level
script.set_saturationLevel(2.0f);

// Execute the Kernel
script.forEach_saturation(inputAllocation, outputAllocation);

// Copy the result data to the output Bitmap
outputAllocation.copyTo(outputBitmap);

// Display the result Bitmap somewhere
someImageView.setImageBitmap(outputBitmap);

```

### Conclusion

With this introduction you should be all set to write your own RenderScript Kernels for simple image manipulation. However there are a few things you have to keep in mind:

- **RenderScript only works in Application projects**: Currently RenderScript files cannot be part of a library project.
- **Watch out for memory**: RenderScript is very fast, but it can also be memory intensive. There should never be more than one instance of `RenderScript` at any time. You should also reuse as much as possible. Normally you just need to create your `Allocation` instances once and can reuse them in the future. The same goes for output `Bitmaps` or your script instances. Reuse as much as possible.
- **Do your work in the background**: Again RenderScript is very fast, but not instant in any way. Any Kernel, especially complex ones should be executed off the UI thread in an `AsyncTask` or something similar. However for the most part you don't have to worry about memory leaks. All RenderScript related classes only use the application `Context` and therefore don't cause memory leaks. But you still have to worry about the usual stuff like leaking `View`, `Activity` or any `Context` instance which you use yourself!
- **Use built in stuff**: There are many predefined scripts which perform tasks like image blurring, blending, converting, resizing. And there are many more built in methods which help you implement your kernels. Chances are that if you want to do something there is either a script or method which already does what you are trying to do. Don't reinvent the wheel.

If you want to quickly get started and play around with actual code I recommend you take a look at the example GitHub project which implements the exact example talked about in this tutorial. You can find the project [**here**](https://github.com/Wrdlbrnft/RenderScriptSample). Have fun with RenderScript!



## Blur a View


### BlurBitmapTask.java

```java
public class BlurBitmapTask extends AsyncTask<Bitmap, Void, Bitmap> {
    private final WeakReference<ImageView> imageViewReference;
    private final RenderScript renderScript;

    private boolean shouldRecycleSource = false;

    public BlurBitmapTask(@NonNull Context context, @NonNull ImageView imageView) {
        // Use a WeakReference to ensure
        // the ImageView can be garbage collected
        imageViewReference = new WeakReference<>(imageView);
        renderScript = RenderScript.create(context);
    }

    // Decode image in background.
    @Override
    protected Bitmap doInBackground(Bitmap... params) {
        Bitmap bitmap = params[0];
        return blurBitmap(bitmap);
    }

    // Once complete, see if ImageView is still around and set bitmap.
    @Override
    protected void onPostExecute(Bitmap bitmap) {
        if (bitmap == null || isCancelled()) {
            return;
        }

        final ImageView imageView = imageViewReference.get();
        if (imageView == null) {
            return;
        }

        imageView.setImageBitmap(bitmap);
    }

    public Bitmap blurBitmap(Bitmap bitmap) {
        // https://plus.google.com/+MarioViviani/posts/fhuzYkji9zz

        //Let's create an empty bitmap with the same size of the bitmap we want to blur
        Bitmap outBitmap = Bitmap.createBitmap(bitmap.getWidth(), bitmap.getHeight(),
                Bitmap.Config.ARGB_8888);

        //Instantiate a new Renderscript


        //Create an Intrinsic Blur Script using the Renderscript
        ScriptIntrinsicBlur blurScript = ScriptIntrinsicBlur.create(renderScript, Element.U8_4(renderScript));

        //Create the in/out Allocations with the Renderscript and the in/out bitmaps
        Allocation allIn = Allocation.createFromBitmap(renderScript, bitmap);
        Allocation allOut = Allocation.createFromBitmap(renderScript, outBitmap);

        //Set the radius of the blur
        blurScript.setRadius(25.f);

        //Perform the Renderscript
        blurScript.setInput(allIn);
        blurScript.forEach(allOut);

        //Copy the final bitmap created by the out Allocation to the outBitmap
        allOut.copyTo(outBitmap);

        // recycle the original bitmap
        // nope, we are using the original bitmap as well :/
        if (shouldRecycleSource) {
            bitmap.recycle();
        }

        //After finishing everything, we destroy the Renderscript.
        renderScript.destroy();

        return outBitmap;
    }

    public boolean isShouldRecycleSource() {
        return shouldRecycleSource;
    }

    public void setShouldRecycleSource(boolean shouldRecycleSource) {
        this.shouldRecycleSource = shouldRecycleSource;
    }
}

```

### Usage:

```java
ImageView imageViewOverlayOnViewToBeBlurred
        .setImageDrawable(ContextCompat.getDrawable(this, android.R.color.transparent));
View viewToBeBlurred.setDrawingCacheQuality(View.DRAWING_CACHE_QUALITY_LOW);
viewToBeBlurred.setDrawingCacheEnabled(true);
BlurBitmapTask blurBitmapTask = new BlurBitmapTask(this, imageViewOverlayOnViewToBeBlurred);
blurBitmapTask.execute(Bitmap.createBitmap(viewToBeBlurred.getDrawingCache()));
viewToBeBlurred.setDrawingCacheEnabled(false);

```



## Blur an image


This example demonstrates how to use Renderscript API to blur an image (using Bitmap). This example uses [ScriptInstrinsicBlur](https://developer.android.com/reference/android/renderscript/ScriptIntrinsicBlur.html) provided by android Renderscript API (API >= 17).

```java
public class BlurProcessor {
    
    private RenderScript rs;
    private Allocation inAllocation;
    private Allocation outAllocation;
    private int width;
    private int height;

    private ScriptIntrinsicBlur blurScript;

    public BlurProcessor(RenderScript rs) {
        this.rs = rs;
    }
    
    public void initialize(int width, int height) {
        blurScript = ScriptIntrinsicBlur.create(rs, Element.U8_4(rs));
        blurScript.setRadius(7f); // Set blur radius. 25 is max

        if (outAllocation != null) {
            outAllocation.destroy();
            outAllocation = null;
        }
        
        // Bitmap must have ARGB_8888 config for this type
        Type bitmapType = new Type.Builder(rs, Element.RGBA_8888(rs))
            .setX(width)
            .setY(height)
            .setMipmaps(false) // We are using MipmapControl.MIPMAP_NONE
            .create();
        
        // Create output allocation
        outAllocation = Allocation.createTyped(rs, bitmapType);

        // Create input allocation with same type as output allocation
        inAllocation = Allocation.createTyped(rs, bitmapType);
    }

    public void release() {
        
        if (blurScript != null) {
            blurScript.destroy();
            blurScript = null;
        }
    
        if (inAllocation != null) {
            inAllocation.destroy();
            inAllocation = null;
        }

        if (outAllocation != null) {
            outAllocation.destroy();
            outAllocation = null;
        }
    }

    public Bitmap process(Bitmap bitmap, boolean createNewBitmap) {
        if (bitmap.getWidth() != width || bitmap.getHeight() != height) {
            // Throw error if required
            return null;
        }
        
        // Copy data from bitmap to input allocations
        inAllocation.copyFrom(bitmap);

        // Set input for blur script
        blurScript.setInput(inAllocation);

        // process and set data to the output allocation
        blurScript.forEach(outAllocation);
        
        if (createNewBitmap) {
            Bitmap returnVal = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888);
            outAllocation.copyTo(returnVal);
            return returnVal;
        }

        outAllocation.copyTo(bitmap);
        return bitmap;
    }
}    

```

Each script has a kernel which processes the data and it is generally invoked via `forEach` method.

```java
public class BlurActivity extends AppCompatActivity {
    private BlurProcessor blurProcessor;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        // setup layout and other stuff

        
        blurProcessor = new BlurProcessor(Renderscript.create(getApplicationContext()));
    }

    private void loadImage(String path) {
        // Load image to bitmap
        Bitmap bitmap = loadBitmapFromPath(path);
        
        // Initialize processor for this bitmap
        blurProcessor.release();
        blurProcessor.initialize(bitmap.getWidth(), bitmap.getHeight());
        
        // Blur image
        Bitmap blurImage = blurProcessor.process(bitmap, true); // Use newBitamp as false if you don't want to create a new bitmap
    }
}

```

This concluded the example here. It is advised to do the processing in a background thread.

