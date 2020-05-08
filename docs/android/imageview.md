---
metaTitle: "Android - ImageView"
description: "Set tint, Set alpha, Set Scale Type, Set Image Resource, ImageView ScaleType - Center, ImageView ScaleType - CenterCrop, ImageView ScaleType - CenterInside, ImageView ScaleType - FitStart and FitEnd, ImageView ScaleType - FitCenter, ImageView ScaleType - FitXy, MLRoundedImageView.java"
---

# ImageView


ImageView (`android.widget.ImageView`) is a View for displaying and manipulating image resources, such as Drawables and Bitmaps.

Some effects, discussed in this topic, can be applied to the image. The image source can be set in XML file (`layout` folder) or by programatically in Java code.



## Set tint


Set a tinting color for the image. By default, the tint will blend using `SRC_ATOP` mode.

set tint using XML attribute:

```java
android:tint="#009c38"

```

**Note:** Must be a color value, in the form of `"#rgb"`, `"#argb"`, `"#rrggbb"`, or `"#aarrggbb"`.

set tint programmatically:

```java
imgExample.setColorFilter(Color.argb(255, 0, 156, 38));

```

and you can clear this color filter:

```java
imgExample.clearColorFilter();

```

**Example:**

[<img src="http://i.stack.imgur.com/0dT7q.png" alt="enter image description here" />](http://i.stack.imgur.com/0dT7q.png)



## Set alpha


"alpha" is used to specify the opacity for an image.

set alpha using XML attribute:

```java
android:alpha="0.5"  

```

**Note:** takes float value from 0 (transparent) to 1 (fully visible)

set alpha programmatically:

```java
imgExample.setAlpha(0.5f);

```

[<img src="https://i.stack.imgur.com/Mtvee.png" alt="enter image description here" />](https://i.stack.imgur.com/Mtvee.png)



## Set Scale Type


Controls how the image should be resized or moved to match the size of `ImageView`.

XML attribute:

```java
android:scaleType="..."

```

i will illustrate different scale types with a square `ImageView` which has a black background and we want to display a rectangular drawable in white background in `ImageView`.

```

<ImageView
  android:id="@+id/imgExample"
  android:layout_width="200dp"
  android:layout_height="200dp"
  android:background="#000" 
  android:src="@drawable/android2"
  android:scaleType="..."/>

```

scaleType must be one of the following values:

1. `center`:Center the image in the view, but perform no scaling.

[<img src="http://i.stack.imgur.com/dP3mv.png" alt="enter image description here" />](http://i.stack.imgur.com/dP3mv.png)

1. `centerCrop`: Scale the image uniformly (maintain the image's aspect ratio) so both dimensions (width and height) of the image will be equal to or larger than the corresponding dimension of the view (minus padding). The image is then centered in the view.

[<img src="http://i.stack.imgur.com/ZRcGg.png" alt="enter image description here" />](http://i.stack.imgur.com/ZRcGg.png)

1. `centerInside`: Scale the image uniformly (maintain the image's aspect ratio) so that both dimensions (width and height) of the image will be equal to or less than the corresponding dimension of the view (minus padding). The image is then centered in the view.

[<img src="http://i.stack.imgur.com/Xtlr4.png" alt="enter image description here" />](http://i.stack.imgur.com/Xtlr4.png)

1. `matrix` : Scale using the image matrix when drawing.

[<img src="http://i.stack.imgur.com/tLTMx.png" alt="enter image description here" />](http://i.stack.imgur.com/tLTMx.png)

1. `fitXY`: Scale the image using [FILL](https://developer.android.com/reference/android/graphics/Matrix.ScaleToFit.html#FILL).

[<img src="http://i.stack.imgur.com/eVrlt.png" alt="enter image description here" />](http://i.stack.imgur.com/eVrlt.png)

1. `fitStart`: Scale the image using [START](https://developer.android.com/reference/android/graphics/Matrix.ScaleToFit.html#START).

[<img src="http://i.stack.imgur.com/9DRc1.png" alt="enter image description here" />](http://i.stack.imgur.com/9DRc1.png)

1. `fitCenter`: Scale the image using [CENTER](https://developer.android.com/reference/android/graphics/Matrix.ScaleToFit.html#CENTER).

[<img src="http://i.stack.imgur.com/XqB7I.png" alt="enter image description here" />](http://i.stack.imgur.com/XqB7I.png)

1. `fitEnd`: Scale the image using [END](https://developer.android.com/reference/android/graphics/Matrix.ScaleToFit.html#END).

[<img src="http://i.stack.imgur.com/Idwdh.png" alt="enter image description here" />](http://i.stack.imgur.com/Idwdh.png)



## Set Image Resource


```java
<ImageView
 android:id="@+id/imgExample"
 android:layout_width="wrap_content"
 android:layout_height="wrap_content"
 ...
 />

```

set a drawable as content of `ImageView` using XML attribute:

```java
android:src="@drawable/android2"  

```

set a drawable programmatically:

```

ImageView imgExample = (ImageView) findViewById(R.id.imgExample);
 imgExample.setImageResource(R.drawable.android2);

```



## ImageView ScaleType - Center


The image contained in the ImageView may not fit the exact size given to the container. In that case, the framework allows you to resize the image in a number of ways.

**Center**

```

   <ImageView android:layout_width="20dp"
           android:layout_height="20dp"
           android:src="@mipmap/ic_launcher"
           android:id="@+id/imageView"
           android:scaleType="center"
           android:background="@android:color/holo_orange_light"/>

```

This will not resize the image, and it will center it inside the container
**(Orange = container)**

[<img src="https://i.stack.imgur.com/1ORfG.png" alt="Center" />](https://i.stack.imgur.com/1ORfG.png)

In case that the ImageView is smaller than the image, the image will not be resized and you will only be able to see a part of it

[<img src="https://i.stack.imgur.com/b7hZS.png" alt="Center image bigger than imageView" />](https://i.stack.imgur.com/b7hZS.png)

**strong text**



## ImageView ScaleType - CenterCrop


Scale the image uniformly (maintain the image's aspect ratio) so that both dimensions (width and height) of the image will be equal to or larger than the corresponding dimension of the view (minus padding).

[Official Docs](https://developer.android.com/reference/android/widget/ImageView.ScaleType.html)

When the image matches the proportions of the container:

[<img src="http://i.stack.imgur.com/1se4G.png" alt="enter image description here" />](http://i.stack.imgur.com/1se4G.png)

When the image is wider than the container it will expand it to the bigger size (in this case height) and adjust the width of the image without changing it's proportions, causing it to crop.

[<img src="http://i.stack.imgur.com/81ohC.png" alt="enter image description here" />](http://i.stack.imgur.com/81ohC.png)



## ImageView ScaleType - CenterInside


Scale the image uniformly (maintain the image's aspect ratio) so that both dimensions (width and height) of the image will be equal to or less than the corresponding dimension of the view (minus padding).

[Official Docs](https://developer.android.com/reference/android/widget/ImageView.ScaleType.html)

It will center the image and resize it to the smaller size, if both container sizes are bigger it will act the same as center.

[<img src="http://i.stack.imgur.com/osAmy.png" alt="center inside 1" />](http://i.stack.imgur.com/osAmy.png)

But if one of the sizes are small, it will fit to that size.

[<img src="http://i.stack.imgur.com/R6bh5.png" alt="center inside 2" />](http://i.stack.imgur.com/R6bh5.png)



## ImageView ScaleType - FitStart and FitEnd


Scale the image using START.

Scale the image using END.

[Official Docs](https://developer.android.com/reference/android/widget/ImageView.ScaleType.html)

**FitStart**

This will fit to the smallest size of the container, and it will align it to the start.

```java
<ImageView android:layout_width="200dp"
           android:layout_height="200dp"
           android:src="@mipmap/ic_launcher"
           android:id="@+id/imageView"
           android:scaleType="fitStart"
           android:layout_gravity="center"
           android:background="@android:color/holo_orange_light"/>

```

[<img src="http://i.stack.imgur.com/a5bmE.png" alt="fit to top" />](http://i.stack.imgur.com/a5bmE.png)

[<img src="http://i.stack.imgur.com/OfVmD.png" alt="fit to left" />](http://i.stack.imgur.com/OfVmD.png)

**FitEnd**

This will fit to the smallest size of the container, and it will align it to the end.

```java
<ImageView android:layout_width="200dp"
           android:layout_height="100dp"
           android:src="@mipmap/ic_launcher"
           android:id="@+id/imageView"
           android:scaleType="fitEnd"
           android:layout_gravity="center"
           android:background="@android:color/holo_orange_light"/>

```

[<img src="http://i.stack.imgur.com/NwOJG.png" alt="fit to bottom" />](http://i.stack.imgur.com/NwOJG.png)

[<img src="http://i.stack.imgur.com/CuXz3.png" alt="fit to right" />](http://i.stack.imgur.com/CuXz3.png)



## ImageView ScaleType - FitCenter


Scale the image using CENTER.

[Official Docs](https://developer.android.com/reference/android/widget/ImageView.ScaleType.html)

This expands the image to try to match the container and it will align it to the center, it will fit to the smaller size.

Bigger height ( fit to width )

[<img src="http://i.stack.imgur.com/MUZRF.png" alt="enter image description here" />](http://i.stack.imgur.com/MUZRF.png)

Same width and height.

[<img src="http://i.stack.imgur.com/IZfTx.png" alt="enter image description here" />](http://i.stack.imgur.com/IZfTx.png)



## ImageView ScaleType - FitXy


Scale the image using FILL.

[Official Docs](https://developer.android.com/reference/android/widget/ImageView.ScaleType.html)

```java
<ImageView android:layout_width="100dp"
           android:layout_height="200dp"
           android:src="@mipmap/ic_launcher"
           android:id="@+id/imageView"
           android:scaleType="fitXY"
           android:layout_gravity="center"
           android:background="@android:color/holo_orange_light"/>

```

[<img src="http://i.stack.imgur.com/kw2Nj.png" alt="enter image description here" />](http://i.stack.imgur.com/kw2Nj.png)

[<img src="http://i.stack.imgur.com/lLDGC.png" alt="enter image description here" />](http://i.stack.imgur.com/lLDGC.png)

[<img src="http://i.stack.imgur.com/ou0iQ.png" alt="enter image description here" />](http://i.stack.imgur.com/ou0iQ.png)



## MLRoundedImageView.java


Copy and Paste following class in your package:

```java
public class MLRoundedImageView extends ImageView {

    public MLRoundedImageView(Context context) {
        super(context);
    }

    public MLRoundedImageView(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public MLRoundedImageView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
    }

    @Override
    protected void onDraw(Canvas canvas) {

        Drawable drawable = getDrawable();

        if (drawable == null) {
            return;
        }

        if (getWidth() == 0 || getHeight() == 0) {
            return;
        }
        Bitmap b = ((BitmapDrawable) drawable).getBitmap();
        Bitmap bitmap = b.copy(Bitmap.Config.ARGB_8888, true);

        int w = getWidth(), h = getHeight();

        Bitmap roundBitmap = getCroppedBitmap(bitmap, w);
        canvas.drawBitmap(roundBitmap, 0, 0, null);

    }

    public static Bitmap getCroppedBitmap(Bitmap bmp, int radius) {
        Bitmap sbmp;
        
        if (bmp.getWidth() != radius || bmp.getHeight() != radius) {
            float smallest = Math.min(bmp.getWidth(), bmp.getHeight());
            float factor = smallest / radius;
            sbmp = Bitmap.createScaledBitmap(bmp, (int)(bmp.getWidth() / factor), (int)(bmp.getHeight() / factor), false);
        } else {
            sbmp = bmp;
        }
        
        Bitmap output = Bitmap.createBitmap(radius, radius,
                Config.ARGB_8888);
        Canvas canvas = new Canvas(output);

        final int color = 0xffa19774;
        final Paint paint = new Paint();
        final Rect rect = new Rect(0, 0, radius, radius);

        paint.setAntiAlias(true);
        paint.setFilterBitmap(true);
        paint.setDither(true);
        canvas.drawARGB(0, 0, 0, 0);
        paint.setColor(Color.parseColor("#BAB399"));
        canvas.drawCircle(radius / 2 + 0.7f,
                radius / 2 + 0.7f, radius / 2 + 0.1f, paint);
        paint.setXfermode(new PorterDuffXfermode(Mode.SRC_IN));
        canvas.drawBitmap(sbmp, rect, rect, paint);

        return output;
    }
}

```

Use this Class in XML with package name instead of `ImageView`

```java
<com.androidbuts.example.MLRoundedImageView
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:src="@mipmap/ic_launcher" />

```



#### Syntax


- The method `setImageResource(int resId)` sets a drawable as the content of this `ImageView`.
- **Usage:** `imageView.setImageResource(R.drawable.anyImage)`



#### Parameters


|**Parameter**|**Description**
|---|---|---|---|---|---|---|---|---|---
|`resId`|your Image file name in the `res` folder (usually in `drawable` folder)

