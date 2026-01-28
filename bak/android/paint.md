---
metaTitle: "Android - Paint"
description: "Creating a Paint, Setting up Paint for text, Setting up Paint for drawing shapes, Setting flags"
---

# Paint


A paint is one of the four objects needed to draw, along with a Canvas (holds drawing calls), a Bitmap (holds the pixels), and a drawing primitive (Rect, Path, Bitmap...)



## Creating a Paint


You can create a new paint with one of these 3 constructors:

- `new Paint()` Create with default settings
- `new Paint(int flags)` Create with flags
- `new Paint(Paint from)` Copy settings from another paint

It is generally suggested to never create a paint object, or any other object in onDraw() as it can lead to performance issues. (Android Studio will probably warn you) Instead, make it global and initialize it in your class constructor like so:

```java
public class CustomView extends View {
    
    private Paint paint;
    
    public CustomView(Context context) {
        super(context);
        paint = new Paint();
        //...
    }
    
    @Override
    protected void onDraw(Canvas canvas) {
        super.onDraw(canvas);
        paint.setColor(0xFF000000);
        // ...
    }
}

```



## Setting up Paint for text


### **Text drawing settings**

- `setTypeface(Typeface typeface)` Set the font face. See [Typeface](https://developer.android.com/reference/android/graphics/Typeface.html)
- `setTextSize(int size)` Set the font size, in pixels.
- `setColor(int color)` Set the paint drawing color, including the text color. You can also use `setARGB(int a, int r, int g, int b` and `setAlpha(int alpha)`
- `setLetterSpacing(float size)` Set the spacing between characters, in ems. Default value is 0, a negative value will tighten the text, while a positive one will expand it.
- `setTextAlign(Paint.Align align)` Set text alignment relative to its origin. `Paint.Align.LEFT` will draw it to the right of the origin, `RIGHT` will draw it to the left, and `CENTER` will draw it centered on the origin (horizontally)
- `setTextSkewX(float skewX)` This could be considered as fake italic. SkewX represents the horizontal offset of the text bottom. (use -0.25 for italic)
- `setStyle(Paint.Style style)` Fill text `FILL`, Stroke text `STROKE`, or both `FILL_AND_STROKE`

Note that you can use `TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_SP, size, getResources().getDisplayMetrics())` to convert from SP or DP to pixels.

### **Measuring text**

- `float width = paint.measureText(String text)` Measure the width of text
- `float height = paint.ascent()` Measure the height of text
- `paint.getTextBounds(String text, int start, int end, Rect bounds` Stores the text dimensions. You have allocate the Rect, it cannot be null:

```

String text = "Hello world!";
 Rect bounds = new Rect();
 paint.getTextBounds(text, 0, text.length(), bounds);

```

There are other methods for measuring, however these three should fit most purposes.



## Setting up Paint for drawing shapes


- `setStyle(Paint.Style style)` Filled shape `FILL`, Stroke shape `STROKE`, or both `FILL_AND_STROKE`
- `setColor(int color)` Set the paint drawing color. You can also use `setARGB(int a, int r, int g, int b` and `setAlpha(int alpha)`
- `setStrokeCap(Paint.Cap cap)` Set line caps, either `ROUND`, `SQUARE`, or `BUTT` (none) See [this](https://elevenworks.gitbooks.io/touchdraw-for-ipad/content/attachments/3247710/8806443.png).
- `setStrokeJoin(Paint.Join join)` Set line joins, either `MITER` (pointy), `ROUND`, or `BEVEL`. See [this](https://www.w3.org/TR/SVG/images/painting/linejoin.svg).
- `setStrokeMiter(float miter)` Set miter join limit. This can prevent miter join from going on indefinitively, turning it into a bevel join after x pixels. See [this](http://www.mikeswanson.com/blog/images/Miter%20Limit%20Illustration.gif).
- `setStrokeWidth(float width)` Set stroke width. `0` will draw in hairline mode, independant of the canvas matrix. (always 1 pixel)



## Setting flags


You can set the following flags in the constructor, or with `setFlags(int flags)`

- `Paint.ANTI_ALIAS_FLAG` Enable antialiasing, smooths the drawing.
- `Paint.DITHER_FLAG` Enable dithering. If color precision is higher than the device's, [this will happen](http://img11.hostingpics.net/pics/332173dithering.png).
- `Paint.EMBEDDED_BITMAP_TEXT_FLAG` Enables the use of bitmap fonts.
- `Paint.FAKE_BOLD_TEXT_FLAG` will draw text with a fake bold effect, can be used instead of using a bold typeface. Some fonts have styled bold, [fake bold won't](https://www.smashingmagazine.com/wp-content/uploads/2012/06/3_big_bolditalic.jpg)
- `Paint.FILTER_BITMAP_FLAG` Affects the sampling of bitmaps when transformed.
- `Paint.HINTING_OFF`, `Paint.HINTING_ON` Toggles font hinting, see [this](http://showinfo.rietveldacademie.nl/verdana/Image/cap2_5200.JPG)
- `Paint.LINEAR_TEXT_FLAG` Disables font scaling, draw operations are scaled instead
- `Paint.SUBPIXEL_TEXT_FLAG` Text will be computed using subpixel accuracy.
- `Paint.STRIKE_THRU_TEXT_FLAG` Text drawn will be striked
- `Paint.UNDERLINE_TEXT_FLAG` Text drawn will be underlined

You can add a flag and remove flags like this:

```java
Paint paint = new Paint();
paint.setFlags(paint.getFlags() | Paint.FLAG);   // Add flag
paint.setFlags(paint.getFlags() & ~Paint.FLAG);  // Remove flag

```

Trying to remove a flag that isn't there or adding a flag that is already there won't change anything. Also note that most flags can also be set using `set<Flag>(boolean enabled)`, for example `setAntialias(true)`.

You can use `paint.reset()` to reset the paint to its default settings. The only default flag is `EMBEDDED_BITMAP_TEXT_FLAG`. It will be set even if you use `new Paint(0)`, you will have

