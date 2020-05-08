---
metaTitle: "Android - SpannableString"
description: "Add styles to a TextView, Multi string , with multi color"
---

# SpannableString




## Add styles to a TextView


In the following example, we create an Activity to display a single TextView.

The TextView will use a `SpannableString` as its content, which will illustrate some of the available styles.

Here' what we're gonna do with the text :

- Make it larger
- Bold
- Underline
- Italicize
- Strike-through
- Colored
- Highlighted
- Show as superscript
- Show as subscript
- Show as a link
- Make it clickable.

```java
@Override
protected void onCreate(Bundle savedInstanceState) {
super.onCreate(savedInstanceState);
     
 SpannableString styledString
  = new SpannableString("Large\n\n"     // index 0 - 5
       + "Bold\n\n"          // index 7 - 11
       + "Underlined\n\n"    // index 13 - 23
       + "Italic\n\n"        // index 25 - 31
       + "Strikethrough\n\n" // index 33 - 46
       + "Colored\n\n"       // index 48 - 55
       + "Highlighted\n\n"   // index 57 - 68
       + "K Superscript\n\n" // "Superscript" index 72 - 83 
       + "K Subscript\n\n"   // "Subscript" index 87 - 96
       + "Url\n\n"           //  index 98 - 101
       + "Clickable\n\n");   // index 103 - 112
  
 // make the text twice as large
 styledString.setSpan(new RelativeSizeSpan(2f), 0, 5, 0);
  
 // make text bold
 styledString.setSpan(new StyleSpan(Typeface.BOLD), 7, 11, 0);
  
 // underline text
 styledString.setSpan(new UnderlineSpan(), 13, 23, 0);
  
 // make text italic
 styledString.setSpan(new StyleSpan(Typeface.ITALIC), 25, 31, 0);
  
 styledString.setSpan(new StrikethroughSpan(), 33, 46, 0);
  
 // change text color
 styledString.setSpan(new ForegroundColorSpan(Color.GREEN), 48, 55, 0);
  
 // highlight text
 styledString.setSpan(new BackgroundColorSpan(Color.CYAN), 57, 68, 0);
  
 // superscript
 styledString.setSpan(new SuperscriptSpan(), 72, 83, 0);
 // make the superscript text smaller
 styledString.setSpan(new RelativeSizeSpan(0.5f), 72, 83, 0);
  
 // subscript
 styledString.setSpan(new SubscriptSpan(), 87, 96, 0);
 // make the subscript text smaller
 styledString.setSpan(new RelativeSizeSpan(0.5f), 87, 96, 0);
  
 // url
 styledString.setSpan(new URLSpan("http://www.google.com"), 98, 101, 0);
  
 // clickable text
 ClickableSpan clickableSpan = new ClickableSpan() {

 @Override
 public void onClick(View widget) {
// We display a Toast. You could do anything you want here.
Toast.makeText(SpanExample.this, "Clicked", Toast.LENGTH_SHORT).show();
 
  }
};

 styledString.setSpan(clickableSpan, 103, 112, 0);


 // Give the styled string to a TextView
 TextView textView = new TextView(this);

 // this step is mandated for the url and clickable styles.
 textView.setMovementMethod(LinkMovementMethod.getInstance());

 // make it neat
textView.setGravity(Gravity.CENTER);
textView.setBackgroundColor(Color.WHITE);

textView.setText(styledString);

setContentView(textView);

}

```

And the result will look like this:
[<img src="https://i.stack.imgur.com/IZd33.png" alt="Result Image" />](https://i.stack.imgur.com/IZd33.png)



## Multi string , with multi color


Method: **setSpanColor**

```java
public Spanned setSpanColor(String string, int color){
    SpannableStringBuilder builder = new SpannableStringBuilder();
    SpannableString ss = new SpannableString(string);
    ss.setSpan(new ForegroundColorSpan(color), 0, string.length(), 0);
    builder.append(ss);
    return ss;
}

```

Usage:

```java
String a = getString(R.string.string1);
String b = getString(R.string.string2);

Spanned color1 = setSpanColor(a,Color.CYAN);
Spanned color2 = setSpanColor(b,Color.RED);
Spanned mixedColor = TextUtils.concat(color1, " ", color2);
// Now we use `mixedColor`

```



#### Syntax


- `char charAt (int i)`
- `boolean equals (Object o)`
- `void getChars (int start, int end, char[] dest, int off)`
- `int getSpanEnd (Object what)`
- `int getSpanFlags (Object what)`
- `int getSpanStart (Object what)`
- `T[] getSpans (int queryStart, int queryEnd, Class<T> kind)`
- `int hashCode ()`
- `int length ()`
- `int nextSpanTransition (int start, int limit, Class kind)`
- void removeSpan (Object what)
- `void setSpan (Object what, int start, int end, int flags)`
- `CharSequence subSequence (int start, int end)`
- `String toString ()`
- `SpannableString valueOf (CharSequence source)`

