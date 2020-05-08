---
metaTitle: "Android - Capturing Screenshots"
description: "Capturing Screenshot via Android Studio, Capturing Screenshot via ADB and saving directly in your PC, Taking a screenshot of a particular view, Capturing Screenshot via Android Device Monitor, Capturing Screenshot via ADB"
---

# Capturing Screenshots




## Capturing Screenshot via Android Studio


1. Open Android Monitor Tab
<li>Click on Screen Capture Button
[<img src="http://i.stack.imgur.com/Am6Gt.png" alt="Android Studio" />](http://i.stack.imgur.com/Am6Gt.png)</li>



## Capturing Screenshot via ADB and saving directly in your PC


If you use Linux (or Windows with Cygwin), you can run:

```java
adb shell screencap -p | sed 's/\r$//' > screenshot.png

```



## Taking a screenshot of a particular view


If you want to take a screenshot of a particular View `v`, then you can use the following code:

```java
Bitmap viewBitmap = Bitmap.createBitmap(v.getWidth(), v.getHeight(), Bitmap.Config.RGB_565);
Canvas viewCanvas = new Canvas(viewBitmap);
Drawable backgroundDrawable = v.getBackground();

if(backgroundDrawable != null){
    // Draw the background onto the canvas.
    backgroundDrawable.draw(viewCanvas);
}
else{
    viewCanvas.drawColor(Color.GREEN);
    // Draw the view onto the canvas.
    v.draw(viewCanvas) 
}

// Write the bitmap generated above into a file.
String fileStamp = new SimpleDateFormat("yyyyMMdd_HHmmss").format(new Date());
OutputStream outputStream = null;
try{
    imgFile = new File(Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_PICTURES), fileStamp + ".png");
    outputStream = new FileOutputStream(imgFile);
    viewBitmap.compress(Bitmap.CompressFormat.PNG, 40, outputStream);
    outputStream.close();
}
catch(Exception e){
    e.printStackTrace();
}

```



## Capturing Screenshot via Android Device Monitor


1. Open Android Device Monitor (* ie C:<ANDROID_SDK_LOCATION>\tools\monitor.bat*)
1. Select your device
1. Click on Screen Capture Button

[<img src="http://i.stack.imgur.com/TDMzE.png" alt="Android Device Monitor" />](http://i.stack.imgur.com/TDMzE.png)



## Capturing Screenshot via ADB


Example below saves a screenshot on Devices's Internal Storage.

```java
adb shell screencap /sdcard/screen.png

```

