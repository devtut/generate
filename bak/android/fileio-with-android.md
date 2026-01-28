---
metaTitle: "Android - FileIO with Android"
description: "Obtaining the working folder, Writing raw array of bytes, Serializing the object, Writing to external storage (SD card), Solving Invisible MTP files problem., Working with big files"
---

# FileIO with Android


Reading and writing files in Android are not different from reading and writing files in standard Java. Same `java.io` package can be used. However, there is some specific related to the folders where you are allowed to write, permissions in general and MTP work arounds.



## Obtaining the working folder


You can get your working folder by calling the method [`getFilesDir()`](https://developer.android.com/reference/android/content/Context.html#getFilesDir()) on your Activity (Activity is the central class in your application that inherits from Context. See [here](http://stackoverflow.com/documentation/android/1481/activity)). Reading is not different. Only your application will have access to this folder.

Your activity could contain the following code, for instance:

```

 File myFolder = getFilesDir();
  File myFile = new File(myFolder, "myData.bin");

```



## Writing raw array of bytes


```

 File myFile = new File(getFilesDir(), "myData.bin");
  FileOutputStream out = new FileOutputStream(myFile);

  // Write four bytes one two three four:
  out.write(new byte [] { 1, 2, 3, 4}
  out.close()

```

There is nothing Android specific with this code. If you write lots of small values often, use [BufferedOutputStream](https://developer.android.com/reference/java/io/BufferedOutputStream.html) to reduce the wear of the device internal SSD.



## Serializing the object


The old good Java object serialization is available for you in Android. you can define Serializable classes like:

```

 class Cirle implements Serializable {
    final int radius;
    final String name;

    Circle(int radius, int name) {
      this.radius = radius;
      this.name = name;
    }
  }

```

and then write then to the ObjectOutputStream:

```

 File myFile = new File(getFilesDir(), "myObjects.bin");
  FileOutputStream out = new FileOutputStream(myFile);
  ObjectOutputStream oout = new ObjectOutputStream(new BufferedOutputStream(out));

  oout.writeObject(new Circle(10, "One"));
  oout.writeObject(new Circle(12, "Two"));
  
  oout.close()

```

Java object serialization may be either perfect or really bad choice, depending on what do you want to do with it - outside the scope of this tutorial and sometimes opinion based. Read about the [versioning](http://www.javaworld.com/article/2071731/core-java/ensure-proper-version-control-for-serialized-objects.html) first if you decide to use it.



## Writing to external storage (SD card)


You can also read and write from/to memory card (SD card) that is present in many Android devices. Files in this location can be accessed by other programs, also directly by the user after connecting device to PC via USB cable and enabling MTP protocol.

Finding the SD card location is somewhat more problematic. The [Environment](https://developer.android.com/reference/android/os/Environment.html) class contains static methods to get "external directories" that should normally be inside the SD card, also information if the SD card exists at all and is writable. [This question](http://stackoverflow.com/questions/5694933/find-an-external-sd-card-location) contains valuable answers how to make sure the right location will be found.

Accessing external storage requires permissions in you Android manifest:

```java
<uses-permission android:name="android.permission.WRITE_EXTERNAL_STORAGE" />
<uses-permission android:name="android.permission.READ_EXTERNAL_STORAGE" />

```

For older versions of Android putting permissions it is enough to put these permissions into manifest (the user must approve during installation). However starting from Android 6.0 Android asks the user for approval at the time of the first access, and you must support this new approach. Otherwise access is denied regardless of your manifest.

In Android 6.0, first you need to check for permission, then, if not granted, request it. The code examples can be found inside [this SO question](http://stackoverflow.com/questions/33139754/android-6-0-marshmallow-cannot-write-to-sd-card).



## Solving "Invisible MTP files" problem.


If you create files for exporting via USB cable to desktop using MTP protocol, may be a problem that newly created files are not immediately visible in the file explorer running on the connected desktop PC. To to make new files visible, you need to call [MediaScannerConnection](https://developer.android.com/reference/android/media/MediaScannerConnection.html):

```java
File file = new File(Environment.getExternalStoragePublicDirectory(
        Environment.DIRECTORY_DOCUMENTS), "theDocument.txt");
FileOutputStream out = new FileOutputStream(file)

... (write the document)

out.close()
MediaScannerConnection.scanFile(this, new String[] {file.getPath()}, null, null);
    context.sendBroadcast(new Intent(Intent.ACTION_MEDIA_SCANNER_SCAN_FILE,
            Uri.fromFile(file)));

```

This MediaScannerConnection call code works for files only, not for directories. The problem is described in [this Android bug report](https://code.google.com/p/android/issues/detail?id=38282). This may be fixed for some version in the future, or on some devices.



## Working with big files


Small files are processed in a fraction of second and you can read / write them in place of the code where you need this. However if the file is bigger or otherwise slower to process, you may need to use AsyncTask in Android to work with the file in the background:

```

   class FileOperation extends AsyncTask<String, Void, File> {

        @Override
        protected File doInBackground(String... params) {
          try {
            File file = new File(Environment.getExternalStoragePublicDirectory(
            Environment.DIRECTORY_DOCUMENTS), "bigAndComplexDocument.odf");
            FileOutputStream out = new FileOutputStream(file)

           ... (write the document)

            out.close()
            return file;
            } catch (IOException ex) {
               Log.e("Unable to write", ex);
               return null;
              }
        }

        @Override
        protected void onPostExecute(File result) {
          // This is called when we finish 
        }

        @Override
        protected void onPreExecute() {
           // This is called before we begin
        }

        @Override
        protected void onProgressUpdate(Void... values) {
            // Unlikely required for this example
        }
    }
}

```

and then

```java
new FileOperation().execute("Some parameters");    

```

[This SO question](http://stackoverflow.com/questions/9671546/asynctask-android-example) contains the complete example on how to create and call the AsyncTask. Also see the [question on error](http://stackoverflow.com/questions/3690980/asynctask-error-handling) handling on how to handle IOExceptions and other errors.



#### Remarks


Android provides means for sharing the file between multiple applications as documented [here](https://developer.android.com/training/secure-file-sharing/setup-sharing.html). This is not required if there is only one app that creates and uses the file.

Android provides [alternative storage options](https://developer.android.com/guide/topics/data/data-storage.html) like shared and private preferences, saved bundles and built-in database. In some cases, they are better choice than just using plain files.

Android activity does have few specific methods that look like replacements of the Java standard File IO methods. For instance, instead for [`File.delete()`](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#delete()) you can call [`Context.deleteFile()`](https://developer.android.com/reference/android/content/Context.html#deleteFile(java.lang.String)), and instead of applying [`File.listFiles()`](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#listFiles()) recursively you can call [`Context.fileList()`](https://developer.android.com/reference/android/content/Context.html#fileList()) to get the list of all your app specific files with somewhat less code. However, they do not provide extra functionality beyond standard `java.io` package.

