---
metaTitle: "Android - Storing Files in Internal & External Storage"
description: "Android: Internal and External Storage - Terminology Clarification, Using External Storage, Using Internal Storage, Save Database on SD Card (Backup DB on SD), Fetch Device Directory :"
---

# Storing Files in Internal & External Storage




## Android: Internal and External Storage - Terminology Clarification


Android developers(mainly beginners) have been confused regarding Internal & External  storage terminology. There are lot of questions on Stackoverflow regarding the same. This is mainly because of the fact that **terminology** according to Google/official Android documentation is quite different to that of normal Android OS user. Hence I thought documenting this would help.

**What we think - User’s Terminology (UT)**

|Internal  storage(UT)|External  storage(UT)
|---|---|---|---|---|---|---|---|---|---
|phone’s inbuilt internal memory|removable Secure Digital(SD) card or micro SD storage
|**Example:** Nexus 6P's 32 GB internal memory.|**Example:** storage space in removable SD cards provided by vendors like samsung, sandisk, strontium, transcend and others

But, **According to Android Documentation/Guide - Google’s Terminology (GT)**

**Internal  storage(GT):**

> 
<p>By default, files saved to the internal storage are private to your
application and other applications cannot access them (nor can the
user).</p>


**External  storage(GT):**

> 
<p>This can be a removable storage media (such as an SD card) or an
internal (non-removable) storage.</p>


**External Storage(GT) can be categorized into two types:**

|Primary External Storage|Secondary External Storage or Removable storage(GT)
|---|---|---|---|---|---|---|---|---|---
|This is same as phone’s inbuilt internal memory (or) Internal storage(UT)|This is same as removable micro SD card storage (or) External storage(UT)
|**Example:** Nexus 6P's 32 GB internal memory.|**Example:** storage space in removable SD cards provided by vendors like samsung, sandisk, strontium, transcend and others
|This type of storage can be accessed on windows PC by connecting your phone to PC via USB cable and selecting **Camera(PTP)** in the USB options notification.|This type of storage can be accessed on windows PC by connecting your phone to PC via USB cable and selecting **File transfer** in the USB options notification.

In a nutshell,

**External Storage(GT) = Internal Storage(UT) and External Storage(UT)**

**Removable Storage(GT) = External Storage(UT)**

**Internal Storage(GT) doesn't have a term in UT.**

Let me explain clearly,

**Internal Storage(GT):** By default, files saved to the internal storage are private to your application and other applications cannot access them. Your app user also can't access them using file manager; even after enabling "show hidden files" option in file manager. To access files in Internal Storage(GT), you have to root your Android phone. Moreover, when the user uninstalls your application, these files are removed/deleted.

So Internal Storage(GT) is **NOT** what we think as Nexus 6P's 32/64 GB internal memory

Generally, **Internal Storage(GT) location** would be: `/data/data/your.application.package.appname/someDirectory/`

**External Storage(GT):**

> 
Every Android-compatible device supports a shared "external storage" that you can use to save files. Files saved to the external storage are world-readable and can be modified by the user when they enable USB mass storage to transfer files on a computer.


**External Storage(GT) location:** It could be **anywhere** in your internal storage(UT) or in your removable storage(GT) i.e. micro SD card. It depends on your phone's OEM and also on Android OS version.

In order to read or write files on the External Storage(GT), your app must acquire the `READ_EXTERNAL_STORAGE` or `WRITE_EXTERNAL_STORAGE` system permissions.

For example:

```java
<manifest ...>
    <uses-permission android:name="android.permission.WRITE_EXTERNAL_STORAGE" />
    ...
</manifest>

```

> 
<p>If you need to both read and write files, then you need to request
only the `WRITE_EXTERNAL_STORAGE` permission, because it implicitly
requires read access as well.</p>


In **External Storage(GT)**, you may also save files that are **app-private**

But,

> 
When the user uninstalls your application, this directory and all its contents are deleted.


When do you need to save files that are **app-private** in **External Storage(GT)**?

> 
If you are handling files that are not intended for other apps to use (such as graphic textures or sound effects used by only your app), you should use a private storage directory on the external storage


> 
Beginning with Android 4.4, reading or writing files in your app's private directories does not require the `READ_EXTERNAL_STORAGE` or `WRITE_EXTERNAL_STORAGE` permissions. So you can declare the permission should be requested only on the lower versions of Android by adding the `maxSdkVersion` attribute:


```java
<manifest ...>
    <uses-permission android:name="android.permission.WRITE_EXTERNAL_STORAGE"
                     android:maxSdkVersion="18" />
    ...
</manifest

```

**Methods to store in Internal Storage(GT):**

Both these methods are present in [Context](https://developer.android.com/reference/android/content/Context.html) class

```java
File getDir (String name, int mode)

File getFilesDir () 

```

**Methods to store in Primary External Storage i.e. Internal Storage(UT):**

```java
File getExternalStorageDirectory ()

File getExternalFilesDir (String type)

File getExternalStoragePublicDirectory (String type)

```

In the beginning, everyone used [Environment.getExternalStorageDirectory()](https://developer.android.com/reference/android/os/Environment.html#getExternalStorageDirectory()) , which pointed to the **root** of **Primary External Storage**. As a result, Primary External Storage was filled with random content.

Later, these two methods were added:

<li>
In `Context` class, they added [getExternalFilesDir()](https://developer.android.com/reference/android/content/Context.html#getExternalFilesDir(java.lang.String)), pointing to an **app-specific directory** on Primary External Storage. This directory and its contents **will be deleted** when the app is uninstalled.
</li>
<li>
[Environment.getExternalStoragePublicDirectory()](https://developer.android.com/reference/android/os/Environment.html#getExternalStoragePublicDirectory(java.lang.String)) for centralized places to store well-known file types, like photos and movies. This directory and its contents **will NOT be deleted** when the app is uninstalled.
</li>

**Methods to store in Removable Storage(GT) i.e. micro SD card**

Before **API level 19**, there was **no official way** to store in SD card. But, many could do it using unofficial libraries or APIs.

Officially, one method was introduced in `Context` class in API level 19 (Android version 4.4 - Kitkat).

```java
File[] getExternalFilesDirs (String type)

```

> 
<p>It returns absolute paths to application-specific directories on all
shared/external storage devices where the application can place
persistent files it owns. These files are internal to the application,
and not typically visible to the user as media.</p>


That means, it will return paths to **both** types of External Storage(GT) - Internal memory and Micro SD card. Generally **second path** would be storage path of micro SD card(but not always). So you need to check it out by executing the code with this method.

**Example with code snippet:**

I created a new android project with empty activity, wrote the following code inside

`protected void onCreate(Bundle savedInstanceState)` method of `MainActivity.java`

```

   File internal_m1 = getDir("custom", 0);
    File internal_m2 = getFilesDir();

    File external_m1 =  Environment.getExternalStorageDirectory();
    
    File external_m2 =  getExternalFilesDir(null);
    File external_m2_Args = getExternalFilesDir(Environment.DIRECTORY_PICTURES);

    File external_m3 = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_PICTURES);
    
    File[] external_AND_removable_storage_m1 = getExternalFilesDirs(null);
    File[] external_AND_removable_storage_m1_Args = getExternalFilesDirs(Environment.DIRECTORY_PICTURES);

```

After executing above code,

**Output:**

```java
internal_m1: /data/data/your.application.package.appname/app_custom

internal_m2: /data/data/your.application.package.appname/files

external_m1: /storage/emulated/0

external_m2: /storage/emulated/0/Android/data/your.application.package.appname/files

external_m2_Args: /storage/emulated/0/Android/data/your.application.package.appname/files/Pictures

external_m3: /storage/emulated/0/Pictures

external_AND_removable_storage_m1 (first path):
/storage/emulated/0/Android/data/your.application.package.appname/files

external_AND_removable_storage_m1 (second path):    
/storage/sdcard1/Android/data/your.application.package.appname/files

 external_AND_removable_storage_m1_Args (first path):
/storage/emulated/0/Android/data/your.application.package.appname/files/Pictures

external_AND_removable_storage_m1_Args (second path): /storage/sdcard1/Android/data/your.application.package.appname/files/Pictures

```

**Note:** I have connected my phone to Windows PC; enabled both developer options, USB debugging and then ran this code. If you **do not connect your phone**; but instead run this on **Android emulator**, your output may vary. My phone model is Coolpad Note 3 - running on Android 5.1

**Storage locations on my phone:**

**Micro SD storage location**: `/storage/sdcard1`

**Internal Storage(UT) location**: `/storage/sdcard0`.

Note that `/sdcard` & `/storage/emulated/0` also point to Internal Storage(UT). But these are symlinks to `/storage/sdcard0`.

To clearly understand different storage paths in Android, Please go through [this answer](http://stackoverflow.com/a/38813578/2818583)

**Disclaimer:** All the storage paths mentioned above are paths on **my** phone. Your files may **not** be stored on same storage paths. Because, the storage locations/paths may vary on other mobile phones depending on your vendor, manufacturer and different versions of Android OS.



## Using External Storage


"External" Storage is another type of storage that we can use to save files to the user's device. It has some key differences from "Internal" Storage, namely:

- It is not always available. In the case of a removable medium (SD card), the user can simply remove the storage.
- It is not private. The user (and other applications) have access to these files.
- If the user uninstalls the app, the files you save in the directory retrieved with `getExternalFilesDir()` will be removed.

To use External Storage, we need to first obtain the proper permissions. You will need to use:

- [`android.permission.WRITE_EXTERNAL_STORAGE`](https://developer.android.com/reference/android/Manifest.permission.html#WRITE_EXTERNAL_STORAGE) for reading and writing
- [`android.permission.READ_EXTERNAL_STORAGE`](https://developer.android.com/reference/android/Manifest.permission.html#READ_EXTERNAL_STORAGE) for just reading

To grant these permissions, you will need to identify them in your `AndroidManifest.xml` as such

```java
<uses-permission android:name="android.permission.WRITE_EXTERNAL_STORAGE" />

```

> 
NOTE: Since they are [Dangerous permissions](https://developer.android.com/guide/topics/security/permissions.html#normal-dangerous) if you are using **API Level 23** or above, you will need to request the [**permissions at runtime**](https://developer.android.com/training/permissions/requesting.html).


Before attempting to write or read from External Storage, you should always check that the storage medium is available.

```java
String state = Environment.getExternalStorageState();
if (state.equals(Environment.MEDIA_MOUNTED)) {
    // Available to read and write
}
if (state.equals(Environment.MEDIA_MOUNTED) || 
        state.equals(Environment.MEDIA_MOUNTED_READ_ONLY)) {
    // Available to at least read
}

```

When writing files to the External Storage, you should decide if the file should be recognized as Public or Private. While both of these types of files are still accessible to the user and other applications on the device, there is a key distinction between them.

Public files should remain on the device when the user uninstalls the app. An example of a file that should be saved as Public would be photos that are taken through your application.

Private files should all be removed when the user uninstalls the app. These types of files would be app specific, and not be of use to the user or other applications. Ex. temporary files downloaded/used by your application.

Here's how to get access to the `Documents` directory for both Public and Private files.

**Public**

```java
// Access your app's directory in the device's Public documents directory
File docs = new File(Environment.getExternalStoragePublicDirectory(
        Environment.DIRECTORY_DOCUMENTS), "YourAppDirectory");
// Make the directory if it does not yet exist
myDocs.mkdirs();

```

**Private**

```java
// Access your app's Private documents directory
File file = new File(context.getExternalFilesDir(Environment.DIRECTORY_DOCUMENTS), 
        "YourAppDirectory");
// Make the directory if it does not yet exist
myDocs.mkdirs();

```



## Using Internal Storage


By default, any files that you save to Internal Storage are private to your application. They cannot be accessed by other applications, nor the user under normal circumstances. **These files are deleted when the user uninstalls the application**.

**To Write Text to a File**

```java
String fileName= "helloworld";
String textToWrite = "Hello, World!";
FileOutputStream fileOutputStream;

try {
  fileOutputStream = openFileOutput(fileName, Context.MODE_PRIVATE);
  fileOutputStream.write(textToWrite.getBytes());
  fileOutputStream.close();
} catch (Exception e) {
  e.printStackTrace();
}

```

**To Append Text to an Existing File**

Use `Context.MODE_APPEND` for the mode parameter of `openFileOutput`

```java
fileOutputStream = openFileOutput(fileName, Context.MODE_APPEND);

```



## Save Database on SD Card (Backup DB on SD)


```

   public static Boolean ExportDB(String DATABASE_NAME , String packageName , String folderName){
    //DATABASE_NAME including ".db" at the end like "mayApp.db"
    String DBName = DATABASE_NAME.substring(0, DATABASE_NAME.length() - 3);
    File data = Environment.getDataDirectory();
    FileChannel source=null;
    FileChannel destination=null;
    String currentDBPath = "/data/"+ packageName +"/databases/"+DATABASE_NAME; // getting app db path

    File sd = Environment.getExternalStorageDirectory(); // getting phone SD card path
    String backupPath = sd.getAbsolutePath() + folderName; // if you want to set backup in specific folder name
        /* be careful , foldername must initial like this : "/myFolder" . dont forget "/" at begin of folder name
            you could define foldername like this : "/myOutterFolder/MyInnerFolder" and so on ...
        */
    File dir = new File(backupPath);
    if(!dir.exists()) // if there was no folder at this path , it create it .
    {
        dir.mkdirs();
    }

    DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss");
    Date date = new Date();
        /* use date including file name for arrange them and preventing to make file with the same*/
    File currentDB = new File(data, currentDBPath);
    File backupDB = new File(backupPath, DBName +"("+ dateFormat.format(date)+").db");
    try {
        if (currentDB.exists() && !backupDB.exists()) {
            source = new FileInputStream(currentDB).getChannel();
            destination = new FileOutputStream(backupDB).getChannel();
            destination.transferFrom(source, 0, source.size());
            source.close();
            destination.close();
            return true;
        }
        return false;
    } catch(IOException e) {
        e.printStackTrace();
        return false;
    }
}

```

call this method this way :

> 
ExportDB("myDB.db","com.example.exam","/myFolder");




## Fetch Device Directory :


**First Add Storage permission to read/fetch device directory.**

```java
<uses-permission android:name="android.permission.WRITE_EXTERNAL_STORAGE" />
<uses-permission android:name="android.permission.READ_EXTERNAL_STORAGE" />

```

**Create model class**

```java
//create one directory model class
//to store directory title and type in list

public class DirectoryModel {
    String dirName;
    int dirType; // set 1 or 0, where 0 for directory and 1 for file.

    public int getDirType() {
        return dirType;
    }

    public void setDirType(int dirType) {
        this.dirType = dirType;
    }

    public String getDirName() {
        return dirName;
    }

    public void setDirName(String dirName) {
        this.dirName = dirName;
    }
}

```

**Create list using directory model to add directory data.**

```java
//define list to show directory

List<DirectoryModel> rootDir = new ArrayList<>();

```

**Fetch directory using following method.**

```java
//to fetch device directory

private void getDirectory(String currDir) { // pass device root directory
        File f = new File(currDir);
        File[] files = f.listFiles();
        if (files != null) {
            if (files.length > 0) {
                rootDir.clear();
                for (File inFile : files) {
                    if (inFile.isDirectory()) { //return true if it's directory
                        // is directory
                        DirectoryModel dir = new DirectoryModel();
                        dir.setDirName(inFile.toString().replace("/storage/emulated/0", ""));
                        dir.setDirType(0); // set 0 for directory
                        rootDir.add(dir);
                    } else if (inFile.isFile()) { // return true if it's file
                        //is file
                        DirectoryModel dir = new DirectoryModel();
                        dir.setDirName(inFile.toString().replace("/storage/emulated/0", ""));
                        dir.setDirType(1); // set 1 for file
                        rootDir.add(dir);
                    }
                }
            }
            printDirectoryList();
        }
    }

```

**Print directory list in log.**

```java
//print directory list in logs

private void printDirectoryList() {
    for (int i = 0; i < rootDir.size(); i++) {
        Log.e(TAG, "printDirectoryLogs: " + rootDir.get(i).toString());
    }
}

```

**Usage**

```java
//to Fetch Directory Call function with root directory.

String rootPath = Environment.getExternalStorageDirectory().toString(); // return ==>  /storage/emulated/0/
getDirectory(rootPath );

```

**To fetch inner files/folder of specific directory use same method just change argument,  pass the current selected path in argument and handle response for same.**

**To get File Extension :**

```java
private String getExtension(String filename) {

    String filenameArray[] = filename.split("\\.");
    String extension = filenameArray[filenameArray.length - 1];
    Log.d(TAG, "getExtension: " + extension);

    return extension;
}

```



#### Syntax


- FileOutputStream openFileInput (String name)
- FileOutputStream openFileOutput (String name, int mode)
- File(File dir, String name)
- File(String path)
- File getExternalStoragePublicDirectory (String type)
- File getExternalFilesDir (String type)



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|name|The name of the file to open. NOTE: Cannot contain path separators
|mode|Operating mode. Use `MODE_PRIVATE` for default operation, and `MODE_APPEND` to append an existing file. Other modes include `MODE_WORLD_READABLE` and `MODE_WORLD_WRITEABLE`, which were both deprecated in API 17.
|dir|Directory of the file to create a new file in
|path|Path to specify the location of the new file
|type|Type of files directory to retrieve. Can be `null`, or any of the following: `DIRECTORY_MUSIC`, `DIRECTORY_PODCASTS`, `DIRECTORY_RINGTONES`, `DIRECTORY_ALARMS`, `DIRECTORY_NOTIFICATIONS`, `DIRECTORY_PICTURES`, or `DIRECTORY_MOVIES`

