---
metaTitle: "Android - Loading Bitmaps Effectively"
description: "Load the Image from Resource from Android Device. Using Intents."
---

# Loading Bitmaps Effectively


This Topic Mainly Concentrate on Loading the Bitmaps Effectively in Android Devices.

When it comes to loading a bitmap, the question comes where it is loaded from.
Here we are going to discuss about how to load the Bitmap from Resource with in the Android Device. i.e. eg from Gallery.

We will go through this by example which are discussed below.



## Load the Image from Resource from Android Device. Using Intents.


**Using Intents to Load the Image from Gallery.**

1. Initially you need to have the permission

```

 <uses-permission android:name="android.permission.READ_EXTERNAL_STORAGE"/>

```


1. Use the Following Code to have the layout as designed follows.

[<img src="https://i.stack.imgur.com/bkmV5.png" alt="Layout Visual" />](https://i.stack.imgur.com/bkmV5.png)

```

  <?xml version="1.0" encoding="utf-8"?>
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:tools="http://schemas.android.com/tools"
    android:orientation="vertical"
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    android:paddingBottom="@dimen/activity_vertical_margin"
    android:paddingLeft="@dimen/activity_horizontal_margin"
    android:paddingRight="@dimen/activity_horizontal_margin"
    android:paddingTop="@dimen/activity_vertical_margin"
    tools:context="androidexamples.idevroids.loadimagefrmgallery.MainActivity">

    <ImageView
        android:id="@+id/imgView"
        android:layout_width="fill_parent"
        android:layout_height="wrap_content"
        android:layout_weight="1"
        android:background="@color/abc_search_url_text_normal"></ImageView>

    <Button
        android:id="@+id/buttonLoadPicture"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:layout_weight="0"
        android:text="Load Picture"
        android:layout_gravity="bottom|center"></Button>

</LinearLayout>

```


1. Use the Following code to Display the image with  button Click.

Button Click will be

```java
Button loadImg = (Button) this.findViewById(R.id.buttonLoadPicture);
loadImg.setOnClickListener(new View.OnClickListener() {
    @Override
    public void onClick(View v) {

        Intent i = new Intent(Intent.ACTION_PICK, MediaStore.Images.Media.EXTERNAL_CONTENT_URI);
        startActivityForResult(i, RESULT_LOAD_IMAGE);
    }
});

```


1. Once you clicked on the button , it will open the gallery with help of intent.

You need to select image and send it back to main activity. Here with help of onActivityResult we can do that.

```java
protected void onActivityResult(int requestCode, int resultCode, Intent data)  {
    super.onActivityResult(requestCode, resultCode, data);

    if (requestCode == RESULT_LOAD_IMAGE && resultCode == RESULT_OK && null != data) {
        Uri selectedImage = data.getData();
        String[] filePathColumn = { MediaStore.Images.Media.DATA };

        Cursor cursor = getContentResolver().query(selectedImage,
                filePathColumn, null, null, null);
        cursor.moveToFirst();

        int columnIndex = cursor.getColumnIndex(filePathColumn[0]);
        String picturePath = cursor.getString(columnIndex);
        cursor.close();

        ImageView imageView = (ImageView) findViewById(R.id.imgView);
        imageView.setImageBitmap(BitmapFactory.decodeFile(picturePath));

    }
}

```



#### Syntax


- `<uses-permission>`  **-- >** Tag Used for the Permission .
- `android:name` **-- >** An attribute used to give name for the   permission we are going to request.
- `android.permission.READ_EXTERNAL_STORAGE` **-->** It is System permissions
- example  "android.permission.CAMERA" or "android.permission.READ_CONTACTS"

