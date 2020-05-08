---
metaTitle: "Android - Barcode and QR code reading"
description: "Using QRCodeReaderView (based on Zxing)"
---

# Barcode and QR code reading



## Using QRCodeReaderView (based on Zxing)


[QRCodeReaderView](https://github.com/dlazaro66/QRCodeReaderView) implements an Android view which show camera and notify when there's a QR code inside the preview.

It uses the [zxing](https://github.com/zxing/zxing) open-source, multi-format 1D/2D barcode image processing library.

### Adding the library to your project

Add QRCodeReaderView dependency to your build.gradle

```java
dependencies{
    compile 'com.dlazaro66.qrcodereaderview:qrcodereaderview:2.0.0'
}

```

### First use

- Add to your layout a `QRCodeReaderView`

```

<com.dlazaro66.qrcodereaderview.QRCodeReaderView
        android:id="@+id/qrdecoderview"
        android:layout_width="match_parent"
        android:layout_height="match_parent" />

```


- Create an Activity which implements `onQRCodeReadListener`, and use it as a listener of the `QrCodeReaderView`.
- Make sure you have camera permissions in order to use the library. ([https://developer.android.com/training/permissions/requesting.html)](https://developer.android.com/training/permissions/requesting.html))

Then in your Activity, you can use it as follows:

```

   public class DecoderActivity extends Activity implements OnQRCodeReadListener {

    private TextView resultTextView;
    private QRCodeReaderView qrCodeReaderView;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_decoder);

        qrCodeReaderView = (QRCodeReaderView) findViewById(R.id.qrdecoderview);
        qrCodeReaderView.setOnQRCodeReadListener(this);

        // Use this function to enable/disable decoding
        qrCodeReaderView.setQRDecodingEnabled(true);

        // Use this function to change the autofocus interval (default is 5 secs)
        qrCodeReaderView.setAutofocusInterval(2000L);

        // Use this function to enable/disable Torch
        qrCodeReaderView.setTorchEnabled(true);

        // Use this function to set front camera preview
        qrCodeReaderView.setFrontCamera();

        // Use this function to set back camera preview
        qrCodeReaderView.setBackCamera();
    }

    // Called when a QR is decoded
    // "text" : the text encoded in QR
    // "points" : points where QR control points are placed in View
    @Override
    public void onQRCodeRead(String text, PointF[] points) {
        resultTextView.setText(text);
    }

    @Override
    protected void onResume() {
        super.onResume();
        qrCodeReaderView.startCamera();
    }

    @Override
    protected void onPause() {
        super.onPause();
        qrCodeReaderView.stopCamera();
    }
}

```



#### Remarks


[QRCodeReaderView](https://github.com/dlazaro66/QRCodeReaderView)

[Zxing](https://github.com/zxing/zxing)

