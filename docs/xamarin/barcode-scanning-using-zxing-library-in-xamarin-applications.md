---
metaTitle: "Xamarin - Barcode scanning using ZXing library in Xamarin Applications"
description: "Sample Code"
---

# Barcode scanning using ZXing library in Xamarin Applications


Zxing library is well known for image processing. Zxing was based on java and .Net module is also available and it can be used in xamarin applications. click here to check official documentation.
[http://zxingnet.codeplex.com/](http://zxingnet.codeplex.com/)

I recently used this libarry.

Step1: Add ZXing.Net.Mobile component into solution.

step2: In whichever activity we need to show barcode scanner, in that activity initialise MobileBarcodeScanner.

Step3: Write below code when tapped on any view to start scanning.



## Sample Code


```cs
button.Click +=async delegate 
{ 
var MScanner = new MobileBarcodeScanner(); 
var Result = await MScanner.Scan(); 
if(Result == null) 
{ 
    return; 
} 
//get the bar code text here 
string BarcodeText = Result.text; 
}

```

