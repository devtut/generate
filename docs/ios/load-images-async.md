---
metaTitle: "iOS - Load images async"
description: "Easiest way, Check that the cell is still visible after download"
---

# Load images async




## Easiest way


The most simple way to create this is to use [Alamofire](https://github.com/Alamofire/Alamofire) and its [UIImageViewExtension](https://github.com/Alamofire/AlamofireImage#uiimageview-extension). What we need is a tableview with a cell that has an imageView in it and lets call it `imageView`.

In the cellForRowAt: function of the tableView we would download the image and set in the following way:

```swift
let url = URL(string: "https://httpbin.org/image/png")!
let placeholderImage = UIImage(named: "placeholder")!

imageView.af_setImage(withURL: url, placeholderImage: placeholderImage)

```

The url should point to the image that you want to download and the placeHolder image should be a stored image. We then call the `af_setImage` method on the `imageView` which downloads the image at the given url and during the download the placeholder image will be shown. As soon as the image is downloaded the requested image is displayed



## Check that the cell is still visible after download


Sometimes the download takes longer than the cell is being displayed. In this case it can happen, that the downloaded image is shown in the wrong cell. To fix this we can not use the [UIImageView Extension](https://github.com/Alamofire/AlamofireImage#uiimageview-extension).

We still will be using [Alamofire](https://github.com/Alamofire/Alamofire) however we will use the completion handler to display the image.

In this scenario we still need a tableView with a cell which has a imageView in it. In the cellForRowAt: method we would download the image with the following code:

```swift
let placeholderImage = UIImage(named: "placeholder")!
imageView.image = placeholderImage

let url = URL(string: "https://httpbin.org/image/png")!

Alamofire.request(url!, method: .get).responseImage { response in
    guard let image = response.result.value else { return }

    if let updateCell = tableView.cellForRow(at: indexPath) {
        updateCell.imageView.image = image
    }
}

```

In this example we first set the image to the placeholder image. Afterwards we download the image with the `request` method of [Alamofire](https://github.com/Alamofire/AlamofireImage#uiimageview-extension). We pass the url as the first argument and since we just want to get the image we will use the `.get` HTTP method. Since we are downloading an image we want the response to be an image therefore we use the `.responseImage` method.

After the image has been downloaded the closure gets called and first of all we make sure that the downloaded image actually exists. Then we make sure that the cell is still visible by checking that the cellForRow(at: indexPath) doesn't return nil. If it does nothing happens, if it doesn't we assign the recently downloaded image.

This last if statement ensures that the cell is still visible if the user already scrolled over the cell the updateCell will be nil and the if statement returns nil. This helps us prevent displaying the wrong image in a cell.

