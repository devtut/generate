---
metaTitle: "Xamarin - Connecting with Microsoft Cognitive Services"
description: "Connecting with Microsoft Cognitive Services"
---

# Connecting with Microsoft Cognitive Services



## Connecting with Microsoft Cognitive Services


In this example you will learn how to use Microsoft Cognitive Services with Xamarin iOS mobile application. We will use Computer Vision API to detect what is in the picture.

Once you create Xamarin.iOS project please add below NuGet package to the project:

[https://www.nuget.org/packages/Microsoft.ProjectOxford.Vision/](https://www.nuget.org/packages/Microsoft.ProjectOxford.Vision/)

With this library we will be able to utilize Cognitive Services in our iOS app.
I assume that you have Microsoft account already registered to use it and you have enabled Computer Vision Api like on the screen below:
[<img src="http://i.stack.imgur.com/UBcMp.png" alt="enter image description here" />](http://i.stack.imgur.com/UBcMp.png)

Once you click "Subscribe" at the bottom Api Key will be generated:

[<img src="http://i.stack.imgur.com/h9UV6.png" alt="enter image description here" />](http://i.stack.imgur.com/h9UV6.png)

Now we can start configuring access to Cognitive Services from the iOS app. Firstly we need to get some picture for the analysis. To do it we can use Xamarin Media Component available below:
[https://components.xamarin.com/view/mediaplugin](https://components.xamarin.com/view/mediaplugin)

Once it is successfully installed let's create simple UI with the image and button to select picture from the gallery. Size of the controls is up to you.

Open Main.storyboard and add UIImageView and UIButton controls do default ViewController. Add them names: "SelectedPictureImageView" and "SelectButton":

[<img src="http://i.stack.imgur.com/Qb2c5.png" alt="enter image description here" />](http://i.stack.imgur.com/Qb2c5.png)

Now we should add "Touch Up Inside" event handler to handle image selection:

```cs
partial void SelectButtonClick(UIButton sender)
{
    selectImage();
}

async void selectImage()
{
    var selectedImage = await CrossMedia.Current.PickPhotoAsync();
    SelectedPictureImageView.Image =  new UIImage(NSData.FromStream(selectedImage.GetStream()));
}

```

Now we would like to display analysis information once Cognitive Services returns the information. Add label below the button called "AnalysisLabel":
[<img src="http://i.stack.imgur.com/8nCjz.png" alt="enter image description here" />](http://i.stack.imgur.com/8nCjz.png)

It is time to connect Computer Vision API!

To get information about selected picture add below method. Remember to paste you API Key!

```cs
async Task analyseImage(Stream imageStream)
{
    try
    {
        VisionServiceClient visionClient = new VisionServiceClient("<<YOUR API KEY HERE>>");
        VisualFeature[] features = { VisualFeature.Tags, VisualFeature.Categories, VisualFeature.Description };
        var analysisResult = await visionClient.AnalyzeImageAsync(imageStream, features.ToList(), null);
        AnalysisLabel.Text = string.Empty;
        analysisResult.Description.Tags.ToList().ForEach(tag => AnalysisLabel.Text = AnalysisLabel.Text + tag + "\n");
    }
    catch (Microsoft.ProjectOxford.Vision.ClientException ex)
    {
        AnalysisLabel.Text = ex.Error.Message;
    }
}

```

Now you can invoke it in "selectImage" method:

```cs
async void selectImage()
{
    var selectedImage = await CrossMedia.Current.PickPhotoAsync();
    SelectedPictureImageView.Image =  new UIImage(NSData.FromStream(selectedImage.GetStream()));
    await analyseImage(selectedImage.GetStream());
}

```

Once you select image, Microsoft Cognitive Services will analyze it and return the result:

[<img src="http://i.stack.imgur.com/c1RHw.png" alt="enter image description here" />](http://i.stack.imgur.com/c1RHw.png)

Remember that you image cannot be too large - in this case you will receive information like below:

[<img src="http://i.stack.imgur.com/7ReZi.png" alt="enter image description here" />](http://i.stack.imgur.com/7ReZi.png)

There are many other services which you can try to use. Please refer to the official documentation (link attached) to discover more.



#### Remarks


In this example we used Microsoft.ProjectOxford.Vision NuGet package:
[https://www.nuget.org/packages/Microsoft.ProjectOxford.Vision/](https://www.nuget.org/packages/Microsoft.ProjectOxford.Vision/)

To read more about Microsoft Cognitive Services please refer to the official documentation:
[https://www.microsoft.com/cognitive-services/en-us/computer-vision-api](https://www.microsoft.com/cognitive-services/en-us/computer-vision-api)

Please find uploaded sample on my GitHub:
[https://github.com/Daniel-Krzyczkowski/XamarinIOS/tree/master/XamariniOS_CognitiveServices](https://github.com/Daniel-Krzyczkowski/XamarinIOS/tree/master/XamariniOS_CognitiveServices)

I also attach link to my blog where I presented how to use Cognitive Services with Xamarin Forms application:
[http://mobileprogrammer.pl](http://mobileprogrammer.pl)

