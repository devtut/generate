---
metaTitle: "iOS - AWS SDK"
description: "Upload an image or a video to S3 using AWS SDK"
---

# AWS SDK



## Upload an image or a video to S3 using AWS SDK


Before starting with the example I'd recommend to create a Singleton with a delegate class member so you could achieve a use case of uploading a file in the background and let the user keep using your app while the files are being uploaded even when the app is the background.

Let's start, first, we should create an enum that represent the S3 configuration:

```swift
enum S3Configuration : String
{
    case IDENTITY_POOL_ID   = "YourIdentityPoolId"
    case BUCKET_NAME        = "YourBucketName"
    case CALLBACK_KEY       = "YourCustomStringForCallBackWhenUploadingInTheBackground"
    case CONTENT_TYPE_IMAGE = "image/png"
    case CONTENT_TYPE_VIDEO = "video/mp4"
}

```

Now, we should set the credentials when your app launch for the first time, thus, we should set them inside the `AppDelegate` at the `didFinishLaunchingWithOptions` method (pay attention that you should set your region at the `regionType` param):

```swift
func application(application: UIApplication, didFinishLaunchingWithOptions launchOptions: [NSObject: AnyObject]?) -> Bool
{
  let credentialProvider = AWSCognitoCredentialsProvider(regionType: .EUWest1, identityPoolId: S3Configuration.IDENTITY_POOL_ID.rawValue)
  let configuration = AWSServiceConfiguration(region: .EUWest1, credentialsProvider: credentialProvider)
  AWSS3TransferUtility.registerS3TransferUtilityWithConfiguration(configuration, forKey: S3Configuration.CALLBACK_KEY.rawValue)
}

```

Since we are already inside the AppDelegate, we should implement the background callback that is handled by the AWS SDK:

```swift
func application(application: UIApplication, handleEventsForBackgroundURLSession identifier: String, completionHandler: () -> Void)
{
    //  Will print the identifer you have set at the enum: .CALLBACK_KEY
    print("Identifier: " + identifier)
    //  Stores the completion handler.
    AWSS3TransferUtility.interceptApplication(application,
                                              handleEventsForBackgroundURLSession: identifier,
                                              completionHandler: completionHandler)
}

```

Now, when the user will move the app to the background your upload will continue the actual upload.

In order to upload the file using the AWS SDK we will have to write the file to the device and give the SDK the actual path. For the sake of the example, imagine we have a UIImage (could be a video also..) and we will write it to a temp folder:

```swift
// Some image....
let image = UIImage()
let fileURL = NSURL(fileURLWithPath: NSTemporaryDirectory()).URLByAppendingPathComponent(fileName)
let filePath = fileURL.path!
let imageData = UIImageJPEGRepresentation(image, 1.0)
imageData!.writeToFile(filePath, atomically: true)

```

FileURL and fileName will be used for the actual uploading later.

There are 2 closures we will have to define that are provided by the AWS SDK,

1. `AWSS3TransferUtilityUploadCompletionHandlerBlock` - A closure that notifies when the upload is done (or not)
1. `AWSS3TransferUtilityUploadProgressBlock` - A closure that notifies each byte sent

If you plan to have a Singleton you should define those types as class members. The implementation should look like this:

```swift
var completionHandler : AWSS3TransferUtilityUploadCompletionHandlerBlock? =
    { (task, error) -> Void in

        if ((error) != nil)
        {
          print("Upload failed")
        }
        else
        {
          print("File uploaded successfully")
        }
    }

var progressBlock : AWSS3TransferUtilityUploadProgressBlock? = 
    { [unowned self] (task, bytesSent:Int64, totalBytesSent:Int64,  totalBytesExpectedToSend:Int64) -> Void in

     let progressInPercentage = Float(Double(totalBytesSent) / Double(totalBytesExpectedToSend)) * 100
     print(progressInPercentage)
    }

```

**NOTE:** If you are using a Singleton you might want to define a delegate that will report back with progress or when the file is done. If you are not using a Singleton you can create a static method that would have the relevant types:

```

   static func uploadImageToS3(fileURL : NSURL,
                               fileName : String,
                progressFunctionUpdater : Float -> Void,
                            resultBlock : (NSError?) -> Void)
{
 //    Actual implementation .....
 //    ...
 //    ...
}

```


1. `progressFunctionUpdater` - will report back to a function with progress.
1. `resultBlock` - If you return nil then upload was successfully else, you send the error object

Ladies and gentlemen, the actual upload:

```

       let fileData = NSData(contentsOfFile: fileURL.relativePath!)

        let expression = AWSS3TransferUtilityUploadExpression()
        expression.uploadProgress = progressBlock
        
        let transferUtility = AWSS3TransferUtility.S3TransferUtilityForKey(S3Configuration.CALLBACK_KEY.rawValue)
        
        transferUtility?.uploadData(fileData!,
            bucket: S3Configuration.BUCKET_NAME.rawValue,
            key: fileName,
            contentType: S3Configuration.CONTENT_TYPE_IMAGE.rawData,
            expression: expression,
            completionHander: completionHandler).continueWithBlock
            { (task : AWSTask) -> AnyObject? in
                
                if let error = task.error
                {
                    print(error)
                }
                if let exception = task.exception
                {
                    print("Exception: " + exception.description)
                }
                if let uploadTask = task.result as? AWSS3TransferUtilityUploadTask
                {
                    print("Upload started...")
                }
                
                return nil
        }

```

Happy S3 uploading :)

