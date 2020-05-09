---
metaTitle: "iOS - Create a video from images"
description: "Create Video from UIImages"
---

# Create a video from images


Create a video from images using AVFoundation



## Create Video from UIImages


First of all you need to create `AVAssetWriter`

```swift
NSError *error = nil;
NSURL *outputURL = <#NSURL object representing the URL where you want to save the video#>;
AVAssetWriter *assetWriter = [AVAssetWriter assetWriterWithURL:outputURL fileType:AVFileTypeQuickTimeMovie error:&error];
if (!assetWriter) {
    // handle error
}

```

`AVAssetWriter` needs at least one asset writer input.

```

NSDictionary *writerInputParams = [NSDictionary dictionaryWithObjectsAndKeys:
                                              AVVideoCodecH264, AVVideoCodecKey,
                                              [NSNumber numberWithInt:renderSize.width], AVVideoWidthKey,
                                              [NSNumber numberWithInt:renderSize.height], AVVideoHeightKey,
                                              AVVideoScalingModeResizeAspectFill, AVVideoScalingModeKey,
                                              nil];
    
 AVAssetWriterInput *assetWriterInput = [AVAssetWriterInput assetWriterInputWithMediaType:AVMediaTypeVideo outputSettings:writerInputParams];
 if ([assetWriter canAddInput:assetWriterInput]) {
     [assetWriter addInput:assetWriterInput];
 } else {
     // show error message
 }

```

To append `CVPixelBufferRef`'s to `AVAssetWriterInput` we need to create `AVAssetWriterInputPixelBufferAdaptor`

```swift
NSDictionary *attributes = [NSDictionary dictionaryWithObjectsAndKeys:
                            [NSNumber numberWithUnsignedInt:kCVPixelFormatType_32ARGB], (NSString*)kCVPixelBufferPixelFormatTypeKey,
                            [NSNumber numberWithBool:YES], (NSString *)kCVPixelBufferCGImageCompatibilityKey,
                            [NSNumber numberWithBool:YES], (NSString *)kCVPixelBufferCGBitmapContextCompatibilityKey,
                            nil];
AVAssetWriterInputPixelBufferAdaptor *writerAdaptor = [AVAssetWriterInputPixelBufferAdaptor assetWriterInputPixelBufferAdaptorWithAssetWriterInput:assetWriterInput sourcePixelBufferAttributes:attributes];

```

Now we can start writing

```swift
[assetWriter startWriting];
[assetWriter startSessionAtSourceTime:kCMTimeZero];
[assetWriterInput requestMediaDataWhenReadyOnQueue:exportingQueue usingBlock:^{
    for (int i = 0; i < images.count; ++i) {
        while (![assetWriterInput isReadyForMoreMediaData]) {
            [NSThread sleepForTimeInterval:0.01];
            // can check for attempts not to create an infinite loop 
        }

        UIImage *uIImage = images[i];
        
        CVPixelBufferRef buffer = NULL;
        CVReturn err = PixelBufferCreateFromImage(uIImage.CGImage, &buffer);
        if (err) {
            // handle error
        }

        // frame duration is duration of single image in seconds   
        CMTime presentationTime = CMTimeMakeWithSeconds(i * frameDuration, 1000000);
        
        [writerAdaptor appendPixelBuffer:buffer withPresentationTime:presentationTime];
        
        CVPixelBufferRelease(buffer);
    }

    [assetWriterInput markAsFinished];
    [assetWriter finishWritingWithCompletionHandler:^{
        if (assetWriter.error) {
            // show error message
        } else {
            // outputURL
        }
    }];
}];

```

Here is a function to get `CVPixelBufferRef` from `CGImageRef`

```swift
CVReturn PixelBufferCreateFromImage(CGImageRef imageRef, CVPixelBufferRef *outBuffer) {
    CIContext *context = [CIContext context];
    CIImage *ciImage = [CIImage imageWithCGImage:imageRef];
    
    NSDictionary *attributes = [NSDictionary dictionaryWithObjectsAndKeys:
                                [NSNumber numberWithBool:YES], (NSString *)kCVPixelBufferCGBitmapContextCompatibilityKey,
                                [NSNumber numberWithBool:YES], (NSString *)kCVPixelBufferCGImageCompatibilityKey
                                ,nil];
    
    CVReturn err = CVPixelBufferCreate(kCFAllocatorDefault, CGImageGetWidth(imageRef), CGImageGetHeight(imageRef), kCVPixelFormatType_32ARGB, (__bridge CFDictionaryRef _Nullable)(attributes), outBuffer);
    if (err) {
        return err;
    }
    
    if (outBuffer) {
        [context render:ciImage toCVPixelBuffer:*outBuffer];
    }
   
    return kCVReturnSuccess;
}

```

