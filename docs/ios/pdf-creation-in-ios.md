---
metaTitle: "iOS - PDF Creation in iOS"
description: "Create PDF, Show PDF, Multiple page PDF, Create PDF from any Microsoft Document loaded in UIWebview"
---

# PDF Creation in iOS




## Create PDF


```swift
UIGraphicsBeginPDFContextToFile(fileName, CGRectZero, nil);

UIGraphicsBeginPDFPageWithInfo(CGRectMake(0, 0, 612, 792), nil);
    
[self drawText];
    
UIGraphicsEndPDFContext();

```

**fileName** is the document file where You are going to append or attach

```

NSString* temporaryFile = @"firstIOS.PDF";
    NSArray *arrayPaths =
    NSSearchPathForDirectoriesInDomains(
                                        NSDocumentDirectory,
                                        NSUserDomainMask,
                                        YES);
    
    NSString *path = [arrayPaths objectAtIndex:0];
    
    NSString* fileName = [path stringByAppendingPathComponent:fileName];

```

Where **drawText** is

```

  (void)drawText
{
    NSString* textToDraw = @"Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book.";
   
    CFStringRef stringRef = (__bridge CFStringRef)textToDraw;
    
    CFAttributedStringRef currentText = CFAttributedStringCreate(NULL, stringRef, NULL);
    
    CTFramesetterRef framesetter = CTFramesetterCreateWithAttributedString(currentText);
    
    CGRect frameRect = CGRectMake(0, 0, 300, 100);
    
    CGMutablePathRef framePath = CGPathCreateMutable();
    
    CGPathAddRect(framePath, NULL, frameRect);
    
    CFRange currentRange = CFRangeMake(0, 0);
    
    CTFrameRef frameRef = CTFramesetterCreateFrame(framesetter, currentRange, framePath, NULL);
    CGPathRelease(framePath);
    
    CGContextRef currentContext = UIGraphicsGetCurrentContext();
    
   
    CGContextSetTextMatrix(currentContext, CGAffineTransformIdentity);
    
   
    CGContextTranslateCTM(currentContext, 0, 450);
    
    CGContextScaleCTM(currentContext, 2, -2);
    
    CTFrameDraw(frameRef, currentContext);
    
    CFRelease(frameRef);

    CFRelease(stringRef);

    CFRelease(framesetter);
}

```

[<img src="http://i.stack.imgur.com/FC6Jw.png" alt="PDF" />](http://i.stack.imgur.com/FC6Jw.png)



## Show PDF


```

 NSString* fileName = @"firstIOS.PDF";


NSArray *arrayPaths =
NSSearchPathForDirectoriesInDomains(
                                    NSDocumentDirectory,
                                    NSUserDomainMask,
                                    YES);

NSString *path = [arrayPaths objectAtIndex:0];

NSString* pdfFileName = [path stringByAppendingPathComponent:fileName];

UIWebView* webView = [[UIWebView alloc] initWithFrame:CGRectMake(0, 0, 320, 480)];

NSURL *url = [NSURL fileURLWithPath:pdfFileName];

NSURLRequest *request = [NSURLRequest requestWithURL:url];


[webView setScalesPageToFit:YES];

[webView loadRequest:request];

[self.view addSubview:webView];

```



## Multiple page PDF


```

UIGraphicsBeginPDFContextToFile(fileName, CGRectZero, nil);
    
    UIGraphicsBeginPDFPageWithInfo(CGRectMake(0, 0, 600, 792), nil);
    
    UIGraphicsBeginPDFPageWithInfo(CGRectMake(0, 0, 600, 792), nil);
    
    UIGraphicsBeginPDFPageWithInfo(CGRectMake(0, 0, 600, 792), nil);
         
    UIGraphicsEndPDFContext();

```



## Create PDF from any Microsoft Document loaded in UIWebview


```swift
#define kPaperSizeA4 CGSizeMake(595.2,841.8)

```

First of all implement UIPrintPageRenderer protocol

```swift
@interface UIPrintPageRenderer (PDF)

- (NSData*) printToPDF;

@end

@implementation UIPrintPageRenderer (PDF)

- (NSData*) printToPDF
{
    NSMutableData *pdfData = [NSMutableData data];
    UIGraphicsBeginPDFContextToData( pdfData, self.paperRect, nil );
    [self prepareForDrawingPages: NSMakeRange(0, self.numberOfPages)];
    CGRect bounds = UIGraphicsGetPDFContextBounds();
    for ( int i = 0 ; i < self.numberOfPages ; i++ )
    {
        UIGraphicsBeginPDFPage();
        [self drawPageAtIndex: i inRect: bounds];
    }
    UIGraphicsEndPDFContext();
    return pdfData;
}
@end

```

Then, call below method after document finished loading in `UIWebView`

```swift
-(void)createPDF:(UIWebView *)webView {

UIPrintPageRenderer *render = [[UIPrintPageRenderer alloc] init];
[render addPrintFormatter:webView.viewPrintFormatter startingAtPageAtIndex:0];

float padding = 10.0f;
CGRect paperRect = CGRectMake(0, 0, kPaperSizeA4.width, kPaperSizeA4.height);
CGRect printableRect = CGRectMake(padding, padding, kPaperSizeA4.width-(padding * 2), kPaperSizeA4.height-(padding * 2));

[render setValue:[NSValue valueWithCGRect:paperRect] forKey:@"paperRect"];
[render setValue:[NSValue valueWithCGRect:printableRect] forKey:@"printableRect"];

NSData *pdfData = [render printToPDF];

dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
    
    if (pdfData) {
        [pdfData writeToFile:directoryPath atomically: YES];
    }
    else
    {
        NSLog(@"PDF couldnot be created");
    }
});}

```

