---
metaTitle: "iOS - QR Code Scanner"
description: "UIViewController scanning for QR and displaying video input, Scanning QR code with AVFoudation framework"
---

# QR Code Scanner


QR (Quick Response) codes are two-dimensional barcodes which are widely used on machine-readable optical labels. iOS do provide a way to read the QR codes by using `AVFoundation` framework from iOS 7 onwards. This framework provides set of API's to setup/open the camera and read QR codes from the camera feed.



## UIViewController scanning for QR and displaying video input


```swift
import AVFoundation
class QRScannerViewController: UIViewController,
     AVCaptureMetadataOutputObjectsDelegate {
   
    func viewDidLoad() {
        self.initCaptureSession()
    }
    
    private func initCaptureSession() {
        let captureDevice = AVCaptureDevice
            .defaultDevice(withMediaType: AVMediaTypeVideo)
        do {
            let input = try AVCaptureDeviceInput(device: captureDevice)
            let captureMetadataOutput = AVCaptureMetadataOutput()
            self.captureSession?.addOutput(captureMetadataOutput)
            captureMetadataOutput.setMetadataObjectsDelegate(self,
                 queue: DispatchQueue.main)
            captureMetadataOutput
                .metadataObjectTypes = [AVMetadataObjectTypeQRCode]
            
            self.videoPreviewLayer = 
                AVCaptureVideoPreviewLayer(session: self.captureSession)
            self.videoPreviewLayer?
                .videoGravity = AVLayerVideoGravityResizeAspectFill
            self.videoPreviewLayer?.frame =    
                self.view.layer.bounds

            self._viewController?.view.layer
                .addSublayer(videoPreviewLayer!)
            self.captureSession?.startRunning()
        } catch {
            //TODO: handle input open error
        }
    }
    private func dismissCaptureSession() {
        if let running = self.captureSession?.isRunning, running {
            self.captureSession?.stopRunning()
        }
        self.captureSession = nil
        self.videoPreviewLayer?.removeFromSuperLayer()
        self.videoPreviewLayer = nil
    }
    
    func captureOutput(_ captureOutput: AVCaptureOutput, 
        didOutputMetadataObjects metadataObjects: [Any]!, 
        from connection: AVCaptureConnection) {
        guard metadataObjects != nil && metadataObjects.count != 0 else {
            //Nothing captured
            return
        }

        if let metadataObj = 
            metadataObjects[0] as? AVMetadataMachineReadableCodeObject {
            guard metadataObj.type == AVMetadataObjectTypeQRCode else {
                return
            }

            let barCodeObject = videoPreviewLayer?
                .transformedMetadataObject(for: 
                    metadataObj as AVMetadataMachineReadableCodeObject)
                 as! AVMetadataMachineReadableCodeObject
        
            if let qrValue = metadataObj.stringValue {
                self.handleQRRead(value: qrValue)
            }
        }
    }

    private handleQRRead(value: String) {
        //TODO: Handle the read qr
    }
    private captureSession: AVCaptureSession?
    private videoPreviewLayer: AVCaptureVideo
}

```

`handleQRRead` - will be called on a successful scan
`initCaptureSession` - initialize scanning for QR and camera input
`dismissCaptureSession` - hide the camera input and stop scanning



## Scanning QR code with AVFoudation framework


Prior to iOS 7 when you want to scan a QR code, we might need to rely on third party frameworks or libraries like [zBar](https://github.com/ZBar/ZBar/tree/master/iphone) or [zXing](https://github.com/TheLevelUp/ZXingObjC). But Apple introduced `AVCaptureMetaDataOutput` from iOS 7 for reading barcodes.

To read QR code using `AVFoundation` we need to setup/create `AVCaptureSession` and use `captureOutput:didOutputMetadataObjects:fromConnection:` delegate method.

### Step 1

Import `AVFoundation` framework and confirm to `AVCaptureMetadataOutputObjectsDelegate` protocol

### Step 2

QR code reading is totally based on video capture. So to capture continuous video create an `AVCaptureSession` and set up device input and output. Add the below code in view controller `viewDidLoad` method

### Step 3

Implement `AVCaptureMetadataOutputObjectsDelegate` delegate method to read the QR code

here metadata object can give you the bounds of the QR code read on the camera feed as well. To get the bounds simply pass the metadata object to `videoPreviewLayer`' s `transformedMetadataObject` method like below.

