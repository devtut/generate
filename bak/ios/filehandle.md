---
metaTitle: "iOS - FileHandle"
description: "Read file from document directory in chunks"
---

# FileHandle


Read file in chunks from document directory



## Read file from document directory in chunks


I get the file path from document directory and read that file in chunks of 1024 and save (append) to `NSMutableData` object or you can directly write to socket.

```swift
// MARK: - Get file data as chunks Methode.
func getFileDataInChunks() {
    
    let doumentDirectoryPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] as NSString
    let filePath = doumentDirectoryPath.appendingPathComponent("video.mp4")
    
    
    //Check file exits at path or not.
    if FileManager.default.fileExists(atPath: filePath) {
        
        let chunkSize = 1024 // divide data into 1 kb
        
        //Create NSMutableData object to save read data.
        let ReadData = NSMutableData()
        
        do {
            
            //open file for reading.
            outputFileHandle = try FileHandle(forReadingFrom: URL(fileURLWithPath: filePath))
            
            // get the first chunk
            var datas = outputFileHandle?.readData(ofLength: chunkSize)
            
            //check next chunk is empty or not.
            while !(datas?.isEmpty)! {
                
                //here I write chunk data to ReadData or you can directly write to socket.
                ReadData.append(datas!)
                
                // get the next chunk
                datas = outputFileHandle?.readData(ofLength: chunkSize)
                
                print("Running: \(ReadData.length)")
            }
            
            //close outputFileHandle after reading data complete.
            outputFileHandle?.closeFile()
            
            print("File reading complete")
            
        }catch let error as NSError {
            print("Error : \(error.localizedDescription)")
        }
    }
}

```

After file reading complete you will get file Data in `ReadData` variable
Here outputFileHandle is a object of `FileHandle`

```swift
var outputFileHandle:FileHandle?

```

