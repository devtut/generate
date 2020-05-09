---
metaTitle: "Swift - Completion Handler"
description: "Completion handler with no input argument, Completion handler with input argument"
---

# Completion Handler


Virtually all apps are using asynchronous functions to keep the code from blocking the main thread.



## Completion handler with no input argument


```swift
func sampleWithCompletion(completion:@escaping (()-> ())){
    let delayInSeconds = 1.0
    DispatchQueue.main.asyncAfter(deadline: DispatchTime.now() + delayInSeconds) {
        
        completion()
        
    }
}

//Call the function
sampleWithCompletion {
    print("after one second")
}

```



## Completion handler with input argument


```swift
enum ReadResult{
    case Successful
    case Failed
    case Pending
}

struct OutpuData {
    var data = Data()
    var result: ReadResult
    var error: Error?
}

func readData(from url: String, completion: @escaping (OutpuData) ->  Void) {
    var _data = OutpuData(data: Data(), result: .Pending, error: nil)
    DispatchQueue.global().async {
        let url=URL(string: url)
        do {
            let rawData = try Data(contentsOf: url!)
            _data.result = .Successful
            _data.data = rawData
            completion(_data)
        }
        catch let error {
            _data.result = .Failed
            _data.error = error
            completion(_data)
        }
        
    }
}

readData(from: "https://raw.githubusercontent.com/trev/bearcal/master/sample-data-large.json") { (output) in
    switch output.result {
    case .Successful:
        break
    case .Failed:
        break
    case .Pending:
        break
        
    }
}

```

