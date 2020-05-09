---
metaTitle: "iOS - DispatchGroup"
description: "Introduction"
---

# DispatchGroup


Related topics:

[Grand Central Dispatch](https://stackoverflow.com/documentation/ios/4626/grand-central-dispatch#t=201702170420103061945)

[Concurrency](https://stackoverflow.com/documentation/ios/1090/concurrency#t=201702170420548190993)



## Introduction


Suppose you have multiple threads running. Each thread is doing one task. You want to get notified either on the mainThread OR another thread, when all the task-threads are completed.

The simplest solution to such a problem is a `DispatchGroup`.

When using a `DispatchGroup`, for each request, you `enter` the group and for each completed request, you `leave` the group.

When there are no longer requests in the group, you will be `notify` (notified).

**Usage:**

```swift
import UIKit

class ViewController: UIViewController {

    override func viewDidLoad() {
        super.viewDidLoad()
        
        
        let dispatchGroup = DispatchGroup() //Create a group for the tasks.
        let session: URLSession = URLSession.shared
        
        dispatchGroup.enter() //Enter the group for the first task.
        
        let firstTask = session.dataTask(with: URLRequest(url: URL(string: "https://stackoverflow.com")!)) { (data, response, error) in
            
            //Process Response..
            
            dispatchGroup.leave() //Leave the group for the first task.
        }

        
        dispatchGroup.enter()  //Enter the group for the second task.
        
        let secondTask = session.dataTask(with: URLRequest(url: URL(string: "https://google.ca")!)) { (data, response, error) in
            
            //Process Response..
            
            dispatchGroup.leave()  //Leave the group for the second task.
        }
        
        
        //Get notified on the main thread/queue.. when ALL of the tasks above has been completed.
        dispatchGroup.notify(queue: DispatchQueue.main) { 
            
            print("Every task is complete")
            
        }
        
        
        //Start the tasks.
        firstTask.resume()
        secondTask.resume()
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
    }
}

```

With the above, you don't have to `wait` infinitely until all the tasks are completed. You can display a loader BEFORE all the tasks have started and dismiss the loader AFTER all tasks are completed. This way, your main thread does not get blocked and your code remains clean.

Now suppose you also wanted the tasks to be `ordered` or add their responses to an array sequentially. You could do the following:

```swift
import UIKit

//Locking mechanism..
func synchronized(_ lock: AnyObject, closure: () -> Void) {
    objc_sync_enter(lock)
    closure()
    objc_sync_exit(lock)
}

class ViewController: UIViewController {

    let lock = NSObject() //Object to lock on.
    var responseArray = Array<Data?>() //Array of responses.
    
    override func viewDidLoad() {
        super.viewDidLoad()
        
        let dispatchGroup = DispatchGroup()
        let session: URLSession = URLSession.shared
        
        dispatchGroup.enter() //Enter the group for the first task.
        
        let firstTask = session.dataTask(with: URLRequest(url: URL(string: "https://stackoverflow.com")!)) { (data, response, error) in
            
            //Process Response..

            synchronized(self.lock, closure: { () -> Void in
                self.responseArray[0] = data ?? nil
            })

            dispatchGroup.leave() //Leave the group for the first task.
        }

        
        dispatchGroup.enter()  //Enter the group for the second task.
        
        let secondTask = session.dataTask(with: URLRequest(url: URL(string: "https://google.ca")!)) { (data, response, error) in
            
            //Process Response..
            
            synchronized(self.lock, closure: { () -> Void in
                self.responseArray[1] = data ?? nil
            })
            
            dispatchGroup.leave()  //Leave the group for the second task.
        }
        
        
        //Get notified on the main thread.. when ALL of the requests above has been completed.
        dispatchGroup.notify(queue: DispatchQueue.main) { 
            
            print("Every task is complete..")
            
            for i in 0..<self.responseArray.count {
                
                if self.responseArray[i] == nil {
                    print("Request #\(i) Failed.\n")
                }
                else {
                    print("Request #\(i) Succeeded.\n")
                }
            }
        }
        
        //Two tasks added to the array. Responses are assumed nil until they complete.
        self.responseArray.append(nil)
        self.responseArray.append(nil)
        
        //Start the tasks.
        firstTask.resume()
        secondTask.resume()
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
    }
}

```

**Notes**

Every entry must have an exit in a `DispatchGroup`. If you forget to `leave` after `entering`, you are setting yourself up. You will NEVER be notified when the tasks are completed.

The amount of `enter` must equal the amount of `leave`.

