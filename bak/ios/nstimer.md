---
metaTitle: "iOS - NSTimer"
description: "Creating a Timer, Manually firing a timer, Timer frequency options, Invalidating a timer, Passing of data using Timer"
---

# NSTimer



## Creating a Timer


This will create a timer to call the `doSomething` method on `self` in 5 seconds.

**Swift**

```swift
let timer = NSTimer.scheduledTimerWithTimeInterval(5,
                               target: self,
                             selector: Selector(doSomething()),
                             userInfo: nil,
                              repeats: false)

```

**Swift 3**

```

let timer = Timer.scheduledTimer(timeInterval: 1,
                                        target: self, 
                                      selector: #selector(doSomething()), 
                                      userInfo: nil, 
                                       repeats: true)

```

**Objective-C**

```swift
NSTimer *timer = [NSTimer scheduledTimerWithTimeInterval:5.0 target:self selector:@selector(doSomething) userInfo:nil repeats:NO];

```

Setting repeats to `false/NO` indicates that we want the timer to fire only once. If we set this to `true/YES`, it would fire every five seconds until manually invalidated.



## Manually firing a timer


**Swift**

```swift
timer.fire()

```

**Objective-C**

```swift
[timer fire];

```

Calling the `fire` method causes an NSTimer to perform the task it would have usually performed on a schedule.

In a **non-repeating timer**, this will automatically invalidate the timer. That is, calling `fire` before the time interval is up will result in only one invocation.

In a **repeating timer**, this will simply invoke the action without interrupting the usual schedule.



## Timer frequency options


### Repeated Timer event

**Swift**

```swift
class ViewController: UIViewController {
 
    var timer = NSTimer()
    
    override func viewDidLoad() {
        NSTimer.scheduledTimerWithTimeInterval(1.0, target: self, selector: Selector(self.timerMethod()), userInfo: nil, repeats: true)
    }

    func timerMethod() {
        print("Timer method called")
    }

    func endTimer() {
        timer.invalidate()
    }
}

```

**Swift 3**

```swift
class ViewController: UIViewController {
     
        var timer = Timer()
        
        override func viewDidLoad() {
            Timer.scheduledTimer(timeInterval: 1.0, target: self, selector: #selector(self.timerMethod()), userInfo: nil, repeats: true)
        }
    
        func timerMethod() {
            print("Timer method called")
        }

        func endTimer() {
            timer.invalidate()
        }
    }

```

Must be invalidated manually if desired.

**Swift**

### Non-repeated delayed Timer event

```swift
NSTimer.scheduledTimerWithTimeInterval(3.0, target: self, selector: Selector(self.timerMethod()), userInfo: nil, repeats: false)

```

**Swift 3**

```

Timer.scheduledTimer(timeInterval: 3.0, target: self, selector: #selector(self.timerMethod()), userInfo: nil, repeats: false)

```

Timer will be fired once, 3 seconds after time of execution. Will be invalidated automatically, once fired.



## Invalidating a timer


**Swift**

```swift
timer.invalidate()

```

**Objective-C**

```swift
[timer invalidate];

```

This will stop the timer from firing. **Must be called from the thread the timer was created in,** see [Apple's notes](https://developer.apple.com/library/ios/documentation/Cocoa/Reference/Foundation/Classes/NSTimer_Class/#//apple_ref/occ/instm/NSTimer/invalidate):

> 
You must send this message from the thread on which the timer was installed. If you send this message from another thread, the input source associated with the timer may not be removed from its run loop, which could prevent the thread from exiting properly.


**Notes: Once timer has been invalidated, its impossible to fire same invalidated timer.Instead you need to initialise the invalidated timer again and trigger fire method.**



## Passing of data using Timer


If you you want to pass some data with the timer trigger you can do it with the  `userInfo`parameter.

Here is the simple approach that gives brief idea about how you can pass the data to triggered method from the Timer.

[**Swift 3**]

```swift
Timer.scheduledTimer(timeInterval: 1.0, target: self, selector:#selector(iGotCall(sender:)), userInfo: ["Name": "i am iOS guy"], repeats:true)

```

[**Objective - C**]

```swift
NSTimer* timer = [NSTimer scheduledTimerWithTimeInterval:1.0
                                                      target:self
                                                    selector:@selector(iGotCall:)
                                                    userInfo:@"i am iOS guy" repeats:YES];

```

The above line of code passing `["Name": "i am iOS guy"]` into the `userInfo`.
So now when the `iGotCall` get call you can get the passed value as below code snippet.

[**Swift 3**]

```swift
func iGotCall(sender: Timer) {
        print((sender.userInfo)!)
    }

```

[**Objective - C**]

```swift
- (void)iGotCall:(NSTimer*)theTimer {
    NSLog (@"%@", (NSString*)[theTimer userInfo]);
}

```



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|`interval`|The time, in seconds, to wait beforing firing the timer; or, in repeating timers, the time between firings.
|`target`|The object to call the `selector` on
|`selector`|In Swift, a `Selector` object specifying the method to call on the `target`
|`repeats`|If `false`, fire the timer only once. If `true`, fire the timer every `interval` seconds.



#### Remarks


An [`NSTimer`](https://developer.apple.com/library/ios/documentation/Cocoa/Reference/Foundation/Classes/NSTimer_Class/) allows you to send a message to a target after a specified period of time elapses.

