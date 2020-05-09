---
metaTitle: "iOS - Concurrency"
description: "Dispatch group - waiting for other threads completed., Running code concurrently -- Running code while running other code, Executing on the main thread"
---

# Concurrency


Related topic: [Grand Central Dispatch](https://stackoverflow.com/documentation/ios/4626/grand-central-dispatch#t=20170217041555082968)



## Dispatch group - waiting for other threads completed.


```swift
dispatch_group_t preapreWaitingGroup = dispatch_group_create();

dispatch_group_enter(preapreWaitingGroup);
[self doAsynchronousTaskWithComplete:^(id someResults, NSError *error) { 
    // Notify that this task has been completed.
    dispatch_group_leave(preapreWaitingGroup);  
}]

dispatch_group_enter(preapreWaitingGroup);
[self doOtherAsynchronousTaskWithComplete:^(id someResults, NSError *error) { 
    dispatch_group_leave(preapreWaitingGroup);  
}]

dispatch_group_notify(preapreWaitingGroup, dispatch_get_main_queue(), ^{
    // This block will be executed once all above threads completed and call dispatch_group_leave
    NSLog(@"Prepare completed. I'm readyyyy");
});

```

**Update 1.** Swift 3 version.

```swift
let prepareGroup = DispatchGroup()
prepareGroup.enter()
doAsynchronousTaskWithComplete() { (someResults, error) in
    // Notify that this task has been completed.
    prepareGroup.leave()
}

prepareGroup.enter()
doOtherAsynchronousTaskWithComplete() { (someResults, error) in
    // Notify that this task has been completed.
    prepareGroup.leave()
}

prepareGroup.notify(queue: DispatchQueue.main) {
    // This block will be executed once all above threads completed and call dispatch_group_leave
    print("Prepare completed. I'm readyyyy")
}

```



## Running code concurrently -- Running code while running other code


Say you want to perform in action (in this case, logging "Foo"), while doing something else (logging "Bar"). Normally, if you don't use concurrency, one of these actions is going to be fully executed, and the other run will run only after it's completely finished. But with concurrency, you can make both actions run at the same time:

```swift
dispatch_async(dispatch_queue_create("Foo", DISPATCH_QUEUE_CONCURRENT), ^{
    for (int i = 0; i < 100; i++) {
        NSLog(@"Foo");
        usleep(100000);
    }
});

for (int i = 0; i < 100; i++) {
    NSLog(@"Bar");
    usleep(50000);
}

```

This will log "Foo" 100 times, pausing for 100ms each time it logs, but it will do all this on a separate thread. While `Foo` is being logged, "Bar" will also be logged in 50ms intervals, at the same time. You should ideally see an output with "Foo"s and "Bars" mixed together



## Executing on the main thread


When performing tasks asynchronously there typically becomes a need to ensure a piece of code is run on the main thread.  For example you may want to hit a REST API asynchronously, but put the result in a UILabel on the screen.  Before updating the UILabel you must ensure that your code is run on the main thread:

```swift
dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
    //Perform expensive tasks
    //...

    //Now before updating the UI, ensure we are back on the main thread
    dispatch_async(dispatch_get_main_queue(), ^{
        label.text = //....
    });
}

```

Whenever you update views on the screen, always ensure you are doing so on the main thread, otherwise undefined behavior could occur.



#### Syntax


- dispatch_async - Runs a block of code in a separate queue, and doesn't stop the current queue. If the queue is on a different thread than the one dispatch_async was called on, the code in the block will run while code after the dispatch_async also runs
- dispatch_sync - Runs a block of code in a separate queue, and **does** stop the current queue. If the queue is on a different thread than the one dispatch_async was called on, the code in the block will run, and execution on the thread where the method was called will only resume after it finishes



#### Parameters


|queue|The queue that the code in the dispatch block will run in. A **queue** is like (but not exactly the same as) a thread; code in different queues can run in parallel. Use `dispatch_get_main_queue` to get the queue for the main thread To create a new queue, which in turn creates a new thread, use `dispatch_queue_create("QUEUE_NAME", DISPATCH_QUEUE_CONCURRENT)`. The first parameter is the name of the queue, which is displayed in the debugger if you pause while the block is still running. The second parameter doesn't matter unless you want to use the same queue for multiple `dispatch_async` or `dispatch_sync` calls. It describes what happens when another block is put on the same queue; `DISPATCH_QUEUE_CONCURRENT` will cause both blocks to run at the same time, while `DISPATCH_QUEUE_SERIAL` will make the second block wait for the first block to finish
|---|---|---|---|---|---|---|---|---|---
|block|Code in this block will run in the queue `queue`; put code you want to run on the separate queue here. A helpful tip: If you're writing this in Xcode and the block argument has the blue outline around it, double click on the argument and Xcode will automatically make an empty block (this applies to all block arguments in any function or method)



#### Remarks


Whenever you do something on a separate thread, which happens when using queues, it's important to maintain thread safety. Some methods, in particular ones for `UIView`s, may not work and/or crash on threads other than main thread. Also, make sure not to change anything (variables, properties, etc.) that is also being used on the main thread, unless you are accounting for this change

