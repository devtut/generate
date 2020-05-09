---
metaTitle: "iOS - GCD (Grand Central Dispatch)"
description: "Dispatch Semaphore, Dispatch Group, Create a dispatch queue, Getting the Main Queue, Serial vs Concurrent Dispatch Queues"
---

# GCD (Grand Central Dispatch)


Grand Central Dispatch (GCD) is Apple's answer to multithreading. It is a lightweight framework for performing tasks synchronously or asynchronously in queues and handles CPU threads for you behind the scenes.

Related Topic: [Concurrency](https://stackoverflow.com/documentation/ios/1090/concurrency#t=201702170245593002465)



## Dispatch Semaphore


> 
<p>DispatchSemaphore provides an efficient implementation of a
traditional counting semaphore, which can be used to control access to
a resource across multiple execution contexts.</p>


A scenario for when to use a semaphore could be if you are doing some file reading/writing, if multiple tasks are trying to read and write from file at the same time, it could increase your performance to make each task wait its turn so as to not overburden the I/O controller.

**Swift 3**

```swift
func do2TasksAtATime () {
    print("starting long running tasks (2 at a time)")
    let sem = DispatchSemaphore(value: 2)            //this semaphore only allows 2 tasks to run at the same time (the resource count)
    for i in 0...7 {                                 //launch a bunch of tasks
        DispatchQueue.global().async {               //run tasks on a background thread
            sem.wait()                               //wait here if no resources available
            sleep(2)                                 //do some long task eg file access (here we are just sleeping for a 2 seconds for demonstration purposes)
            print("long task \(i) done! \(Date())")
            sem.signal()                             //let the semaphore know this resource is now available
        }
    }
}

```

Example output: (notice the time stamps)

```swift
starting long running tasks (2 at a time)
long task 0 done! 2017-02-16 07:11:53 +0000
long task 1 done! 2017-02-16 07:11:53 +0000
long task 2 done! 2017-02-16 07:11:55 +0000
long task 3 done! 2017-02-16 07:11:55 +0000
long task 5 done! 2017-02-16 07:11:57 +0000
long task 4 done! 2017-02-16 07:11:57 +0000
long task 6 done! 2017-02-16 07:11:59 +0000
long task 7 done! 2017-02-16 07:11:59 +0000

```

For more info, refer to the [Apple Docs](https://developer.apple.com/reference/dispatch/dispatchsemaphore)



## Dispatch Group


> 
<p>DispatchGroup allows for aggregate synchronization of work. You can
use them to submit multiple different work items and track when they
all complete, even though they might run on different queues. This
behavior can be helpful when progress can’t be made until all of the
specified tasks are complete.</p>


A Scenario when this could be useful is if you have multiple webservice calls that all need to finish before continuing. For example, you need to download multiple sets of data that needs to be processed by some function. You have to wait for all webservices to complete before calling the function to process all the received data.

**Swift 3**

```swift
func doLongTasksAndWait () {
    print("starting long running tasks")
    let group = DispatchGroup()          //create a group for a bunch of tasks we are about to do
    for i in 0...3 {                     //launch a bunch of tasks (eg a bunch of webservice calls that all need to be finished before proceeding to the next ViewController)
        group.enter()                    //let the group know that something is being added
        DispatchQueue.global().async {   //run tasks on a background thread
            sleep(arc4random() % 4)      //do some long task eg webservice or database lookup (here we are just sleeping for a random amount of time for demonstration purposes)
            print("long task \(i) done!")
            group.leave()                //let group know that the task is finished
        }
    }
    group.wait()                         //will block whatever thread we are on here until all the above tasks have finished (so maybe dont use this function on your main thread)
    print("all tasks done!")
}

```

Alternatively, if you do not want to wait for the groups to finish, but instead want to run a function once all the tasks have completed, use the `notify` function in place of the `group.wait()`

```swift
group.notify(queue: DispatchQueue.main) { //the queue: parameter is which queue this block will run on, if you need to do UI updates, use the main queue
    print("all tasks done!")              //this will execute when all tasks have left the group
}

```

Example output:

```swift
starting long running tasks
long task 0 done!
long task 3 done!
long task 1 done!
long task 2 done!
all tasks done!

```

For more info, refer to the [Apple Docs](https://developer.apple.com/reference/dispatch/dispatchgroup) or the related [topic](https://stackoverflow.com/documentation/ios/4624/dispatchgroup#t=201702170254423773445)



## Create a dispatch queue


You can create your own queue using `dispatch_queue_create`

**Objective-C**

```swift
dispatch_queue_t queue = dispatch_queue_create("com.example.myqueue",  DISPATCH_QUEUE_SERIAL);

```

**Swift**

```swift
// Before Swift 3
let queue = dispatch_queue_create("com.example.myqueue", DISPATCH_QUEUE_SERIAL)
// Swift 3
let queue = DispatchQueue(label: "com.example.myqueue") //default is serial queue, unless .concurrent is specified as an attribute otherwise

```



## Getting the Main Queue


The main queue is the dispatch queue in which all the UI updates take place and the code involving UI changes are placed.

You need to get to the main queue in order to update UI on completion of an asynchronous process like `NSURLSession`

There are two types of main queue calls `synchronous` and `asynchronous`.
When you invoke something `synchronously`, it means that the thread that initiated that operation will wait for the task to finish before continuing. `Asynchronous` means that it will not wait.

**Code Objective-C**

**`Synchronous` Main Queue call**

```swift
dispatch_queue_t queue = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0);

```

**`Asynchronous` Main Queue call**

```swift
dispatch_async(dispatch_get_main_queue(), ^{
   // do work here to Usually to update the User Interface
});

```

**SWIFT 3**

**`Asynchronous` Main Queue call**

```swift
DispatchQueue.main.async {

}

```

**`Synchronous` Main Queue call**

```swift
DispatchQueue.main.sync {

}

```



## Serial vs Concurrent Dispatch Queues


**Swift 3**

**Serial Queue**

```swift
func serialQueues () {
    let serialQueue = DispatchQueue(label: "com.example.serial") //default queue type is a serial queue
    let start = Date ()
    for i in 0...3 {                                             //launch a bunch of tasks
        serialQueue.async {                                      //run tasks on a background thread, using our serial queue
            sleep(2)                                             //do some long task eg webservice or database lookup
            let timeTaken = Date().timeIntervalSince(start)
            print("serial long task \(i) done! total time taken: \(timeTaken)")
        }
    }
}

```

Example output:

```swift
serial long task 0 done! total time taken: 2.07241100072861
serial long task 1 done! total time taken: 4.16347700357437
serial long task 2 done! total time taken: 6.23209798336029
serial long task 3 done! total time taken: 8.30682599544525

```

**Concurrent Queue**

```swift
func concurrentQueues () {
    let concurrentQueue = DispatchQueue(label: "com.example.concurrent", attributes: .concurrent) //explicitly specify the queue to be a concurrent queue
    let start = Date ()
    for i in 0...3 {            //launch a bunch of tasks
        concurrentQueue.async { //run tasks on a background thread, using our concurrent queue
            sleep(2)            //do some long task eg webservice or database lookup
            let timeTaken = Date().timeIntervalSince(start)
            print("concurrent long task \(i) done! total time taken: \(timeTaken)")
        }
    }
}

```

Example output:

```swift
concurrent long task 3 done! total time taken: 2.07092100381851
concurrent long task 0 done! total time taken: 2.07087397575378
concurrent long task 2 done! total time taken: 2.07086700201035
concurrent long task 1 done! total time taken: 2.07089096307755

```

**Discussion**

As we can see from the examples above, a serial queue will complete each task in the order they are submitted to the queue. Each task will wait for the previous task to finish before executing. As for the concurrent queue, each task does not wait on the others in the queue and executes as soon as possible; the advantage is that all tasks on the queue will run at the same time on separate threads, making a concurrent queue take less time than a serial queue.

If order of execution of tasks is not important, always use a concurrent queue for the best efficiency.

