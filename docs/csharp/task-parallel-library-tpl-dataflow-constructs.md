---
metaTitle: "Task Parallel Library (TPL) Dataflow Constructs"
description: "ActionBlock<T>, BroadcastBlock<T>, BufferBlock<T>, JoinBlock<T1, T2,…>, WriteOnceBlock<T>, BatchedJoinBlock<T1, T2,…>, TransformBlock<TInput, TOutput>, TransformManyBlock<TInput, TOutput>, BatchBlock<T>"
---

# Task Parallel Library (TPL) Dataflow Constructs



## ActionBlock<T>


(foreach)

This class can be thought of logically as a buffer for data to be processed combined with tasks for processing that data, with the “dataflow block” managing both. In its most basic usage, we can instantiate an ActionBlock and “post” data to it; the delegate provided at the ActionBlock’s construction will be executed asynchronously for every piece of data posted.

[<img src="http://i.stack.imgur.com/exRaP.png" alt="enter image description here" />](http://i.stack.imgur.com/exRaP.png)

**Synchronous Computation**

```cs
var ab = new ActionBlock<TInput>(i => 
{
    Compute(i);
});
…
ab.Post(1);
ab.Post(2);
ab.Post(3);

```

**Throttling Asynchronous Downloads to at most 5 concurrently**

```cs
var downloader = new ActionBlock<string>(async url =>
{
    byte [] imageData = await DownloadAsync(url);
    Process(imageData);
}, new DataflowBlockOptions { MaxDegreeOfParallelism = 5 }); 

downloader.Post("http://website.com/path/to/images");
downloader.Post("http://another-website.com/path/to/images");

```

[Introduction to TPL Dataflow by Stephen Toub](https://www.microsoft.com/en-us/download/details.aspx?id=14782)



## BroadcastBlock<T>


(Copy an item and send the copies to every block that it’s linked to)

Unlike BufferBlock, BroadcastBlock’s mission in life is to enable all targets linked from the block to get a copy of every element published, continually overwriting the “current” value with those propagated to it.

Additionally, unlike BufferBlock, BroadcastBlock doesn’t hold on to data unnecessarily.  After a particular datum has been offered to all targets, that element will be overwritten by whatever piece of data is next in line (as with all dataflow blocks, messages are handled in FIFO order).  That element will be offered to all targets, and so on.

[<img src="http://i.stack.imgur.com/ZStaY.png" alt="enter image description here" />](http://i.stack.imgur.com/ZStaY.png)

**Asynchronous Producer/Consumer with a Throttled Producer**

```cs
var ui = TaskScheduler.FromCurrentSynchronizationContext();
var bb = new BroadcastBlock<ImageData>(i => i);

var saveToDiskBlock = new ActionBlock<ImageData>(item =>
    item.Image.Save(item.Path)
);

var showInUiBlock = new ActionBlock<ImageData>(item =>
    imagePanel.AddImage(item.Image), 
    new DataflowBlockOptions { TaskScheduler = TaskScheduler.FromCurrentSynchronizationContext() }
);

bb.LinkTo(saveToDiskBlock);
bb.LinkTo(showInUiBlock);

```

**Exposing Status from an Agent**

```cs
public class MyAgent
{
    public ISourceBlock<string> Status { get; private set; }
    
    public MyAgent()
    {
        Status = new BroadcastBlock<string>();
        Run();
    } 

    private void Run()
    {
        Status.Post("Starting");
        Status.Post("Doing cool stuff");
        …
        Status.Post("Done");
    }
}

```

[Introduction to TPL Dataflow by Stephen Toub](https://www.microsoft.com/en-us/download/details.aspx?id=14782)



## BufferBlock<T>


(FIFO Queue: The data that comes in is the data that goes out)

In short, BufferBlock provides an unbounded or bounded buffer for storing instances of T.<br />
You can “post” instances of T to the block, which cause the data being posted to be stored in a first-in-first-out (FIFO) order by the block.<br />
You can “receive” from the block, which allows you to synchronously or asynchronously obtain instances of T previously stored or available in the future (again, FIFO).

[<img src="http://i.stack.imgur.com/S5vXJ.png" alt="enter image description here" />](http://i.stack.imgur.com/S5vXJ.png)

**Asynchronous Producer/Consumer with a Throttled Producer**

```cs
// Hand-off through a bounded BufferBlock<T>
private static BufferBlock<int> _Buffer = new BufferBlock<int>(
    new DataflowBlockOptions { BoundedCapacity = 10 });

// Producer
private static async void Producer()
{
    while(true)
    {
        await _Buffer.SendAsync(Produce());
    }
}

// Consumer
private static async Task Consumer()
{
    while(true)
    {
        Process(await _Buffer.ReceiveAsync());
    } 
}

// Start the Producer and Consumer
private static async Task Run()
{
    await Task.WhenAll(Producer(), Consumer());
}

```

[Introduction to TPL Dataflow by Stephen Toub](https://www.microsoft.com/en-us/download/details.aspx?id=14782)



## JoinBlock<T1, T2,…>


(Collects 2-3 inputs and combines them into a Tuple)

Like BatchBlock, JoinBlock<T1, T2, …> is able to group data from multiple data sources.  In fact, that’s JoinBlock<T1, T2, …>’s primary purpose.

For example, a JoinBlock<string, double, int> is an ISourceBlock<Tuple<string, double, int>>.

As with BatchBlock, JoinBlock<T1, T2,…> is capable of operating in both greedy and non-greedy mode.

- In the default greedy mode, all data offered to targets are accepted, even if the other target doesn’t have the necessary data with which to form a tuple.
- In non-greedy mode, the block’s targets will postpone data until all targets have been offered the necessary data to create a tuple, at which point the block will engage in a two-phase commit protocol to atomically retrieve all necessary items from the sources.  This postponement makes it possible for another entity to consume the data in the meantime so as to allow the overall system to make forward progress.

[<img src="http://i.stack.imgur.com/mmXJ8.png" alt="enter image description here" />](http://i.stack.imgur.com/mmXJ8.png)

**Processing Requests with a Limited Number of Pooled Objects**

```cs
var throttle = new JoinBlock<ExpensiveObject, Request>();
for(int i=0; i<10; i++) 
{
    requestProcessor.Target1.Post(new ExpensiveObject()); 
}

var processor = new Transform<Tuple<ExpensiveObject, Request>, ExpensiveObject>(pair =>
{
    var resource = pair.Item1;
    var request = pair.Item2;
    
    request.ProcessWith(resource);
    
    return resource;
});

throttle.LinkTo(processor);
processor.LinkTo(throttle.Target1);

```

[Introduction to TPL Dataflow by Stephen Toub](https://www.microsoft.com/en-us/download/details.aspx?id=14782)



## WriteOnceBlock<T>


(Readonly variable: Memorizes its first data item and passes out copies of it as its output. Ignores all other data items)

If BufferBlock is the most fundamental block in TPL Dataflow, WriteOnceBlock is the simplest.<br />
It stores at most one value, and once that value has been set, it will never be replaced or overwritten.

You can think of WriteOnceBlock in as being similar to a readonly member variable in C#, except instead of only being settable in a constructor and then being immutable, it’s only settable once and is then immutable.

[<img src="http://i.stack.imgur.com/7M5Mp.png" alt="enter image description here" />](http://i.stack.imgur.com/7M5Mp.png)

**Splitting a Task’s Potential Outputs**

```cs
public static async void SplitIntoBlocks(this Task<T> task,
    out IPropagatorBlock<T> result, 
    out IPropagatorBlock<Exception> exception)
{
    result = new WriteOnceBlock<T>(i => i);
    exception = new WriteOnceBlock<Exception>(i => i);

    try 
    { 
        result.Post(await task); 
    }
    catch(Exception ex) 
    { 
        exception.Post(ex); 
    }
}

```

[Introduction to TPL Dataflow by Stephen Toub](https://www.microsoft.com/en-us/download/details.aspx?id=14782)



## BatchedJoinBlock<T1, T2,…>


(Collects a certain number of total items from 2-3 inputs and groups them into a Tuple of collections of data items)

BatchedJoinBlock<T1, T2,…> is in a sense a combination of BatchBlock and JoinBlock<T1, T2,…>.<br />
Whereas JoinBlock<T1, T2,…> is used to aggregate one input from each target into a tuple, and BatchBlock is used to aggregate N inputs into a collection, BatchedJoinBlock<T1, T2,…> is used to gather N inputs from across all of the targets into tuples of collections.

[<img src="http://i.stack.imgur.com/FSgue.png" alt="enter image description here" />](http://i.stack.imgur.com/FSgue.png)

**Scatter/Gather**

Consider a scatter/gather problem where N operations are launched, some of which may succeed and produce string outputs, and others of which may fail and produce Exceptions.

```cs
var batchedJoin = new BatchedJoinBlock<string, Exception>(10);

for (int i=0; i<10; i++)
{
    Task.Factory.StartNew(() => {
        try { batchedJoin.Target1.Post(DoWork()); }
        catch(Exception ex) { batchJoin.Target2.Post(ex); }
    });
}

var results = await batchedJoin.ReceiveAsync();

foreach(string s in results.Item1) 
{
    Console.WriteLine(s);
}

foreach(Exception e in results.Item2) 
{
    Console.WriteLine(e);
}

```

[Introduction to TPL Dataflow by Stephen Toub](https://www.microsoft.com/en-us/download/details.aspx?id=14782)



## TransformBlock<TInput, TOutput>


(Select, one-to-one)

As with ActionBlock, TransformBlock<TInput, TOutput> enables the execution of a delegate to perform some action for each input datum; **unlike with ActionBlock, this processing has an output.**  This delegate can be a Func<TInput, TOutput>, in which case processing of that element is considered completed when the delegate returns, or it can be a Func<TInput,Task>, in which case processing of that element is considered completed not when the delegate returns but when the returned Task completes.
For those familiar with LINQ, it’s somewhat similar to Select() in that it takes an input, transforms that input in some manner, and then produces an output.

By default, TransformBlock<TInput, TOutput> processes its data sequentially with a MaxDegreeOfParallelism equal to 1.
In addition to receiving buffered input and processing it, this block will take all of its processed output and buffer that as well (data that has not been processed, and data that has been processed).

It has 2 tasks: One to process the data, and one to push data to the next block.

[<img src="http://i.stack.imgur.com/jQcFo.png" alt="enter image description here" />](http://i.stack.imgur.com/jQcFo.png)

**A Concurrent Pipeline**

```cs
var compressor = new TransformBlock<byte[], byte[]>(input => Compress(input));
var encryptor = new TransformBlock<byte[], byte[]>(input => Encrypt(input));

compressor.LinkTo(Encryptor); 

```

[Introduction to TPL Dataflow by Stephen Toub](https://www.microsoft.com/en-us/download/details.aspx?id=14782)



## TransformManyBlock<TInput, TOutput>


(SelectMany, 1-m: The results of this mapping are “flattened”, just like LINQ’s SelectMany)

TransformManyBlock<TInput, TOutput> is very similar to TransformBlock<TInput, TOutput>.<br />
The key difference is that whereas a TransformBlock<TInput, TOutput> produces one and only one output for each input, TransformManyBlock<TInput, TOutput> produces any number (zero or more) outputs for each input.  As with ActionBlock and TransformBlock<TInput, TOutput>, this processing may be specified using delegates, both for synchronous and asynchronous processing.

A Func<TInput, IEnumerable> is used for synchronous, and a Func<TInput, Task<IEnumerable>> is used for asynchronous. As with both ActionBlock and TransformBlock<TInput, TOutput>, TransformManyBlock<TInput, TOutput> defaults to sequential processing, but may be configured otherwise.

The mapping delegate retuns a collection of items, which are inserted individually into the output buffer.

[<img src="http://i.stack.imgur.com/h7mip.png" alt="enter image description here" />](http://i.stack.imgur.com/h7mip.png)

**Asynchronous Web Crawler**

```cs
var downloader = new TransformManyBlock<string, string>(async url =>
{
    Console.WriteLine(“Downloading “ + url);
    try 
    { 
        return ParseLinks(await DownloadContents(url)); 
    } 
    catch{}
    
    return Enumerable.Empty<string>();
});
downloader.LinkTo(downloader);

```

**Expanding an Enumerable Into Its Constituent Elements**

```cs
var expanded = new TransformManyBlock<T[], T>(array => array);

```

**Filtering by going from 1 to 0 or 1 elements**

```cs
public IPropagatorBlock<T> CreateFilteredBuffer<T>(Predicate<T> filter)
{
    return new TransformManyBlock<T, T>(item =>
        filter(item) ? new [] { item } : Enumerable.Empty<T>());
}

```

[Introduction to TPL Dataflow by Stephen Toub](https://www.microsoft.com/en-us/download/details.aspx?id=14782)



## BatchBlock<T>


(Groups a certain number of sequential data items into collections of data items)

BatchBlock combines N single items into one batch item, represented as an array of elements. An instance is created with a specific batch size, and the block then creates a batch as soon as it’s received that number of elements, asynchronously outputting the batch to the output buffer.

BatchBlock is capable of executing in both greedy and non-greedy modes.

- In the default greedy mode, all messages offered to the block from any number of sources are accepted and buffered to be converted into batches.
<li>
<ul>
- In non-greedy mode, all messages are postponed from sources until enough sources have offered messages to the block to create a batch.  Thus, a BatchBlock can be used to receive 1 element from each of N sources, N elements from 1 source, and a myriad of options in between.

[<img src="http://i.stack.imgur.com/tLRyw.png" alt="enter image description here" />](http://i.stack.imgur.com/tLRyw.png)

**Batching Requests into groups of 100 to Submit to a Database**

```cs
var batchRequests = new BatchBlock<Request>(batchSize:100);
var sendToDb = new ActionBlock<Request[]>(reqs => SubmitToDatabase(reqs));

batchRequests.LinkTo(sendToDb);

```

**Creating a batch once a second**

```cs
var batch = new BatchBlock<T>(batchSize:Int32.MaxValue);
new Timer(() => { batch.TriggerBatch(); }).Change(1000, 1000);

```

[Introduction to TPL Dataflow by Stephen Toub](https://www.microsoft.com/en-us/download/details.aspx?id=14782)

