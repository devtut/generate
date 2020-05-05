---
metaTitle: "C# | Reactive Extensions (Rx)"
description: "Observing TextChanged event on a TextBox, Streaming Data from Database with Observable"
---

# Reactive Extensions (Rx)



## Observing TextChanged event on a TextBox


An observable is created from the TextChanged event of the TextBox. Also any input is only selected if it's different from the last input and if there was no input within 0.5 seconds.
The output in this example is sent to the console.

```cs
Observable
     .FromEventPattern(textBoxInput, "TextChanged")
     .Select(s => ((TextBox) s.Sender).Text)
     .Throttle(TimeSpan.FromSeconds(0.5))
     .DistinctUntilChanged()
     .Subscribe(text => Console.WriteLine(text));

```



## Streaming Data from Database with Observable


Assume having a method returning `IEnumerable<T>`, f.e.

```cs
private IEnumerable<T> GetData()
{
    try 
    {
        // return results from database 
    }
    catch(Exception exception)
    {
        throw;
    }
}  

```

Creates an Observable and starts a method asynchronously. `SelectMany` flattens the collection and the subscription is fired every 200 elements through `Buffer`.

```cs
int bufferSize = 200;

Observable
    .Start(() => GetData())
    .SelectMany(s => s)
    .Buffer(bufferSize)
    .ObserveOn(SynchronizationContext.Current)
    .Subscribe(items => 
    {
        Console.WriteLine("Loaded {0} elements", items.Count);
        
        // do something on the UI like incrementing a ProgressBar
    },
    () => Console.WriteLine("Completed loading"));

```

