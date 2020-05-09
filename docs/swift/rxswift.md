---
metaTitle: "Swift - RxSwift"
description: "Disposing, RxSwift basics, Creating observables, Bindings, RxCocoa and ControlEvents"
---

# RxSwift



## Disposing


After the subscription was created, it is important to manage its correct deallocation.

The docs told us that

> 
If a sequence terminates in finite time, not calling dispose or not using addDisposableTo(disposeBag) won't cause any permanent resource leaks. However, those resources will be used until the sequence completes, either by finishing production of elements or returning an error.


There are two ways of deallocate resources.

1. Using `disposeBag`s and `addDisposableTo` operator.
1. Using `takeUntil` operator.

In the first case you manually pass the subscription to the `DisposeBag` object, which correctly clears all taken memory.

```swift
let bag = DisposeBag()
Observable.just(1).subscribeNext { 
    print($0)
}.addDisposableTo(bag)

```

You don't actually need to create `DisposeBag`s in every class that you create, just take a look at **RxSwift Community**'s project named [NSObject+Rx](https://github.com/RxSwiftCommunity/NSObject-Rx). Using the framework the code above can be rewritten as follows:

```swift
Observable.just(1).subscribeNext { 
    print($0)
}.addDisposableTo(rx_disposeBag)

```

In the second case, if the subscription time coincides with the `self` object lifetime, it is possible to implement disposing using `takeUntil(rx_deallocated)`:

```swift
let _ = sequence
    .takeUntil(rx_deallocated)
    .subscribe {
        print($0)
    }

```



## RxSwift basics


FRP, or Functional Reactive Programming, has some basic terms which you need to know.

Every piece of data can be represented as `Observable`, which is an asynchronous data stream. The power of FRP is in representation synchronous and asynchronous events as streams, `Observable`s, and providing the same interface to work with it.

Usually `Observable` holds several (or none) events that holds the date - `.Next` events, and then it can be terminated successfully (`.Success`) or with an error (`.Error`).

Let's take a look at following marble diagram:

```swift
--(1)--(2)--(3)|-->

```

In this example there is a stream of `Int` values. As time moves forward, three `.Next` events occurred, and then the stream terminated successfully.

```swift
--X->

```

The diagram above shows a case where no data was emitted and `.Error` event terminates the `Observable`.

Before we move on, there are some useful resources:

1. [RxSwift](https://github.com/ReactiveX/RxSwift). Look at examples, read docs and getting started.
1. [RxSwift Slack room](http://rxswift.slack.com) has a few channels for education problem solving.
1. Play around with [RxMarbles](http://rxmarbles.com/) to know what operator does, and which is the most useful in your case.
1. Take a look [on this example](https://github.com/scotteg/RxSwiftPlayer), explore the code by yourself.



## Creating observables


**RxSwift** offers many ways to create an `Observable`, let's take a look:

```swift
import RxSwift

let intObservale = Observable.just(123) // Observable<Int>
let stringObservale = Observable.just("RxSwift") // Observable<String>
let doubleObservale = Observable.just(3.14) // Observable<Double>

```

So, the observables are created. They holds just one value and then terminates with success. Nevertheless, nothing happening after it was created. Why?

There are two steps in working with `Observable`s: you **observe** something to **create** a stream and then you **subscribe** to the stream or **bind** it to something to **interact** with it.

```swift
Observable.just(12).subscribe {
    print($0)
}

```

The console will print:

```swift
.Next(12)
.Completed()

```

And if I interested only in working with data, which take place in `.Next` events, I'd use `subscribeNext` operator:

```swift
Observable.just(12).subscribeNext {
    print($0) // prints "12" now
}

```

If I want create an observable of many values, I use different operators:

```swift
Observable.of(1,2,3,4,5).subscribeNext {
    print($0)
}
// 1
// 2
// 3
// 4
// 5

// I can represent existing data types as Observables also:
[1,2,3,4,5].asObservable().subscribeNext { 
    print($0) 
}
// result is the same as before.

```

And finally, maybe I want an `Observable` that does some work. For example, it is convenient to wrap a network operation into `Observable<SomeResultType>`. Let's take a look of do one can achieve this:

```swift
Observable.create { observer in    // create an Observable ...
    MyNetworkService.doSomeWorkWithCompletion { (result, error) in
        if let e = error {
            observer.onError(e)    // ..that either holds an error
        } else {
            observer.onNext(result) // ..or emits the data
            observer.onCompleted()  // ..and terminates successfully.
        }
    }
    return NopDisposable.instance // here you can manually free any resources
                                //in case if this observable is being disposed.
}

```



## Bindings


```swift
Observable.combineLatest(firstName.rx_text, lastName.rx_text) { $0 + " " + $1 }
.map { "Greetings, \($0)" }
.bindTo(greetingLabel.rx_text)

```

Using the `combineLatest` operator every time an item is emitted by either of two `Observables`, combine the latest item emitted by each `Observable`. So in this way we combine the result of the two `UITextField`'s creating a new message with the text `"Greetings, \($0)"` using string interpolation to later bind to the text of a  `UILabel`.

We can bind data to any `UITableView` and `UICollectionView` in an very easy way:

```swift
viewModel
.rows
.bindTo(resultsTableView.rx_itemsWithCellIdentifier("WikipediaSearchCell", cellType: WikipediaSearchCell.self)) { (_, viewModel, cell) in
    cell.title = viewModel.title
    cell.url = viewModel.url
}
.addDisposableTo(disposeBag)

```

That’s an Rx wrapper around the `cellForRowAtIndexPath` data source method. And also Rx takes care of the implementation of the `numberOfRowsAtIndexPath`, which is a required method in a traditional sense, but you don’t have to implement it here, it’s taken care of.



## RxCocoa and ControlEvents


RxSwift provides not only the ways to control your data, but to represent user actions in a reactive way also.

RxCocoa contains everything you need. It wraps most of the UI components' properties into `Observable`s, but not really. There are some upgraded `Observable`s called `ControlEvent`s (which represent events) and `ControlProperties` (which represent properties, surprise!). These things holds `Observable` streams under the hood, but also have some nuances:

- It never fails, so no errors.
- It will `Complete` sequence on control being deallocated.
- It delivers events on the main thread (`MainScheduler.instance`).

Basically, you can work with them as usual:

```swift
button.rx_tap.subscribeNext { _ in   // control event
    print("User tapped the button!")
}.addDisposableTo(bag)

textField.rx_text.subscribeNext { text in // control property
    print("The textfield contains: \(text)")
}.addDisposableTo(bag)
// notice that ControlProperty generates .Next event on subscription
// In this case, the log will display 
// "The textfield contains: "
// at the very start of the app.

```

This is very important to use: as long as you use Rx, forget about the `@IBAction` stuff, everything you need you can bind and configure at once. For example, `viewDidLoad` method of your view controller is a good candidate to describe how the UI-components work.

Ok, another example: suppose we have a textfield, a button, and a label. We want to **validate text** in the textfield when we **tap** the button, and **display** the results in the label. Yep, seems like an another validate-email task, huh?

First of all, we grab the `button.rx_tap` ControlEvent:

```swift
----()-----------------------()----->

```

Here empty parenthesis show user taps. Next, we take what's written in the textField with `withLatestFrom` operator (take a look at it [here](http://rxmarbles.com/#withLatestFrom), imagine that upper stream represents user taps, bottom one represents text in the text field).

```swift
button.rx_tap.withLatestFrom(textField.rx_text)

----("")--------------------("123")--->
//  ^ tap   ^ i wrote 123    ^ tap

```

Nice, we have a stream of strings to validate, emitted only when we need to validate.

Any `Observable` has such familiar operators as `map` or `filter`, we'll take `map` to validate the text. Create `validateEmail` function yourself, use any regex you want.

```swift
button.rx_tap                                // ControlEvent<Void>
        .withLatestFrom(textField.rx_text)   // Observable<String>
        .map(validateEmail)                  // Observable<Bool>
        .map { (isCorrect) in
            return isCorrect ? "Email is correct" : "Input the correct one, please"
        }                                    // Observable<String>
        .bindTo(label.rx_text)              
        .addDisposableTo(bag) 

```

Done! If you need more custom logic (like showing error views in case of error, making a transition to another screen on success...), just subscribe to the final `Bool` stream and write it there.

