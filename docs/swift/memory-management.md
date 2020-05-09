---
metaTitle: "Swift - Memory Management"
description: "Reference Cycles and Weak References, Manual Memory Management"
---

# Memory Management




## Reference Cycles and Weak References


A **reference cycle** (or **retain cycle**) is so named because it indicates a [cycle](https://en.wikipedia.org/wiki/Cycle_(graph_theory)) in the [object graph](https://en.wikipedia.org/wiki/Object_graph):

Each arrow indicates one object [retaining](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/AutomaticReferenceCounting.html) another (a strong reference). Unless the cycle is broken, the memory for these objects will **never be freed**.

A retain cycle is created when two instances of classes reference each other:

```swift
class A { var b: B? = nil }
class B { var a: A? = nil }

let a = A()
let b = B()

a.b = b  // a retains b
b.a = a  // b retains a -- a reference cycle

```

Both instances they will live on until the program terminates. This is a retain cycle.

### Weak References

To avoid retain cycles, use the keyword `weak` or `unowned` when creating references to break retain cycles.

```swift
class B { **weak** var a: A? = nil }
```

Weak or unowned references will not increase the reference count of an instance. These references don't contribute to retain cycles. The weak reference **becomes `nil`** when the object it references is deallocated.

```swift
a.b = b  // a retains b
b.a = a  // b holds a weak reference to a -- not a reference cycle

```

When working with closures, you can also use [`weak` and `unowned` in capture lists](http://stackoverflow.com/documentation/swift/262/closures/947/captures-strong-weak-references-and-retain-cycles#t=201607211834265333132).



## Manual Memory Management


When interfacing with C APIs, one might want to back off Swift reference counter. Doing so is achieved with unmanaged objects.

If you need to supply a type-punned pointer to a C function, use `toOpaque` method of the `Unmanaged` structure to obtain a raw pointer, and `fromOpaque` to recover the original instance:

```swift
setupDisplayLink() {
  let pointerToSelf: UnsafeRawPointer = Unmanaged.passUnretained(self).toOpaque()
  CVDisplayLinkSetOutputCallback(self.displayLink, self.redraw, pointerToSelf)
}

func redraw(pointerToSelf: UnsafeRawPointer, /* args omitted */) {
  let recoveredSelf = Unmanaged<Self>.fromOpaque(pointerToSelf).takeUnretainedValue()
  recoveredSelf.doRedraw()
}

```

Note that, if using `passUnretained` and counterparts, it's necessary to take all precautions as with `unowned` references.

To interact with legacy Objective-C APIs, one might want to manually affect reference count of a certain object. For that `Unmanaged` has respective methods `retain` and `release`. Nonetheless, it is more desired to use `passRetained` and `takeRetainedValue`, which perform retaining before returning the result:

```swift
func preferredFilenameExtension(for uti: String) -> String! {
  let result = UTTypeCopyPreferredTagWithClass(uti, kUTTagClassFilenameExtension)
  guard result != nil else { return nil }

  return result!.takeRetainedValue() as String
}

```

These solutions should always be the last resort, and language-native APIs sould always be preferred.



#### Remarks


<a class="remarks-subsection-anchor" name="remarks-when-to-use-the-weak-keyword:-0"></a>
<h3>When to use the weak-keyword:</h3>
The `weak`-keyword should be used, if a referenced object may be deallocated during the lifetime of the object holding the reference.

<a class="remarks-subsection-anchor" name="remarks-when-to-use-the-unowned-keyword:-1"></a>
<h3>When to use the unowned-keyword:</h3>
The `unowned`-keyword should be used, if a referenced object is not expected to be deallocated during the lifetime of the object holding the reference.

<a class="remarks-subsection-anchor" name="remarks-pitfalls-2"></a>
<h3>Pitfalls</h3>
A frequent error is to forget to create references to objects, which are required to live on after a function ends, like location managers, motion managers, etc.

Example:

```swift
class A : CLLocationManagerDelegate
{
    init()
    {
        let locationManager = CLLocationManager()
        locationManager.delegate = self
        locationManager.startLocationUpdates()
    }
}

```

This example will not work properly, as the location manager is deallocated after the initializer returns. The proper solution is to create a strong reference as an instance variable:

```swift
class A : CLLocationManagerDelegate
{
    let locationManager:CLLocationManager

    init()
    {
        locationManager = CLLocationManager()
        locationManager.delegate = self
        locationManager.startLocationUpdates()
    }
}

```

