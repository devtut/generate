---
metaTitle: "Swift - Getting Started with Protocol Oriented Programming"
description: "Leveraging Protocol Oriented Programming for Unit Testing, Using protocols as first class types"
---

# Getting Started with Protocol Oriented Programming




## Leveraging Protocol Oriented Programming for Unit Testing


Protocol Oriented Programming is a useful tool in order to easily write better unit tests for our code.

Let's say we want to test a UIViewController that relies on a ViewModel class.

The needed steps on the production code are:

1. Define a protocol that exposes the public interface of the class ViewModel, with all the properties and methods needed by the UIViewController.
1. Implement the real ViewModel class, conforming to that protocol.
1. Use a dependency injection technique to let the view controller use the implementation we want, passing it as the protocol and not the concrete instance.

```swift
protocol ViewModelType {
   var title : String {get}
   func confirm()
}

class ViewModel : ViewModelType {
   let title : String

   init(title: String) {
       self.title = title
   }
   func confirm() { ... }
}

class ViewController : UIViewController {
   // We declare the viewModel property as an object conforming to the protocol
   // so we can swap the implementations without any friction.
   var viewModel : ViewModelType! 
   @IBOutlet var titleLabel : UILabel!

   override func viewDidLoad() {
       super.viewDidLoad()
       titleLabel.text = viewModel.title
   }

   @IBAction func didTapOnButton(sender: UIButton) {
       viewModel.confirm()
   }
}

// With DI we setup the view controller and assign the view model.
// The view controller doesn't know the concrete class of the view model, 
// but just relies on the declared interface on the protocol.
let viewController = //... Instantiate view controller
viewController.viewModel = ViewModel(title: "MyTitle")

```

Then, on unit test:

1. Implement a mock ViewModel that conforms to the same protocol
1. Pass it to the UIViewController under test using dependency injection, instead of the real instance.
1. Test!

```swift
class FakeViewModel : ViewModelType {
   let title : String = "FakeTitle"

   var didConfirm = false
   func confirm() {
       didConfirm = true
   }
}

class ViewControllerTest : XCTestCase {
    var sut : ViewController!
    var viewModel : FakeViewModel!

    override func setUp() {
        super.setUp()

        viewModel = FakeViewModel()
        sut = // ... initialization for view controller
        sut.viewModel = viewModel

        XCTAssertNotNil(self.sut.view) // Needed to trigger view loading
    } 

    func testTitleLabel() {
        XCTAssertEqual(self.sut.titleLabel.text, "FakeTitle")
    }

    func testTapOnButton() {
        sut.didTapOnButton(UIButton())
        XCTAssertTrue(self.viewModel.didConfirm)
    }
}

```



## Using protocols as first class types


Protocol oriented programing can be used as a core Swift design pattern.

Different types are able to conform to the same protocol, value types can even conform to multiple protocols and even provide default method implementation.

Initially protocols are defined that can represent commonly used properties and/or methods with either specific or generic types.

```swift
protocol ItemData {
    
    var title: String { get }
    var description: String { get }
    var thumbnailURL: NSURL { get }
    var created: NSDate { get }
    var updated: NSDate { get }
    
}

protocol DisplayItem {
    
    func hasBeenUpdated() -> Bool
    func getFormattedTitle() -> String
    func getFormattedDescription() -> String

}

protocol GetAPIItemDataOperation {
    
    static func get(url: NSURL, completed: ([ItemData]) -> Void)
}

```

A default implementation for the get method can be created, though if desired conforming types may override the implementation.

```swift
extension GetAPIItemDataOperation {
    
    static func get(url: NSURL, completed: ([ItemData]) -> Void) {
        
        let date = NSDate(
        timeIntervalSinceNow: NSDate().timeIntervalSince1970
            + 5000)
        
        // get data from url
        let urlData: [String: AnyObject] = [
            "title": "Red Camaro",
            "desc": "A fast red car.",
            "thumb":"http://cars.images.com/red-camaro.png",
            "created": NSDate(), "updated": date]
        
        // in this example forced unwrapping is used
        // forced unwrapping should never be used in practice
        // instead conditional unwrapping should be used (guard or if/let)
        let item = Item(
            title: urlData["title"] as! String,
            description: urlData["desc"] as! String,
            thumbnailURL: NSURL(string: urlData["thumb"] as! String)!,
            created: urlData["created"] as! NSDate,
            updated: urlData["updated"] as! NSDate)
        
        completed([item])
        
    }
}

struct ItemOperation: GetAPIItemDataOperation { }

```

A value type that conforms to the ItemData protocol, this value type is also able to conform to other protocols.

```swift
struct Item: ItemData {
    
    let title: String
    let description: String
    let thumbnailURL: NSURL
    let created: NSDate
    let updated: NSDate
    
}

```

Here the item struct is extended to conform to a display item.

```swift
extension Item: DisplayItem {
    
    func hasBeenUpdated() -> Bool {
        return updated.timeIntervalSince1970 >
            created.timeIntervalSince1970
    }
    
    func getFormattedTitle() -> String {
        return title.stringByTrimmingCharactersInSet(
            .whitespaceAndNewlineCharacterSet())
    }
    
    func getFormattedDescription() -> String {
        return description.stringByTrimmingCharactersInSet(
            .whitespaceAndNewlineCharacterSet())
    }
}

```

An example call site for using the static get method.

```swift
ItemOperation.get(NSURL()) { (itemData) in
    
    // perhaps inform a view of new data
    // or parse the data for user requested info, etc.
    dispatch_async(dispatch_get_main_queue(), { 
        
        // self.items = itemData
    })
    
}

```

Different use cases will require different implementations. The main idea here is to show conformance from varying types where the protocol is the main point of the focus in the design. In this example perhaps the API data is conditionally saved to a Core Data entity.

```swift
// the default core data created classes + extension
class LocalItem: NSManagedObject { }

extension LocalItem {
    
    @NSManaged var title: String
    @NSManaged var itemDescription: String
    @NSManaged var thumbnailURLStr: String
    @NSManaged var createdAt: NSDate
    @NSManaged var updatedAt: NSDate
}

```

Here the Core Data backed class can also conform to the DisplayItem protocol.

```swift
extension LocalItem: DisplayItem {
    
    func hasBeenUpdated() -> Bool {
        return updatedAt.timeIntervalSince1970 >
            createdAt.timeIntervalSince1970
    }
    
    func getFormattedTitle() -> String {
        return title.stringByTrimmingCharactersInSet(
            .whitespaceAndNewlineCharacterSet())
    }
    
    func getFormattedDescription() -> String {
        return itemDescription.stringByTrimmingCharactersInSet(
            .whitespaceAndNewlineCharacterSet())
    }
}

// In use, the core data results can be
// conditionally casts as a protocol
class MyController: UIViewController {

    override func viewDidLoad() {
        
        let fr: NSFetchRequest = NSFetchRequest(
        entityName: "Items")
    
        let context = NSManagedObjectContext(
        concurrencyType: .MainQueueConcurrencyType)
        
        do {
            
            let items: AnyObject = try context.executeFetchRequest(fr)
            if let displayItems = items as? [DisplayItem] {
                
                print(displayItems)
            }
        
        } catch let error as NSError {
            print(error.localizedDescription)
        }
        
    }
}

```



#### Remarks


For more information on this topic, see the WWDC 2015 talk [Protocol-Oriented Programming in Swift](https://developer.apple.com/videos/play/wwdc2015/408/).

There is also a great written guide on the same: [Introducing Protocol-Oriented Programming in Swift 2](https://www.raywenderlich.com/109156/introducing-protocol-oriented-programming-in-swift-2).

