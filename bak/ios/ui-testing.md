---
metaTitle: "iOS - UI Testing"
description: "Accessibility Identifier, Adding Test Files to Xcode Project, Disable animations during UI Testing, Lunch and Terminate application while executing, Rotate devices"
---

# UI Testing



## Accessibility Identifier


### When Accessibility enabled in Utilities

- Select `storyboard`.
- Expand `the Utilities`
- Select `Identity Inspector`
- Select your element on storyboard
- Add new Accessibility Identifier (in example `addButton`)

[<img src="https://i.stack.imgur.com/poCUD.png" alt="enter image description here" />](https://i.stack.imgur.com/poCUD.png)

### When Accessibility disabled in Utilities

- Select `storyboard`.
- Expand `the Utilities`
- Select `Identity Inspector`
- Select your element on storyboard
- Add attribute in `User Defined Runtime Attributes`
- For `Key Path` type - `accessibilityIdentifier`
- For `Type` - `String
- For `Value` - new accessibility identifier for your element (in example `view`)

[<img src="https://i.stack.imgur.com/Y9Psk.png" alt="enter image description here" />](https://i.stack.imgur.com/Y9Psk.png)

### Setting up in UITest file

```swift
import XCTest

class StackOverFlowUITests: XCTestCase {

    private let app = XCUIApplication()

    //Views

    private var view: XCUIElement!

    //Buttons

    private var addButton: XCUIElement!


    override func setUp() {
        super.setUp()
    
        app.launch()
    
        //Views
    
        view = app.otherElements["view"]
    
        //Buttons
    
        addButton = app.buttons["addButton"]
    }

    func testMyApp() {

        addButton.tap()
        view.tap()
    }    
}

```

In `[ ]` add Accessibility Identifier for element.

### UIView, UIImageView, UIScrollView

```swift
let imageView = app.images["imageView"]
let scrollView = app.scrollViews["scrollView"]
let view = app.otherElements["view"]

```

### UILabel

```swift
let label = app.staticTexts["label"]

```

### UIStackView

```swift
let stackView = app.otherElements["stackView"]

```

### UITableView

```swift
let tableView = app.tables["tableView"]

```

### UITableViewCell

```swift
let tableViewCell = tableView.cells["tableViewCell"]

```

### UITableViewCell elements

```swift
let tableViewCellButton = tableView.cells.element(boundBy: 0).buttons["button"]

```

### UICollectionView

```swift
let collectionView = app.collectionViews["collectionView"]

```

### UIButton, UIBarButtonItem

```swift
let button = app.buttons["button"]
let barButtonItem = app.buttons["barButtonItem"]

```

### UITextField

- normal UITextField

```swift
let textField = app.textFields["textField"]

```


- password UITextField

```swift
let passwordTextField = app.secureTextFields["passwordTextField"]

```

### UITextView

```swift
let textView = app.textViews["textView"]

```

### UISwitch

```swift
let switch = app.switches["switch"]

```

### Alerts

```swift
let alert = app.alerts["About yourself"] // Title of presented alert

```



## Adding Test Files to Xcode Project


### When creating the project

You should check "Include UI Tests" in the project creation dialog.

[<img src="https://i.stack.imgur.com/WrHnW.png" alt="enter image description here" />](https://i.stack.imgur.com/WrHnW.png)

### After creating the project

If you missed checking `UI target` while creating project, you could always add test target later.

Setps:

- While project open go to `File` -> `New` -> `Target`
- Find `iOS UI Testing Bundle`

[<img src="https://i.stack.imgur.com/zDp7e.png" alt="enter image description here" />](https://i.stack.imgur.com/zDp7e.png)



## Disable animations during UI Testing


In a test you can disable animations by adding in `setUp`:

```

   app.launchEnvironment = ["animations": "0"]

```

Where `app` is instance of XCUIApplication.



## Lunch and Terminate application while executing


### Lunch application for testing

```swift
override func setUp() {
    super.setUp()

    let app = XCUIApplication()

    app.launch()
}

```

### Terminating application

```swift
func testStacOverFlowApp() {
    
    app.terminate()
}

```



## Rotate devices


Device can be rotate by changing `orientation` in `XCUIDevice.shared().orientation`:

```swift
XCUIDevice.shared().orientation = .landscapeLeft
XCUIDevice.shared().orientation = .portrait

```



#### Syntax


- XCUIApplication() // Proxy for an application. The information identifying the application is specified in the Xcode target settings as the "Target Application".
- XCUIElement() // A user interface element in an application.

