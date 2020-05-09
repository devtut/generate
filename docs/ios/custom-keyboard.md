---
metaTitle: "iOS - Custom Keyboard"
description: "Custom KeyBoard Example"
---

# Custom Keyboard



## Custom KeyBoard Example


**Objective-C and Xib**

Add a target to an existing XCode project

[<img src="https://i.stack.imgur.com/tZZCr.png" alt="enter image description here" />](https://i.stack.imgur.com/tZZCr.png)

In the Add Target select Custom KeyBoard[<img src="https://i.stack.imgur.com/17jG0.png" alt="enter image description here" />](https://i.stack.imgur.com/17jG0.png)

Add the target like this:

[<img src="https://i.stack.imgur.com/EBX9s.png" alt="enter image description here" />](https://i.stack.imgur.com/EBX9s.png)

Your project file directory should look something like this

[<img src="https://i.stack.imgur.com/lmep3.png" alt="enter image description here" />](https://i.stack.imgur.com/lmep3.png)

Here myKeyBoard is the name of the added Target

Add new Cocoatouch file of type of type UIView and add an interface file

[<img src="https://i.stack.imgur.com/VSnGG.png" alt="enter image description here" />](https://i.stack.imgur.com/VSnGG.png)

Finally your project directory should look like this

[<img src="https://i.stack.imgur.com/6bn96.png" alt="enter image description here" />](https://i.stack.imgur.com/6bn96.png)

make the `keyBoardView.xib` a subclass of `keyBoardView`

[<img src="https://i.stack.imgur.com/7Fey1.png" alt="enter image description here" />](https://i.stack.imgur.com/7Fey1.png)

Make interface in the `keyBoardView.xib` file

[<img src="https://i.stack.imgur.com/QZuRQ.png" alt="enter image description here" />](https://i.stack.imgur.com/QZuRQ.png)

Make connections to from the `keyBoardView.xib` to `keyBoardView.h` file

`keyBoardView.h` should look like

```swift
#import <UIKit/UIKit.h>

@interface keyBoardView : UIView

@property (weak, nonatomic) IBOutlet UIButton *deleteKey;
//IBOutlet for the delete Key
@property (weak, nonatomic) IBOutlet UIButton *globe;
//Outlet for the key with title globe which changes the keyboard type
@property (strong, nonatomic) IBOutletCollection(UIButton) NSArray *keys;
//Contains a colloection of all the keys '0 to 9' '+' '-' and '.'

@end

```

In the `keyBoardViewController.h` file import `#import "keyBoardView.h"`

Declare a property for keyboard  `@property (strong, nonatomic)keyBoardView *keyboard;`

Comment out the

```swift
@property (nonatomic, strong) UIButton *nextKeyboardButton and all the code associated with it

```

The KeyboardViewController.m file's viewDidLoad() function should look like this

```swift
- (void)viewDidLoad {
    [super viewDidLoad];
    self.keyboard=[[[NSBundle mainBundle]loadNibNamed:@"keyBoardView" owner:nil options:nil]objectAtIndex:0];
      self.inputView=self.keyboard;
    [self addGestureToKeyboard];

    // Perform custom UI setup here
//    self.nextKeyboardButton = [UIButton buttonWithType:UIButtonTypeSystem];
//    
//    [self.nextKeyboardButton setTitle:NSLocalizedString(@"Next Keyboard", @"Title for 'Next Keyboard' button") forState:UIControlStateNormal];
//    [self.nextKeyboardButton sizeToFit];
//    self.nextKeyboardButton.translatesAutoresizingMaskIntoConstraints = NO;
//    
//    [self.nextKeyboardButton addTarget:self action:@selector(advanceToNextInputMode) forControlEvents:UIControlEventTouchUpInside];
//    
//    [self.view addSubview:self.nextKeyboardButton];
//    
//    [self.nextKeyboardButton.leftAnchor constraintEqualToAnchor:self.view.leftAnchor].active = YES;
//    [self.nextKeyboardButton.bottomAnchor constraintEqualToAnchor:self.view.bottomAnchor].active = YES;
}

```

The functions `addGestureToKeyboard`, `pressDeleteKey`, `keyPressed`  are defined below

```swift
-(void) addGestureToKeyboard
{
    [self.keyboard.deleteKey addTarget:self action:@selector(pressDeleteKey) forControlEvents:UIControlEventTouchUpInside];
    [self.keyboard.globe addTarget:self action:@selector(advanceToNextInputMode) forControlEvents:UIControlEventTouchUpInside];
    
    for (UIButton *key in self.keyboard.keys)
    {
        [key addTarget:self action:@selector(keyPressed:) forControlEvents:UIControlEventTouchUpInside];
    }
    
    
}
-(void) pressDeleteKey
{
    [self.textDocumentProxy deleteBackward];
}

-(void)keyPressed:(UIButton *)key
{
    [self.textDocumentProxy insertText:[key currentTitle]];
}

```

Run the Main Application and go to Settings->General->Keyboard->Add New Keyboard-> and add keyboard from the third party keyboard section (The displayed keyboardName would be keyBoardCustom)

The keyboard name can be changed by adding a key called `Bundle display name` and in the Value String Value enter the desired name for the keyboard of the main Project.

[<img src="https://i.stack.imgur.com/PrKMW.png" alt="enter image description here" />](https://i.stack.imgur.com/PrKMW.png)

You can also watch this  [Youtube Video](https://www.youtube.com/watch?v=gczzfq6DuHo)

