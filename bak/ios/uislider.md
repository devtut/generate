---
metaTitle: "iOS - UISlider"
description: "UISlider, SWIFT Example, Adding a custom thumb image"
---

# UISlider



## UISlider


**Objective-C**

Declare a slider property in the `ViewController.h` or in the interface of `ViewController.m`

```swift
@property (strong, nonatomic)UISlider *slider;

//Define frame of slider and add to view
CGRect frame = CGRectMake(0.0, 100.0, 320.0, 10.0);
UISlider *slider = [[UISlider alloc] initWithFrame:frame];
[slider addTarget:self action:@selector(sliderAction:) forControlEvents:UIControlEventValueChanged];
[self.slider setBackgroundColor:[UIColor clearColor]];
self.slider.minimumValue = 0.0;
self.slider.maximumValue = 50.0;
//sending a NO/False would update the value of slider only when the user is no longer touching the screen. Hence sending only the final value
self.slider.continuous = YES;
self.slider.value = 25.0;
[self.view addSubview slider];

```

Handle the slider change event

```swift
- (IBAction)sliderAction:(id)sender {
    NSLog(@"Slider Value %f", sender.value);
}

```



## SWIFT Example


```

let frame = CGRect(x: 0, y: 100, width: 320, height: 10)
 let slider = UISlider(frame: frame)
 slider.addTarget(self, action:  #selector(sliderAction), for: .valueChanged)
 slider.backgroundColor = .clear
 slider.minimumValue = 0.0
 slider.maximumValue = 50.0
 //sending a NO/False would update the value of slider only when the user is no longer touching the screen. Hence sending only the final value
 slider.isContinuous = true
 slider.value = 25.0
 view.addSubview(slider)

```

Handling the slider change event

```

func sliderAction(sender:UISlider!)
 {
    print("value--\(sender.value)")
 }

```



## Adding a custom thumb image


To add a custom image for the thumb of the slider, simply call the [setThumbImage](https://developer.apple.com/reference/uikit/uislider/1621336-setthumbimage) method with your custom image:

Swift 3.1:

```swift
let slider = UISlider()
let thumbImage = UIImage
slider.setThumbImage(thumbImage, for: .normal)

```

