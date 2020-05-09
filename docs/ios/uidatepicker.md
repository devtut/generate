---
metaTitle: "iOS - UIDatePicker"
description: "Create a Date Picker, Setting Minimum-Maximum Date, Modes, Setting minute interval, Count Down Duration"
---

# UIDatePicker



## Create a Date Picker


### Swift

```swift
let datePicker = UIDatePicker(frame: CGRect(x: 0, y: 0, width: 320, height: 200)       

```

### Objective-C

```swift
UIDatePicker *datePicker = [[UIDatePicker alloc] initWithFrame:CGRectMake(x: 0, y: 0, width: 320, height: 200)];

```



## Setting Minimum-Maximum Date


You can set the minimum and the maximum date that UIDatePicker can show.

### Minimum date

```swift
[datePicker setMinimumDate:[NSDate date]];

```

### Maximum date

```swift
[datePicker setMaximumDate:[NSDate date]];

```



## Modes


UIDatePicker has various picker modes.

```swift
enum UIDatePickerMode : Int {
    case Time
    case Date
    case DateAndTime
    case CountDownTimer
} 

```


<li>`Time` - The date picker displays hours, minutes, and (optionally) an
AM/PM designation.</li>
- `Date` - The date picker displays months, days of the month, and years.
- `DateAndTime` - The date picker displays dates (as unified day of the week, month, and day of the month values) plus hours, minutes, and (optionally) an AM/PM designation.
- `CountDownTimer` - The date picker displays hour and minute values, for example [ 1 | 53 ]. The application must set a timer to fire at the proper interval and set the date picker as the seconds tick down.

Setting property datePickerMode

```swift
let datePicker = UIDatePicker(frame: CGRect(x: 0, y: 0, width: 320, height: 200)  
datePicker.datePickerMode = .Date

```



## Setting minute interval


You can change property `minuteInterval` to set the interval displayed by the minutes wheel.
The default value is 1, the maximum value is 30.

```swift
let datePicker = UIDatePicker(frame: CGRect(x: 0, y: 0, width: 320, height: 200)  
datePicker.minuteInterval = 15

```



## Count Down Duration


The `NSTimeInterval` value of this property indicates the seconds from which the date picker in countdown-timer mode counts down. If the mode of the date picker is not `CountDownTimer`, this value is ignored. Maximum value is 86,399 seconds (23:59)

```swift
let datePicker = UIDatePicker(frame: CGRect(x: 0, y: 0, width: 320, height: 200)  
datePicker.countDownDuration = 60 * 60

```



#### Remarks


`UIDatePicker` does not inherit from `UIPickerView`, but it manages a custom picker-view object as a subview.

