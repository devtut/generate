---
metaTitle: "iOS - EventKit"
description: "Accessing different types of calendars, Requesting Permission, Adding an event"
---

# EventKit



## Accessing different types of calendars


### Accessing the array of calendars

To access the array of `EKCalendar`s, we use the `calendarsForEntityType` method:

### Swift

```swift
let calendarsArray = eventStore.calendarsForEntityType(EKEntityType.Event) as! [EKCalendar]

```

### Iterating through calendars

Just use a simple `for` loop:

### Swift

```swift
for calendar in calendarsArray{
    //...
}

```

### Accessing the calendar title and color

### Swift

```swift
let calendarColor = UIColor(CGColor: calendar.CGColor)
let calendarTitle = calendar.title

```

### Objective-C

```swift
UIColor *calendarColor = [UIColor initWithCGColor: calendar.CGColor];
NSString *calendarTitle = calendar.title;

```



## Requesting Permission


Your app can't access your reminders and your calendar without permission. Instead, it must show an alert to user, requesting him/her to grant access to events for the app.

To get started, import the `EventKit` framework:

### Swift

```swift
import EventKit

```

### Objective-C

```swift
#import <EventKit/EventKit.h>

```

### Making an `EKEventStore`

Then, we make an `EKEventStore` object. This is the object from which we can access calendar and reminders data:

### Swift

```swift
let eventStore = EKEventStore()

```

### Objective-C

```swift
EKEventStore *eventStore = [[EKEventStore alloc] init];

```

> 
<h3>Note</h3>
Making an `EKEventStore` object every time we need to access calendar is not efficient. Try to make it once and use it everywhere in your code.


### Checking Availability

Availability has three different status: Authorized, Denied and Not Determined. Not Determined means the app needs to grant access.

To check availability, we use `authorizationStatusForEntityType()` method of the `EKEventStore` object:

### Swift

```swift
switch EKEventStore.authorizationStatusForEntityType(EKEntityTypeEvent){
    case .Authorized: //...
    case .Denied: //...
    case .NotDetermined: //...
    default: break
}

```

### Objective-C

```swift
switch ([EKEventStore authorizationStatusForEntityType:EKEntityTypeEvent]){
    case EKAuthorizationStatus.Authorized:
        //...
        break;
    case EKAuthorizationStatus.Denied:
        //...
        break;
    case EKAuthorizationStatus.NotDetermined:
        //...
        break;
    default:
        break;
}

```

### Requesting Permission

Put the following code in `NotDetermined` case:

### Swift

```swift
eventStore.requestAccessToEntityType(EKEntityTypeEvent, completion: { [weak self] (userGrantedAccess, _) -> Void in
    if userGrantedAccess{
        //access calendar
    }
}

```



## Adding an event


### Creating the event object

### Swift

```swift
var event = EKEvent(eventStore: eventStore)

```

### Objective-C

```swift
EKEvent *event = [EKEvent initWithEventStore:eventStore];

```

### Setting related calendar, title and dates

### Swift

```swift
event.calendar = calendar
event.title = "Event Title"
event.startDate = startDate //assuming startDate is a valid NSDate object
event.endDate = endDate //assuming endDate is a valid NSDate object

```

### Adding event to calendar

### Swift

```swift
try {
    do eventStore.saveEvent(event, span: EKSpan.ThisEvent)
} catch let error as NSError {
    //error
}

```

### Objective-C

```swift
NSError *error;
BOOL *result = [eventStore saveEvent:event span:EKSpanThisEvent error:&error];
if (result == NO){
    //error
}

```

