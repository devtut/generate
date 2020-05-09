---
metaTitle: "iOS - Contacts Framework"
description: "Authorizing Contact Access, Accessing Contacts, Adding a Contact"
---

# Contacts Framework



## Authorizing Contact Access


### Importing the framework

### Swift

```swift
import Contacts

```

### Objective-C

```swift
#import <Contacts/Contacts.h>

```

### Checking accessibility

### Swift

```swift
switch CNContactStore.authorizationStatusForEntityType(CNEntityType.Contacts){
case .Authorized: //access contacts
case .Denied, .NotDetermined: //request permission
default: break
}

```

### Objective-C

```swift
switch ([CNContactStore authorizationStatusForEntityType:CNEntityType.Contacts]){
case CNAuthorizationStatus.Authorized:
    //access contacts
    break;
case CNAuthorizationStatus.Denied:
    //request permission
    break;
case CNAuthorizationStatus.NotDetermined:
    //request permission
    break;
}

```

### Requesting Permission

### Swift

```swift
var contactStore = CKContactStore()
contactStore.requestAccessForEntityType(CKEntityType.Contacts, completionHandler: { (ok, _) -> Void in
    if access{
        //access contacts
    }
}

```



## Accessing Contacts


### Applying a filter

To access contacts, we should apply a filter of type `NSPredicate` to our contactStore variable which we defined it in Authorizing Contact Access example. For example, here we want to sort out contacts with name matching with our own:

### Swift

```swift
let predicate = CNContact.predicateForContactsMatchingName("Some Name")

```

### Objective-C

```swift
NSPredicate *predicate = [CNContact predicateForContactsMatchingName:@"Some Name"];

```

### Specifying keys to fetch

Here, we want to fetch the contact's first name, last name and profile image:

### Swift

```swift
let keys = [CNContactGivenNameKey, CNContactFamilyNameKey, CNContactImageDataKey]

```

### Fetching contacts

### Swift

```swift
do {
    let contacts = try contactStore.unifiedContactsMatchingPredicate(predicate, keysToFetch: keys)
} catch let error as NSError {
    //...
}

```

### Accessing contact details

### Swift

```swift
print(contacts[0].givenName)
print(contacts[1].familyName)
let image = contacts[2].imageData

```



## Adding a Contact


### Swift

```swift
import Contacts
 
// Creating a mutable object to add to the contact
let contact = CNMutableContact()
 
contact.imageData = NSData() // The profile picture as a NSData object
 
contact.givenName = "John"
contact.familyName = "Appleseed"
 
let homeEmail = CNLabeledValue(label:CNLabelHome, value:"john@example.com")
let workEmail = CNLabeledValue(label:CNLabelWork, value:"j.appleseed@icloud.com")
contact.emailAddresses = [homeEmail, workEmail]
 
contact.phoneNumbers = [CNLabeledValue(
    label:CNLabelPhoneNumberiPhone,
    value:CNPhoneNumber(stringValue:"(408) 555-0126"))]
 
let homeAddress = CNMutablePostalAddress()
homeAddress.street = "1 Infinite Loop"
homeAddress.city = "Cupertino"
homeAddress.state = "CA"
homeAddress.postalCode = "95014"
contact.postalAddresses = [CNLabeledValue(label:CNLabelHome, value:homeAddress)]
 
let birthday = NSDateComponents()
birthday.day = 1
birthday.month = 4
birthday.year = 1988  // You can omit the year value for a yearless birthday
contact.birthday = birthday
 
// Saving the newly created contact
let store = CNContactStore()
let saveRequest = CNSaveRequest()
saveRequest.addContact(contact, toContainerWithIdentifier:nil)
try! store.executeSaveRequest(saveRequest)

```



#### Remarks


### Useful Links

<li>
[Apple Documentation](https://developer.apple.com/library/watchos/documentation/Contacts/Reference/Contacts_Framework/index.html)
</li>
<li>
[Stack Overflow Related Q&A](http://stackoverflow.com/questions/32669612/how-to-fetch-all-contacts-record-in-ios-9-using-contacts-framework)
</li>
<li>
[WWDC15 Session Video](https://developer.apple.com/videos/play/wwdc2015/223/)
</li>

