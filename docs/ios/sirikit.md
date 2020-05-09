---
metaTitle: "iOS - SiriKit"
description: "Adding Siri Extension to App"
---

# SiriKit



## Adding Siri Extension to App


To integrate Siri capabilities in your app, you should add an extensions as you would do while creating an iOS 10 Widget (old Today View Extension) or a custom keyboard.

### Adding capability

1- In the project settings, select your iOS app target and go to Capabilities tab

2- Enable the Siri capability

### Adding the extension

1- Go to File -> New -> Target...

2- Select iOS -> Application Extension from the left pane

3- Double-click Intents Extension from right

> 
<h3>According to Apple:</h3>
Intents Extension template builds an Intents extension that allows your app to handle intents issued by system services like Siri and Maps.


[<img src="http://i.stack.imgur.com/fWg8H.png" alt="enter image description here" />](http://i.stack.imgur.com/fWg8H.png)

4- Choose a name, and be sure to check "Include UI Extension"

[<img src="http://i.stack.imgur.com/YGgzc.png" alt="enter image description here" />](http://i.stack.imgur.com/YGgzc.png)

By doing this steps, two new targets (Intents Extension and UI Extension) are created, and by default they contain Workout Intent code. For different types of Siri requests, see Remarks.

> 
<h3>Note</h3>
Anytime you want to debug your extension, just select the Intent scheme from the available schemes.


> 
<h3>Note</h3>
You can't test SiriKit apps in the Simulator. Instead, you need a real device.




#### Remarks


### Different types of Siri requests

<li>
Ride Booking (e.g. Get me a ride to New York via MyApp)
</li>
<li>
Messaging (e.g. Send a text to John using MyApp)
</li>
<li>
Photo Search (e.g. Look for beach photos taken last summer in MyApp)
</li>
<li>
Payments (e.g. Send $20 to John for dinner last night using MyApp)
</li>
<li>
VoIP Calling (e.g. Call Mike on my MyApp)
</li>
<li>
Workouts (e.g. Start my daily run workout from MyApp)
</li>
<li>
Climate and Radio (specifically designed for CarPlay, e.g. Set the heater to 72 degrees)
</li>

