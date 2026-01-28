---
metaTitle: "iOS - WCSessionDelegate"
description: "Watch kit controller (WKInterfaceController)"
---

# WCSessionDelegate




## Watch kit controller (WKInterfaceController)


```swift
import WatchConnectivity

var watchSession : WCSession?

    override func awake(withContext context: Any?) {
        super.awake(withContext: context)
        // Configure interface objects here.
        startWatchSession()
    }

func startWatchSession(){
        
        if(WCSession.isSupported()){
            watchSession = WCSession.default()
            watchSession!.delegate = self
            watchSession!.activate()
        }
    }
    
//Callback in below delegate method when iOS app triggers event
func session(_ session: WCSession, didReceiveApplicationContext applicationContext: [String : Any]) {
        print("did ReceiveApplicationContext at watch")
    }

```

