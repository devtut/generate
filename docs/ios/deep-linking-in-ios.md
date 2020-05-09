---
metaTitle: "iOS - Deep Linking in iOS"
description: "Adding a URL scheme to your own app, Opening an app based on its URL scheme, Setting up deeplink for your app"
---

# Deep Linking in iOS



## Adding a URL scheme to your own app


Let's say you're working on an app called `MyTasks`, and you want to allow inbound URLs to create a new task with a title and a body. The URL you're designing might look something like this:

`mytasks://create?title=hello&body=world`

(Of course, the `text` and `body` parameters are used to populate our task that we're creating!)

**Here are the Big Steps to adding this URL scheme to your project:**

1. Register a URL scheme in your app's `Info.plist` file, so the system knows when to route a URL to your app.
1. Add a function to your `UIApplicationDelegate` that accepts and handles incoming URLs.
1. Perform whatever task needs to occur when that URL is opened.

### Step One: Register a URL scheme in Info.plist:

First, we need to add a "URL Types" entry to our Info.plist file. Click the (+) button here:
[<img src="http://i.stack.imgur.com/2aLZt.png" alt="adding a new row to Info.plist" />](http://i.stack.imgur.com/2aLZt.png)

...then enter a unique identifier for your app, as well as the URL scheme you want to use. Be specific! You don't want the URL scheme to conflict with another app's implementation. Better to be too-long here than too-short!
[<img src="http://i.stack.imgur.com/5W7Lk.png" alt="filling out the URL types entry in Info.plist" />](http://i.stack.imgur.com/5W7Lk.png)

### Step Two: Handle the URL in the UIApplicationDelegate

We need to implement `application:openURL:options:` on our `UIApplicationDelegate`. We'll inspect the incoming `URL` and see if there's an action we can take!

One implementation would be this:

```swift
func application(app: UIApplication, openURL url: NSURL, options: [String : AnyObject]) -> Bool {
    if url.scheme == "mytasks" && url.host == "create" {
        let title = // get the title out of the URL's query using a method of your choice
        let body = // get the title out of the URL's query using a method of your choice
        self.rootViewController.createTaskWithTitle(title, body: body)
        return true
    }

    return false
}

```

### Step Three: Perform a task depending on the URL.

When a user opens your app via a URL, they probably expected **something** to happen. Maybe that's navigating to a piece of content, maybe that's creating a new item - in this example, we're going to create a new task in the app!

In the above code, we can see a call to `self.rootViewController.createTaskWithTitle(:body:)` - so assuming that your `AppDelegate` has a pointer to its root view controller which implements the function properly, you're all set!



## Opening an app based on its URL scheme


To open an app with defined URL scheme `todolist://`:

Objective-C

```swift
NSURL *myURL = [NSURL URLWithString:@"todolist://there/is/something/to/do"];
[[UIApplication sharedApplication] openURL:myURL];

```

Swift

```swift
let stringURL = "todolist://there/is/something/to/do"
if let url = NSURL(string: stringURL) {
    UIApplication.shared().openURL(url)
}

```

HTML

```swift
<a href="todolist://there/is/something/to/do">New SMS Message</a>

```

> 
**Note:** It's useful to check if link can be opened to otherwise display an appropriate message to the user. This can be done using `canOpenURL:` method.




## Setting up deeplink for your app


Setting up deep-linking for your app is easy.You just need a small url using which you want to open your app.

Follow the steps to set up deep-linking for your app.

<li>
Lets create a project and name it DeepLinkPOC.
</li>
<li>
Now select your project target.
</li>
<li>
After selecting target,select the 'info' tab.
</li>
<li>
Scroll down to the bottom until you see an option of **URL Types**
</li>
<li>
Click '+' option.
</li>
<li>
You will see **URL schemes** add a string using which you want to open your app.Lets add "**DeepLinking**" in URL schemes.
</li>

So, to open your app you can launch it by typing **"DeepLinking://"** into your safari.
Your deep-linking string has following format.

```swift
[scheme]://[host]/[path]  --> DeepLinking://path/Page1

```

where,
Scheme : "DeepLinking"
Host : "path"
path : "Page1"

**Note** : Even if don't add host and path it will launch the app,so no worries.But you can add host and path to additionally redirect to particular page after application launch.

1. Now add following method to your appdelegate.

Swift:

```

func application(application: UIApplication, openURL url: NSURL, sourceApplication: String?, annotation: AnyObject) -> Bool 

```

Objective-c:

```swift
-(BOOL)application:(UIApplication *)application
          openURL:(NSURL *)url
          sourceApplication:(NSString *)sourceApplication
          annotation:(id)annotation

```

The above method is called whenever your app is launched using a deep-linking string you set for your app.

1. Now is the time to install your app but wait before you directly jump to run button.Lets do a small change in scheme's app-launch method.

- Select and edit your scheme as

[<img src="http://i.stack.imgur.com/jZX8B.png" alt="enter image description here" />](http://i.stack.imgur.com/jZX8B.png)

- change its launch type and close [<img src="http://i.stack.imgur.com/1e6Fw.png" alt="enter image description here" />](http://i.stack.imgur.com/1e6Fw.png)

<li>
Now click the Run button (if you want you can add breakpoint to your didFinishLaunchingWithOptions and openURL methods to observe values)
</li>
<li>
You'll see a message "Waiting for DeepLinkPOC(or your app name) to launch".
</li>
<li>
Open safari and type in "**DeepLinking://**" into the search bar this will show prompt "open this page in DeepLinkPOC" click open to launch your app.
</li>

Hope you got to know how to set up deep-linking for your app :)



#### Remarks


Useful [Apple documentation](https://developer.apple.com/library/ios/documentation/iPhone/Conceptual/iPhoneOSProgrammingGuide/Inter-AppCommunication/Inter-AppCommunication.html) with examples and clarification.

