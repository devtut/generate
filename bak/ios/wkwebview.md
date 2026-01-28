---
metaTitle: "iOS - WKWebView"
description: "Creating a Simple WebBrowser, Adding custom user script loaded from app bundle , Send messages from JavaScript and Handle them on the native side"
---

# WKWebView


WKWebView is the centerpiece of the modern WebKit API introduced in iOS 8 & OS X Yosemite. It replaces UIWebView in UIKit and WebView in AppKit, offering a consistent API across the two platforms.

Boasting responsive 60fps scrolling, built-in gestures, streamlined communication between app and webpage, and the same JavaScript engine as Safari, WKWebView is one of the most significant announcements to come out of WWDC 2014.



## Creating a Simple WebBrowser


```swift
import UIKit
import WebKit


class ViewController: UIViewController, UISearchBarDelegate, WKNavigationDelegate, WKUIDelegate {
    
    var searchbar: UISearchBar! //All web-browsers have a search-bar.
    var webView: WKWebView! //The WKWebView we'll use.
    var toolbar: UIToolbar! //Toolbar at the bottom just like in Safari.
    var activityIndicator: UIActivityIndicatorView! //Activity indicator to let the user know the page is loading.

    override func viewDidLoad() {
        super.viewDidLoad()
        
        self.initControls()
        self.setTheme()
        self.doLayout()
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
    }


    func initControls() {
        self.searchbar = UISearchBar()
        
        //WKUserContentController allows us to add Javascript scripts to our webView that will run either at the beginning of a page load OR at the end of a page load.

        let configuration = WKWebViewConfiguration()
        let contentController = WKUserContentController()
        configuration.userContentController = contentController
        
        //create the webView with the custom configuration.
        self.webView = WKWebView(frame: .zero, configuration: configuration)
        
        self.toolbar = UIToolbar()
        self.layoutToolbar()

        self.activityIndicator = UIActivityIndicatorView(activityIndicatorStyle: .gray)
        self.activityIndicator.hidesWhenStopped = true
    }
    
    func setTheme() {
        self.edgesForExtendedLayout = UIRectEdge(rawValue: 0)
        self.navigationController?.navigationBar.barTintColor = UIColor.white()
        
        //Theme the keyboard and searchBar. Setup delegates.
        self.searchbar.delegate = self
        self.searchbar.returnKeyType = .go
        self.searchbar.searchBarStyle = .prominent
        self.searchbar.placeholder = "Search or enter website name"
        self.searchbar.autocapitalizationType = .none
        self.searchbar.autocorrectionType = .no
        
        //Set the WebView's delegate.
        self.webView.navigationDelegate = self //Delegate that handles page navigation
        self.webView.uiDelegate = self //Delegate that handles new tabs, windows, popups, layout, etc..

        self.activityIndicator.transform = CGAffineTransform(scaleX: 1.5, y: 1.5)
    }
    
    func layoutToolbar() {
        //Browsers typically have a back button, forward button, refresh button, and newTab/newWindow button.

        var items = Array<UIBarButtonItem>()
        
        let space = UIBarButtonItem(barButtonSystemItem: .flexibleSpace, target: nil, action: nil)
        
        items.append(UIBarButtonItem(title: "<", style: .plain, target: self, action: #selector(onBackButtonPressed)))
        items.append(space)
        items.append(UIBarButtonItem(title: ">", style: .plain, target: self, action: #selector(onForwardButtonPressed)))
        items.append(space)
        items.append(UIBarButtonItem(barButtonSystemItem: .refresh, target: self, action: #selector(onRefreshPressed)))
        items.append(space)
        items.append(UIBarButtonItem(barButtonSystemItem: .organize, target: self, action: #selector(onTabPressed)))
        
        self.toolbar.items = items
    }
    
    func doLayout() {
        //Add the searchBar to the navigationBar.
        self.navigationItem.titleView = self.searchbar
        
        //Add all other subViews to self.view.
        self.view.addSubview(self.webView)
        self.view.addSubview(self.toolbar)
        self.view.addSubview(self.activityIndicator)
        
        //Setup which views will be constrained.

        let views: [String: AnyObject] = ["webView": self.webView, "toolbar": self.toolbar, "activityIndicator": self.activityIndicator];
        var constraints = Array<String>();
        
        constraints.append("H:|-0-[webView]-0-|")
        constraints.append("H:|-0-[toolbar]-0-|")
        constraints.append("V:|-0-[webView]-0-[toolbar(50)]-0-|")
        
        
        //constrain the subviews using the above visual constraints.

        for constraint in constraints {
            self.view.addConstraints(NSLayoutConstraint.constraints(withVisualFormat: constraint, options: NSLayoutFormatOptions(rawValue: 0), metrics: nil, views: views))
        }
        
        for view in self.view.subviews {
            view.translatesAutoresizingMaskIntoConstraints = false
        }
        
        
        //constraint the activity indicator to the center of the view.
        self.view.addConstraint(NSLayoutConstraint(item: self.activityIndicator, attribute: .centerX, relatedBy: .equal, toItem: self.view, attribute: .centerX, multiplier: 1.0, constant: 0.0))
        self.view.addConstraint(NSLayoutConstraint(item: self.activityIndicator, attribute: .centerY, relatedBy: .equal, toItem: self.view, attribute: .centerY, multiplier: 1.0, constant: 0.0))
    }
    
    //Searchbar Delegates
    
    func searchBarSearchButtonClicked(_ searchBar: UISearchBar) {
        self.searchbar.resignFirstResponder()
        
        if let searchText = self.searchbar.text, url = URL(string: searchText) {
            //Get the URL from the search bar. Create a new NSURLRequest with it and tell the webView to navigate to that URL/Page. Also specify a timeout for if the page takes too long. Also handles cookie/caching policy.

            let request = URLRequest(url: url, cachePolicy: .useProtocolCachePolicy, timeoutInterval: 30)
            self.webView.load(request)
        }
    }
    
    
    //Toolbar Delegates
    
    func onBackButtonPressed(button: UIBarButtonItem) {
        if (self.webView.canGoBack) { //allow the user to go back to the previous page.
            self.webView.goBack()
        }
    }
    
    func onForwardButtonPressed(button: UIBarButtonItem) {
        if (self.webView.canGoForward) { //allow the user to go forward to the next page.
            self.webView.goForward()
        }
    }
    
    func onRefreshPressed(button: UIBarButtonItem) {
        self.webView.reload()  //reload the current page.
    }
    
    func onTabPressed(button: UIBarButtonItem) {
        //TODO: Open a new tab or web-page.
    }
    
    
    //WebView Delegates
    
    func webView(_ webView: WKWebView, decidePolicyFor navigationAction: WKNavigationAction, decisionHandler: (WKNavigationActionPolicy) -> Void) {
        
        decisionHandler(.allow) //allow the user to navigate to the requested page.
    }
    
    func webView(_ webView: WKWebView, decidePolicyFor navigationResponse: WKNavigationResponse, decisionHandler: (WKNavigationResponsePolicy) -> Void) {
        
        decisionHandler(.allow) //allow the webView to process the response.
    }
    
    func webView(_ webView: WKWebView, didStartProvisionalNavigation navigation: WKNavigation!) {
        self.activityIndicator.startAnimating()
    }
    
    func webView(_ webView: WKWebView, didFailProvisionalNavigation navigation: WKNavigation!, withError error: NSError) {
        self.activityIndicator.stopAnimating()

        //Handle the error. Display an alert to the user telling them what happened.
        
        let alert = UIAlertController(title: "Error", message: error.localizedDescription, preferredStyle: .alert)
        let action = UIAlertAction(title: "OK", style: .default) { (action) in
            alert.dismiss(animated: true, completion: nil)
        }
        alert.addAction(action)
        self.present(alert, animated: true, completion: nil)
    }
    
    func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
        self.activityIndicator.stopAnimating()
        
        //Update our search bar with the webPage's final endpoint-URL.
        if let url = self.webView.url {
            self.searchbar.text = url.absoluteString ?? self.searchbar.text
        }
    }
    
    func webView(_ webView: WKWebView, didReceiveServerRedirectForProvisionalNavigation navigation: WKNavigation!) {
        //When the webview receives a "Redirect" to a different page or endpoint, this is called.
    }
    
    func webView(_ webView: WKWebView, didCommit navigation: WKNavigation!) {
        //When the content for the webpage starts arriving, this is called.
    }
    
    func webView(_ webView: WKWebView, didFail navigation: WKNavigation!, withError error: NSError) {
        
    }
    
    func webView(_ webView: WKWebView, didReceive challenge: URLAuthenticationChallenge, completionHandler: (URLSession.AuthChallengeDisposition, URLCredential?) -> Void) {
        
        completionHandler(.performDefaultHandling, .none) //Handle SSL connections by default. We aren't doing SSL pinning or custom certificate handling.
        
    }
    
    
    //WebView's UINavigation Delegates
    
    //This is called when a webView or existing loaded page wants to open a new window/tab.
    func webView(_ webView: WKWebView, createWebViewWith configuration: WKWebViewConfiguration, for navigationAction: WKNavigationAction, windowFeatures: WKWindowFeatures) -> WKWebView? {
        
        //The view that represents the new tab/window. This view will have an X button at the top left corner + a webView.
        let container = UIView()
        
        //New tabs need an exit button.
        let XButton = UIButton()
        XButton.addTarget(self, action: #selector(onWebViewExit), for: .touchUpInside)
        XButton.layer.cornerRadius = 22.0
        
        //Create the new webView window.
        let webView = WKWebView(frame: .zero, configuration: configuration)
        webView.navigationDelegate = self
        webView.uiDelegate = self
        
        //Layout the tab.
        container.addSubview(XButton)
        container.addSubview(webView)
        
        let views: [String: AnyObject] = ["XButton": XButton, "webView": webView];
        var constraints = Array<String>()
        
        constraints.append("H:|-(-22)-[XButton(44)]")
        constraints.append("H:|-0-[webView]-0-|")
        constraints.append("V:|-(-22)-[XButton(44)]-0-[webView]-0-|")
        
        
        //constrain the subviews.
        for constraint in constraints {
            container.addConstraints(NSLayoutConstraint.constraints(withVisualFormat: constraint, options: NSLayoutFormatOptions(rawValue: 0), metrics: nil, views: views))
        }
        
        for view in container.subviews {
            view.translatesAutoresizingMaskIntoConstraints = false
        }
        
        //TODO: Add the containerView to self.view or present it with a new controller. Keep track of tabs..
        
        return webView
    }
    
    func onWebViewExit(button: UIButton) {
        //TODO: Destroy the tab. Remove the new tab from the current window or controller.
    }
}

```

Showing the custom `GO` button in the keyboard:

[<img src="http://i.stack.imgur.com/ZA8YD.png" alt="enter image description here" />](http://i.stack.imgur.com/ZA8YD.png)

Showing the toolbar and fully loaded page.

[<img src="http://i.stack.imgur.com/fMRDe.png" alt="enter image description here" />](http://i.stack.imgur.com/fMRDe.png)



## Adding custom user script loaded from app bundle 


```swift
let configuration = WKWebViewConfiguration()

if let path = NSBundle.mainBundle().pathForResource("customUserScript", ofType: "js"), 
       source = try? NSString(contentsOfFile: path, encoding: NSUTF8StringEncoding) as String {

        let userScript = WKUserScript(source: source, injectionTime: WKUserScriptInjectionTime.AtDocumentStart, forMainFrameOnly: false)

        let userContentController = WKUserContentController()
        userContentController.addUserScript(userScript)
        configuration.userContentController = userContentController   
    }

let webView = WKWebView(frame: self.view.bounds, configuration: configuration)

```

Any value in the [`WKUserScriptInjectionTime`](https://developer.apple.com/library/ios/documentation/WebKit/Reference/WKUserScript_Ref/index.html#//apple_ref/c/tdef/WKUserScriptInjectionTime) enum is valid: `.AtDocumentStart`, `.AtDocumentEnd`



## Send messages from JavaScript and Handle them on the native side


Messages can be sent from JavaScript using the following code

```swift
window.webkit.messageHandlers.{NAME}.postMessage()

```

Here how to create a script message handler to handle the messages:

```swift
class NotificationScriptMessageHandler: NSObject, WKScriptMessageHandler {
    func userContentController(userContentController: WKUserContentController, didReceiveScriptMessage message: WKScriptMessage!) {
        if message.name == "{NAME}" {
            // to be sure of handling the correct message
            print(message.body)
        }
    }
}

```

Here how to configure the script message handler in the WKWebView:

```swift
let configuration = WKWebViewConfiguration()
let userContentController = WKUserContentController()
let handler = NotificationScriptMessageHandler()
userContentController.addScriptMessageHandler(handler, name: "{NAME}")
configuration.userContentController = userContentController 
let webView = WKWebView(frame: self.view.bounds, configuration: configuration)

```

> 
**NOTE:** adding same `"{NAME}"` handler with `addScriptMessageHandler:name:` more than once, results in `NSInvalidArgumentException`exception.


