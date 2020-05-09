---
metaTitle: "Objective-C - NSUrl send a post request"
description: "Simple POST request, Simple Post Request With Timeout"
---

# NSUrl send a post request



## Simple POST request


```objc
// Create the request.
NSMutableURLRequest *request = [NSMutableURLRequest requestWithURL:[NSURL URLWithString:@"http://google.com"]];
    
// Specify that it will be a POST request
request.HTTPMethod = @"POST";
    
// This is how we set header fields
[request setValue:@"application/xml; charset=utf-8" forHTTPHeaderField:@"Content-Type"];
 
// Convert your data and set your request's HTTPBody property
NSString *stringData = @"some data";
NSData *requestBodyData = [stringData dataUsingEncoding:NSUTF8StringEncoding];
request.HTTPBody = requestBodyData;
    
// Create url connection and fire request
NSURLConnection *conn = [[NSURLConnection alloc] initWithRequest:request delegate:self];

```



## Simple Post Request With Timeout


```objc
// Create the request.
NSMutableURLRequest *request = [NSMutableURLRequest requestWithURL:[NSURL URLWithString:@"http://google.com"]];
    
// Specify that it will be a POST request
request.HTTPMethod = @"POST";

// Setting a timeout 
request.timeoutInterval = 20.0;   
// This is how we set header fields
[request setValue:@"application/xml; charset=utf-8" forHTTPHeaderField:@"Content-Type"];
 
// Convert your data and set your request's HTTPBody property
NSString *stringData = @"some data";
NSData *requestBodyData = [stringData dataUsingEncoding:NSUTF8StringEncoding];
request.HTTPBody = requestBodyData;
    
// Create url connection and fire request
NSURLConnection *conn = [[NSURLConnection alloc] initWithRequest:request delegate:self];

```

