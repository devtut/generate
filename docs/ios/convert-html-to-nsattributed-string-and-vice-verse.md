---
metaTitle: "iOS - Convert HTML to NSAttributed string and vice verse"
description: "Objective C code to convert HTML string to NSAttributedString and Vice Versa"
---

# Convert HTML to NSAttributed string and vice verse



## Objective C code to convert HTML string to NSAttributedString and Vice Versa


**HTML to NSAttributedString conversion Code :-**

```

//HTML String 
 NSString *htmlString=[[NSString alloc]initWithFormat:@"<!DOCTYPE html><html><body><h1>My First Heading</h1><p>My first paragraph.</p></body></html>"];
//Converting HTML string with UTF-8 encoding to NSAttributedString
 NSAttributedString *attributedString = [[NSAttributedString alloc]
                                        initWithData: [htmlString dataUsingEncoding:NSUnicodeStringEncoding]
                                        options: @{ NSDocumentTypeDocumentAttribute: NSHTMLTextDocumentType }
                                        documentAttributes: nil
                                        error: nil ];

```

**NSAttributedString to HTML Conversion :-**

```

//Dictionary to hold all the attributes of NSAttributed String
 NSDictionary *documentAttributes = @{NSDocumentTypeDocumentAttribute: NSHTMLTextDocumentType};
 //Saving the NSAttributedString with all its attributes as a NSData Entity
 NSData *htmlData = [attributedString dataFromRange:NSMakeRange(0, attributedString.length) documentAttributes:documentAttributes error:NULL];
 //Convert the NSData into HTML String with UTF-8 Encoding
 NSString *htmlString = [[NSString alloc] initWithData:htmlData encoding:NSUTF8StringEncoding];

```

