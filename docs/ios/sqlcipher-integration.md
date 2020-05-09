---
metaTitle: "iOS - SqlCipher integration"
description: "Integration of code:"
---

# SqlCipher integration


SQLite is already a popular API for persistent data storage in iOS apps so the upside for development is obvious. As a programmer you work with a stable, well-documented API that happens to have many good wrappers available in Objective-C, such as FMDB and Encrypted Core Data. All security concerns are cleanly decoupled from application code and managed by the underlying framework.



## Integration of code:


Integration to open database using password.

```swift
-(void)checkAndOpenDB{
    sqlite3 *db;
    NSString *strPassword = @"password";

        if (sqlite3_open_v2([[databaseURL path] UTF8String], &db, SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, NULL) == SQLITE_OK) {
            const char* key = [strPassword UTF8String];
            sqlite3_key(db, key, (int)strlen(key));
            if (sqlite3_exec(db1, (const char*) "SELECT count(*) FROM sqlite_master;", NULL, NULL, NULL) == SQLITE_OK) {
                NSLog(@"Password is correct, or a new database has been initialized");
            } else {
                NSLog(@"Incorrect password!");
            }
            sqlite3_close(db);
        }  
}

- (NSURL *)databaseURL
{
    NSArray *URLs = [[NSFileManager defaultManager] URLsForDirectory:NSDocumentDirectory inDomains:NSUserDomainMask];
    NSURL *directoryURL = [URLs firstObject];
    NSURL *databaseURL = [directoryURL URLByAppendingPathComponent:@"database.sqlite"];
    return  databaseURL;
}

```



#### Remarks


1. Open the Terminal, switch into your project's root directory and checkout the SQLCipher project code using Git:

```

$ git clone https://github.com/sqlcipher/sqlcipher.git

```


1. Right click on the project and choose "Add Files to "My App"" (the label will vary depending on your app's name). Since we cloned SQLCipher directly into the same folder as your iOS app you should see a sqlcipher folder in your root project folder. Open this folder and select **sqlcipher.xcodeproj**

[<img src="https://i.stack.imgur.com/dhprS.png" alt="enter image description here" />](https://i.stack.imgur.com/dhprS.png)

<li>
Select the Build Settings pane. In the search field, type in "Header Search Paths". Double-click on the field under the target column and add the following path: **$(PROJECT_DIR)/sqlcipher/src**
</li>
<li>
Start typing "Other Linker Flags" into the search field until the setting appears, double click to edit it, and add the following value: **$(BUILT_PRODUCTS_DIR)/libsqlcipher.a**
</li>
<li>
Start typing "Other C Flags" into the search field until the setting appears, double click to edit it, and in the pop-up add the following value: **-DSQLITE_HAS_CODEC**
</li>
<li>
Expand Target Dependencies and click on the + button at the end of the list. In the browser that opens, select the **sqlcipher** static library target:
</li>

[<img src="https://i.stack.imgur.com/PhwpO.png" alt="enter image description here" />](https://i.stack.imgur.com/PhwpO.png)

1. Expand Link Binary With Libraries, click on the +button at the end of the list, and select the **libsqlcipher.a** library.

[<img src="https://i.stack.imgur.com/r5JvO.png" alt="enter image description here" />](https://i.stack.imgur.com/r5JvO.png)

1. Finally, also under Link With Libraries, add **Security.framework**.

