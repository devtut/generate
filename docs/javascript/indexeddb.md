---
metaTitle: "IndexedDB"
description: "Opening a database, Testing for IndexedDB availability, Adding objects, Retrieving data"
---

# IndexedDB



## Opening a database


Opening a database is an asynchronous operation. We need to send a request to open our database and then listen for events so we know when it's ready.

We'll open a DemoDB database. If it doesn't exist yet, it will get created when we send the request.

The `2` below says that we're asking for version 2 of our database. Only one version
exists at any time, but we can use the version number to upgrade old data, as you'll see.

```
var db = null, // We'll use this once we have our database
    request = window.indexedDB.open("DemoDB", 2);

// Listen for success. This will be called after onupgradeneeded runs, if it does at all
request.onsuccess = function() {
    db = request.result; // We have a database!

    doThingsWithDB(db);
};

// If our database didn't exist before, or it was an older version than what we requested,
// the `onupgradeneeded` event will be fired.
// 
// We can use this to setup a new database and upgrade an old one with new data stores
request.onupgradeneeded = function(event) {
    db = request.result;

    // If the oldVersion is less than 1, then the database didn't exist. Let's set it up
    if (event.oldVersion < 1) {
        // We'll create a new "things" store with `autoIncrement`ing keys
        var store = db.createObjectStore("things", { autoIncrement: true });
    }

    // In version 2 of our database, we added a new index by the name of each thing
    if (event.oldVersion < 2) {
        // Let's load the things store and create an index
        var store = request.transaction.objectStore("things");

        store.createIndex("by_name", "name");
    }
};

// Handle any errors
request.onerror = function() {
    console.error("Something went wrong when we tried to request the database!");
};

```



## Testing for IndexedDB availability


You can test for IndexedDB support in the current environment by checking for the presence of the `window.indexedDB` property:

```
if (window.indexedDB) {
    // IndexedDB is available
}

```



## Adding objects


Anything that needs to happen with data in an IndexedDB database happens in a transaction. There are a few things to note about transactions that are mentioned in the Remarks section at the bottom of this page.

We'll use the database we set up in **Opening a database.**

```
// Create a new readwrite (since we want to change things) transaction for the things store
var transaction = db.transaction(["things"], "readwrite");

// Transactions use events, just like database open requests. Let's listen for success
transaction.oncomplete = function() {
    console.log("All done!");
};

// And make sure we handle errors
transaction.onerror = function() {
    console.log("Something went wrong with our transaction: ", transaction.error);
};

// Now that our event handlers are set up, let's get our things store and add some objects!
var store = transaction.objectStore("things");


// Transactions can do a few things at a time. Let's start with a simple insertion
var request = store.add({
    // "things" uses auto-incrementing keys, so we don't need one, but we can set it anyway
    key: "coffee_cup",
    name: "Coffee Cup",
    contents: ["coffee", "cream"]
});

// Let's listen so we can see if everything went well
request.onsuccess = function(event) {
    // Done! Here, `request.result` will be the object's key, "coffee_cup"
};


// We can also add a bunch of things from an array. We'll use auto-generated keys
var thingsToAdd = [{ name: "Example object" }, { value: "I don't have a name" }];

// Let's use more compact code this time and ignore the results of our insertions
thingsToAdd.forEach(e => store.add(e));

```



## Retrieving data


Anything that needs to happen with data in an IndexedDB database happens in a transaction. There are a few things to note about transactions that are mentioned in the Remarks section at the bottom of this page.

We'll use the database we set up in Opening a database.

```
// Create a new transaction, we'll use the default "readonly" mode and the things store
var transaction = db.transaction(["things"]);

// Transactions use events, just like database open requests. Let's listen for success
transaction.oncomplete = function() {
    console.log("All done!");
};

// And make sure we handle errors
transaction.onerror = function() {
    console.log("Something went wrong with our transaction: ", transaction.error);
};

// Now that everything is set up, let's get our things store and load some objects!
var store = transaction.objectStore("things");


// We'll load the coffee_cup object we added in Adding objects
var request = store.get("coffee_cup");

// Let's listen so we can see if everything went well
request.onsuccess = function(event) {
    // All done, let's log our object to the console
    console.log(request.result);
};


// That was pretty long for a basic retrieval. If we just want to get just
// the one object and don't care about errors, we can shorten things a lot
db.transaction("things").objectStore("things")
    .get("coffee_cup").onsuccess = e => console.log(e.target.result);

```



#### Remarks


### Transactions

Transactions need to be used immediately after they're created. If they aren't used in the current event loop (basically before we wait for anything like a web request) they'll go into an inactive state where you can't use them.

Databases can only have one transaction that writes to a particular object store at a time. So you can have as many as you want that read from our `things` store, but only one can make changes at any given time.

