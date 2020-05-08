---
metaTitle: "Android - Firebase Realtime DataBase"
description: "Quick setup,  Firebase Realtime DataBase event handler, Designing and understanding how to retrieve realtime data from the Firebase Database, Denormalization: Flat Database Structure, Understanding firebase JSON database, Retrieving data from firebase, Listening for child updates, Retrieving data with pagination"
---

# Firebase Realtime DataBase




## Quick setup


<li>
<p>Complete the [Installation and setup part](http://stackoverflow.com/documentation/android/3843/firebase/19049/add-firebase-to-your-android-project#t=201608180547233218048) to connect your app to Firebase.<br />
This will create the project in Firebase.</p>
</li>
<li>
Add the dependency for Firebase Realtime Database to your module-level `build.gradle` file:
</li>

```java
compile 'com.google.firebase:firebase-database:10.2.1'

```


1. Configure [Firebase Database Rules](http://stackoverflow.com/documentation/firebase/3352/database-rules#t=201608180548072123785)

Now you are ready to work with the Realtime Database in Android.

For example you write a `Hello World` message to the database under the `message` key.

```java
// Write a message to the database
FirebaseDatabase database = FirebaseDatabase.getInstance();
DatabaseReference myRef = database.getReference("message");

myRef.setValue("Hello, World!");

```



##  Firebase Realtime DataBase event handler


First Initialize FirebaseDatabase:

```java
FirebaseDatabase database = FirebaseDatabase.getInstance();

```

Write to your database:

```java
// Write a message to the database
FirebaseDatabase database = FirebaseDatabase.getInstance();
DatabaseReference myRef = database.getReference("message");

myRef.setValue("Hello, World!");

```

Read from your database:

```java
// Read from the database
myRef.addValueEventListener(new ValueEventListener() {
    @Override
    public void onDataChange(DataSnapshot dataSnapshot) {
        // This method is called once with the initial value and again
        // whenever data at this location is updated.
        String value = dataSnapshot.getValue(String.class);
        Log.d(TAG, "Value is: " + value);
    }

    @Override
    public void onCancelled(DatabaseError error) {
        // Failed to read value
        Log.w(TAG, "Failed to read value.", error.toException());
    }
});

```

Retrieve Data on Android events:

```java
ChildEventListener childEventListener = new ChildEventListener() {
    @Override
    public void onChildAdded(DataSnapshot dataSnapshot, String previousChildName) {
        Log.d(TAG, "onChildAdded:" + dataSnapshot.getKey());
    }

    @Override
    public void onChildChanged(DataSnapshot dataSnapshot, String previousChildName) {
        Log.d(TAG, "onChildChanged:" + dataSnapshot.getKey());
    }

    @Override
    public void onChildRemoved(DataSnapshot dataSnapshot) {
        Log.d(TAG, "onChildRemoved:" + dataSnapshot.getKey());

    }

    @Override
    public void onChildMoved(DataSnapshot dataSnapshot, String previousChildName) {
        Log.d(TAG, "onChildMoved:" + dataSnapshot.getKey());

    }

    @Override
    public void onCancelled(DatabaseError databaseError) {
        Log.w(TAG, "postComments:onCancelled", databaseError.toException());
        Toast.makeText(mContext, "Failed to load comments.",
                Toast.LENGTH_SHORT).show();
    }
};
ref.addChildEventListener(childEventListener);

```



## Designing and understanding how to retrieve realtime data from the Firebase Database


This example assumes that you have already set up a Firebase Realtime Database. If you are a starter, then please inform yourself [here](https://firebase.google.com/docs/android/setup) on how to add Firebase to your Android project.

First, add the dependency of the Firebase Database to the app level **build.gradle** file:

```java
compile 'com.google.firebase:firebase-database:9.4.0'

```

Now, let us create a chat app which stores data into the Firebase Database.

### Step 1: Create a class named Chat

Just create a class with some basic variables required for the chat:

```java
public class Chat{
    public String name, message;
}

```

### Step 2: Create some JSON data

For sending/retrieving data to/from the Firebase Database, you need to use JSON. Let us assume that some chats are already stored at the root level in the database. The data of these chats could look like as follows:

```java
[
    {
        "name":"John Doe",
        "message":"My first Message"
    },
    {
        "name":"John Doe",
        "message":"Second Message"
    },
    {
        "name":"John Doe",
        "message":"Third Message"
    }
]

```

### Step 3: Adding the listeners

There are three types of listeners. In the following example we are going to use the `childEventListener`:

```java
DatabaseReference chatDb = FirebaseDatabase.getInstance().getReference() // Referencing the root of the database.
        .child("chats"); // Referencing the "chats" node under the root.

chatDb.addChildEventListener(new ChildEventListener() {
    @Override
    public void onChildAdded(DataSnapshot dataSnapshot, String s) {
        // This function is called for every child id chat in this case, so using the above
        // example, this function is going to be called 3 times.
        
        // Retrieving the Chat object from this function is simple.
        Chat chat; // Create a null chat object.

        // Use the getValue function in the dataSnapshot and pass the object's class name to
        // which you want to convert and get data. In this case it is Chat.class.
        chat = dataSnapshot.getValue(Chat.class);

        // Now you can use this chat object and add it into an ArrayList or something like
        // that and show it in the recycler view.
    }

    @Override
    public void onChildChanged(DataSnapshot dataSnapshot, String s) {
        // This function is called when any of the node value is changed, dataSnapshot will
        // get the data with the key of the child, so you can swap the new value with the
        // old one in the ArrayList or something like that.

        // To get the key, use the .getKey() function.
        // To get the value, use code similar to the above one.
    }

    @Override
    public void onChildRemoved(DataSnapshot dataSnapshot) {
        // This function is called when any of the child node is removed. dataSnapshot will
        // get the data with the key of the child.

        // To get the key, use the s String parameter .
    }

    @Override
    public void onChildMoved(DataSnapshot dataSnapshot, String s) {
        // This function is called when any of the child nodes is moved to a different position.

        // To get the key, use the s String parameter.
    }

    @Override
    public void onCancelled(DatabaseError databaseError) {
        // If anything goes wrong, this function is going to be called.

        // You can get the exception by using databaseError.toException();
    }
});

```

### Step 4: Add data to the database

Just create a Chat class object and add the values as follows:

```java
Chat chat=new Chat();
chat.name="John Doe";
chat.message="First message from android";

```

Now get a reference to the chats node as done in the retrieving session:

```java
DatabaseReference chatDb = FirebaseDatabase.getInstance().getReference().child("chats");

```

Before you start adding data, keep in mind that you need one more deep reference since a chat node has several more nodes and adding a new chat means adding a new node containing the chat details. We can generate a new and unique name of the node using the `push()` function on the `DatabaseReference` object, which will return another `DatabaseReference`, which in turn points to a newly formed node to insert the chat data.

### Example

```java
// The parameter is the chat object that was newly created a few lines above.
chatDb.push().setValue(chat);

```

The `setValue()` function will make sure that all of the application's `onDataChanged` functions are getting called (including the same device), which happens to be the attached listener of the "chats" node.



## Denormalization: Flat Database Structure


Denormalization and a flat database structure is neccessary to efficiently download separate calls. With the following structure, it is also possible to maintain two-way relationships. The disadvantage of this approach is, that you always need to update the data in multiple places.

For an example, imagine an app which allows the user to store messages to himself (memos).

Desired flat database structure:

```java
|--database
  |-- memos
     |-- memokey1
       |-- title: "Title"
       |-- content: "Message"
     |-- memokey2
       |-- title: "Important Title"
       |-- content: "Important Message"
  |-- users
     |-- userKey1
       |-- name: "John Doe"
       |-- memos
         |-- memokey1 : true //The values here don't matter, we only need the keys.
         |-- memokey2 : true
     |-- userKey2
       |-- name: "Max Doe"

```

**The used memo class**

```java
public class Memo {
    private String title, content;
    //getters and setters ... 

    //toMap() is necessary for the push process
    private Map<String, Object> toMap() {
        HashMap<String, Object> result = new HashMap<>();
        result.put("title", title);
        result.put("content", content);
        return result;
    }
}

```

**Retrieving the memos of a user**

```java
//We need to store the keys and the memos seperately
private ArrayList<String> mKeys = new ArrayList<>();
private ArrayList<Memo> mMemos = new ArrayList<>();

//The user needs to be logged in to retrieve the uid
String currentUserId = FirebaseAuth.getInstance().getCurrentUser().getUid();

//This is the reference to the list of memos a user has 
DatabaseReference currentUserMemoReference = FirebaseDatabase.getInstance().getReference()
    .child("users").child(currentUserId).child("memos");

//This is a reference to the list of all memos
DatabaseReference memoReference = FirebaseDatabase.getInstance().getReference()
    .child("memos");

//We start to listen to the users memos, 
//this will also retrieve the memos initially
currentUserMemoReference.addChildEventListener(new ChildEventListener() {
        @Override
        public void onChildAdded(DataSnapshot dataSnapshot, String s) {
            //Here we retrieve the key of the memo the user has.
            String key = dataSnapshot.getKey(); //for example memokey1
            //For later manipulations of the lists, we need to store the key in a list
            mKeys.add(key);
            //Now that we know which message belongs to the user, 
            //we request it from our memos:
            memoReference.child(key).addValueEventListener(new ValueEventListener() {
                @Override
                    public void onDataChange(DataSnapshot dataSnapshot) {
                         //Here we retrieve our memo:
                         Memo memo = dataSnapshot.getValue(Memo.class);
                         mMemos.add(memo);
                    }

                @Override
                public void onCancelled(DatabaseError databaseError) { }
            });                           
        }

        @Override
        public void onChildChanged(DataSnapshot dataSnapshot, String s) { }

        @Override
        public void onChildRemoved(DataSnapshot dataSnapshot) { }

        @Override
        public void onChildMoved(DataSnapshot dataSnapshot, String s) { }

        @Override
        public void onCancelled(DatabaseError databaseError) { }
    }

```

**Creating a memo**

```java
//The user needs to be logged in to retrieve the uid
String currentUserUid = FirebaseAuth.getInstance().getCurrentUser().getUid();

//This is the path to the list of memos a user has 
String userMemoPath = "users/" + currentUserUid + "/memos/";

//This is the path to the list of all memos
String memoPath = "memos/";

//We need to retrieve an unused key from the memos reference
DatabaseReference memoReference = FirebaseDatabase.getInstance().getReference().child("memos");
String key = memoReference.push().getKey();
Memo newMemo = new Memo("Important numbers", "1337, 42, 3.14159265359");

Map<String, Object> childUpdates = new HashMap<>(); 
//The second parameter **here** (the value) does not matter, it's just that the key exists
childUpdates.put(userMemoPath + key, true);
childUpdates.put(memoPath + key, newMemo.toMap());

FirebaseDatabase.getInstance().getReference().updateChildren(childUpdates);

```

After the push, or database looks like this:

```java
|--database
  |-- memos
     |-- memokey1
       |-- title: "Title"
       |-- content: "Message"
     |-- memokey2
       |-- title: "Important Title"
       |-- content: "Important Message"
     |-- generatedMemokey3 
       |-- title: "Important numbers"
       |-- content: "1337, 42, 3.14159265359"
  |-- users
     |-- userKey1
       |-- name: "John Doe"
       |-- memos
         |-- memokey1 : true //The values here don't matter, we only need the keys.
         |-- memokey2 : true
         |-- generatedMemokey3 : true
     |-- userKey2
       |-- name: "Max Doe"

```



## Understanding firebase JSON database


Before we get our hands dirty with code, I feel it is necessary to understand how data is stored in firebase. Unlike relational databases, firebase stores data in JSON format. Think of each row in a relational database as a JSON object (which is basically unordered key-value pair). So the column name becomes key and the value stored in that column for one particular row is the value. This way the entire row is represented as a JSON object and a list of these represent an entire database table. The immediate benefit that I see for this is schema modification becomes much more cheaper operation compared to old RDBMS. It is easier to add a couple of more attributes to a JSON than altering a table structure.

here is a sample JSON to show how data is stored in firebase:

```

  {
    "user_base" : {
      "342343" : {
        "email" : "kaushal.xxxxx@gmail.com",
        "authToken" : "some string",
        "name" : "Kaushal",
        "phone" : "+919916xxxxxx",
        "serviceProviderId" : "firebase",
        "signInServiceType" : "google",
      },
      "354895" : {
        "email" : "xxxxx.devil@gmail.com",
        "authToken" : "some string",
        "name" : "devil",
        "phone" : "+919685xxxxxx",
        "serviceProviderId" : "firebase",
        "signInServiceType" : "github"
      },
      "371298" : {
        "email" : "bruce.wayne@wayneinc.com",
        "authToken" : "I am batman",
        "name" : "Bruce Wayne",
        "phone" : "+14085xxxxxx",
        "serviceProviderId" : "firebase",
        "signInServiceType" : "shield"
      }
    },
    "user_prefs": {
      "key1":{
        "data": "for key one"
      },
      "key2":{
        "data": "for key two"
      },
      "key3":{
        "data": "for key three"
      }
    },
    //other structures
  }

```

This clearly shows how data that we used to store in relational databases can be stored in JSON format. Next let's see how to read this data in android devices.



## Retrieving data from firebase


I am gonna assume you already know about adding gradle dependencies firebase in android studio. If you don't just follow the guide from [here](https://firebase.google.com/). Add your app in firebase console, gradle sync android studio after adding dependencies. All dependencies are not needed just firebase database and firebase auth.

Now that we know how data is stored and how to add gradle dependencies let's see how to use the imported firebase android SDK to retrieve data.

create a firebase database reference

```java
DatabaseReference userDBRef = FirebaseDatabase.getInstance().getReference();
// above statement point to base tree
userDBRef = DatabaseReference.getInstance().getReference().child("user_base")
// points to user_base table JSON (see previous section)

```

from here you can chain multiple child() method calls to point to the data you are interested in. For example if data is stored as depicted in previous section and you want to point to Bruce Wayne user you can use:

```java
DatabaseReference bruceWayneRef = userDBRef.child("371298");
// 371298 is key of bruce wayne user in JSON structure (previous section)

```

Or simply pass the whole reference to the JSON object:

```java
DatabaseReference bruceWayneRef = DatabaseReference.getInstance().getReference()
     .child("user_base/371298");
// deeply nested data can also be referenced this way, just put the fully
// qualified path in pattern shown in above code "blah/blah1/blah1-2/blah1-2-3..."

```

Now that we have the reference of the data we want to fetch, we can use listeners to fetch data in android apps. Unlike the traditional calls where you fire REST API calls using retrofit or volley, here a simple callback listener is required to get the data. Firebase sdk calls the callback methods and you are done.

There are basically two types of listeners you can attach, one is [ValueEventListener](https://firebase.google.com/docs/reference/android/com/google/firebase/database/ValueEventListener) and the other one is [ChildEventListener](https://firebase.google.com/docs/reference/android/com/google/firebase/database/ChildEventListener) (described in next section). For any change in data under the node we have references and added listeners to, value event listeners return the entire JSON structure and child event listener returns specific child where the change has happened. Both of these are useful in their own way. To fetch the data from firebase we can add one or more listeners to a firebase database reference (list userDBRef we created earlier).

Here is some sample code (code explanation after code):

```java
userDBRef.addValueEventListener(new ValueEventListener() {
    @Override
    public void onDataChange(DataSnapshot dataSnapshot) {
        User bruceWayne = dataSnapshot.child("371298").getValue(User.class);
        // Do something with the retrieved data or Bruce Wayne
    }

    @Override
    public void onCancelled(DatabaseError databaseError) {
        Log.e("UserListActivity", "Error occured");
        // Do something about the error
    });

```

Did you notice the Class type passed. [DataSnapshot](https://www.firebase.com/docs/java-api/javadoc/com/firebase/client/DataSnapshot.html) can convert JSON data into our defined POJOs, simple pass the right class type.

If your use case does not require the entire data (in our case user_base table) every time some little change occurs or say you want to ****fetch the data only once****, you can use ****addListenerForSingleValueEvent()**** method of Database reference. This fires the callback only once.

```java
userDBRef.addListenerForSingleValueEvent(new ValueEventListener() {
    @Override
    public void onDataChange(DataSnapshot dataSnapshot) {
        // Do something
    }

    @Override
    public void onCancelled(DatabaseError databaseError) {
        // Do something about the error
    });

```

Above samples will give you the value of the JSON node. To get the key simply call:

```java
String myKey = dataSnapshot.getKey();

```



## Listening for child updates


Take a use case, like a chat app or a collaborative grocery list app (that basically requires a list of objects to be synced across users). If you use firebase database and add a value event listener to the chat parent node or grocery list parent node, you will end with entire chat structure from the beginning of time (i meant beginning of your chat) every time a chat node is added (i.e. anyone says hi). That we don't want to do, what we are interested in is only the new node or only the old node that got deleted or modified, the unchanged ones should not be returned.

In this case we can use [ChildEvenListener](https://firebase.google.com/docs/reference/android/com/google/firebase/database/ChildEventListener). Without any further adieu, here is code sample (see prev sections for sample JSON data):

```java
userDBRef.addChildEventListener(new ChildEventListener() {
    @Override
    public void onChildAdded(DataSnapshot dataSnapshot, String s) {
    }

    @Override
    public void onChildChanged(DataSnapshot dataSnapshot, String s) {
    }

    @Override
    public void onChildRemoved(DataSnapshot dataSnapshot) {
    }

    @Override
    public void onChildMoved(DataSnapshot dataSnapshot, String s) {
        //If not dealing with ordered data forget about this
    }

    @Override
    public void onCancelled(DatabaseError databaseError) {
    });

```

Method names are self explanatory. As you can see whenever a new user is added or some property of existing user is modified or user is deleted or removed appropriate callback method of child event listener is called with relevant data. So if you are keeping UI refreshed for say chat app, get the JSON from onChildAdded() parse into POJO and fit it in your UI. Just remember to remove your listener when user leaves the screen.

onChildChanged() gives the entire child value with changed properties (new ones).

onChiledRemoved() returns the removed child node.



## Retrieving data with pagination


When you have a huge JSON database, adding a value event listener doesn't make sense. It will return the huge JSON and parsing it would be time consuming. In such cases we can use pagination and fetch part of data and display or process it. Kind of like lazy loading or like fetching old chats when user clicks on show older chat. In this case [Query](https://firebase.google.com/docs/reference/android/com/google/firebase/database/Query) can used.

Let's take the our old example in previous sections. The user base contains 3 users, if it grows to say 3 hundred thousand user and you want to fetch the user list in batches of 50:

```java
// class level
final int limit = 50;
int start = 0;

// event level
Query userListQuery = userDBRef.orderByChild("email").limitToFirst(limit)
        .startAt(start)
userListQuery.addValueEventListener(new ValueEventListener() {
    @Override
        public void onDataChange(DataSnapshot dataSnapshot) {
        // Do something
        start += (limit+1);
    }

    @Override
    public void onCancelled(DatabaseError databaseError) {
        // Do something about the error
    });

```

Here value or child events can be added and listened to. Call query again to fetch next 50. **Make sure to add the orderByChild() method**, this will not work without that. Firebase needs to know the order by which you are paginating.



#### Remarks


### Other related topics:

- [Firebase](http://stackoverflow.com/documentation/android/3843/firebase#t=201608180555120758354)

