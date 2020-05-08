---
metaTitle: "Android - Realm"
description: "Sorted queries, Using Realm with RxJava, Basic Usage, List of primitives (RealmList<Integer/String/...>), Adding Realm to your project, Realm Models, Async queries, try-with-resources"
---

# Realm


[Realm](https://realm.io/products/realm-mobile-database/) Mobile Database is an alternative to SQLite. Realm Mobile Database is much faster than an ORM, and often faster than raw SQLite.

**Benefits**

Offline functionality, Fast queries, Safe threading, Cross-platform apps, Encryption, Reactive architecture.



## Sorted queries


In order to sort a query, instead of using `findAll()`, you should use `findAllSorted()`.

```java
RealmResults<SomeObject> results = realm.where(SomeObject.class)
                                            .findAllSorted("sortField", Sort.ASCENDING);

```

**Note:**

**`sort()`** returns a completely new RealmResults that is sorted, but an update to this RealmResults will reset it. If you use `sort()`, you should always re-sort it in your `RealmChangeListener`, remove the `RealmChangeListener` from the previous `RealmResults` and add it to the returned new `RealmResults`. Using `sort()` on a `RealmResults` returned by an async query that is not yet loaded will fail.

**`findAllSorted()`** will always return the results sorted by the field, even if it gets updated. It is recommended to use `findAllSorted()`.



## Using Realm with RxJava


For queries, Realm provides the `realmResults.asObservable()` method. Observing results is only possible on looper threads (typically the UI thread).

For this to work, your configuration must contain the following

```java
realmConfiguration = new RealmConfiguration.Builder(context)       //
                          .rxFactory(new RealmObservableFactory()) //
                             //...
                          .build();

```

Afterwards, you can use your results as an observable.

```java
Observable<RealmResults<SomeObject>> observable = results.asObservable();

```

For asynchronous queries, you should filter the results by `isLoaded()`, so that you receive an event only when the query has been executed. This `filter()` is not needed for synchronous queries (`isLoaded()` always returns `true` on sync queries).

```

   Subscription subscription = RxTextView.textChanges(editText).switchMap(charSequence -> 
        realm.where(SomeObject.class)
             .contains("searchField", charSequence.toString(), Case.INSENSITIVE)
             .findAllAsync()
             .asObservable())
    .filter(RealmResults::isLoaded) //
    .subscribe(objects -> adapter.updateData(objects));

```

For writes, you should either use the `executeTransactionAsync()` method, or open a Realm instance on the background thread, execute the transaction synchronously, then close the Realm instance.

```java
public Subscription loadObjectsFromNetwork() {
    return objectApi.getObjects()
        .subscribeOn(Schedulers.io())
        .subscribe(response -> {
            try(Realm realmInstance = Realm.getDefaultInstance()) {
                realmInstance.executeTransaction(realm -> realm.insertOrUpdate(response.objects));
            }
        });
}

```



## Basic Usage


### Setting up an instance

To use Realm you first need to obtain an instance of it. Each Realm instance maps to a file on disk. The most basic way to get an instance is as follows:

```java
// Create configuration
RealmConfiguration realmConfiguration = new RealmConfiguration.Builder(context).build();

// Obtain realm instance
Realm realm = Realm.getInstance(realmConfiguration);
// or
Realm.setDefaultConfiguration(realmConfiguration);
Realm realm = Realm.getDefaultInstance();

```

The method `Realm.getInstance()` creates the database file if it has not been created, otherwise opens the file. The `RealmConfiguration` object controls all aspects of how a Realm is created - whether it's an `inMemory()` database, name of the Realm file, if the Realm should be cleared if a migration is needed, initial data, etc.

Please note that calls to `Realm.getInstance()` are reference counted (each call increments a counter), and the counter is decremented when `realm.close()` is called.

### Closing an instance

On background threads, it's **very important** to ****close**** the Realm instance(s) once it's no longer used (for example, transaction is complete and the thread execution ends). Failure to close all Realm instances on background thread results in version pinning, and can cause a large growth in file size.

```java
Runnable runnable = new Runnable() {
    Realm realm = null;
    try {
        realm = Realm.getDefaultInstance();
        // ...
    } finally {
        if(realm != null) {
            realm.close();
        }
    }
};

new Thread(runnable).start(); // background thread, like `doInBackground()` of AsyncTask

```

It's worth noting that above API Level 19, you can replace this code with just this:

```java
try(Realm realm = Realm.getDefaultInstance()) {
    // ...
}

```

### Models

Next step would be creating your models. Here a question might be asked, "what is a model?". A model is a structure which defines properties of an object being stored in the database. For example, in the following we model a book.

```java
public class Book extends RealmObject {
 
    // Primary key of this entity
    @PrimaryKey
    private long id;
 
    private String title;

    @Index // faster queries
    private String author;
 
    // Standard getters & setter
    public long getId() {
        return id;
    }
 
    public void setId(long id) {
        this.id = id;
    }
 
    public String getTitle() {
        return title;
    }
 
    public void setTitle(String title) {
        this.title = title;
    }
 
    public String getAuthor() {
        return author;
    }
 
    public void setAuthor(String author) {
        this.author = author;
    }
}

```

Note that your models should extend RealmObject class. Primary key is also specified by `@PrimaryKey` annotation. Primary keys can be null, but only one element can have `null` as a primary key. Also you can use the `@Ignore` annotation for the fields that should not be persisted to the disk:

```java
@Ignore
private String isbn;

```

### Inserting or updating data

In order to store a book object to your Realm database instance, you can first create an instance of your model and then store it to the database via `copyToRealm` method. For creating or updating you can use `copyToRealmOrUpdate`. (A faster alternative is the newly added `insertOrUpdate()`).

```java
// Creating an instance of the model
Book book = new Book();
book.setId(1);
book.setTitle("Walking on air");
book.setAuthor("Taylor Swift")

// Store to the database
realm.executeTransaction(new Realm.Transaction() {
    @Override
    public void execute(Realm realm) {
        realm.insertOrUpdate(book);
    }
});

```

Note that all changes to data must happen in a transaction. Another way to create an object is using the following pattern:

```java
Book book = realm.createObject(Book.class, primaryKey);
...

```

### Querying the database

<li>
All books:

```java
RealmResults<Book> results = realm.where(Book.class).findAll();

```


</li>
<li>
All books having id greater than 10:

```java
RealmResults<Book> results = realm.where(Book.class)
                                  .greaterThan("id", 10)
                                  .findAll();

```


</li>
<li>
Books by `'Taylor Swift'` or `'%Peter%'`:

```java
RealmResults<Book> results = realm.where(Book.class)
                                  .beginGroup()
                                      .equalTo("author", "Taylor Swift")
                                      .or()
                                      .contains("author", "Peter")
                                  .endGroup().findAll();

```


</li>

### Deleting an object

For example, we want to delete all books by Taylor Swift:

```java
// Start of transaction
realm.executeTransaction(new Realm.Transaction() {
    @Override
    public void execute(Realm realm) {
        // First Step: Query all Taylor Swift books
        RealmResults<Book> results = ...
        
        // Second Step: Delete elements in Realm
        results.deleteAllFromRealm();
    }
});

```



## List of primitives (RealmList<Integer/String/...>)


Realm currently does not support storing a list of primitives. It is on their todo list ([GitHub issue #575](https://github.com/realm/realm-java/issues/575)), but for the meantime, here is a workaround.

Create a new class for your primitive type, this uses Integer, but change it for whatever you want to store.

```java
public class RealmInteger extends RealmObject {
    private int val;

    public RealmInteger() {
    }

    public RealmInteger(int val) {
        this.val = val;
    }

    // Getters and setters
}

```

You can now use this in your `RealmObject`.

```java
public class MainObject extends RealmObject {

    private String name;
    private RealmList<RealmInteger> ints;

    // Getters and setters
}

```

If you are using `GSON` to populate your `RealmObject`, you will need to add a custom type adapter.

```java
Type token = new TypeToken<RealmList<RealmInteger>>(){}.getType();
Gson gson = new GsonBuilder()
        .setExclusionStrategies(new ExclusionStrategy() {
            @Override
            public boolean shouldSkipField(FieldAttributes f) {
                return f.getDeclaringClass().equals(RealmObject.class);
            }

            @Override
            public boolean shouldSkipClass(Class<?> clazz) {
                return false;
            }
        })
        .registerTypeAdapter(token, new TypeAdapter<RealmList<RealmInteger>>() {

            @Override
            public void write(JsonWriter out, RealmList<RealmInteger> value) throws IOException {
                // Empty
            }

            @Override
            public RealmList<RealmInteger> read(JsonReader in) throws IOException {
                RealmList<RealmInteger> list = new RealmList<RealmInteger>();
                in.beginArray();
                while (in.hasNext()) {
                    list.add(new RealmInteger(in.nextInt()));
                }
                in.endArray();
                return list;
            }
        })
        .create();

```



## Adding Realm to your project


Add the following dependency to your **project** level `build.gradle` file.

```java
dependencies {
    classpath "io.realm:realm-gradle-plugin:3.1.2"
}

```

Add the following right at the top of your **app** level `build.gradle` file.

```java
apply plugin: 'realm-android'

```

Complete a gradle sync and you now have Realm added as a dependency to your project!

Realm requires an initial call since 2.0.0 before using it. You can do this in your `Application` class or in your first Activity's `onCreate` method.

```java
Realm.init(this); // added in Realm 2.0.0
Realm.setDefaultConfiguration(new RealmConfiguration.Builder().build());

```



## Realm Models


[Realm models](https://realm.io/docs/java/latest/#models) must extend the `RealmObject` base class, they define the schema of the underlying database.

Supported field types are `boolean`, `byte`, `short`, `int`, `long`, `float`, `double`, `String`, `Date`, `byte[]`, links to other `RealmObject`s, and `RealmList<T extends RealmModel>`.

```java
public class Person extends RealmObject {
    @PrimaryKey //primary key is also implicitly an @Index 
                //it is required for `copyToRealmOrUpdate()` to update the object.
    private long id;

    @Index //index makes queries faster on this field
    @Required //prevents `null` value from being inserted
    private String name; 

    private RealmList<Dog> dogs; //->many relationship to Dog

    private Person spouse; //->one relationship to Person

    @Ignore
    private Calendar birthday; //calendars are not supported but can be ignored

    // getters, setters
}

```

If you add (or remove) a new field to your RealmObject (or you add a new RealmObject class or delete an existing one), a **migration** will be needed. You can either set `deleteIfMigrationNeeded()` in your `RealmConfiguration.Builder`, or define the necessary migration. Migration is also required when adding (or removing) `@Required`, or `@Index`, or `@PrimaryKey` annotation.

Relationships must be set manually, they are NOT automatic based on primary keys.

Since 0.88.0, it is also possible to use public fields instead of private fields/getters/setters in RealmObject classes.

It is also possible to implement [`RealmModel`](https://realm.io/docs/java/latest/#realmmodel-interface) instead of extending `RealmObject`, if the class is also annotated with `@RealmClass`.

```java
@RealmClass
public class Person implements RealmModel {
    // ...
}

```

In that case, methods like `person.deleteFromRealm()` or `person.addChangeListener()` are replaced with `RealmObject.deleteFromRealm(person)` and `RealmObject.addChangeListener(person)`.

[Limitations](https://realm.io/docs/java/latest/#limitations) are that by a `RealmObject`, only `RealmObject` can be extended, and there is no support for `final`, `volatile` and `transient` fields.

It is important that a **managed** RealmObject class can only be modified in a transaction. A **managed** RealmObject cannot be passed between threads.



## Async queries


Every synchronous query method (such as `findAll()` or `findAllSorted()`) has an asynchronous counterpart (`findAllAsync()` / `findAllSortedAsync()`).

Asynchronous queries offload the evaluation of the `RealmResults` to another thread. In order to receive these results on the current thread, the current thread must be a looper thread (read: async queries typically only work on the UI thread).

```java
RealmChangeListener<RealmResults<SomeObject>> realmChangeListener; // field variable

realmChangeListener = new RealmChangeListener<RealmResults<SomeObject>>() {
    @Override
    public void onChange(RealmResults<SomeObject> element) {
        // asyncResults are now loaded
        adapter.updateData(element);
    }
};

RealmResults<SomeObject> asyncResults = realm.where(SomeObject.class).findAllAsync();
asyncResults.addChangeListener(realmChangeListener);

```



## try-with-resources


```java
try (Realm realm = Realm.getDefaultInstance()) {
    realm.executeTransaction(new Realm.Transaction() {
            @Override
            public void execute(Realm realm) {
                //whatever Transaction that has to be done
            }
    });
    //No need to close realm in try-with-resources
}

```

The Try with resources can be used only from KITKAT (minSDK 19)



#### Remarks


When you use Realm, you must remember that you mustn't pass RealmObjects, RealmResults and Realm instances between threads. If you need a query on a given thread, open a Realm instance on that thread. At the termination of the thread, you should close the Realm.

> 
<p>****LEGAL NOTE****: You understand that the Software may contain cryptographic functions that may be subject to export restrictions,
and you represent and warrant that **you are not located in a country**
that is subject to United States export restriction or embargo,
**including Cuba, Iran, North Korea, Sudan, Syria or the Crimea region**,
and that you are not on the Department of Commerce list of Denied
Persons, Unverified Parties, or affiliated with a Restricted Entity.</p>


