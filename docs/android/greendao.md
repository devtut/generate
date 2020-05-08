---
metaTitle: "Android - GreenDAO"
description: "Helper methods for SELECT, INSERT, DELETE, UPDATE queries, Creating an Entity with GreenDAO 3.X that has a Composite Primary Key, Getting started with GreenDao v3.X"
---

# GreenDAO


GreenDAO is an Object-Relational Mapping library to help developers use SQLite databases for persistent local storage.



## Helper methods for SELECT, INSERT, DELETE, UPDATE queries


This example shows a helper class that contains methods useful, when executing the queries for data. Every method here uses Java Generic's in order to be very flexible.

```java
public <T> List<T> selectElements(AbstractDao<T, ?> dao) {
    if (dao == null) {
        return null;
    }
    QueryBuilder<T> qb = dao.queryBuilder();
    return qb.list();
}

public <T> void insertElements(AbstractDao<T, ?> absDao, List<T> items) {
    if (items == null || items.size() == 0 || absDao == null) {
        return;
    }
    absDao.insertOrReplaceInTx(items);
}

public <T> T insertElement(AbstractDao<T, ?> absDao, T item) {
    if (item == null || absDao == null) {
        return null;
    }
    absDao.insertOrReplaceInTx(item);
    return item;
}

public <T> void updateElements(AbstractDao<T, ?> absDao, List<T> items) {
    if (items == null || items.size() == 0 || absDao == null) {
        return;
    }
    absDao.updateInTx(items);
}

public <T> T selectElementByCondition(AbstractDao<T, ?> absDao,
                                      WhereCondition... conditions) {
    if (absDao == null) {
        return null;
    }
    QueryBuilder<T> qb = absDao.queryBuilder();
    for (WhereCondition condition : conditions) {
        qb = qb.where(condition);
    }
    List<T> items = qb.list();
    return items != null && items.size() > 0 ? items.get(0) : null;
}

public <T> List<T> selectElementsByCondition(AbstractDao<T, ?> absDao,
                                             WhereCondition... conditions) {
    if (absDao == null) {
        return null;
    }
    QueryBuilder<T> qb = absDao.queryBuilder();
    for (WhereCondition condition : conditions) {
        qb = qb.where(condition);
    }
    List<T> items = qb.list();
    return items != null ? items : null;
}

public <T> List<T> selectElementsByConditionAndSort(AbstractDao<T, ?> absDao,
                                                    Property sortProperty,
                                                    String sortStrategy,
                                                    WhereCondition... conditions) {
    if (absDao == null) {
        return null;
    }
    QueryBuilder<T> qb = absDao.queryBuilder();
    for (WhereCondition condition : conditions) {
        qb = qb.where(condition);
    }
    qb.orderCustom(sortProperty, sortStrategy);
    List<T> items = qb.list();
    return items != null ? items : null;
}

public <T> List<T> selectElementsByConditionAndSortWithNullHandling(AbstractDao<T, ?> absDao,
                                                                    Property sortProperty,
                                                                    boolean handleNulls,
                                                                    String sortStrategy,
                                                                    WhereCondition... conditions) {
    if (!handleNulls) {
        return selectElementsByConditionAndSort(absDao, sortProperty, sortStrategy, conditions);
    }
    if (absDao == null) {
        return null;
    }
    QueryBuilder<T> qb = absDao.queryBuilder();
    for (WhereCondition condition : conditions) {
        qb = qb.where(condition);
    }
    qb.orderRaw("(CASE WHEN " + "T." + sortProperty.columnName + " IS NULL then 1 ELSE 0 END)," + "T." + sortProperty.columnName + " " + sortStrategy);
    List<T> items = qb.list();
    return items != null ? items : null;
}

public <T, V extends Class> List<T> selectByJoin(AbstractDao<T, ?> absDao,
                                                 V className,
                                                 Property property, WhereCondition whereCondition) {
    QueryBuilder<T> qb = absDao.queryBuilder();
    qb.join(className, property).where(whereCondition);
    return qb.list();
}

public <T> void deleteElementsByCondition(AbstractDao<T, ?> absDao,
                                          WhereCondition... conditions) {
    if (absDao == null) {
        return;
    }
    QueryBuilder<T> qb = absDao.queryBuilder();
    for (WhereCondition condition : conditions) {
        qb = qb.where(condition);
    }
    List<T> list = qb.list();
    absDao.deleteInTx(list);
}

public <T> T deleteElement(DaoSession session, AbstractDao<T, ?> absDao, T object) {
    if (absDao == null) {
        return null;
    }
    absDao.delete(object);
    session.clear();
    return object;
}

public <T, V extends Class> void deleteByJoin(AbstractDao<T, ?> absDao,
                                              V className,
                                              Property property, WhereCondition whereCondition) {
    QueryBuilder<T> qb = absDao.queryBuilder();
    qb.join(className, property).where(whereCondition);
    qb.buildDelete().executeDeleteWithoutDetachingEntities();
}

public <T> void deleteAllFromTable(AbstractDao<T, ?> absDao) {
    if (absDao == null) {
        return;
    }
    absDao.deleteAll();
}

public <T> long countElements(AbstractDao<T, ?> absDao) {
    if (absDao == null) {
        return 0;
    }
    return absDao.count();
}

```



## Creating an Entity with GreenDAO 3.X that has a Composite Primary Key


When creating a model for a table that has a composite primary key, additional work is required on the Object for the model Entity to respect those constraints.

The following example SQL table and Entity demonstrates the structure to store a review left by a customer for an item in an online store. In this example, we want the `customer_id` and `item_id` columns to be a composite primary key, allowing only one review to exist between a specific customer and item.

**SQL Table**

```java
CREATE TABLE review (
    customer_id STRING NOT NULL,
    item_id STRING NOT NULL,
    star_rating INTEGER NOT NULL,
    content STRING,
    PRIMARY KEY (customer_id, item_id)
);

```

Usually we would use the `@Id` and `@Unique` annotations above the respective fields in the entity class, however for a composite primary key we do the following:

<li>
Add the `@Index` annotation inside the class-level `@Entity` annotation. The value property contains a comma-delimited list of the fields that make up the key. Use the `unique` property as shown to enforce uniqueness on the key.
</li>
<li>
GreenDAO requires every Entity have a `long` or `Long` object as a primary key. We still need to add this to the Entity class, however we do not need to use it or worry about it affecting our implementation. In the example below it is called `localID`
</li>

**Entity**

```java
@Entity(indexes = { @Index(value = "customer_id,item_id", unique = true)})
public class Review {

    @Id(autoincrement = true)
    private Long localID;

    private String customer_id;
    private String item_id;

    @NotNull
    private Integer star_rating;

    private String content;

    public Review() {}
}

```



## Getting started with GreenDao v3.X


After adding the GreenDao library dependency and Gradle plugin, we need to first create an entity object.

**Entity**

An entity is a Plain Old Java Object **(POJO)** that models some data in the database. GreenDao will use this class to create a table in the SQLite database and automatically generate helper classes we can use to access and store data without having to write SQL statements.

```java
@Entity
public class Users {

    @Id(autoincrement = true)
    private Long id;

    private String firstname;
    private String lastname;

    @Unique
    private String email;

    // Getters and setters for the fields...

}

```

**One-time GreenDao setup**

Each time an application is launched GreenDao needs to be initialized. GreenDao suggests keeping this code in an Application class or somewhere it will only be run once.

```java
DaoMaster.DevOpenHelper helper = new DaoMaster.DevOpenHelper(this, "mydatabase", null);
db = helper.getWritableDatabase();
DaoMaster daoMaster = new DaoMaster(db);
DaoSession daoSession = daoMaster.newSession();

```

**GreenDao Helper Classes**

After the entity object is created, GreenDao automatically creates the helper classes used to interact with the database. These are named similarly to the name of the entity object that was created, followed by `Dao` and are retrieved from the `daoSession` object.

```java
UsersDao usersDao = daoSession.getUsersDao();

```

Many typical database actions can now be performed using this Dao object with the entity object.

**Query**

```java
String email = "jdoe@example.com";
String firstname = "John";

// Single user query WHERE email matches "jdoe@example.com"
Users user = userDao.queryBuilder()
                .where(UsersDao.Properties.Email.eq(email)).build().unique();

// Multiple user query WHERE firstname = "John"
List<Users> user = userDao.queryBuilder()
                .where(UsersDao.Properties.Firstname.eq(firstname)).build().list();

```

**Insert**

```java
Users newUser = new User("John","Doe","jdoe@example.com");
usersDao.insert(newUser);

```

**Update**

```java
// Modify a previously retrieved user object and update
user.setLastname("Dole");
usersDao.update(user);

```

**Delete**

```java
// Delete a previously retrieved user object
usersDao.delete(user);

```

