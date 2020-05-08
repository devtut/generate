---
metaTitle: "Android - ORMLite in android"
description: "Android OrmLite over SQLite example"
---

# ORMLite in android



## Android OrmLite over SQLite example


**ORMLite** is an Object Relational Mapping package that provides simple and lightweight functionality for persisting Java objects to SQL databases while avoiding the complexity and overhead of more standard ORM packages.

Speaking for Android, OrmLite is implemented over the out-of-the-box supported database, SQLite. It makes direct calls to the API to access SQLite.

### Gradle setup

To get started you should include the package to the build gradle.

```

// https://mvnrepository.com/artifact/com.j256.ormlite/ormlite-android
compile group: 'com.j256.ormlite', name: 'ormlite-android', version: '5.0'
POJO configuration

```

Then you should configure a POJO to be persisted to the database. Here care must be taken to the annotations:

<li>Add the @DatabaseTable annotation to the top of each class. You can
also use @Entity.</li>
<li>Add the @DatabaseField annotation right before each field to be
persisted. You can also use @Column and others.</li>
<li>Add a no-argument constructor to each class with at least package
visibility.</li>

```

@DatabaseTable(tableName = "form_model")
 public class FormModel implements Serializable {

    @DatabaseField(generatedId = true)
    private Long id;
    @DatabaseField(dataType = DataType.SERIALIZABLE)
    ArrayList<ReviewItem> reviewItems;

    @DatabaseField(index = true)
    private String username;

    @DatabaseField
    private String createdAt;

    public FormModel() {
    }

    public FormModel(ArrayList<ReviewItem> reviewItems, String username, String createdAt) {
        this.reviewItems = reviewItems;
        this.username = username;
        this.createdAt = createdAt;
    }
}

```

At the example above there is one table (form_model) with 4 fields.

id field is auto generated index.

username is an index to the database.

More information about the annotation can be found at the [official documentation](http://ormlite.com/javadoc/ormlite-core/doc-files/ormlite_2.html#Local-Annotations).

### Database Helper

To continue with, you will need to create a database helper class which should extend the OrmLiteSqliteOpenHelper class.

This class creates and upgrades the database when your application is installed and can also provide the DAO classes used by your other classes.

DAO stands for Data Access Object and it provides all the scrum functionality and specializes in the handling a single persisted class.

The helper class must implement the following two methods:

<li>
<p>onCreate(SQLiteDatabase sqliteDatabase, ConnectionSource
connectionSource);</p>
onCreate creates the database when your app is first installed
</li>
<li>
<p>onUpgrade(SQLiteDatabase database, ConnectionSource connectionSource,
int oldVersion, int newVersion);</p>
onUpgrade handles the upgrading of the database tables when you upgrade your app to a new version
</li>

Database Helper class example:

```

 public class OrmLite extends OrmLiteSqliteOpenHelper {
    
        //Database name
        private static final String DATABASE_NAME = "gaia";
        //Version of the database. Changing the version will call {@Link OrmLite.onUpgrade}
        private static final int DATABASE_VERSION = 2;
    
        /**
         * The data access object used to interact with the Sqlite database to do C.R.U.D operations.
         */
        private Dao<FormModel, Long> todoDao;
    
    
    
        public OrmLite(Context context) {
            super(context, DATABASE_NAME, null, DATABASE_VERSION,
                    /**
                     * R.raw.ormlite_config is a reference to the ormlite_config2.txt file in the
                     * /res/raw/ directory of this project
                     * */
                    R.raw.ormlite_config2);
        }
    
        @Override
        public void onCreate(SQLiteDatabase database, ConnectionSource connectionSource) {
            try {
    
                /**
                 * creates the database table
                 */
                TableUtils.createTable(connectionSource, FormModel.class);
    
            } catch (SQLException e) {
                e.printStackTrace();
            } catch (java.sql.SQLException e) {
                e.printStackTrace();
            }
        }
        /*
            It is called when you construct a SQLiteOpenHelper with version newer than the version of the opened database.
         */
        @Override
        public void onUpgrade(SQLiteDatabase database, ConnectionSource connectionSource,
                              int oldVersion, int newVersion) {
            try {
                /**
                 * Recreates the database when onUpgrade is called by the framework
                 */
                TableUtils.dropTable(connectionSource, FormModel.class, false);
                onCreate(database, connectionSource);
    
            } catch (SQLException | java.sql.SQLException e) {
                e.printStackTrace();
            }
        }
    
        /**
         * Returns an instance of the data access object
         * @return
         * @throws SQLException
         */
        public Dao<FormModel, Long> getDao() throws SQLException {
            if(todoDao == null) {
                try {
                    todoDao = getDao(FormModel.class);
                } catch (java.sql.SQLException e) {
                    e.printStackTrace();
                }
            }
            return todoDao;
        }
    }

```

### Persisting Object to SQLite

Finally, the class that persists the object to the database.

```

    public class ReviewPresenter {
    Dao<FormModel, Long> simpleDao;


    public ReviewPresenter(Application application) {
        this.application = (GaiaApplication) application;
        simpleDao = this.application.getHelper().getDao();
    }

    public void storeFormToSqLite(FormModel form) {

        try {
            simpleDao.create(form);
        } catch (SQLException e) {
            e.printStackTrace();
        }
        List<FormModel> list = null;
        try {
// query for all of the data objects in the database
            list = simpleDao.queryForAll();
        } catch (SQLException e) {
            e.printStackTrace();
        }
// our string builder for building the content-view
        StringBuilder sb = new StringBuilder();
        int simpleC = 1;
        for (FormModel simple : list) {
            sb.append('#').append(simpleC).append(": ").append(simple.getUsername()).append('\n');
            simpleC++;
        }
        System.out.println(sb.toString());
    }
    
    //Query to database to get all forms by username
    public List<FormModel> getAllFormsByUsername(String username) {
        List<FormModel> results = null;
        try {
            results = simpleDao.queryBuilder().where().eq("username", PreferencesManager.getInstance().getString(Constants.USERNAME)).query();
        } catch (SQLException e) {
            e.printStackTrace();
        }
        return results;
    }
}

```

The accessor of the DOA at the constructor of the above class is defined as:

```

private OrmLite dbHelper = null;

/*
Provides the SQLite Helper Object among the application
 */
public OrmLite getHelper() {
    if (dbHelper == null) {
        dbHelper = OpenHelperManager.getHelper(this, OrmLite.class);
    }
    return dbHelper;
}

```

