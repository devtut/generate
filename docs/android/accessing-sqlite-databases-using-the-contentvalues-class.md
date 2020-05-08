---
metaTitle: "Android - Accessing SQLite databases using the ContentValues class"
description: "Inserting and updating rows in a SQLite database"
---

# Accessing SQLite databases using the ContentValues class




## Inserting and updating rows in a SQLite database


First, you need to open your SQLite database, which can be done as follows:

```java
SQLiteDatabase myDataBase; 
String mPath = dbhelper.DATABASE_PATH + dbhelper.DATABASE_NAME;
myDataBase = SQLiteDatabase.openDatabase(mPath, null, SQLiteDatabase.OPEN_READWRITE);

```

After opening the database, you can easily insert or update rows by using the [`ContentValues`](https://developer.android.com/reference/android/content/ContentValues.html) class. The following examples assume that a first name is given by `str_edtfname` and a last nameby `str_edtlname`. You also need to replace `table_name` by the name of your table that you want to modify.

### Inserting data

```java
ContentValues values = new ContentValues();
values.put("First_Name", str_edtfname);
values.put("Last_Name", str_edtlname);
myDataBase.insert("table_name", null, values);

```

### Updating data

```java
ContentValues values = new ContentValues();
values.put("First_Name", str_edtfname);
values.put("Last_Name", str_edtlname);
myDataBase.update("table_name", values, "id" + " = ?", new String[] {id});

```

