---
metaTitle: "sqlite3_stmt: Prepared Statement (C API)"
description: "Executing a Statement, Reading Data from a Cursor, Executing a prepared statement multiple times"
---

# sqlite3_stmt: Prepared Statement (C API)



## Executing a Statement


A statement is constructed with a function such as [sqlite3_prepare_v2()](http://www.sqlite.org/c3ref/prepare.html).

A prepared statement object **must** be cleaned up with [sqlite3_finalize()](http://www.sqlite.org/c3ref/finalize.html). Do not forget this in case of an error.

If [parameters](http://www.sqlite.org/lang_expr.html#varparam) are used, set their values with the [sqlite3_bind_xxx() functions](http://www.sqlite.org/c3ref/bind_blob.html).

The actual execution happens when [sqlite3_step()](http://www.sqlite.org/c3ref/step.html) is called.

```sql
const char *sql = "INSERT INTO MyTable(ID, Name) VALUES (?, ?)";
sqlite3_stmt *stmt;
int err;

err = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
if (err != SQLITE_OK) {
    printf("prepare failed: %s\n", sqlite3_errmsg(db));
    return /* failure */;
}

sqlite3_bind_int (stmt, 1, 42);                          /* ID */
sqlite3_bind_text(stmt, 2, "Bob", -1, SQLITE_TRANSIENT); /* name */

err = sqlite3_step(stmt);
if (err != SQLITE_DONE) {
    printf("execution failed: %s\n", sqlite3_errmsg(db));
    sqlite3_finalize(stmt);
    return /* failure */;
}

sqlite3_finalize(stmt);
return /* success */;

```



## Reading Data from a Cursor


A SELECT query is [executed](http://stackoverflow.com/documentation/sqlite/5456/sqlite3-stmt-prepared-statement-c-api/19405/executing-a-statement) like any other statement.
To read the returned data, call [sqlite3_step()](http://www.sqlite.org/c3ref/step.html) in a loop. It returns:

- SQLITE_ROW: if the data for the next row is available, or
- SQLITE_DONE: if there are no more rows, or
- any error code.

If a query does not return any rows, the very first step returns SQLITE_DONE.

To read the data from the current row, call the [sqlite3_column_xxx()](http://www.sqlite.org/c3ref/column_blob.html) functions:

```sql
const char *sql = "SELECT ID, Name FROM MyTable";
sqlite3_stmt *stmt;
int err;

err = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
if (err != SQLITE_OK) {
    printf("prepare failed: %s\n", sqlite3_errmsg(db));
    return /* failure */;
}

for (;;) {
    err = sqlite3_step(stmt);
    if (err != SQLITE_ROW)
        break;

    int         id   = sqlite3_column_int (stmt, 0);
    const char *name = sqlite3_column_text(stmt, 1);
    if (name == NULL)
        name = "(NULL)";
    printf("ID: %d, Name: %s\n", id, name);
}

if (err != SQLITE_DONE) {
    printf("execution failed: %s\n", sqlite3_errmsg(db));
    sqlite3_finalize(stmt);
    return /* failure */;
}

sqlite3_finalize(stmt);
return /* success */;

```



## Executing a prepared statement multiple times


After a statement was [executed](http://stackoverflow.com/documentation/sqlite/5456/sqlite3-stmt-prepared-statement-c-api/19405/executing-a-statement), a call to [sqlite3_reset()](http://www.sqlite.org/c3ref/reset.html) brings it back into the original state so that it can be re-executed.

Typically, while the statement itself stays the same, the parameters are changed:

```sql
const char *sql = "INSERT INTO MyTable(ID, Name) VALUES (?, ?)";
sqlite3_stmt *stmt;
int err;

err = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
if (err != SQLITE_OK) {
    printf("prepare failed: %s\n", sqlite3_errmsg(db));
    return /* failure */;
}

for (...) {
    sqlite3_bind_int (stmt, 1, ...);   /* ID */
    sqlite3_bind_text(stmt, 2, ...);   /* name */

    err = sqlite3_step(stmt);
    if (err != SQLITE_DONE) {
        printf("execution failed: %s\n", sqlite3_errmsg(db));
        sqlite3_finalize(stmt);
        return /* failure */;
    }

    sqlite3_reset(stmt);
}

sqlite3_finalize(stmt);
return /* success */;

```



#### Remarks


official documentation: [Prepared Statement Object](http://www.sqlite.org/c3ref/stmt.html)

