---
metaTitle: "MySQL - Transaction"
description: "Start Transaction, COMMIT , ROLLBACK and AUTOCOMMIT, Transaction using JDBC Driver"
---

# Transaction



## Start Transaction


A transaction is a sequential group of SQL statements such as select,insert,update or delete, which is performed as one single work unit.

In other words, a transaction will never be complete unless each individual operation within the group is successful. If any operation within the transaction fails, the entire transaction will fail.

Bank transaction will be best example for explaining this. Consider a transfer between two accounts. To achieve this you have to write SQL statements that do the following

1. Check the availability of requested amount in the first account
1. Deduct requested amount from first account
1. Deposit it in second account

If anyone these process fails, the whole should be reverted to their previous state.

****ACID : Properties of Transactions****

Transactions have the following four standard properties

<li>**Atomicity:** ensures that all operations within the work unit are
completed successfully; otherwise, the transaction is aborted at the
point of failure, and previous operations are rolled back to their
former state.</li>
<li>**Consistency:** ensures that the database properly changes states upon a
successfully committed transaction.</li>
<li>**Isolation:** enables transactions to operate independently of and
transparent to each other.</li>
<li>**Durability:** ensures that the result or effect of a committed
transaction persists in case of a system failure.</li>

Transactions begin with the statement `START TRANSACTION` or `BEGIN WORK` and end with either a `COMMIT` or a `ROLLBACK` statement. The SQL commands between the beginning and ending statements form the bulk of the transaction.

```sql
START TRANSACTION;
SET @transAmt = '500';
SELECT @availableAmt:=ledgerAmt FROM accTable WHERE customerId=1 FOR UPDATE;
UPDATE accTable SET ledgerAmt=ledgerAmt-@transAmt WHERE customerId=1;
UPDATE accTable SET ledgerAmt=ledgerAmt+@transAmt WHERE customerId=2;
COMMIT;

```

With `START TRANSACTION`, autocommit remains disabled until you end the transaction with `COMMIT` or `ROLLBACK`. The autocommit mode then reverts to its previous state.

The `FOR UPDATE` indicates (and locks) the row(s) for the duration of the transaction.

While the transaction remains uncommitted, this transaction will not be available for others users.

**General Procedures involved in Transaction**

- Begin transaction by issuing SQL command `BEGIN WORK` or `START TRANSACTION`.
- Run all your SQL statements.
- Check whether everything is executed according to your requirement.
<li>If yes, then issue `COMMIT` command, otherwise issue a `ROLLBACK`
command to revert everything to the previous state.</li>
- Check for errors even after `COMMIT` if you are using, or might eventually use, Galera/PXC.



## COMMIT , ROLLBACK and AUTOCOMMIT


**AUTOCOMMIT**

MySQL automatically commits statements that are not part of a transaction. The results of any `UPDATE`,`DELETE` or `INSERT` statement not preceded with a `BEGIN` or `START TRANSACTION` will immediately be visible to all connections.

The `AUTOCOMMIT` variable is set **true** by default. This can be changed in the following way,

```sql
--->To make autcommit false
SET AUTOCOMMIT=false;
--or
SET AUTOCOMMIT=0;

--->To make autcommit true
SET AUTOCOMMIT=true;
--or
SET AUTOCOMMIT=1;

```

To view `AUTOCOMMIT` status

```sql
SELECT @@autocommit;

```

**COMMIT**

If `AUTOCOMMIT` set to false and the transaction not committed, the changes will be visible only for the current connection.

After `COMMIT` statement commits the changes to the table, the result will be visible for all connections.

We consider two connections to explain this

**Connection 1**

```sql
--->Before making autocommit false one row added in a new table
mysql> INSERT INTO testTable VALUES (1);

--->Making autocommit = false
mysql> SET autocommit=0;

mysql> INSERT INTO testTable VALUES (2), (3);    
mysql> SELECT * FROM testTable;
+-----+
| tId |
+-----+
|   1 |
|   2 |
|   3 |
+-----+

```

**Connection 2**

```sql
mysql> SELECT * FROM testTable;
+-----+
| tId |
+-----+
|   1 |
+-----+
---> Row inserted before autocommit=false only visible here

```

**Connection 1**

```sql
mysql> COMMIT;
--->Now COMMIT is executed in connection 1
mysql> SELECT * FROM testTable;
    +-----+
    | tId |
    +-----+
    |   1 |
    |   2 |
    |   3 |
    +-----+

```

**Connection 2**

```sql
mysql> SELECT * FROM testTable;
    +-----+
    | tId |
    +-----+
    |   1 |
    |   2 |
    |   3 |
    +-----+
--->Now all the three rows are visible here

```

**ROLLBACK**

If anything went wrong in your query execution, `ROLLBACK` in used to revert the changes. See the explanation below

```sql
--->Before making autocommit false one row added in a new table
mysql> INSERT INTO testTable VALUES (1);

--->Making autocommit = false
mysql> SET autocommit=0;

mysql> INSERT INTO testTable VALUES (2), (3);    
mysql> SELECT * FROM testTable;
+-----+
| tId |
+-----+
|   1 |
|   2 |
|   3 |
+-----+

```

Now we are executing `ROLLBACK`

```sql
--->Rollback executed now
mysql> ROLLBACk;

mysql> SELECT * FROM testTable;
+-----+
| tId |
+-----+
|   1 |
+-----+
--->Rollback removed all rows which all are not committed

```

Once `COMMIT` is executed, then `ROLLBACK` will not cause anything

```sql
mysql> INSERT INTO testTable VALUES (2), (3);    
mysql> SELECT * FROM testTable;
mysql> COMMIT;
+-----+
| tId |
+-----+
|   1 |
|   2 |
|   3 |
+-----+

--->Rollback executed now
mysql> ROLLBACk;

mysql> SELECT * FROM testTable;
+-----+
| tId |
+-----+
|   1 |
|   2 |
|   3 |
+-----+
--->Rollback not removed any rows

```

If `AUTOCOMMIT` is set **true**, then `COMMIT` and `ROLLBACK` is useless



## Transaction using JDBC Driver


Transaction using JDBC driver is used to control how and when a transaction should commit and rollback. Connection to MySQL server is created using JDBC driver

[JDBC driver for MySQL](https://dev.mysql.com/downloads/connector/j/5.0.html) can be downloaded here

Lets start with getting a connection to database using JDBC driver

```sql
Class.forName("com.mysql.jdbc.Driver");  
Connection con = DriverManager.getConnection(DB_CONNECTION_URL,DB_USER,USER_PASSWORD);
--->Example for connection url "jdbc:mysql://localhost:3306/testDB");

```

**Character Sets** : This indicates what character set the client will use to send SQL statements to the server. It also specifies the character set that the server should use for sending results back to the client.

This should be mentioned while creating connection to server. So the connection string should be like,

```sql
jdbc:mysql://localhost:3306/testDB?useUnicode=true&amp;characterEncoding=utf8

```

See this for more details about [Character Sets and Collations](http://stackoverflow.com/documentation/mysql/4569/character-sets-and-collations#t=201609081237366802241)

When you open connection, the `AUTOCOMMIT` mode is set to **true** by default, that should be changed **false** to start transaction.

```sql
con.setAutoCommit(false);

```

You should always call `setAutoCommit()` method right after you open a connection.

Otherwise use `START TRANSACTION` or `BEGIN WORK` to start a new transaction.
By using `START TRANSACTION` or `BEGIN WORK`, no need to change `AUTOCOMMIT` **false**. That will be automatically disabled.

Now you can start transaction. See a complete JDBC transaction example below.

```sql
package jdbcTest;
 
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;


public class accTrans {

    public static void doTransfer(double transAmount,int customerIdFrom,int customerIdTo) {

        Connection con = null;
        PreparedStatement pstmt = null;
        ResultSet rs = null;
 
        try {
            String DB_CONNECTION_URL = "jdbc:mysql://localhost:3306/testDB?useUnicode=true&amp;characterEncoding=utf8";

            Class.forName("com.mysql.jdbc.Driver");  
            con = DriverManager.getConnection(DB_CONNECTION_URL,DB_USER,USER_PASSWORD);

            --->set auto commit to false
            con.setAutoCommit(false);
            ---> or use con.START TRANSACTION / con.BEGIN WORK

            --->Start SQL Statements for transaction
            --->Checking availability of amount
            double availableAmt    = 0;
            pstmt = con.prepareStatement("SELECT ledgerAmt FROM accTable WHERE customerId=? FOR UPDATE");
            pstmt.setInt(1, customerIdFrom);
            rs = pstmt.executeQuery();
            if(rs.next())
                availableAmt    = rs.getDouble(1);

            if(availableAmt >= transAmount)
            {
                ---> Do Transfer
                ---> taking amount from cutomerIdFrom
                pstmt = con.prepareStatement("UPDATE accTable SET ledgerAmt=ledgerAmt-? WHERE customerId=?");                        
                pstmt.setDouble(1, transAmount);
                pstmt.setInt(2, customerIdFrom);
                pstmt.executeUpdate();

                ---> depositing amount in cutomerIdTo
                pstmt = con.prepareStatement("UPDATE accTable SET ledgerAmt=ledgerAmt+? WHERE customerId=?");                        
                pstmt.setDouble(1, transAmount);
                pstmt.setInt(2, customerIdTo);
                pstmt.executeUpdate();

                con.commit();
            }
            --->If you performed any insert,update or delete operations before 
            ----> this availability check, then include this else part
            /*else { --->Rollback the transaction if availability is less than required
                con.rollback();
            }*/

        } catch (SQLException ex) {
            ---> Rollback the transaction in case of any error
            con.rollback();
        } finally {
            try {
                if(rs != null)  rs.close();
                if(pstmt != null) pstmt.close();
                if(con != null) con.close();
            }
        }
    }
 
    public static void main(String[] args) {
        doTransfer(500, 1020, 1021);
        -->doTransfer(transAmount, customerIdFrom, customerIdTo);
    }
}

```

JDBC transaction make sure of all SQL statements within a transaction block are executed successful, if either one of the SQL statement within transaction block is failed, abort and rollback everything within the transaction block.

