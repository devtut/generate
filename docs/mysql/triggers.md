---
metaTitle: "MySQL - TRIGGERS"
description: "Basic Trigger, Types of triggers"
---

# TRIGGERS



## Basic Trigger


**Create Table**

```sql
mysql> CREATE TABLE account (acct_num INT, amount DECIMAL(10,2));
Query OK, 0 rows affected (0.03 sec)

```

**Create Trigger**

```sql
mysql> CREATE TRIGGER ins_sum BEFORE INSERT ON account
    -> FOR EACH ROW SET @sum = @sum + NEW.amount;
Query OK, 0 rows affected (0.06 sec)

```

The CREATE TRIGGER statement creates a trigger named ins_sum that is associated with the account table. It also includes clauses that specify the trigger action time, the triggering event, and what to do when the trigger activates

**Insert Value**

To use the trigger, set the accumulator variable (@sum) to zero, execute an INSERT statement, and then see what value the variable has afterward:

```sql
mysql> SET @sum = 0;
mysql> INSERT INTO account VALUES(137,14.98),(141,1937.50),(97,-100.00);
mysql> SELECT @sum AS 'Total amount inserted';
+-----------------------+
| Total amount inserted |
+-----------------------+
| 1852.48               |
+-----------------------+

```

In this case, the value of @sum after the INSERT statement has executed is 14.98 + 1937.50 - 100, or 1852.48.

**Drop Trigger**

```sql
mysql> DROP TRIGGER test.ins_sum;

```

If you drop a table, any triggers for the table are also dropped.



## Types of triggers


### Timing

There are two trigger action time modifiers :

- `BEFORE` trigger activates before executing the request,
- `AFTER` trigger fire after change.

### Triggering event

There are three events that triggers can be attached to:

- `INSERT`
- `UPDATE`
- `DELETE`

### Before Insert trigger example

```sql
DELIMITER $$

CREATE TRIGGER insert_date 
    BEFORE INSERT ON stack
    FOR EACH ROW
BEGIN
        -- set the insert_date field in the request before the insert
    SET NEW.insert_date = NOW();
END;

$$
DELIMITER ;

```

### Before Update trigger example

```sql
DELIMITER $$

CREATE TRIGGER update_date 
    BEFORE UPDATE ON stack
    FOR EACH ROW
BEGIN
        -- set the update_date field in the request before the update
    SET NEW.update_date = NOW();
END;

$$
DELIMITER ;

```

### After Delete trigger example

```sql
DELIMITER $$

CREATE TRIGGER deletion_date 
    AFTER DELETE ON stack
    FOR EACH ROW
BEGIN
        -- add a log entry after a successful delete
    INSERT INTO log_action(stack_id, deleted_date) VALUES(OLD.id, NOW());
END;

$$
DELIMITER ;

```



#### Syntax


<li>CREATE
[DEFINER = { user | CURRENT_USER }]
TRIGGER trigger_name
trigger_time trigger_event
ON tbl_name FOR EACH ROW
[trigger_order]
trigger_body</li>
- trigger_time: { BEFORE | AFTER }
- trigger_event: { INSERT | UPDATE | DELETE }
- trigger_order: { FOLLOWS | PRECEDES } other_trigger_name



#### Remarks


Two points need to draw your attention if you already use triggers on others DB :

### FOR EACH ROW

**`FOR EACH ROW` is a mandatory part of the syntax**

You can't make a **statement** trigger (once by query) like Oracle do. It's more a performance related issue than a real missing feature

### CREATE OR REPLACE TRIGGER

**The `CREATE OR REPLACE` is not supported by MySQL**

MySQL don't allow this syntax, you have instead to use the following :

```sql
DELIMITER $$

DROP TRIGGER IF EXISTS myTrigger;
$$
CREATE TRIGGER myTrigger
-- ...

$$
DELIMITER ;

```

Be careful, this is **not an atomic transaction** :

- you'll loose the old trigger if the `CREATE` fail
- on a heavy load, others operations can occurs between the `DROP` and the `CREATE`, use a `LOCK TABLES myTable WRITE;` first to avoid data inconsistency and `UNLOCK TABLES;` after the `CREATE` to release the table

