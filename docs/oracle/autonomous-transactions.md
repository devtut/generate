---
metaTitle: "Oracle Database - Autonomous Transactions"
description: "Using autonomous transaction for logging errors"
---

# Autonomous Transactions



## Using autonomous transaction for logging errors


The following procedure is a generic one which will be used to log all errors in an application to a common error log table.

```sql
CREATE OR REPLACE PROCEDURE log_errors
(
  p_calling_program IN VARCHAR2,
  p_error_code IN INTEGER,
  p_error_description IN VARCHAR2
)
IS
  PRAGMA AUTONOMOUS_TRANSACTION;
BEGIN
  INSERT INTO error_log
  VALUES
  (
  p_calling_program,
  p_error_code,
  p_error_description,
  SYSDATE,
  USER
  );
  COMMIT;
END log_errors;

```

The following anonymous PLSQL block shows how to call the log_errors procedure.

```sql
BEGIN
   DELETE FROM dept WHERE deptno = 10;
EXCEPTION
   WHEN OTHERS THEN
      log_errors('Delete dept',sqlcode, sqlerrm);
      RAISE;
END;

SELECT * FROM error_log;

CALLING_PROGRAM    ERROR_CODE    ERROR_DESCRIPTION                                                ERROR_DATETIME         DB_USER
Delete dept        -2292         ORA-02292: integrity constraint violated - child record found    08/09/2016             APEX_PUBLIC_USER

```



#### Remarks


Typical use cases for autonomous transaction are.

1. For building any kind of logging framework like the error logging framework explained in the above example.
1. For auditing DML operations in triggers on tables irrespective of the final status of the transaction (COMMIT or ROLLBACK).

