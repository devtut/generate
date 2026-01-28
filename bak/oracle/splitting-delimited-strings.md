---
metaTitle: "Oracle Database - Splitting Delimited Strings"
description: "Splitting Strings using a Hierarchical Query, Splitting Strings using a PL/SQL Function, Splitting Strings using a Recursive Sub-query Factoring Clause, Splitting Strings using a Correlated Table Expression, Splitting Strings using XMLTable and FLWOR expressions, Splitting Strings using CROSS APPLY (Oracle 12c), Splitting Delimited Strings using XMLTable"
---

# Splitting Delimited Strings




## Splitting Strings using a Hierarchical Query


**Sample Data**:

```sql
CREATE TABLE table_name ( id, list ) AS
SELECT 1, 'a,b,c,d' FROM DUAL UNION ALL -- Multiple items in the list
SELECT 2, 'e'       FROM DUAL UNION ALL -- Single item in the list
SELECT 3, NULL      FROM DUAL UNION ALL -- NULL list
SELECT 4, 'f,,g'    FROM DUAL;          -- NULL item in the list

```

**Query**:

```sql
SELECT t.id,
       REGEXP_SUBSTR( list, '([^,]*)(,|$)', 1, LEVEL, NULL, 1 ) AS value,
       LEVEL AS lvl
FROM   table_name t
CONNECT BY
       id = PRIOR id
AND    PRIOR SYS_GUID() IS NOT NULL
AND    LEVEL < REGEXP_COUNT( list, '([^,]*)(,|$)' )

```

**Output**:

```

       ID ITEM           LVL
---------- ------- ----------
         1 a                1 
         1 b                2 
         1 c                3 
         1 d                4 
         2 e                1 
         3 (NULL)           1 
         4 f                1 
         4 (NULL)           2 
         4 g                3 

```



## Splitting Strings using a PL/SQL Function


**PL/SQL Function**:

```sql
CREATE OR REPLACE FUNCTION split_String(
  i_str    IN  VARCHAR2,
  i_delim  IN  VARCHAR2 DEFAULT ','
) RETURN SYS.ODCIVARCHAR2LIST DETERMINISTIC
AS
  p_result       SYS.ODCIVARCHAR2LIST := SYS.ODCIVARCHAR2LIST();
  p_start        NUMBER(5) := 1;
  p_end          NUMBER(5);
  c_len CONSTANT NUMBER(5) := LENGTH( i_str );
  c_ld  CONSTANT NUMBER(5) := LENGTH( i_delim );
BEGIN
  IF c_len > 0 THEN
    p_end := INSTR( i_str, i_delim, p_start );
    WHILE p_end > 0 LOOP
      p_result.EXTEND;
      p_result( p_result.COUNT ) := SUBSTR( i_str, p_start, p_end - p_start );
      p_start := p_end + c_ld;
      p_end := INSTR( i_str, i_delim, p_start );
    END LOOP;
    IF p_start <= c_len + 1 THEN
      p_result.EXTEND;
      p_result( p_result.COUNT ) := SUBSTR( i_str, p_start, c_len - p_start + 1 );
    END IF;
  END IF;
  RETURN p_result;
END;
/

```

**Sample Data**:

```sql
CREATE TABLE table_name ( id, list ) AS
SELECT 1, 'a,b,c,d' FROM DUAL UNION ALL -- Multiple items in the list
SELECT 2, 'e'       FROM DUAL UNION ALL -- Single item in the list
SELECT 3, NULL      FROM DUAL UNION ALL -- NULL list
SELECT 4, 'f,,g'    FROM DUAL;          -- NULL item in the list

```

**Query**:

```sql
SELECT t.id,
       v.column_value AS value,
       ROW_NUMBER() OVER ( PARTITION BY id ORDER BY ROWNUM ) AS lvl
FROM   table_name t,
       TABLE( split_String( t.list ) ) (+) v

```

**Output**:

```

       ID ITEM           LVL
---------- ------- ----------
         1 a                1 
         1 b                2 
         1 c                3 
         1 d                4 
         2 e                1 
         3 (NULL)           1 
         4 f                1 
         4 (NULL)           2 
         4 g                3 

```



## Splitting Strings using a Recursive Sub-query Factoring Clause


**Sample Data**:

```sql
CREATE TABLE table_name ( id, list ) AS
SELECT 1, 'a,b,c,d' FROM DUAL UNION ALL -- Multiple items in the list
SELECT 2, 'e'       FROM DUAL UNION ALL -- Single item in the list
SELECT 3, NULL      FROM DUAL UNION ALL -- NULL list
SELECT 4, 'f,,g'    FROM DUAL;          -- NULL item in the list

```

**Query**:

```sql
WITH bounds ( id, list, start_pos, end_pos, lvl ) AS (
  SELECT id, list, 1, INSTR( list, ',' ), 1 FROM table_name
UNION ALL
  SELECT id,
         list,
         end_pos + 1,
         INSTR( list, ',', end_pos + 1 ),
         lvl + 1
  FROM   bounds
  WHERE  end_pos > 0
)
SELECT id,
       SUBSTR(
         list,
         start_pos,
         CASE end_pos
           WHEN 0
           THEN LENGTH( list ) + 1
           ELSE end_pos
         END - start_pos
       ) AS item,
      lvl
FROM   bounds
ORDER BY id, lvl;

```

**Output**:

```

       ID ITEM           LVL
---------- ------- ----------
         1 a                1 
         1 b                2 
         1 c                3 
         1 d                4 
         2 e                1 
         3 (NULL)           1 
         4 f                1 
         4 (NULL)           2 
         4 g                3 

```



## Splitting Strings using a Correlated Table Expression


**Sample Data**:

```sql
CREATE TABLE table_name ( id, list ) AS
SELECT 1, 'a,b,c,d' FROM DUAL UNION ALL -- Multiple items in the list
SELECT 2, 'e'       FROM DUAL UNION ALL -- Single item in the list
SELECT 3, NULL      FROM DUAL UNION ALL -- NULL list
SELECT 4, 'f,,g'    FROM DUAL;          -- NULL item in the list

```

**Query**:

```sql
SELECT t.id,
       v.COLUMN_VALUE AS value,
       ROW_NUMBER() OVER ( PARTITION BY id ORDER BY ROWNUM ) AS lvl
FROM   table_name t,
       TABLE(
         CAST(
           MULTISET(
             SELECT REGEXP_SUBSTR( t.list, '([^,]*)(,|$)', 1, LEVEL, NULL, 1 )
             FROM   DUAL
             CONNECT BY LEVEL < REGEXP_COUNT( t.list, '[^,]*(,|$)' )
           )
           AS SYS.ODCIVARCHAR2LIST
         )
       ) v;

```

**Output**:

```

       ID ITEM           LVL
---------- ------- ----------
         1 a                1 
         1 b                2 
         1 c                3 
         1 d                4 
         2 e                1 
         3 (NULL)           1 
         4 f                1 
         4 (NULL)           2 
         4 g                3 

```



## Splitting Strings using XMLTable and FLWOR expressions


This solution uses the [`ora:tokenize` XQuery function](http://docs.oracle.com/database/121/ADXDB/xdb_xquery.htm#ADXDB-GUID-57984592-3A02-4DB4-9FDA-D5E2CCB3A797) that is available from Oracle 11.

**Sample Data**:

```sql
CREATE TABLE table_name ( id, list ) AS
SELECT 1, 'a,b,c,d' FROM DUAL UNION ALL -- Multiple items in the list
SELECT 2, 'e'       FROM DUAL UNION ALL -- Single item in the list
SELECT 3, NULL      FROM DUAL UNION ALL -- NULL list
SELECT 4, 'f,,g'    FROM DUAL;          -- NULL item in the list

```

**Query**:

```sql
SELECT t.id,
       x.item,
       x.lvl
FROM   table_name t,
       XMLTABLE(
         'let $list := ora:tokenize(.,","),
              $cnt := count($list)
          for $val at $r in $list 
          where $r < $cnt
          return $val'
       PASSING list||',' 
       COLUMNS
         item VARCHAR2(100) PATH '.',
         lvl FOR ORDINALITY
       ) (+) x;

```

**Output**:

```

       ID ITEM           LVL
---------- ------- ----------
         1 a                1 
         1 b                2 
         1 c                3 
         1 d                4 
         2 e                1 
         3 (NULL)      (NULL) 
         4 f                1 
         4 (NULL)           2 
         4 g                3 

```



## Splitting Strings using CROSS APPLY (Oracle 12c)


**Sample Data**:

```sql
CREATE TABLE table_name ( id, list ) AS
SELECT 1, 'a,b,c,d' FROM DUAL UNION ALL -- Multiple items in the list
SELECT 2, 'e'       FROM DUAL UNION ALL -- Single item in the list
SELECT 3, NULL      FROM DUAL UNION ALL -- NULL list
SELECT 4, 'f,,g'    FROM DUAL;          -- NULL item in the list

```

**Query**:

```sql
SELECT t.id,
       REGEXP_SUBSTR( t.list, '([^,]*)($|,)', 1, l.lvl, NULL, 1 ) AS item,
       l.lvl
FROM   table_name t
       CROSS APPLY
       (
         SELECT LEVEL AS lvl
         FROM   DUAL
         CONNECT BY LEVEL <= REGEXP_COUNT( t.list, ',' ) + 1
       ) l;

```

**Output**:

```

       ID ITEM           LVL
---------- ------- ----------
         1 a                1 
         1 b                2 
         1 c                3 
         1 d                4 
         2 e                1 
         3 (NULL)           1 
         4 f                1 
         4 (NULL)           2 
         4 g                3 

```



## Splitting Delimited Strings using XMLTable


**Sample Data**:

```sql
CREATE TABLE table_name ( id, list ) AS
SELECT 1, 'a,b,c,d' FROM DUAL UNION ALL -- Multiple items in the list
SELECT 2, 'e'       FROM DUAL UNION ALL -- Single item in the list
SELECT 3, NULL      FROM DUAL UNION ALL -- NULL list
SELECT 4, 'f,,g'    FROM DUAL;          -- NULL item in the list

```

**Query**:

```sql
SELECT t.id,
       SUBSTR( x.item.getStringVal(), 2 ) AS item,
       x.lvl
FROM   table_name t
       CROSS JOIN
       XMLTABLE(
         ( '"#' || REPLACE( t.list, ',', '","#' ) || '"' )
         COLUMNS item XMLTYPE PATH '.',
                 lvl  FOR ORDINALITY
       ) x;

```

**(Note: the `#` character is appended to facilitate extracting `NULL` values; it is later removed using `SUBSTR( item, 2 )`. If `NULL` values are not required then you can simplify the query and omit this.)**

**Output**:

```

       ID ITEM           LVL
---------- ------- ----------
         1 a                1 
         1 b                2 
         1 c                3 
         1 d                4 
         2 e                1 
         3 (NULL)           1 
         4 f                1 
         4 (NULL)           2 
         4 g                3 

```

