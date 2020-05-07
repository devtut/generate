---
metaTitle: "Oracle Database - Collections and Records"
description: "Use a collection as a return type for a split function"
---

# Collections and Records




## Use a collection as a return type for a split function


It's necessary to declare the type; here `t_my_list`; a collection is a `TABLE OF` **`something`**

```sql
CREATE OR REPLACE TYPE t_my_list AS TABLE OF VARCHAR2(100);

```

Here's the function. Notice the `()` used as a kind of constructor, and the `COUNT` and `EXTEND` keywords that help you create and grow your collection;

```sql
CREATE OR REPLACE
FUNCTION cto_table(p_sep in Varchar2, p_list IN VARCHAR2)
  RETURN t_my_list
AS
--- this function takes a string list, element being separated by p_sep
--                                                         as separator
  l_string VARCHAR2(4000) := p_list || p_sep;
  l_sep_index PLS_INTEGER;
  l_index PLS_INTEGER := 1;
  l_tab t_my_list     := t_my_list();
BEGIN
  LOOP
    l_sep_index := INSTR(l_string, p_sep, l_index);
    EXIT
  WHEN l_sep_index = 0;
    l_tab.EXTEND;
    l_tab(l_tab.COUNT) := TRIM(SUBSTR(l_string,l_index,l_sep_index - l_index));
    l_index            := l_sep_index + 1;
  END LOOP;
  RETURN l_tab;
END cto_table;
/

```

Then you can see the content of the collection with the `TABLE()` function from SQL; it can be used as a list inside a SQL `IN ( ..)` statement:

```sql
select * from A_TABLE 
 where A_COLUMN in ( TABLE(cto_table('|','a|b|c|d')) )
--- gives the records where A_COLUMN in ('a', 'b', 'c', 'd') --

```

