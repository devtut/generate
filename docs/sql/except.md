---
metaTitle: "EXCEPT"
description: "Select dataset except where values are in this other dataset"
---

# EXCEPT



## Select dataset except where values are in this other dataset


```sql
--dataset schemas must be identical
SELECT 'Data1' as 'Column' UNION ALL
SELECT 'Data2' as 'Column' UNION ALL
SELECT 'Data3' as 'Column' UNION ALL
SELECT 'Data4' as 'Column' UNION ALL
SELECT 'Data5' as 'Column'
EXCEPT
SELECT 'Data3' as 'Column'
--Returns Data1, Data2, Data4, and Data5

```



#### Remarks


`EXCEPT` returns any distinct values from the dataset to the left of the EXCEPT operator that are not also returned from the right dataset.

