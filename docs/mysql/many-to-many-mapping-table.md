---
metaTitle: "MySQL - Many-to-many Mapping table"
description: "Typical schema"
---

# Many-to-many Mapping table



## Typical schema


```sql
CREATE TABLE XtoY (
    # No surrogate id for this table
    x_id MEDIUMINT UNSIGNED NOT NULL,   -- For JOINing to one table
    y_id MEDIUMINT UNSIGNED NOT NULL,   -- For JOINing to the other table
    # Include other fields specific to the 'relation'
    PRIMARY KEY(x_id, y_id),            -- When starting with X
    INDEX      (y_id, x_id)             -- When starting with Y
) ENGINE=InnoDB;

```

(See Remarks, below, for rationale.)



#### Remarks


<li>Lack of an `AUTO_INCREMENT` id for this table -- The PK given is the
'natural' PK; there is no good reason for a surrogate.</li>
<li>`MEDIUMINT` --
This is a reminder that all `INTs` should be made as small as is safe
(smaller ⇒ faster). Of course the declaration here must match the
definition in the table being linked to.</li>
<li>`UNSIGNED` -- Nearly all
INTs may as well be declared non-negative</li>
<li>`NOT NULL` -- Well, that's
true, isn't it?</li>
<li>`InnoDB` -- More effecient than MyISAM because of the
way the `PRIMARY KEY` is clustered with the data in InnoDB.</li>
<li>`INDEX(y_id, x_id)` -- The `PRIMARY KEY` makes it efficient to go one
direction; the makes the other direction efficient. No need to say
`UNIQUE`; that would be extra effort on `INSERTs`.</li>
<li>In the secondary
index, saying just `INDEX(y_id)` would work because it would implicit
include `x_id`. But I would rather make it more obvious that I am
hoping for a 'covering' index.</li>

You **may** want to add more columns to the table; this is rare.  The extra columns could provide information about the **relationship** that the table represents.

You **may** want to add `FOREIGN KEY` constraints.

