---
metaTitle: "Character Sets and Collations"
description: "Which CHARACTER SET and COLLATION?, Declaration, Connection, Setting character sets on tables and fields"
---

# Character Sets and Collations



## Which CHARACTER SET and COLLATION?


There are dozens of character sets with hundreds of collations.  (A given collation belongs to only one character set.)  See the output of `SHOW COLLATION;`.

There are usually only 4 `CHARACTER SETs` that matter:

```sql
ascii -- basic 7-bit codes.
latin1 -- ascii, plus most characters needed for Western European languages.
utf8 -- the 1-, 2-, and 3-byte subset of utf8.  This excludes Emoji and some of Chinese.
utf8mb4 -- the full set of UTF8 characters, covering all current languages.

```

All include English characters, encoded identically.  utf8 is a subset of utf8mb4.

Best practice...

- Use utf8mb4 for any `TEXT` or `VARCHAR` column that can have a variety of languages in it.
- Use ascii (latin1 is ok) for hex strings (UUID, MD5, etc) and simple codes (country_code, postal_code, etc).

utf8mb4 did not exist until version 5.5.3, so utf8 was the best available before that.

**Outside of MySQL**, "UTF8" means the same things as MySQL's utf8mb4, not MySQL's utf8.

Collations start with the charset name and usually end with `_ci` for "case and accent insensitive" or `_bin` for "simply compare the bits.

The 'latest' utf8mb4 collation is `utf8mb4_unicode_520_ci`, based on Unicode 5.20.  If you are working with a single language, you might want, say, `utf8mb4_polish_ci`, which will rearrange the letters slightly, based on Polish conventions.



## Declaration


```sql
CREATE TABLE foo ( ...
    name CHARACTER SET utf8mb4
    ... );

```



## Connection


Vital to using character sets is to tell the MySQL-server what encoding the client's bytes are.  Here is one way:

```sql
SET NAMES utf8mb4;

```

Each language (PHP, Python, Java, ...) has its own way the it usually preferable to `SET NAMES`.

For example:  `SET NAMES utf8mb4`, together with a column declared `CHARACTER SET latin1` -- this will convert from latin1 to utf8mb4 when `INSERTing` and convert back when `SELECTing`.



## Setting character sets on tables and fields


You can set a [character set](http://dev.mysql.com/doc/refman/5.7/en/charset-general.html) both per table, as well as per individual field using the `CHARACTER SET` and `CHARSET` statements:

```sql
CREATE TABLE Address (
    `AddressID`   INTEGER NOT NULL PRIMARY KEY,
    `Street`      VARCHAR(80) CHARACTER SET ASCII,
    `City`        VARCHAR(80),
    `Country`     VARCHAR(80) DEFAULT "United States",
    `Active`      BOOLEAN DEFAULT 1,
) Engine=InnoDB default charset=UTF8;

```

`City` and `Country` will use `UTF8`, as we set that as the default character set for the table. `Street` on the other hand will use `ASCII`, as we've specifically told it to do so.

Setting the right character set is highly dependent on your dataset, but can also highly improve portability between systems working with your data.

