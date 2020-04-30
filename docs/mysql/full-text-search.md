---
metaTitle: "Full-Text search"
description: "Simple FULLTEXT search, Simple BOOLEAN search, Multi-column FULLTEXT search"
---

# Full-Text search


MySQL offers FULLTEXT searching. It searches tables with columns containing text for the best matches for words and phrases.



## Simple FULLTEXT search


```

   SET @searchTerm= 'Database Programming';
 SELECT MATCH (Title) AGAINST (@searchTerm IN NATURAL LANGUAGE MODE) Score,
        ISBN, Author, Title 
   FROM book
  WHERE MATCH (Title) AGAINST (@searchTerm IN NATURAL LANGUAGE MODE)
  ORDER BY MATCH (Title) AGAINST (@searchTerm IN NATURAL LANGUAGE MODE) DESC;

```

Given a table named `book` with columns named `ISBN`, 'Title', and 'Author', this finds books matching the terms `'Database Programming'`.  It shows the best matches first.

For this to work, a fulltext index on the `Title` column must be available:

```sql
ALTER TABLE book ADD FULLTEXT INDEX Fulltext_title_index (Title);

```



## Simple BOOLEAN search


```

   SET @searchTerm= 'Database Programming -Java';
 SELECT MATCH (Title) AGAINST (@searchTerm IN BOOLEAN MODE) Score,
        ISBN, Author, Title 
   FROM book
  WHERE MATCH (Title) AGAINST (@searchTerm IN BOOLEAN MODE)
  ORDER BY MATCH (Title) AGAINST (@searchTerm IN BOOLEAN MODE) DESC;

```

Given a table named `book` with columns named `ISBN`, `Title`, and `Author`,  this searches for books with the words `'Database'` and `'Programming'` in the title, but not the word `'Java'`.

For this to work, a fulltext index on the Title column must be available:

```sql
ALTER TABLE book ADD FULLTEXT INDEX Fulltext_title_index (Title);

```



## Multi-column FULLTEXT search


```

   SET @searchTerm= 'Date Database Programming';
 SELECT MATCH (Title, Author) AGAINST (@searchTerm IN NATURAL LANGUAGE MODE) Score,
        ISBN, Author, Title 
   FROM book
  WHERE MATCH (Title, Author) AGAINST (@searchTerm IN NATURAL LANGUAGE MODE)
  ORDER BY MATCH (Title, Author) AGAINST (@searchTerm IN NATURAL LANGUAGE MODE) DESC;

```

Given a table named book with columns named `ISBN`, `Title`, and `Author`, this finds books matching the terms 'Date Database Programming'. It shows the best matches first. The best matches include books written by Prof. C. J. Date.

(But, one of the best matches is also **The Date Doctor's Guide to Dating : How to Get from First Date to Perfect Mate**.  This shows up a limitation of FULLTEXT search: it doesn't pretend to understand such things as parts of speech or the meaning of the indexed words.)

For this to work, a fulltext index on the Title and Author columns must be available:

```sql
ALTER TABLE book ADD FULLTEXT INDEX Fulltext_title_author_index (Title, Author);

```



#### Remarks


`FULLTEXT` searching works strangely on tables containing small numbers of rows. So, when you're experimenting with it, you may find it helpful to obtain a medium-sized table online. Here's a [table of book items](http://www.plumislandmedia.net/wp-content/uploads/2017/01/book.zip), with titles and authors.  You can download it, unzip it, and load it into MySQL.

`FULLTEXT` search is intended for use with human assistance. It's designed to yield more matches than an ordinary `WHERE column LIKE 'text%'` filtering operation.

`FULLTEXT` search is available for `MyISAM` tables. It is also available for `InnoDB` tables in MySQL version 5.6.4 or later.

