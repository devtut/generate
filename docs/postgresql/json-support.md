---
metaTitle: "PostgreSQL - JSON Support"
description: "Using JSONb operators, Querying complex JSON documents, Creating a pure JSON table"
---

# JSON Support




## Using JSONb operators


### Creating a DB and a Table

```sql
DROP DATABASE IF EXISTS books_db;
CREATE DATABASE books_db WITH ENCODING='UTF8' TEMPLATE template0;

DROP TABLE IF EXISTS books;

CREATE TABLE books (
  id SERIAL PRIMARY KEY,
  client TEXT NOT NULL,
  data JSONb NOT NULL
);

```

### Populating the DB

```sql
INSERT INTO books(client, data) values (
    'Joe', 
    '{ "title": "Siddhartha", "author": { "first_name": "Herman", "last_name": "Hesse" } }'
),(
    'Jenny', 
    '{ "title": "Dharma Bums", "author": { "first_name": "Jack", "last_name": "Kerouac" } }'
),(
    'Jenny', 
    '{ "title": "100 años de soledad", "author": { "first_name": "Gabo", "last_name": "Marquéz" } }'
);

```

Lets see everything inside the table books:

```sql
SELECT * FROM books;

```

Output:

<img src="http://i.imgur.com/T26elII.png" alt="" />

### `->` operator returns values out of JSON columns

Selecting 1 column:

```sql
SELECT client, 
    data->'title' AS title
    FROM books;

```

Output:

<img src="http://i.imgur.com/Pab2puE.png" alt="enter image description here" />

Selecting 2 columns:

```sql
SELECT client, 
   data->'title' AS title, data->'author' AS author
   FROM books;

```

Output:

<img src="http://i.imgur.com/fWHUsre.png" alt="enter image description here" />

### `->` vs `->>`

The `->` operator returns the original JSON type (which might be an object), whereas `->>` returns text.

### Return NESTED objects

You can use the `->` to return a nested object and thus chain the operators:

```sql
SELECT client, 
   data->'author'->'last_name' AS author
   FROM books;

```

Output:

<img src="http://i.imgur.com/NgSPIFU.png" alt="enter image description here" />

### Filtering

Select rows based on a value inside your JSON:

```

SELECT 
 client,
 data->'title' AS title
 FROM books
  WHERE data->'title' = '"Dharma Bums"';

```

Notice WHERE uses `->` so we must compare to JSON `'"Dharma Bums"'`

Or we could use `->>` and compare to `'Dharma Bums'`

Output:

<img src="http://i.imgur.com/2seaUNK.png" alt="enter image description here" />

### Nested filtering

Find rows based on the value of a nested JSON object:

```sql
SELECT 
 client,
 data->'title' AS title
 FROM books
  WHERE data->'author'->>'last_name' = 'Kerouac';

```

Output:

<img src="http://i.imgur.com/yeBMj0T.png" alt="enter image description here" />

### A real world example

```sql
CREATE TABLE events (
  name varchar(200),
  visitor_id varchar(200),
  properties json,
  browser json
);

```

We’re going to store events in this table, like pageviews. Each event has properties, which could be anything (e.g. current page) and also sends information about the browser (like OS, screen resolution, etc). Both of these are completely free form and could change over time (as we think of extra stuff to track).

```sql
INSERT INTO events (name, visitor_id, properties, browser) VALUES
(
  'pageview', '1',
  '{ "page": "/" }',
  '{ "name": "Chrome", "os": "Mac", "resolution": { "x": 1440, "y": 900 } }'
),(
  'pageview', '2',
  '{ "page": "/" }',
  '{ "name": "Firefox", "os": "Windows", "resolution": { "x": 1920, "y": 1200 } }'
),(
  'pageview', '1',
  '{ "page": "/account" }',
  '{ "name": "Chrome", "os": "Mac", "resolution": { "x": 1440, "y": 900 } }'
),(
  'purchase', '5',
  '{ "amount": 10 }',
  '{ "name": "Firefox", "os": "Windows", "resolution": { "x": 1024, "y": 768 } }'
),(
  'purchase', '15',
  '{ "amount": 200 }',
  '{ "name": "Firefox", "os": "Windows", "resolution": { "x": 1280, "y": 800 } }'
),(
  'purchase', '15',
  '{ "amount": 500 }',
  '{ "name": "Firefox", "os": "Windows", "resolution": { "x": 1280, "y": 800 } }'
);

```

Now lets select everything:

```sql
SELECT * FROM events;

```

Output:

<img src="http://i.imgur.com/b5Hw0NN.png" alt="enter image description here" />

### JSON operators + PostgreSQL aggregate functions

Using the JSON operators, combined with traditional PostgreSQL aggregate functions, we can pull out whatever we want. You have the full might of an RDBMS at your disposal.

<li>
Lets see browser usage:

```sql
  SELECT browser->>'name' AS browser, 
    count(browser)
    FROM events
    GROUP BY browser->>'name';

```


</li>

Output:

<img src="http://i.imgur.com/jvw6bz7.png" alt="enter image description here" />

<li>
Total revenue per visitor:

```sql
  SELECT visitor_id, SUM(CAST(properties->>'amount' AS integer)) AS total
  FROM events
  WHERE CAST(properties->>'amount' AS integer) > 0
  GROUP BY visitor_id;

```


</li>

Output:

<img src="http://i.imgur.com/6cOnNl9.png" alt="enter image description here" />

<li>
Average screen resolution

```sql
  SELECT AVG(CAST(browser->'resolution'->>'x' AS integer)) AS width,
    AVG(CAST(browser->'resolution'->>'y' AS integer)) AS height
  FROM events;

```


</li>

Output:

<img src="http://i.imgur.com/RfVELht.png" alt="enter image description here" />

More examples and documentation [here](http://schinckel.net/2014/05/25/querying-json-in-postgres/) and [here](http://clarkdave.net/2013/06/what-can-you-do-with-postgresql-and-json/).



## Querying complex JSON documents


Taking a complex JSON document in a table:

```sql
CREATE TABLE mytable (data JSONB NOT NULL);
CREATE INDEX mytable_idx ON mytable USING gin (data jsonb_path_ops);
INSERT INTO mytable VALUES($$
{
    "name": "Alice",
    "emails": [
        "alice1@test.com",
        "alice2@test.com"
    ],
    "events": [
        {
            "type": "birthday",
            "date": "1970-01-01"
        },
        {
            "type": "anniversary",
            "date": "2001-05-05"
        }
    ],
    "locations": {
        "home": {
            "city": "London",
            "country": "United Kingdom"
        },
        "work": {
            "city": "Edinburgh",
            "country": "United Kingdom"
        }
    }
}
$$);

```

Query for a top-level element:

```sql
SELECT data->>'name' FROM mytable WHERE data @> '{"name":"Alice"}';

```

Query for a simple item in an array:

```sql
SELECT data->>'name' FROM mytable WHERE data @> '{"emails":["alice1@test.com"]}';

```

Query for an object in an array:

```sql
SELECT data->>'name' FROM mytable WHERE data @> '{"events":[{"type":"anniversary"}]}';

```

Query for a nested object:

```sql
SELECT data->>'name' FROM mytable WHERE data @> '{"locations":{"home":{"city":"London"}}}';

```

### Performance of `@>` compared to `->` and `->>`

It is important to understand the performance difference between using `@>`, `->` and `->>` in the `WHERE` part of the query.  Although these two queries appear to be broadly equivalent:

```sql
SELECT data FROM mytable WHERE data @> '{"name":"Alice"}';
SELECT data FROM mytable WHERE data->'name' = '"Alice"';
SELECT data FROM mytable WHERE data->>'name' = 'Alice';

```

the first statement will use the index created above whereas the latter two will not, requiring a complete table scan.

It is still allowable to use the `->` operator when obtaining resultant data, so the following queries will also use the index:

```sql
SELECT data->'locations'->'work' FROM mytable WHERE data @> '{"name":"Alice"}';
SELECT data->'locations'->'work'->>'city' FROM mytable WHERE data @> '{"name":"Alice"}';

```



## Creating a pure JSON table


To create a pure JSON table you need to provide a single field with the type `JSONB`:

```sql
CREATE TABLE mytable (data JSONB NOT NULL);

```

You should also create a basic index:

```sql
CREATE INDEX mytable_idx ON mytable USING gin (data jsonb_path_ops);

```

At this point you can insert data in to the table and query it efficiently.

