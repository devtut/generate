---
metaTitle: "Haskell - Databases"
description: "Postgres"
---

# Databases



## Postgres


Postgresql-simple is a mid-level Haskell library for communicating with a PostgreSQL backend database. It is very simple to use and provides a type-safe API for reading/writing to a DB.

Running a simple query is as easy as:

```hs
{-# LANGUAGE OverloadedStrings #-}

import Database.PostgreSQL.Simple

main :: IO ()
main = do
  -- Connect using libpq strings
  conn <- connectPostgreSQL "host='my.dbhost' port=5432 user=bob pass=bob"
  [Only i] <- query_ conn "select 2 + 2"  -- execute with no parameter substitution
  print i

```

### Parameter substitution

PostreSQL-Simple supports parameter substitution for safe parameterised queries using `query`:

```hs
main :: IO ()
main = do
  -- Connect using libpq strings
  conn <- connectPostgreSQL "host='my.dbhost' port=5432 user=bob pass=bob"
  [Only i] <- query conn "select ? + ?" [1, 1]
  print i

```

### Executing inserts or updates

You can run inserts/update SQL queries using `execute`:

```hs
main :: IO ()
main = do
  -- Connect using libpq strings
  conn <- connectPostgreSQL "host='my.dbhost' port=5432 user=bob pass=bob"
  execute conn "insert into people (name, age) values (?, ?)" ["Alex", 31]

```

