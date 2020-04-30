---
metaTitle: "IQueryable interface"
description: "Translating a LINQ query to a SQL query"
---

# IQueryable interface



## Translating a LINQ query to a SQL query


The `IQueryable` and `IQueryable<T>` interfaces allows developers to translate a LINQ query (a 'language-integrated' query) to a specific datasource, for example a relational database. Take this LINQ query written in C#:

```cs
var query = from book in books
            where book.Author == "Stephen King" 
            select book;

```

If the variable `books` is of a type that implements `IQueryable<Book>` then the query above gets passed to the provider (set on the `IQueryable.Provider` property) in the form of an expression tree, a data structure that reflects the structure of the code.

The provider can inspect the expression tree at runtime to determine:

- that there is a predicate for the `Author` property of the `Book` class;
- that the comparison method used is 'equals' (`==`);
- that the value it should equal is `"Stephen King"`.

With this information the provider can translate the C# query to a SQL query at runtime and pass that query to a relational database to fetch only those books that match the predicate:

```cs
select *
from Books
where Author = 'Stephen King'

```

The provider gets called when the `query` variable is iterated over (`IQueryable` implements `IEnumerable`).

(The provider used in this example would require some extra metadata to know which table to query and to know how to match properties of the C# class to columns of the table, but such metadata is outside of the scope of the `IQueryable` interface.)

