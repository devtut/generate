---
metaTitle: "Example Databases and Tables"
description: "Auto Shop Database, Library Database, Countries Table"
---

# Example Databases and Tables




## Auto Shop Database


In the following example - Database for an auto shop business, we have a list of departments, employees, customers and customer cars. We are using foreign keys to create relationships between the various tables.

Live example: [SQL fiddle](http://sqlfiddle.com/#!9/faf2f/1)

### Relationships between tables

- Each Department may have 0 or more Employees
- Each Employee may have 0 or 1 Manager
- Each Customer may have 0 or more Cars

### Departments

<th align="left">Id</th><th align="left">Name</th>
|------
<td align="left">1</td><td align="left">HR</td>
<td align="left">2</td><td align="left">Sales</td>
<td align="left">3</td><td align="left">Tech</td>

SQL statements to create the table:

```sql
CREATE TABLE Departments (
    Id INT NOT NULL AUTO_INCREMENT,
    Name VARCHAR(25) NOT NULL,
    PRIMARY KEY(Id)
);

INSERT INTO Departments
    ([Id], [Name])
VALUES
    (1, 'HR'),
    (2, 'Sales'),
    (3, 'Tech')
;

```

### Employees

<th align="left">Id</th><th align="left">FName</th><th align="left">LName</th><th align="left">PhoneNumber</th><th align="left">ManagerId</th><th align="left">DepartmentId</th><th align="left">Salary</th><th align="left">HireDate</th>
|------
<td align="left">1</td><td align="left">James</td><td align="left">Smith</td><td align="left">1234567890</td><td align="left">NULL</td><td align="left">1</td><td align="left">1000</td><td align="left">01-01-2002</td>
<td align="left">2</td><td align="left">John</td><td align="left">Johnson</td><td align="left">2468101214</td><td align="left">1</td><td align="left">1</td><td align="left">400</td><td align="left">23-03-2005</td>
<td align="left">3</td><td align="left">Michael</td><td align="left">Williams</td><td align="left">1357911131</td><td align="left">1</td><td align="left">2</td><td align="left">600</td><td align="left">12-05-2009</td>
<td align="left">4</td><td align="left">Johnathon</td><td align="left">Smith</td><td align="left">1212121212</td><td align="left">2</td><td align="left">1</td><td align="left">500</td><td align="left">24-07-2016</td>

SQL statements to create the table:

```sql
CREATE TABLE Employees (
    Id INT NOT NULL AUTO_INCREMENT,
    FName VARCHAR(35) NOT NULL,
    LName VARCHAR(35) NOT NULL,
    PhoneNumber VARCHAR(11),
    ManagerId INT,
    DepartmentId INT NOT NULL,
    Salary INT NOT NULL,
    HireDate DATETIME NOT NULL,
    PRIMARY KEY(Id),
    FOREIGN KEY (ManagerId) REFERENCES Employees(Id),
    FOREIGN KEY (DepartmentId) REFERENCES Departments(Id)
);

INSERT INTO Employees
    ([Id], [FName], [LName], [PhoneNumber], [ManagerId], [DepartmentId], [Salary], [HireDate])
VALUES
    (1, 'James', 'Smith', 1234567890, NULL, 1, 1000, '01-01-2002'),
    (2, 'John', 'Johnson', 2468101214, '1', 1, 400, '23-03-2005'),
    (3, 'Michael', 'Williams', 1357911131, '1', 2, 600, '12-05-2009'),
    (4, 'Johnathon', 'Smith', 1212121212, '2', 1, 500, '24-07-2016')
;

```

### Customers

<th align="left">Id</th><th align="left">FName</th><th align="left">LName</th><th align="left">Email</th><th align="left">PhoneNumber</th><th align="left">PreferredContact</th>
|------
<td align="left">1</td><td align="left">William</td><td align="left">Jones</td><td align="left">william.jones@example.com</td><td align="left">3347927472</td><td align="left">PHONE</td>
<td align="left">2</td><td align="left">David</td><td align="left">Miller</td><td align="left">dmiller@example.net</td><td align="left">2137921892</td><td align="left">EMAIL</td>
<td align="left">3</td><td align="left">Richard</td><td align="left">Davis</td><td align="left">richard0123@example.com</td><td align="left">NULL</td><td align="left">EMAIL</td>

SQL statements to create the table:

```sql
CREATE TABLE Customers (
    Id INT NOT NULL AUTO_INCREMENT,
    FName VARCHAR(35) NOT NULL,
    LName VARCHAR(35) NOT NULL,
    Email varchar(100) NOT NULL,
    PhoneNumber VARCHAR(11),
    PreferredContact VARCHAR(5) NOT NULL,
    PRIMARY KEY(Id)
);

INSERT INTO Customers
    ([Id], [FName], [LName], [Email], [PhoneNumber], [PreferredContact])
VALUES
    (1, 'William', 'Jones', 'william.jones@example.com', '3347927472', 'PHONE'),
    (2, 'David', 'Miller', 'dmiller@example.net', '2137921892', 'EMAIL'),
    (3, 'Richard', 'Davis', 'richard0123@example.com', NULL, 'EMAIL')
;

```

### Cars

<th align="left">Id</th><th align="left">CustomerId</th><th align="left">EmployeeId</th><th align="left">Model</th><th align="left">Status</th><th align="left">Total Cost</th>
|------
<td align="left">1</td><td align="left">1</td><td align="left">2</td><td align="left">Ford F-150</td><td align="left">READY</td><td align="left">230</td>
<td align="left">2</td><td align="left">1</td><td align="left">2</td><td align="left">Ford F-150</td><td align="left">READY</td><td align="left">200</td>
<td align="left">3</td><td align="left">2</td><td align="left">1</td><td align="left">Ford Mustang</td><td align="left">WAITING</td><td align="left">100</td>
<td align="left">4</td><td align="left">3</td><td align="left">3</td><td align="left">Toyota Prius</td><td align="left">WORKING</td><td align="left">1254</td>

SQL statements to create the table:

```sql
CREATE TABLE Cars (
    Id INT NOT NULL AUTO_INCREMENT,
    CustomerId INT NOT NULL,
    EmployeeId INT NOT NULL,
    Model varchar(50) NOT NULL,
    Status varchar(25) NOT NULL,
    TotalCost INT NOT NULL,
    PRIMARY KEY(Id),
    FOREIGN KEY (CustomerId) REFERENCES Customers(Id),
    FOREIGN KEY (EmployeeId) REFERENCES Employees(Id)
);

INSERT INTO Cars
    ([Id], [CustomerId], [EmployeeId], [Model], [Status], [TotalCost])
VALUES
    ('1', '1', '2', 'Ford F-150', 'READY', '230'),
    ('2', '1', '2', 'Ford F-150', 'READY', '200'),
    ('3', '2', '1', 'Ford Mustang', 'WAITING', '100'),
    ('4', '3', '3', 'Toyota Prius', 'WORKING', '1254')
;

```



## Library Database


In this example database for a library, we have **Authors**, **Books** and **BooksAuthors** tables.

Live example: [SQL fiddle](http://sqlfiddle.com/#!9/7c06f/1)

**Authors** and **Books** are known as **base tables**, since they contain column definition and data for the actual entities in the relational model. **BooksAuthors** is known as the **relationship table**, since this table defines the relationship between the **Books** and **Authors** table.

### Relationships between tables

- Each author can have 1 or more books
- Each book can have 1 or more authors

### Authors

([**view table**](http://sqlfiddle.com/#!9/7c06f/2))

<th align="left">Id</th><th align="left">Name</th><th align="left">Country</th>
|------
<td align="left">1</td><td align="left">J.D. Salinger</td><td align="left">USA</td>
<td align="left">2</td><td align="left">F. Scott. Fitzgerald</td><td align="left">USA</td>
<td align="left">3</td><td align="left">Jane Austen</td><td align="left">UK</td>
<td align="left">4</td><td align="left">Scott Hanselman</td><td align="left">USA</td>
<td align="left">5</td><td align="left">Jason N. Gaylord</td><td align="left">USA</td>
<td align="left">6</td><td align="left">Pranav Rastogi</td><td align="left">India</td>
<td align="left">7</td><td align="left">Todd Miranda</td><td align="left">USA</td>
<td align="left">8</td><td align="left">Christian Wenz</td><td align="left">USA</td>

SQL to create the table:

```sql
CREATE TABLE Authors (
    Id INT NOT NULL AUTO_INCREMENT,
    Name VARCHAR(70) NOT NULL,
    Country VARCHAR(100) NOT NULL,
    PRIMARY KEY(Id)
);

INSERT INTO Authors
    (Name, Country)
VALUES
    ('J.D. Salinger', 'USA'),
    ('F. Scott. Fitzgerald', 'USA'),
    ('Jane Austen', 'UK'),
    ('Scott Hanselman', 'USA'),
    ('Jason N. Gaylord', 'USA'),
    ('Pranav Rastogi', 'India'),
    ('Todd Miranda', 'USA'),
    ('Christian Wenz', 'USA')
;

```

### Books

([**view table**](http://sqlfiddle.com/#!9/7c06f/3))

<th align="left">Id</th><th align="left">Title</th>
|------
<td align="left">1</td><td align="left">The Catcher in the Rye</td>
<td align="left">2</td><td align="left">Nine Stories</td>
<td align="left">3</td><td align="left">Franny and Zooey</td>
<td align="left">4</td><td align="left">The Great Gatsby</td>
<td align="left">5</td><td align="left">Tender id the Night</td>
<td align="left">6</td><td align="left">Pride and Prejudice</td>
<td align="left">7</td><td align="left">Professional ASP.NET 4.5 in C# and VB</td>

SQL to create the table:

```sql
CREATE TABLE Books (
    Id INT NOT NULL AUTO_INCREMENT,
    Title VARCHAR(50) NOT NULL,
    PRIMARY KEY(Id)
);

INSERT INTO Books
    (Id, Title)
VALUES
    (1, 'The Catcher in the Rye'),
    (2, 'Nine Stories'),
    (3, 'Franny and Zooey'),
    (4, 'The Great Gatsby'),
    (5, 'Tender id the Night'),
    (6, 'Pride and Prejudice'),
    (7, 'Professional ASP.NET 4.5 in C# and VB')
;

```

### BooksAuthors

([**view table**](http://sqlfiddle.com/#!9/7c06f/4))

<th align="left">BookId</th><th align="left">AuthorId</th>
|------
<td align="left">1</td><td align="left">1</td>
<td align="left">2</td><td align="left">1</td>
<td align="left">3</td><td align="left">1</td>
<td align="left">4</td><td align="left">2</td>
<td align="left">5</td><td align="left">2</td>
<td align="left">6</td><td align="left">3</td>
<td align="left">7</td><td align="left">4</td>
<td align="left">7</td><td align="left">5</td>
<td align="left">7</td><td align="left">6</td>
<td align="left">7</td><td align="left">7</td>
<td align="left">7</td><td align="left">8</td>

SQL to create the table:

```sql
CREATE TABLE BooksAuthors (
    AuthorId INT NOT NULL,
    BookId  INT NOT NULL,
    FOREIGN KEY (AuthorId) REFERENCES Authors(Id),
    FOREIGN KEY (BookId) REFERENCES Books(Id)
);

INSERT INTO BooksAuthors
    (BookId, AuthorId)
VALUES
    (1, 1),
    (2, 1),
    (3, 1),
    (4, 2),
    (5, 2),
    (6, 3),
    (7, 4),
    (7, 5),
    (7, 6),
    (7, 7),
    (7, 8)
;

```

### Examples

View all authors ([view live example](http://sqlfiddle.com/#!9/7c06f/2)):

```sql
SELECT * FROM Authors;

```

View all book titles ([view live example](http://sqlfiddle.com/#!9/7c06f/3)):

```sql
SELECT * FROM Books;

```

View all books and their authors ([view live example](http://sqlfiddle.com/#!9/7c06f/5)):

```sql
SELECT
  ba.AuthorId,
  a.Name AuthorName,
  ba.BookId,
  b.Title BookTitle
FROM BooksAuthors ba
  INNER JOIN Authors a ON a.id = ba.authorid
  INNER JOIN Books b ON b.id = ba.bookid
;

```



## Countries Table


In this example, we have a **Countries** table. A table for countries has many uses, especially in Financial applications involving currencies and exchange rates.

Live example: [SQL fiddle](http://sqlfiddle.com/#!9/14cfc6)

Some Market data software applications like Bloomberg and Reuters require you to give their API either a 2 or 3 character country code along with the currency code. Hence this example table has both the 2-character `ISO` code column and the 3 character `ISO3` code columns.

### Countries

([**view table**](http://sqlfiddle.com/#!9/14cfc6/1))

<th align="left">Id</th><th align="left">ISO</th><th align="left">ISO3</th><th align="left">ISONumeric</th><th align="left">CountryName</th><th align="left">Capital</th><th align="left">ContinentCode</th><th align="left">CurrencyCode</th>
|------
<td align="left">1</td><td align="left">AU</td><td align="left">AUS</td><td align="left">36</td><td align="left">Australia</td><td align="left">Canberra</td><td align="left">OC</td><td align="left">AUD</td>
<td align="left">2</td><td align="left">DE</td><td align="left">DEU</td><td align="left">276</td><td align="left">Germany</td><td align="left">Berlin</td><td align="left">EU</td><td align="left">EUR</td>
<td align="left">2</td><td align="left">IN</td><td align="left">IND</td><td align="left">356</td><td align="left">India</td><td align="left">New Delhi</td><td align="left">AS</td><td align="left">INR</td>
<td align="left">3</td><td align="left">LA</td><td align="left">LAO</td><td align="left">418</td><td align="left">Laos</td><td align="left">Vientiane</td><td align="left">AS</td><td align="left">LAK</td>
<td align="left">4</td><td align="left">US</td><td align="left">USA</td><td align="left">840</td><td align="left">United States</td><td align="left">Washington</td><td align="left">NA</td><td align="left">USD</td>
<td align="left">5</td><td align="left">ZW</td><td align="left">ZWE</td><td align="left">716</td><td align="left">Zimbabwe</td><td align="left">Harare</td><td align="left">AF</td><td align="left">ZWL</td>

SQL to create the table:

```sql
CREATE TABLE Countries (
    Id INT NOT NULL AUTO_INCREMENT,
    ISO VARCHAR(2) NOT NULL,
    ISO3 VARCHAR(3) NOT NULL,
    ISONumeric INT NOT NULL,
    CountryName VARCHAR(64) NOT NULL,
    Capital VARCHAR(64) NOT NULL,
    ContinentCode VARCHAR(2) NOT NULL,
    CurrencyCode VARCHAR(3) NOT NULL,
    PRIMARY KEY(Id)
)
;

INSERT INTO Countries
    (ISO, ISO3, ISONumeric, CountryName, Capital, ContinentCode, CurrencyCode)
VALUES
    ('AU', 'AUS', 36, 'Australia', 'Canberra', 'OC', 'AUD'),
    ('DE', 'DEU', 276, 'Germany', 'Berlin', 'EU', 'EUR'),
    ('IN', 'IND', 356, 'India', 'New Delhi', 'AS', 'INR'),
    ('LA', 'LAO', 418, 'Laos', 'Vientiane', 'AS', 'LAK'),
    ('US', 'USA', 840, 'United States', 'Washington', 'NA', 'USD'),
    ('ZW', 'ZWE', 716, 'Zimbabwe', 'Harare', 'AF', 'ZWL')
;

```

