---
metaTitle: "Using SQLite in C#"
description: "Creating simple CRUD using SQLite in C#, Executing Query"
---

# Using SQLite in C#



## Creating simple CRUD using SQLite in C#


First of all we need to add SQLite support to our application. There are two ways of doing that

- Download DLL suiting your system from [SQLite download page](https://sqlite.org/download.html) and then add to the project manually
- Add SQLite dependency via NuGet

We'll do it the second way

First open the NuGet menu

[<img src="https://i.stack.imgur.com/owqid.png" alt="enter image description here" />](https://i.stack.imgur.com/owqid.png)

and search for **System.Data.SQLite**, select it and hit **Install**

[<img src="https://i.stack.imgur.com/4N4MH.png" alt="enter image description here" />](https://i.stack.imgur.com/4N4MH.png)

Installation can also be done from [Package Manager Console](https://docs.nuget.org/ndocs/tools/package-manager-console) with

```cs
PM> Install-Package System.Data.SQLite

```

Or for only core features

```cs
PM> Install-Package System.Data.SQLite.Core 

```

That's it for the download, so we can go right into coding.

First create a simple SQLite database with this table and add it as a file to the project

```cs
CREATE TABLE User(
  Id INTEGER PRIMARY KEY AUTOINCREMENT,
  FirstName TEXT NOT NULL,
  LastName TEXT NOT NULL
);

```

Also do not forget to set the **Copy to Output Directory** property of the file to **Copy if newer** of **Copy always**, based on your needs

[<img src="https://i.stack.imgur.com/baf9b.png" alt="enter image description here" />](https://i.stack.imgur.com/baf9b.png)

Create a class called User, which will be the base entity for our database

```cs
private class User
{
    public string FirstName { get; set; }
    public string Lastname { get; set; }
}

```

We'll write two methods for query execution, first one for inserting, updating or removing from database

```cs
private int ExecuteWrite(string query, Dictionary<string, object> args)
{
    int numberOfRowsAffected;

    //setup the connection to the database
    using (var con = new SQLiteConnection("Data Source=test.db"))
    {
        con.Open();
        
        //open a new command
        using (var cmd = new SQLiteCommand(query, con))
        {
            //set the arguments given in the query
            foreach (var pair in args)
            {
                cmd.Parameters.AddWithValue(pair.Key, pair.Value);
            }

            //execute the query and get the number of row affected
            numberOfRowsAffected = cmd.ExecuteNonQuery();
        }

        return numberOfRowsAffected;
    }
}

```

and the second one for reading from database

```cs
private DataTable Execute(string query)
{
    if (string.IsNullOrEmpty(query.Trim()))
        return null;

    using (var con = new SQLiteConnection("Data Source=test.db"))
    {
        con.Open();
        using (var cmd = new SQLiteCommand(query, con))
        {
            foreach (KeyValuePair<string, object> entry in args)
            {
                cmd.Parameters.AddWithValue(entry.Key, entry.Value);
            }

            var da = new SQLiteDataAdapter(cmd);

            var dt = new DataTable();
            da.Fill(dt);

            da.Dispose();
            return dt;
        }
    }
}

```

Now lets get into our **CRUD** methods

Adding user

```cs
private int AddUser(User user)
{
    const string query = "INSERT INTO User(FirstName, LastName) VALUES(@firstName, @lastName)";

    //here we are setting the parameter values that will be actually 
    //replaced in the query in Execute method
    var args = new Dictionary<string, object>
    {
        {"@firstName", user.FirstName},
        {"@lastName", user.Lastname}
    };

    return ExecuteWrite(query, args);
}

```

Editing user

```cs
private int EditUser(User user)
{
    const string query = "UPDATE User SET FirstName = @firstName, LastName = @lastName WHERE Id = @id";

    //here we are setting the parameter values that will be actually 
    //replaced in the query in Execute method
    var args = new Dictionary<string, object>
    {
        {"@id", user.Id},
        {"@firstName", user.FirstName},
        {"@lastName", user.Lastname}
    };

    return ExecuteWrite(query, args);
}

```

Deleting user

```cs
private int DeleteUser(User user)
{
    const string query = "Delete from User WHERE Id = @id";

    //here we are setting the parameter values that will be actually 
    //replaced in the query in Execute method
    var args = new Dictionary<string, object>
    {
        {"@id", user.Id}
    };

    return ExecuteWrite(query, args);
}

```

Getting user by `Id`

```cs
private User GetUserById(int id)
{
    var query = "SELECT * FROM User WHERE Id = @id";

    var args = new Dictionary<string, object>
    {
        {"@id", id}
    };

    DataTable dt = ExecuteRead(query, args);

    if (dt == null || dt.Rows.Count == 0)
    {
        return null;
    }

    var user = new User
    {
        Id = Convert.ToInt32(dt.Rows[0]["Id"]),
        FirstName = Convert.ToString(dt.Rows[0]["FirstName"]),
        Lastname = Convert.ToString(dt.Rows[0]["LastName"])
    };

    return user;
}

```



## Executing Query


```cs
using (SQLiteConnection conn = new SQLiteConnection(@"Data Source=data.db;Pooling=true;FailIfMissing=false"))
{
    conn.Open();
    using (SQLiteCommand cmd = new SQLiteCommand(conn))
    {
       cmd.CommandText = "query";
       using (SqlDataReader dr = cmd.ExecuteReader())
       {
           while(dr.Read())
           {
               //do stuff
           }
       }
    }
}

```

**Note**: Setting `FailIfMissing` to true creates the file `data.db` if missing. However, the file will be empty. So, any required tables have to be recreated.

