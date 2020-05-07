---
metaTitle: "MongoDB - Getting started with MongoDB"
description: "Execution of a JavaScript file in MongoDB, Making the output of find readable in shell, Complementary Terms, Installation, Basic commands on mongo shell, Hello World"
---

# Getting started with MongoDB



## Execution of a JavaScript file in MongoDB


```js
./mongo localhost:27017/mydb myjsfile.js

```

**Explanation:**
This operation executes the `myjsfile.js` script in a `mongo` shell that connects to the `mydb` database on the `mongod` instance accessible via the `localhost` interface on port `27017`. `localhost:27017` is not mandatory as this is the default port `mongodb` uses.

Also, you can run a `.js` file from within `mongo` console.

```js
>load("myjsfile.js")

```



## Making the output of find readable in shell


We add three records to our collection test as:

```js
> db.test.insert({"key":"value1","key2":"Val2","key3":"val3"})
WriteResult({ "nInserted" : 1 })
> db.test.insert({"key":"value2","key2":"Val21","key3":"val31"})
WriteResult({ "nInserted" : 1 })
> db.test.insert({"key":"value3","key2":"Val22","key3":"val33"})
WriteResult({ "nInserted" : 1 })

```

If we see them via find, they will look very ugly.

```js
> db.test.find()
{ "_id" : ObjectId("5790c5cecae25b3d38c3c7ae"), "key" : "value1", "key2" : "Val2
", "key3" : "val3" }
{ "_id" : ObjectId("5790c5d9cae25b3d38c3c7af"), "key" : "value2", "key2" : "Val2
1", "key3" : "val31" }
{ "_id" : ObjectId("5790c5e9cae25b3d38c3c7b0"), "key" : "value3", "key2" : "Val2
2", "key3" : "val33" }

```

To work around this and make them readable, use the **pretty**() function.

```js
> db.test.find().pretty()
{
        "_id" : ObjectId("5790c5cecae25b3d38c3c7ae"),
        "key" : "value1",
        "key2" : "Val2",
        "key3" : "val3"
}
{
        "_id" : ObjectId("5790c5d9cae25b3d38c3c7af"),
        "key" : "value2",
        "key2" : "Val21",
        "key3" : "val31"
}
{
        "_id" : ObjectId("5790c5e9cae25b3d38c3c7b0"),
        "key" : "value3",
        "key2" : "Val22",
        "key3" : "val33"
}
>

```



## Complementary Terms


|SQL Terms|MongoDB Terms
|---|---|---|---|---|---|---|---|---|---
|Database|Database
|Table|Collection
|Entity / Row|Document
|Column|Key / Field
|Table Join|[Embedded Documents](https://docs.mongodb.com/manual/tutorial/model-embedded-one-to-many-relationships-between-documents/)
|Primary Key|[Primary Key](https://docs.mongodb.com/manual/indexes/#default-id-index) (Default key `_id` provided by mongodb itself)



## Installation


To install MongoDB, follow the steps below:

<li>
**For Mac OS:**
<ul>
- There are two options for Mac OS: manual install or [homebrew](https://brew.sh/).
<li>**Installing with [homebrew](https://brew.sh/):**
<ul>
<li>Type the following command into the terminal:  
<pre class="lang-none prettyprint-override"><code>$ brew install mongodb

```


</li>

<li>
<p>Download the latest release
[here](https://www.mongodb.com/download-center#community). Make sure that you are
downloading the appropriate file, specially check whether your
operating system type is 32-bit or 64-bit. The downloaded file is in format `tgz`.</p>
</li>
<li>
Go to the directory where this file is downloaded. Then type the following command:
        
<pre class="lang-none prettyprint-override"><code>$ tar xvf mongodb-osx-xyz.tgz

```


Instead of `xyz`, there would be some version and system type information. The extracted folder would be same name as the `tgz` file. Inside the folder, their would be a subfolder named `bin` which would contain several binary file along with `mongod` and `mongo`.
</li>
<li>
By default server keeps data in folder `/data/db`. So, we have to create that directory and then run the server having the following commands:
<pre class="lang-none prettyprint-override"><code>$ sudo bash
# mkdir -p /data/db
# chmod 777 /data
# chmod 777 /data/db
# exit

```


</li>
<li>
To start the server, the following command should be given from the current location:
<pre class="lang-none prettyprint-override"><code>$ ./mongod

```


It would start the server on port 27017 by default.
</li>
<li>
To start the client, a new terminal should be opened having the same directory as before. Then the following command would start the client and connect to the server.
<pre class="lang-none prettyprint-override"><code>$ ./mongo

```


By default it connects to the `test` database. If you see the line like `connecting to: test`. Then you have successfully installed MongoDB. Congrats! Now, you can test [Hello World](http://stackoverflow.com/documentation/mongodb/691/introduction-to-mongodb/2291/hello-world#t=20160616174713619659) to be more confident.
</li>

**For Windows:**

<li>
<p>Download the latest release
[here](https://www.mongodb.com/download-center#community). Make sure that you are
downloading the appropriate file, specially check whether your
operating system type is 32-bit or 64-bit.</p>
</li>
<li>
The downloaded binary file has extension `exe`. Run it. It will prompt an installation wizard.
</li>
<li>
Click **Next**.
</li>
<li>
**Accept** the licence agreement and click **Next**.
</li>
<li>
Select **Complete** Installation.
</li>
<li>
Click on **Install**. It might prompt a window for asking administrator's permission. Click **Yes**.
</li>
<li>
After installation click on **Finish**.
</li>
<li>
Now, the mongodb is installed on the path `C:/Program Files/MongoDB/Server/3.2/bin`. Instead of version 3.2, there could be some other version for your case. The path name would be changed accordingly.
</li>
<li>
`bin` directory contain several binary file along with `mongod` and `mongo`. To run it from other folder, you could add the path in system path. To do it:
<ul>
- Right click on **My Computer** and select **Properties**.
- Click on **Advanced system setting** on the left pane.
- Click on **Environment Variables...** under the **Advanced** tab.
- Select **Path** from **System variables** section and click on **Edit...**.
- Before Windows 10, append a semi-colon and paste the path given above. From Windows 10, there is a **New** button to add new path.
- Click **OK**s to save changes.

Now, create a folder named `data` having a sub-folder named `db` where you want to run the server.

Start command prompt from their. Either changing the path in cmd or clicking on **Open command window here** which would be visible after right clicking on the empty space of the folder GUI pressing the Shift and Ctrl key together.

Write the command to start the server:

```js
> mongod

```

It would start the server on port 27017 by default.

Open another command prompt and type the following to start client:

```js
> mongo

```

By default it connects to the `test` database. If you see the line like `connecting to: test`. Then you have successfully installed MongoDB. Congrats! Now, you can test [Hello World](http://stackoverflow.com/documentation/mongodb/691/introduction-to-mongodb/2291/hello-world#t=201606161746308255884) to be more confident.

**For Linux:** Almost same as Mac OS except some equivalent command is needed.

<li>For Debian-based distros (using `apt-get`):
<ul>
<li>
Import MongoDB Repository key.
<pre class="lang-none prettyprint-override"><code>$ sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv EA312927
gpg: Total number processed: 1\
gpg:               imported: 1  (RSA: 1)

```


</li>
<li>
Add repository to package list on **Ubuntu 16.04**.
<pre class="lang-none prettyprint-override"><code>$ echo "deb http://repo.mongodb.org/apt/ubuntu xenial/mongodb-org/3.2 multiverse" | sudo tee /etc/apt/sources.list.d/mongodb-org-3.2.list

```


</li>
<li>
on **Ubuntu 14.04**.
<pre class="lang-none prettyprint-override"><code>$ echo "deb http://repo.mongodb.org/apt/ubuntu trusty/mongodb-org/3.2 multiverse" | sudo tee /etc/apt/sources.list.d/mongodb-org-3.2.list

```


</li>
<li>
Update package list.
<pre class="lang-none prettyprint-override"><code>$ sudo apt-get update

```


</li>
<li>
Install MongoDB.
<pre class="lang-none prettyprint-override"><code>$ sudo apt-get install mongodb-org

```


</li>

<li>
use a text editor which you prefer.
$  vi /etc/yum.repos.d/mongodb-org-3.4.repo
</li>
<li>
Paste following text.
<pre class="lang-none prettyprint-override"><code>[mongodb-org-3.4]
name=MongoDB Repository
baseurl=https://repo.mongodb.org/yum/redhat/$releasever/mongodb-org/3.4/x86_64/
gpgcheck=1
enabled=1
gpgkey=https://www.mongodb.org/static/pgp/server-3.4.asc

```


</li>
<li>
Update package list.
<pre class="lang-none prettyprint-override"><code>$ sudo yum update

```


</li>
<li>
Install MongoDB
<pre class="lang-none prettyprint-override"><code>$ sudo yum install mongodb-org

```


</li>



## Basic commands on mongo shell


Show all available databases:

```js
show dbs;

```

Select a particular database to access, e.g. `mydb`. This will create `mydb` if it does not already exist:

```js
use mydb;

```

Show all collections in the database (be sure to select one first, see above):

```js
show collections; 

```

Show all functions that can be used with the database:

```js
db.mydb.help();

```

To check your currently selected database, use the command `db`

```js
> db
mydb

```

`db.dropDatabase()` command is used to drop a existing database.

```js
db.dropDatabase()

```



## Hello World


After installation process, the following lines should be entered in mongo shell (client terminal).

```js
> db.world.insert({ "speech" : "Hello World!" });
> cur = db.world.find();x=cur.next();print(x["speech"]);

```

> 
Hello World!


**Explanation:**

- In the first line, we have inserted a `{ key : value }` paired document in the default database `test` and in the collection named `world`.
- In the second line we retrieve the data we have just inserted. The retrieved data is kept in a javascript variable named `cur`. Then by the `next()` function, we retrieved the first and only document and kept it in another js variable named `x`. Then printed the value of the document providing the key.



#### Remarks


- Data in the world started to grow tremendously after mobile application came in the market. This huge amount of data became almost impossible to handle with traditional relational database - SQL. NoSQL databases are introduced to handle those data where much more flexibility came like variable number of columns for each data.
- MongoDB is one of the leading NoSQL databases. Each collection contains a number of JSON documents. Any data model that can be expressed in a JSON document can be easily stored in MongoDB.
- MongoDB is a server-client database. Server usually runs with the binary file `mongod` and client runs with `mongo`.
- There is no join operation in MongoDB prior to v.3.2, [for various philosophical and pragmatic reasons](https://www.mongodb.com/blog/post/joins-and-other-aggregation-enhancements-coming-in-mongodb-3-2-part-1-of-3-introduction). But Mongo shell supports javascript, so if $lookup is not available, one can simulate join operations on documents in javascript before inserting.
- To run an instance in production environment, it's strongly advised to follow the [Operations Checklist](https://www.mongodb.com/blog/post/joins-and-other-aggregation-enhancements-coming-in-mongodb-3-2-part-1-of-3-introduction).

