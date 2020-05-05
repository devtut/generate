---
metaTitle: "R - Spark API (SparkR)"
description: "Setup Spark context, Cache data, Create RDDs (Resilient Distributed Datasets)"
---

# Spark API (SparkR)



## Setup Spark context


### Setup Spark context in R

To start working with Sparks distributed dataframes, you must connect your R program with an existing Spark Cluster.

```r
library(SparkR)
sc <- sparkR.init() # connection to Spark context
sqlContext <- sparkRSQL.init(sc) # connection to SQL context

```

[Here are infos](https://spark.apache.org/docs/1.6.0/sparkr.html#starting-up-from-rstudio) how to connect your IDE to a Spark cluster.

### Get Spark Cluster

There is an [Apache Spark introduction topic](http://stackoverflow.com/documentation/apache-spark/833/introduction-to-apache-spark#t=201608091436462968765) with install instructions. Basically, you can employ a Spark Cluster locally via java ([see instructions](http://spark.apache.org/docs/latest/)) or use (non-free) cloud applications (e.g. [Microsoft Azure](https://azure.microsoft.com/en-us/services/hdinsight/apache-spark/) [ [topic site]](http://stackoverflow.com/documentation/azure/topics), [IBM](http://www.ibm.com/analytics/us/en/technology/spark/)).



## Cache data


What:

Caching can optimize computation in Spark. Caching stores data in memory and is a special case of persistence. [Here is explained](http://stackoverflow.com/a/28983767/3889242) what happens when you cache an RDD in Spark.

Why:

Basically, caching saves an interim partial result - usually after transformations - of your original data. So, when you use the cached RDD, the already transformed data from memory is accessed without recomputing the earlier transformations.

How:

Here is an example how to quickly access large data **(here 3 GB big csv)** from in-memory storage when accessing it more then once:

```r
library(SparkR)
# next line is needed for direct csv import:
Sys.setenv('SPARKR_SUBMIT_ARGS'='"--packages" "com.databricks:spark-csv_2.10:1.4.0" "sparkr-shell"')
sc <- sparkR.init()
sqlContext <- sparkRSQL.init(sc)

# loading 3 GB big csv file:  
train <- read.df(sqlContext, "/train.csv", source = "com.databricks.spark.csv", inferSchema = "true")
cache(train)
system.time(head(train))
# output: time elapsed: 125 s. This action invokes the caching at this point.
system.time(head(train))
# output: time elapsed: 0.2 s (!!)

```



## Create RDDs (Resilient Distributed Datasets)


### From dataframe:

```r
mtrdd <- createDataFrame(sqlContext, mtcars)

```

### From csv:

For csv's, you need to add the [csv package](https://github.com/databricks/spark-csv) to the environment before initiating the Spark context:

```r
Sys.setenv('SPARKR_SUBMIT_ARGS'='"--packages" "com.databricks:spark-csv_2.10:1.4.0" "sparkr-shell"') # context for csv import read csv -> 
sc <- sparkR.init()
sqlContext <- sparkRSQL.init(sc)

```

Then, you can load the csv either by infering the data schema of the data in the columns:

```r
train <- read.df(sqlContext, "/train.csv", header= "true", source = "com.databricks.spark.csv", inferSchema = "true")

```

Or by specifying the data schema beforehand:

```

customSchema <- structType(
    structField("margin", "integer"),
    structField("gross", "integer"),
    structField("name", "string"))

 train <- read.df(sqlContext, "/train.csv", header= "true", source = "com.databricks.spark.csv", schema = customSchema)

```



#### Remarks


The `SparkR` package let's you work with distributed data frames on top of a [Spark cluster](http://spark.apache.org/). These allow you to do operations like selection, filtering, aggregation on very large datasets.
[SparkR overview](https://spark.apache.org/docs/latest/sparkr.html)
[SparkR package documentation](https://spark.apache.org/docs/1.5.1/api/R/)

