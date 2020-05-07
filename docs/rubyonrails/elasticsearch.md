---
metaTitle: "Ruby on Rails - Elasticsearch"
description: "Searchkick, Installation and testing, Setting up tools for development, Introduction"
---

# Elasticsearch



## Searchkick


If you want to setup quickly elasticsearch you can use the searchkick gem :

Add searchkick to models you want to search.

Add data to the search index.

And to query, use:

Pretty quick, elasticsearch knowledge not required ;-)

More information here : [https://github.com/ankane/searchkick](https://github.com/ankane/searchkick)



## Installation and testing


The first thing you want to do for local development is install ElasticSearch in your machine and test it to see if it is running. It requires Java to be installed. The installation is pretty straightforward:

- Mac OS X: `brew install elasticsearch`
- Ubuntu: `sudo apt-get install elasticsearch`

Then start it:

- Mac OS X: `brew services start elasticsearch`
- Ubuntu: `sudo service elasticsearch start`

For testing it, the easiest way is with `curl`. It might take a few seconds for it to start, so don't panic if you don't get any response at first.

`curl localhost:9200`

Example response:

```ruby
{
  "name" : "Hydro-Man",
  "cluster_name" : "elasticsearch_gkbonetti",
  "version" : {
    "number" : "2.3.5",
    "build_hash" : "90f439ff60a3c0f497f91663701e64ccd01edbb4",
    "build_timestamp" : "2016-07-27T10:36:52Z",
    "build_snapshot" : false,
    "lucene_version" : "5.5.0"
  },
  "tagline" : "You Know, for Search"
}

```



## Setting up tools for development


When you are getting started with ElasticSearch (ES) it might be good to have a graphical tool that helps you explore your data. A plugin called [`elasticsearch-head`](http://mobz.github.io/elasticsearch-head/) does just that. To install it, do the following:

- Find out in which folder ES is installed: `ls -l $(which elasticsearch)`
- `cd` into this folder and run the plugin installation binary: `elasticsearch/bin/plugin -install mobz/elasticsearch-head`
- Open `http://localhost:9200/_plugin/head/` in your browser

If everything worked as expected you should be seeing a nice GUI where you can explore your data.



## Introduction


ElasticSearch has a well-documented JSON API, but you'll probably want to use some libraries that handle that for you:

<li>
[`Elasticsearch`](https://github.com/elastic/elasticsearch-ruby) - the official low level wrapper for the HTTP API
</li>
<li>
[`Elasticsearch-rails`](https://github.com/elastic/elasticsearch-rails) - the official high level Rails integration that helps you to connect your Rails models with ElasticSearch using either ActiveRecord or Repository pattern
</li>
<li>
[`Chewy`](https://github.com/toptal/chewy) - An alternative, non-official high level Rails integration that is very popular and arguably has better documentation
</li>

Let's use the first option for testing the connection:

`gem install elasticsearch`

Then fire up the ruby terminal and try it out:

```ruby
require 'elasticsearch'

client = Elasticsearch::Client.new log: true
# by default it connects to http://localhost:9200

client.transport.reload_connections!
client.cluster.health

client.search q: 'test'

```

