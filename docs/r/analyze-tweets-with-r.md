---
metaTitle: "R - Analyze tweets with R"
description: "Download Tweets, Get text of tweets"
---

# Analyze tweets with R


(Optional) Every topic has a focus. Tell the readers what they will find here and let future contributors know what belongs.



## Download Tweets


The first thing you need to do is to download tweets. You need to Setup your tweeter account. Much Information can be found in Internet on how to do it. The following two links were useful for my Setup (last checked in May 2017)

In particular, I found the following two links useful (last checked in May 2017):

[Link 1](https://www.credera.com/blog/business-intelligence/twitter-analytics-using-r-part-1-extract-tweets/)

[Link 2](http://thinktostart.com/twitter-authentification-with-r/)

### R Libraries

You will need the following R packages

```r
library("devtools")
library("twitteR")
library("ROAuth")

```

Supposing you have your keys You have to run the following code

```r
api_key <- XXXXXXXXXXXXXXXXXXXXXX
api_secret <- XXXXXXXXXXXXXXXXXXXXXX
access_token <- XXXXXXXXXXXXXXXXXXXXXX
access_token_secret <- XXXXXXXXXXXXXXXXXXXXXX


setup_twitter_oauth(api_key,api_secret)

```

Change `XXXXXXXXXXXXXXXXXXXXXX` to your keys (if you have Setup your tweeter account you know which keys I mean).

Let's now suppose we want to download tweets on coffee. The following code will do it

```r
search.string <- "#coffee"
no.of.tweets <- 1000

c_tweets <- searchTwitter(search.string, n=no.of.tweets, lang="en") 

```

You will get 1000 tweets on "coffee".



## Get text of tweets


Now we need to access the text of the tweets. So we do it in this way (we also need to clean up the tweets from special characters that for now we don't need, like emoticons with the  sapply  function.)

```r
coffee_tweets = sapply(c_tweets, function(t) t$getText())

coffee_tweets <- sapply(coffee_tweets,function(row) iconv(row, "latin1", "ASCII", sub=""))

```

and you can check your tweets with the `head` function.

```r
head(coffee_tweets)

```

