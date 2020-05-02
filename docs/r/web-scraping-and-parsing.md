---
metaTitle: "Web scraping and parsing"
description: "Basic scraping with rvest, Using rvest when login is required"
---

# Web scraping and parsing



## Basic scraping with rvest


[`rvest`](https://github.com/hadley/rvest) is a package for web scraping and parsing by Hadley Wickham inspired by Python's [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/). It leverages Hadley's [`xml2`](https://github.com/hadley/xml2) package's [`libxml2`](http://xmlsoft.org/) bindings for HTML parsing.

As part of the tidyverse, `rvest` is [piped](http://stackoverflow.com/documentation/r/652/pipe-operators-and-others). It uses

- `xml2::read_html` to scrape the HTML of a webpage,
- which can then be subset with its `html_node` and `html_nodes` functions using CSS or XPath selectors, and
- parsed to R objects with functions like `html_text` and `html_table`.

To scrape the table of milestones from [the Wikipedia page on R](https://en.wikipedia.org/wiki/R_(programming_language)), the code would look like

```r
library(rvest)

url <- 'https://en.wikipedia.org/wiki/R_(programming_language)'

        # scrape HTML from website
url %>% read_html() %>% 
    # select HTML tag with class="wikitable"
    html_node(css = '.wikitable') %>% 
    # parse table into data.frame
    html_table() %>%
    # trim for printing
    dplyr::mutate(Description = substr(Description, 1, 70))

##    Release       Date                                                  Description
## 1     0.16            This is the last alpha version developed primarily by Ihaka 
## 2     0.49 1997-04-23 This is the oldest source release which is currently availab
## 3     0.60 1997-12-05 R becomes an official part of the GNU Project. The code is h
## 4   0.65.1 1999-10-07 First versions of update.packages and install.packages funct
## 5      1.0 2000-02-29 Considered by its developers stable enough for production us
## 6      1.4 2001-12-19 S4 methods are introduced and the first version for Mac OS X
## 7      2.0 2004-10-04 Introduced lazy loading, which enables fast loading of data 
## 8      2.1 2005-04-18 Support for UTF-8 encoding, and the beginnings of internatio
## 9     2.11 2010-04-22                          Support for Windows 64 bit systems.
## 10    2.13 2011-04-14 Adding a new compiler function that allows speeding up funct
## 11    2.14 2011-10-31 Added mandatory namespaces for packages. Added a new paralle
## 12    2.15 2012-03-30 New load balancing functions. Improved serialization speed f
## 13     3.0 2013-04-03 Support for numeric index values 231 and larger on 64 bit sy

```

While this returns a data.frame, note that as is typical for scraped data, there is still further data cleaning to be done: here, formatting dates, inserting `NA`s, and so on.

Note that data in a less consistently rectangular format may take looping or other further munging to successfully parse. If the website makes use of jQuery or other means to insert content, `read_html` may be insufficient to scrape, and a more robust scraper like `RSelenium` may be necessary.



## Using rvest when login is required


I common problem encounter when scrapping a web is how to enter a userid and password to log into a web site.

In this example which I created to track my answers posted here to stack overflow.  The overall flow is to login, go to a web page collect information, add it a dataframe and then move to the next page.

```r
library(rvest) 

#Address of the login webpage
login<-"https://stackoverflow.com/users/login?ssrc=head&returnurl=http%3a%2f%2fstackoverflow.com%2f"

#create a web session with the desired login address
pgsession<-html_session(login)
pgform<-html_form(pgsession)[[2]]  #in this case the submit is the 2nd form
filled_form<-set_values(pgform, email="*****", password="*****")
submit_form(pgsession, filled_form)

#pre allocate the final results dataframe.
results<-data.frame()  

#loop through all of the pages with the desired info
for (i in 1:5)
{
  #base address of the pages to extract information from
  url<-"http://stackoverflow.com/users/**********?tab=answers&sort=activity&page="
  url<-paste0(url, i)
  page<-jump_to(pgsession, url)

  #collect info on the question votes and question title
  summary<-html_nodes(page, "div .answer-summary")
  question<-matrix(html_text(html_nodes(summary, "div"), trim=TRUE), ncol=2, byrow = TRUE)

  #find date answered, hyperlink and whether it was accepted
  dateans<-html_node(summary, "span") %>% html_attr("title")
  hyperlink<-html_node(summary, "div a") %>% html_attr("href")
  accepted<-html_node(summary, "div") %>% html_attr("class")

  #create temp results then bind to final results 
  rtemp<-cbind(question, dateans, accepted, hyperlink)
  results<-rbind(results, rtemp)
}

#Dataframe Clean-up
names(results)<-c("Votes", "Answer", "Date", "Accepted", "HyperLink")
results$Votes<-as.integer(as.character(results$Votes))
results$Accepted<-ifelse(results$Accepted=="answer-votes default", 0, 1)

```

The loop in this case is limited to only 5 pages, this needs to change to fit your application. I replaced the user specific values with ******, hopefully this will provide some guidance for you problem.



#### Remarks


**Scraping** refers to using a computer to retrieve the code of a webpage. Once the code is obtained, it must be **parsed** into a useful form for further use in R.

Base R does not have many of the tools required for these processes, so scraping and parsing are typically done with packages. Some packages are most useful for scraping (`RSelenium`, `httr`, `curl`, `RCurl`), some for parsing (`XML`, `xml2`), and some for both (`rvest`).

A related process is scraping a web API, which unlike a webpage returns data intended to be machine-readable. Many of the same packages are used for both.

### Legality

Some websites object to being scraped, whether due to increased server loads or concerns about data ownership. If a website forbids scraping in it Terms of Use, scraping it is illegal.

