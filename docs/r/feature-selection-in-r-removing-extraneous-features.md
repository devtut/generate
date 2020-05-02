---
metaTitle: "Feature Selection in R -- Removing Extraneous Features"
description: "Removing features with zero or near-zero variance, Removing features with high numbers of NA, Removing closely correlated features"
---

# Feature Selection in R -- Removing Extraneous Features



## Removing features with zero or near-zero variance


A feature that has near zero variance is a good candidate for removal.

You can manually detect numerical variance below your own threshold:

```r
data("GermanCredit")
variances<-apply(GermanCredit, 2, var)
variances[which(variances<=0.0025)]

```

Or, you can use the caret package to find near zero variance.  An advantage here is that is defines near zero variance not in the numerical calculation of variance, but rather as a function of rarity:

> 
<p>"nearZeroVar diagnoses predictors that have one unique value (i.e. are
zero variance predictors) or predictors that are have both of the
following characteristics: they have very few unique values relative
to the number of samples and the ratio of the frequency of the most
common value to the frequency of the second most common value is large..."</p>


```r
library(caret)
names(GermanCredit)[nearZeroVar(GermanCredit)]

```



## Removing features with high numbers of NA


If a feature is largely lacking data, it is a good candidate for removal:

```r
library(VIM)
data(sleep)
colMeans(is.na(sleep))

   BodyWgt   BrainWgt       NonD      Dream      Sleep       Span       Gest 
0.00000000 0.00000000 0.22580645 0.19354839 0.06451613 0.06451613 0.06451613 
      Pred        Exp     Danger 
0.00000000 0.00000000 0.00000000 

```

In this case, we may want to remove NonD and Dream, which each have around 20% missing values (your cutoff may vary)



## Removing closely correlated features


Closely correlated features may add variance to your model, and removing one of a correlated pair might help reduce that. There are lots of ways to detect correlation.  Here's one:

```r
library(purrr) # in order to use keep()

# select correlatable vars
toCorrelate<-mtcars %>% keep(is.numeric)

# calculate correlation matrix
correlationMatrix <- cor(toCorrelate)

# pick only one out of each highly correlated pair's mirror image
correlationMatrix[upper.tri(correlationMatrix)]<-0  

# and I don't remove the highly-correlated-with-itself group
diag(correlationMatrix)<-0 

# find features that are highly correlated with another feature at the +- 0.85 level
apply(correlationMatrix,2, function(x) any(abs(x)>=0.85))

  mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb 
 TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE 

```

I'll want to look at what MPG is correlated to so strongly, and decide what to keep and what to toss.  Same for cyl and disp.  Alternatively, I might need to combine some strongly correlated features.

