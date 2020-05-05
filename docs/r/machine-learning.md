---
metaTitle: "Machine learning"
description: "Creating a Random Forest model"
---

# Machine learning




## Creating a Random Forest model


One example of machine learning algorithms is the Random Forest alogrithm (Breiman, L. (2001). Random Forests. **Machine Learning 45(5)**, p. 5-32). This algorithm is implemented in R according to Breiman's original Fortran implementation in the `randomForest` package.

Random Forest classifier objects can be created in R by preparing the class variable as `factor`, which is already apparent in the `iris` data set. Therefore we can easily create a Random Forest by:

```r
library(randomForest)

rf <- randomForest(x = iris[, 1:4], 
                   y = iris$Species, 
                   ntree = 500, 
                   do.trace = 100)

rf

# Call:
#   randomForest(x = iris[, 1:4], y = iris$Species, ntree = 500,      do.trace = 100) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 2
# 
# OOB estimate of  error rate: 4%
# Confusion matrix:
#   setosa versicolor virginica class.error
# setosa         50          0         0        0.00
# versicolor      0         47         3        0.06
# virginica       0          3        47        0.06

```

|parameters|Description
|---|---|---|---
|x|a data frame holding the describing variables of the classes
|y|the classes of the individual obserbations. If this vector is `factor`, a classification model is created, if not a regression model is created.
|ntree|The number of individual CART trees built
|do.trace|every i**th** step, the out-of-the-box errors overall and for  each class are returned

