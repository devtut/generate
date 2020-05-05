---
metaTitle: "PHP - Machine learning"
description: "Classification using PHP-ML, Regression, Clustering"
---

# Machine learning



## Classification using PHP-ML


Classification in Machine Learning is the problem that identifies to which set of categories does a new observation belong. Classification falls under the category of `Supervised Machine Learning`.

> 
Any algorithm that implements classification is known as **classifier**


The classifiers supported in PHP-ML are

- SVC (Support Vector Classification)
- k-Nearest Neighbors
- Naive Bayes

The `train` and `predict` method are same for all classifiers. The only difference would be in the underlying algorithm used.

### SVC (Support Vector Classification)

Before we can start with predicting a new observation, we need to train our classifier. Consider the following code

```php
// Import library
use Phpml\Classification\SVC;
use Phpml\SupportVectorMachine\Kernel;

// Data for training classifier
$samples = [[1, 3], [1, 4], [2, 4], [3, 1], [4, 1], [4, 2]];  // Training samples
$labels = ['a', 'a', 'a', 'b', 'b', 'b'];

// Initialize the classifier
$classifier = new SVC(Kernel::LINEAR, $cost = 1000);
// Train the classifier
$classifier->train($samples, $labels);

```

The code is pretty straight forward. `$cost` used above is a measure of how much we want to avoid misclassifying each training example. For a smaller value of `$cost` you might get misclassified examples. By default it is set to **1.0**

Now that we have the classifier trained we can start making some actual predictions. Consider the following codes that we have for predictions

```php
$classifier->predict([3, 2]); // return 'b'
$classifier->predict([[3, 2], [1, 5]]); // return ['b', 'a']

```

The classifier in the case above can take unclassified samples and predicts there labels. `predict` method can take a single sample as well as an array of samples.

### k-Nearest Neighbors

The classfier for this algorithm takes in two parameters and can be initialized like

```php
$classifier = new KNearestNeighbors($neighbor_num=4);
$classifier = new KNearestNeighbors($neighbor_num=3, new Minkowski($lambda=4));

```

`$neighbor_num` is the number of nearest neighbours to scan in [knn](https://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm) algorithm while the second parameter is distance metric which by default in first case would be `Euclidean`. More on Minkowski can be found [here](https://en.wikipedia.org/wiki/Minkowski_distance).

Following is a short example on how to use this classifier

```php
// Training data
$samples = [[1, 3], [1, 4], [2, 4], [3, 1], [4, 1], [4, 2]];
$labels = ['a', 'a', 'a', 'b', 'b', 'b'];

// Initialize classifier
$classifier = new KNearestNeighbors();
// Train classifier
$classifier->train($samples, $labels);

// Make predictions
$classifier->predict([3, 2]); // return 'b'
$classifier->predict([[3, 2], [1, 5]]); // return ['b', 'a']

```

### NaiveBayes Classifier

`NaiveBayes Classifier` is based on `Bayes' theorem` and does not need any parameters in constructor.

The following code demonstrates a simple prediction implementation

```php
// Training data
$samples = [[5, 1, 1], [1, 5, 1], [1, 1, 5]];
$labels = ['a', 'b', 'c'];

// Initialize classifier
$classifier = new NaiveBayes();
// Train classifier
$classifier->train($samples, $labels);

// Make predictions
$classifier->predict([3, 1, 1]); // return 'a'
$classifier->predict([[3, 1, 1], [1, 4, 1]); // return ['a', 'b']

```

### Practical case

Till now we only used arrays of integer in all our case but that is not the case in real life. Therefore let me try to describe a practical situation on how to use classifiers.

> 
Suppose you have an application that stores characteristics of flowers in nature. For the sake of simplicity we can consider the color and length of petals. So there two characteristics would be used to train our data. `color` is the simpler one where you can assign an int value to each of them and for length, you can have a range like `(0 mm,10 mm)=1 , (10 mm,20 mm)=2`. With the initial data train your classifier. Now one of your user needs identify the kind of flower that grows in his backyard. What he does is select the `color` of the flower and adds the length of the petals. You classifier running can detect the type of flower ("Labels in example above")




## Regression


In classification using `PHP-ML` we assigned labels to new observation. Regression is almost the same with difference being that the output value is not a class label but a continuous value. It is widely used for predictions and forecasting. PHP-ML supports the following regression algorithms

- Support Vector Regression
- LeastSquares Linear Regression

Regression has the same `train` and `predict` methods as used in classification.

### Support Vector Regression

This is the regression version for SVM(Support Vector Machine).The first step like in classification is to train our model.

```php
// Import library
use Phpml\Regression\SVR;
use Phpml\SupportVectorMachine\Kernel;

// Training data
$samples = [[60], [61], [62], [63], [65]];
$targets = [3.1, 3.6, 3.8, 4, 4.1];

// Initialize regression engine
$regression = new SVR(Kernel::LINEAR);
// Train regression engine
$regression->train($samples, $targets);

```

In regression `$targets` are not class labels as opposed to classification. This is one of the differentiating factor for the two. After training our model with the data we can start with the actual predictions

```php
$regression->predict([64])  // return 4.03

```

Note that the predictions return a value outside the target.

### LeastSquares Linear Regression

This algorithm uses `least squares method` to approximate solution. The following demonstrates a simple code of training and predicting

```php
// Training data
$samples = [[60], [61], [62], [63], [65]];
$targets = [3.1, 3.6, 3.8, 4, 4.1];

// Initialize regression engine
$regression = new LeastSquares();
// Train engine
$regression->train($samples, $targets);
// Predict using trained engine
$regression->predict([64]); // return 4.06

```

PHP-ML also provides with the option of `Multiple Linear Regression`. A sample code for the same can be as follows

```php
$samples = [[73676, 1996], [77006, 1998], [10565, 2000], [146088, 1995], [15000, 2001], [65940, 2000], [9300, 2000], [93739, 1996], [153260, 1994], [17764, 2002], [57000, 1998], [15000, 2000]];
$targets = [2000, 2750, 15500, 960, 4400, 8800, 7100, 2550, 1025, 5900, 4600, 4400];

$regression = new LeastSquares();
$regression->train($samples, $targets);
$regression->predict([60000, 1996]) // return 4094.82

```

`Multiple Linear Regression` is particularly useful when multiple factors or traits identify the outcome.

### Practical case

Now let us take an application of regression in real life scenario.

> 
Suppose you run a very popular website, but the traffic keeps on changing. You want a solution that would predict the number of servers you need to deploy at any given instance of time. Lets assume for the sake that your hosting provider gives you an api to spawn out servers and each server takes 15 minutes to boot. Based on previous data of traffic, and regression you can predict the traffic that would hit your application at any instance of time. Using that knowledge you can start a server 15 minutes before the surge thereby preventing your application from going offline.




## Clustering


Clustering is about grouping similar objects together. It is widely used for pattern recognition. `Clustering` comes under `unsupervised machine learning`, therefore there is no training needed.  PHP-ML has support for the following clustering algorithms

- k-Means
- dbscan

### k-Means

k-Means separates the data into `n` groups of equal variance. This means that we need to pass in a number `n` which would be the number of clusters we need in our solution. The following code will help bring more clarity

```php
// Our data set
$samples = [[1, 1], [8, 7], [1, 2], [7, 8], [2, 1], [8, 9]];

// Initialize clustering with parameter `n`
$kmeans = new KMeans(3);
$kmeans->cluster($samples); // return [0=>[[7, 8]], 1=>[[8, 7]], 2=>[[1,1]]]

```

Note that the output contains 3 arrays because because that was the value of `n` in `KMeans` constructor. There can also be an optional second parameter in the constructor which would be the `initialization method`. For example consider

```php
$kmeans = new KMeans(4, KMeans::INIT_RANDOM);

```

`INIT_RANDOM` places a completely random centroid while trying to determine the clusters. But just to avoid the centroid being too far away from the data, it is bound by the space boundaries of data.

The default constructor `initialization method` is [kmeans++](https://en.wikipedia.org/wiki/K-means%2B%2B) which selects centroid in a smart way to speed up the process.

### DBSCAN

As opposed to `KMeans`, `DBSCAN` is a density based clustering algorithm which means that we would not be passing `n` which would determine the number of clusters we want in our result. On the other hand this requires two parameters to work

1. **$minSamples :** The minimum number of objects that should be present in a cluster
1. **$epsilon :** Which is the maximum distance between two samples for them to be considered as in the same cluster.

A quick sample for the same is as follows

```php
// Our sample data set
$samples = [[1, 1], [8, 7], [1, 2], [7, 8], [2, 1], [8, 9]];

$dbscan = new DBSCAN($epsilon = 2, $minSamples = 3);
$dbscan->cluster($samples); // return [0=>[[1, 1]], 1=>[[8, 7]]]

```

The code is pretty much self explanatory. One major difference is that there is no way of knowing the number of elements in output array as opposed to KMeans.

### Practical Case

Let us now have a look on using clustering in real life scenario

> 
Clustering is widely used in `pattern recognition` and `data mining`. Consider that you have a content publishing application. Now in order to retain your users they should look at content that they love. Let us assume for the sake of simplicity that if they are on a specific webpage for more that a minute and they scoll to bottom then they love that content. Now each of your content will be having a unique identifier with it and so will the user. Make cluster based on that and you will get to know which segment of users have a similar content taste. This in turn could be used in recommendation system where you can assume that if some users of same cluster love the article then so will others and that can be shown as recommendations on your application.




#### Remarks


The topic uses PHP-ML for all machine learning algorithms. The installation of the library can be done using

```php
composer require php-ai/php-ml

```

The github repository for the same can be found [here](https://github.com/php-ai/php-ml).

Also it is worth noting that the examples given are very small data-set only for the purpose of demonstration. The actual data-set should be more comprehensive than that.

