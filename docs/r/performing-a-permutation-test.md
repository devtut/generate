---
metaTitle: "Performing a Permutation Test"
description: "A fairly general function"
---

# Performing a Permutation Test



## A fairly general function


We will use the built in [tooth growth dataset](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/ToothGrowth.html).
We are interested in whether there is a statistically significant difference in tooth growth when the guinea pigs are given vitamin C vs orange juice.

Here's the full example:

```r
teethVC = ToothGrowth[ToothGrowth$supp == 'VC',]
teethOJ = ToothGrowth[ToothGrowth$supp == 'OJ',]

permutationTest = function(vectorA, vectorB, testStat){
  N = 10^5
  fullSet = c(vectorA, vectorB)
  lengthA = length(vectorA)
  lengthB = length(vectorB)
  trials <- replicate(N, 
                      {index <- sample(lengthB + lengthA, size = lengthA, replace = FALSE)
                      testStat((fullSet[index]), fullSet[-index])  } )
  trials
}
vec1 =teethVC$len;
vec2 =teethOJ$len;
subtractMeans = function(a, b){ return (mean(a) - mean(b))}
result = permutationTest(vec1, vec2, subtractMeans)
observedMeanDifference = subtractMeans(vec1, vec2)
result = c(result, observedMeanDifference)
hist(result)
abline(v=observedMeanDifference, col = "blue")
pValue = 2*mean(result <= (observedMeanDifference))
pValue

```

After we read in the CSV, we define the function

```r
permutationTest = function(vectorA, vectorB, testStat){
  N = 10^5
  fullSet = c(vectorA, vectorB)
  lengthA = length(vectorA)
  lengthB = length(vectorB)
  trials <- replicate(N, 
                      {index <- sample(lengthB + lengthA, size = lengthA, replace = FALSE)
                      testStat((fullSet[index]), fullSet[-index])  } )
  trials
}

```

This function takes two vectors, and shuffles their contents together, then performs the function `testStat` on the shuffled vectors. The result of `teststat` is added to `trials`, which is the return value.

It does this `N = 10^5` times. Note that the value `N` could very well have been a parameter to the function.

This leaves us with a new set of data, `trials`, the set of means that might result if there truly is no relationship between the two variables.

Now to define our test statistic:

```r
subtractMeans = function(a, b){ return (mean(a) - mean(b))}

```

Perform the test:

```r
result = permutationTest(vec1, vec2, subtractMeans)

```

Calculate our actual observed mean difference:

```r
observedMeanDifference = subtractMeans(vec1, vec2)

```

Let's see what our observation looks like on a histogram of our test statistic.

```r
hist(result)
abline(v=observedMeanDifference, col = "blue")

```

[<img src="https://i.stack.imgur.com/o7gFh.png" alt="enter image description here" />](https://i.stack.imgur.com/o7gFh.png)

It doesn't **look** like our observed result is very likely to occur by random chance...

We want to calculate the p-value, the likeliehood of the original observed result if their is no relationship between the two variables.

```r
pValue = 2*mean(result >= (observedMeanDifference))

```

Let's break that down a bit:

```r
result >= (observedMeanDifference)

```

Will create a boolean vector, like:

```r
FALSE TRUE FALSE FALSE TRUE FALSE ...

```

With `TRUE` every time the value of `result` is greater than or equal to the `observedMean`.

The function `mean` will interpret this vector as `1` for `TRUE` and `0` for `FALSE`, and give us the percentage of `1`'s in the mix, ie the number of times our shuffled vector mean difference surpassed or equalled what we observed.

Finally, we multiply by 2 because the distribution of our test statistic is highly symmetric, and we really want to know which results are "more extreme" than our observed result.

All that's left is to output the p-value, which turns out to be `0.06093939`. Interpretation of this value is subjective, but I would say that it looks like Vitamin C promotes tooth growth quite a lot more than Orange Juice does.

