---
metaTitle: "Probability Distributions with R"
description: "PDF and PMF for different distributions in R"
---

# Probability Distributions with R



## PDF and PMF for different distributions in R


**PMF FOR THE BINOMIAL DISTRIBUTION**

Suppose that a fair die is rolled 10 times. What is the probability of throwing exactly two sixes?

You can answer the question using the dbinom function:

```r
> dbinom(2, 10, 1/6)
[1] 0.29071

```

**PMF FOR THE POISSON DISTRIBUTION**

The number of sandwhich ordered in a restaurant on a given day is known to follow a Poisson distribution with a
mean of 20. What is the probability that exactly eighteen sandwhich will be ordered tomorrow?

You can answer the question with the dpois function:

```r
> dpois(18, 20)
[1] 0.08439355

```

**PDF FOR THE NORMAL DISTRIBUTION**

To find the value of the pdf at x=2.5 for a normal distribution with a mean of 5 and a standard deviation of 2, use
the command:

```r
> dnorm(2.5, mean=5, sd=2)
[1] 0.09132454

```

