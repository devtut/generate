---
metaTitle: "ANOVA"
description: "Basic usage of aov(), Basic usage of Anova()"
---

# ANOVA



## Basic usage of aov()


Analysis of Variance (aov) is used to determine if the means of two or more groups differ significantly from each other. Responses are assumed to be independent of each other, Normally distributed (within each group), and the within-group variances are assumed equal.

In order to complete the analysis data must be in long format (see [reshaping data](http://stackoverflow.com/documentation/r/2904/reshaping-data-between-long-and-wide-forms/) topic).
`aov()` is a wrapper around the `lm()` function, using Wilkinson-Rogers formula notation `y~f` where `y` is the response (independent) variable and `f` is a factor (categorical) variable representing group membership. **If `f` is numeric rather than a factor variable, `aov()` will report the results of a linear regression in ANOVA format, which may surprise inexperienced users.**

The `aov()` function uses Type I (sequential) Sum of Squares. This type of Sum of Squares tests all of the (main and interaction) effects sequentially. The result is that the first effect tested is also assigned shared variance between it and other effects in the model. For the results from such a model to be reliable, data should be balanced (all groups are of the same size).

When the assumptions for Type I Sum of Squares do not hold, Type II or Type III Sum of Squares may be applicable. Type II Sum of Squares test each main effect after every other main effect, and thus controls for any overlapping variance. However, Type II Sum of Squares assumes no interaction between the main effects.

Lastly, Type III Sum of Squares tests each main effect after every other main effect **and** every interaction. This makes Type III Sum of Squares a necessity when an interaction is present.

Type II and Type III Sums of Squares are implemented in the `Anova()` function.

Using the `mtcars` data set as an example.

```r
mtCarsAnovaModel <- aov(wt ~ factor(cyl), data=mtcars)

```

To view summary of ANOVA model:

```r
summary(mtCarsAnovaModel)

```

One can also extract the coefficients of the underlying `lm()` model:

```r
coefficients(mtCarsAnovaModel)

```



## Basic usage of Anova()


When dealing with an unbalanced design and/or non-orthogonal contrasts, Type II or Type III Sum of Squares are necessary. The `Anova()` function from the `car` package implements these. Type II Sum of Squares assumes no interaction between main effects. If interactions are assumed, Type III Sum of Squares is appropriate.

The `Anova()` function wraps around the `lm()` function.

Using the `mtcars` data sets as an example, demonstrating the difference between Type II and Type III when an interaction is tested.

```r
> Anova(lm(wt ~ factor(cyl)*factor(am), data=mtcars), type = 2)
Anova Table (Type II tests)

Response: wt
                       Sum Sq Df F value    Pr(>F)    
factor(cyl)            7.2278  2 11.5266 0.0002606 ***
factor(am)             3.2845  1 10.4758 0.0032895 ** 
factor(cyl):factor(am) 0.0668  2  0.1065 0.8993714    
Residuals              8.1517 26                      
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

> Anova(lm(wt ~ factor(cyl)*factor(am), data=mtcars), type = 3)
Anova Table (Type III tests)

Response: wt
                        Sum Sq Df F value    Pr(>F)    
(Intercept)            25.8427  1 82.4254 1.524e-09 ***
factor(cyl)             4.0124  2  6.3988  0.005498 ** 
factor(am)              1.7389  1  5.5463  0.026346 *  
factor(cyl):factor(am)  0.0668  2  0.1065  0.899371    
Residuals               8.1517 26                      
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

```

