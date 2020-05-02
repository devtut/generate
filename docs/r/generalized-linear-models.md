---
metaTitle: "Generalized linear models"
description: "Logistic regression on Titanic dataset"
---

# Generalized linear models



## Logistic regression on Titanic dataset


Logistic regression is a particular case of the **generalized linear model**, used to model dichotomous outcomes (**probit** and **complementary log-log** models are closely related).

The name comes from the **link function** used, the **logit** or **log-odds** function.
The inverse function of the **logit** is called the **logistic function** and is given by:

<img src="http://latex.codecogs.com/gif.latex?%5Csigma(t)=%5Cfrac%7Be%5Et%7D%7Be%5Et&plus;1%7D&space;=&space;%5Cfrac%7B1%7D%7B1&plus;e%5E%7B-t%7D%7D" title="\sigma(t)=\frac{e^t}{e^t+1} = \frac{1}{1+e^{-t}}" />

This function takes a value between **]-Inf;+Inf[** and returns a value between **0** and **1**; i.e the **logistic function** takes a linear predictor and returns a probability.

Logistic regression can be performed using the `glm` function with the option `family = binomial` (shortcut for `family = binomial(link="logit")`; the **logit** being the default link function for the binomial family).

In this example, we try to predict the fate of the passengers aboard the RMS Titanic.

Read the data:

```r
url <- "http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/titanic.txt"
titanic <- read.csv(file = url, stringsAsFactors = FALSE)

```

Clean the missing values:

In that case, we replace the missing values by an approximation, the average.

```r
titanic$age[is.na(titanic$age)] <- mean(titanic$age, na.rm = TRUE) 

```

Train the model:

```r
titanic.train <- glm(survived ~ pclass + sex + age,
                         family = binomial, data = titanic)

```

Summary of the model:

```r
summary(titanic.train)

```

The output:

```r
Call:
glm(formula = survived ~ pclass + sex + age, family = binomial, data = titanic)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.6452  -0.6641  -0.3679   0.6123   2.5615  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept)  3.552261   0.342188  10.381  < 2e-16 ***
pclass2nd   -1.170777   0.211559  -5.534 3.13e-08 ***
pclass3rd   -2.430672   0.195157 -12.455  < 2e-16 ***
sexmale     -2.463377   0.154587 -15.935  < 2e-16 ***
age         -0.042235   0.007415  -5.696 1.23e-08 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 1686.8  on 1312  degrees of freedom
Residual deviance: 1165.7  on 1308  degrees of freedom
AIC: 1175.7

Number of Fisher Scoring iterations: 5

```


<li>
The first thing displayed is the call. It is a reminder of the model and the options specified.
</li>
<li>
Next we see the deviance residuals, which are a measure of model fit. This part of output shows the distribution of the deviance residuals for individual cases used in the model.
</li>
<li>
The next part of the output shows the coefficients, their standard errors, the z-statistic (sometimes called a Wald z-statistic), and the associated p-values.
<ul>
- The qualitative variables are "dummified". A modality is considered as the reference. The reference modality can be change with `I` in the formula.
- All four predictors are statistically significant at a 0.1 % level.
- The logistic regression coefficients give the change in the log odds of the outcome for a one unit increase in the predictor variable.
- To see the **odds ratio** (multiplicative change in the odds of survival per unit increase in a predictor variable), exponentiate the parameter.
- To see the confidence interval (CI) of the parameter, use `confint`.

Below the table of coefficients are fit indices, including the null and deviance residuals and the Akaike Information Criterion (AIC), which can be used for comparing model performance.

- When comparing models fitted by maximum likelihood to the same data, the smaller the AIC, the better the fit.
- One measure of model fit is the significance of the overall model. This test asks whether the model with predictors fits significantly better than a model with just an intercept (i.e., a null model).

**Example of odds ratios:**

```r
exp(coef(titanic.train)[3])

 pclass3rd 
0.08797765 

```

With this model, compared to the first class, the 3rd class passengers have about a tenth of the odds of survival.

**Example of confidence interval for the parameters:**

```r
confint(titanic.train)

Waiting for profiling to be done...
                  2.5 %      97.5 %
(Intercept)  2.89486872  4.23734280
pclass2nd   -1.58986065 -0.75987230
pclass3rd   -2.81987935 -2.05419500
sexmale     -2.77180962 -2.16528316
age         -0.05695894 -0.02786211

```

**Exemple of calculating the significance of the overall model:**

The test statistic is distributed chi-squared with degrees of freedom equal to the differences in degrees of freedom between the current and the null model (i.e., the number of predictor variables in the model).

```r
with(titanic.train, pchisq(null.deviance - deviance, df.null - df.residual
, lower.tail = FALSE))
[1] 1.892539e-111

```

The p-value is near 0, showing a strongly significant model.

