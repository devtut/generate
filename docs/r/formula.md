---
metaTitle: "R - Formula"
description: "The basics of formula, Create Linear, Quadratic and Second Order Interaction Terms"
---

# Formula



## The basics of formula


Statistical functions in R make heavy use of the so-called Wilkinson-Rogers formula notation<sup>1</sup> .

When running model functions like `lm` for the [Linear Regressions](http://stackoverflow.com/documentation/r/801/linear-regression), they need a `formula`. This `formula` specifies which regression coefficients shall be estimated.

```r
my_formula1 <- formula(mpg ~ wt)
class(my_formula1) 
# gives "formula"

mod1 <- lm(my_formula1, data = mtcars)
coef(mod1)
# gives (Intercept)          wt 
#         37.285126   -5.344472 

```

On the left side of the `~` (LHS) the dependent variable is specified, while the right hand side (RHS) contains the independent variables. Technically the `formula` call above is redundant because the tilde-operator is an infix function that returns an object with formula class:

```r
form <- mpg ~ wt
class(form)
#[1] "formula"

```

The advantage of the `formula` function over `~` is that it also allows an environment for evaluation to be specified:

```r
form_mt <- formula(mpg ~ wt, env = mtcars)

```

In this case, the output shows that a regression coefficient for `wt` is estimated, as well as (per default) an intercept parameter. The intercept can be excluded / forced to be 0 by including `0` or `-1` in the `formula`:

```r
coef(lm(mpg ~ 0 + wt, data = mtcars))
coef(lm(mpg ~ wt -1, data = mtcars))

```

Interactions between variables `a` and `b` can added by included `a:b` to the `formula`:

```

coef(lm(mpg ~ wt:vs, data = mtcars))

```

As it is (from a statistical point of view) generally advisable not have interactions in the model without the main effects, the naive approach would be to expand the `formula` to `a + b + a:b`. This works but can be simplified by writing `a*b`, where the `*` operator indicates factor crossing (when between two factor columns) or multiplication when one or both of the columns are 'numeric':

```r
coef(lm(mpg ~ wt*vs, data = mtcars))

```

Using the `*` notation expands a term to include all lower order effects, such that:

```r
coef(lm(mpg ~ wt*vs*hp, data = mtcars))

```

will give, in addition to the intercept, 7 regression coefficients. One for the three-way interaction, three for the two-way interactions and three for the main effects.

If one wants, for example, to exclude the three-way interaction, but retain all two-way interactions there are two shorthands. First, using `-` we can subtract any particular term:

```r
coef(lm(mpg ~ wt*vs*hp - wt:vs:hp, data = mtcars))

```

Or, we can use the `^` notation to specify which level of interaction we require:

```r
coef(lm(mpg ~ (wt + vs + hp) ^ 2, data = mtcars))

```

Those two formula specifications should create the same model matrix.

Finally, `.` is shorthand to use all available variables as main effects. In this case, the `data` argument is used to obtain the available variables (which are not on the LHS). Therefore:

```r
coef(lm(mpg ~ ., data = mtcars))

```

gives coefficients for the intercept and 10 independent variables. This notation is frequently used in machine learning packages, where one would like to use all variables for prediction or classification. Note that the meaning of `.` depends on context (see e.g. `?update.formula` for a different meaning).

1. G. N. Wilkinson and C. E. Rogers. **Journal of the Royal Statistical Society. Series C (Applied Statistics)** Vol. 22, No. 3 (1973), pp. 392-399



## Create Linear, Quadratic and Second Order Interaction Terms


**`y ~ .`** : Here `.` is interpreted as all variables except `y` in the data frame used in fitting the model. It is equivalent to the linear combinations of predictor variables. For example `y ~ var1 + var2 + var3+...+var15`

**`y ~ . ^ 2`** will give all linear (main effects) and second order interaction terms of the variables in the data frame. It is equivalent to `y ~ var1 + var2 + ...+var15 + var1:var2 + var1:var3 + var1:var4...and so on`

**`y ~ var1 + var2 + ...+var15 + I(var1^2) + I(var2^2) + I(var3^2)...+I(var15^2)`** : Here `I(var^2)` indicates quadratic polynomial of one variable in the data frame.

**`y ~ poly(var1, degree = 2) + poly(var2, degree = 2)+...poly(var15, degree = 2)`**

or

**`y ~ poly(var1, var2, var3, ....var15, degree = 2)`** will be equivalent to the above expression.

`poly(var1, degree = 2)` is equivalent to `var1 + I(var1^2)`.

To get cubic polynomials, use `degree = 3` in `poly()`.

There is a caveat in using `poly` versus `I(var, 2)`, which is after fitting the model, each of them will produce different coefficients, but the fitted values are equivalent, because they represent different parameterizations of the same model. It is recommended to use `I(var, 2)` over `poly()` to avoid the summary effect seen in `poly()`.

In summary, to get linear, quadratic and second order interaction terms, you will have an expression like

**`y ~ .^2 + I(var1^2) + I(var2^2)+...I(var15^2)`**

**Demo for four variables:**

```r
old <- reformulate( 'y ~ x1+x2+x3+x4' )
new <- reformulate( " y ~ .^2 + I(x1^2) + I(x2^2) + I(x3^2) + I(x4^2) ")
tmp <- .Call(stats:::C_updateform, old, new)
terms.formula(tmp, simplify = TRUE )

# ~y ~ x1 + x2 + x3 + x4 + I(x1^2) + I(x2^2) + I(x3^2) + I(x4^2) + 
#   x1:x2 + x1:x3 + x1:x4 + x2:x3 + x2:x4 + x3:x4
# attr(,"variables")
# list(~y, x1, x2, x3, x4, I(x1^2), I(x2^2), I(x3^2), I(x4^2))
# attr(,"factors")
#         x1 x2 x3 x4 I(x1^2) I(x2^2) I(x3^2) I(x4^2) x1:x2 x1:x3 x1:x4 x2:x3 x2:x4 x3:x4
# ~y       0  0  0  0       0       0       0       0     0     0     0     0     0     0
# x1       1  0  0  0       0       0       0       0     1     1     1     0     0     0
# x2       0  1  0  0       0       0       0       0     1     0     0     1     1     0
# x3       0  0  1  0       0       0       0       0     0     1     0     1     0     1
# x4       0  0  0  1       0       0       0       0     0     0     1     0     1     1
# I(x1^2)  0  0  0  0       1       0       0       0     0     0     0     0     0     0
# I(x2^2)  0  0  0  0       0       1       0       0     0     0     0     0     0     0
# I(x3^2)  0  0  0  0       0       0       1       0     0     0     0     0     0     0
# I(x4^2)  0  0  0  0       0       0       0       1     0     0     0     0     0     0
# attr(,"term.labels")
# [1] "x1"      "x2"      "x3"      "x4"      "I(x1^2)" "I(x2^2)" "I(x3^2)" "I(x4^2)"
# [9] "x1:x2"   "x1:x3"   "x1:x4"   "x2:x3"   "x2:x4"   "x3:x4"  
# attr(,"order")
# [1] 1 1 1 1 1 1 1 1 2 2 2 2 2 2
# attr(,"intercept")
# [1] 1
# attr(,"response")
# [1] 1
# attr(,".Environment")
# <environment: R_GlobalEnv>

```

