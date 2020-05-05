---
metaTitle: "R - Using texreg to export models in a paper-ready way"
description: "Printing linear regression results"
---

# Using texreg to export models in a paper-ready way


The texreg package helps to export a model (or several models) in a neat paper-ready way. The result may be exported as HTML or .doc (MS Office Word).



## Printing linear regression results


```r
# models
fit1 <- lm(mpg ~ wt, data = mtcars)
fit2 <- lm(mpg ~ wt+hp, data = mtcars)
fit3 <- lm(mpg ~ wt+hp+cyl, data = mtcars)

# export to html
texreg::htmlreg(list(fit1,fit2,fit3),file='models.html')


# export to doc
texreg::htmlreg(list(fit1,fit2,fit3),file='models.doc')

```

The result looks like a table in a paper.

[<img src="https://i.stack.imgur.com/M8GyJ.png" alt="enter image description here" />](https://i.stack.imgur.com/M8GyJ.png)

There are several additional handy parameters in `texreg::htmlreg()` function. Here is a use case for the most helpful parameters.

```r
# export to html
texreg::htmlreg(list(fit1,fit2,fit3),file='models.html',
                single.row = T,
                custom.model.names = LETTERS[1:3],
                leading.zero = F,
                digits = 3)

```

Which result in a table like this

[<img src="https://i.stack.imgur.com/wMN7q.png" alt="enter image description here" />](https://i.stack.imgur.com/wMN7q.png)



#### Remarks


### Links

- [CRAN page](https://cran.r-project.org/package=texreg)

