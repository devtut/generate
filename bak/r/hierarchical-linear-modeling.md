---
metaTitle: "R - Hierarchical Linear Modeling"
description: "basic model fitting"
---

# Hierarchical Linear Modeling



## basic model fitting


**apologies**: **since I don't know of a channel for discussing/providing feedback on requests for improvement, I'm going to put my question here. Please feel free to point out a better place for this!** @DataTx states that this is "completely unclear, incomplete, or has severe formatting problems".  Since I don't see any big formatting problems (:-) ), a little bit more guidance about what's expected here for improving clarity or completeness, and why what's here is unsalvageable, would be useful.

The primary packages for fitting hierarchical (alternatively "mixed" or "multilevel") linear models in R are `nlme` (older) and `lme4` (newer). These packages differ in many minor ways but should generally result in very similar fitted models.

```r
library(nlme)
library(lme4)
m1.nlme <- lme(Reaction~Days,random=~Days|Subject,data=sleepstudy,method="REML")
m1.lme4 <- lmer(Reaction~Days+(Days|Subject),data=sleepstudy,REML=TRUE)
all.equal(fixef(m1.nlme),fixef(m1.lme4))
## [1] TRUE

```

Differences to consider:

- formula syntax is slightly different
- `nlme` is (still) somewhat better documented (e.g. Pinheiro and Bates 2000 **Mixed-effects models in S-PLUS**; however, see Bates **et al.** 2015 **Journal of Statistical Software**/`vignette("lmer",package="lme4")` for `lme4`)
- `lme4` is faster and allows easier fitting of crossed random effects
- `nlme` provides p-values for linear mixed models out of the box, `lme4` requires add-on packages such as `lmerTest` or `afex`
- `nlme` allows modeling of heteroscedasticity or residual correlations (in space/time/phylogeny)

The unofficial [GLMM FAQ](http://tinyurl.com/glmmFAQ.html) provides more information, although it is focused on **generalized** linear mixed models (GLMMs).

