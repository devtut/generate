---
metaTitle: "Random Forest Algorithm"
description: "Basic examples - Classification and Regression"
---

# Random Forest Algorithm




## Basic examples - Classification and Regression


```

   ######  Used for both Classification and Regression examples
    library(randomForest)
    library(car)            ## For the Soils data
    data(Soils)
    
    ######################################################
    ##    RF Classification Example
    set.seed(656)            ## for reproducibility
    S_RF_Class = randomForest(Gp ~ ., data=Soils[,c(4,6:14)])
    Gp_RF = predict(S_RF_Class, Soils[,6:14])
    length(which(Gp_RF != Soils$Gp))            ## No Errors

    ## Naive Bayes for comparison
    library(e1071)
    S_NB  = naiveBayes(Soils[,6:14], Soils[,4]) 
    Gp_NB = predict(S_NB, Soils[,6:14], type="class")
    length(which(Gp_NB != Soils$Gp))            ## 6 Errors

```

This example tested on the training data, but illustrates that RF can make very good models.

```

   ######################################################
    ##    RF Regression Example
    set.seed(656)            ## for reproducibility
    S_RF_Reg = randomForest(pH ~ ., data=Soils[,6:14])
    pH_RF = predict(S_RF_Reg, Soils[,6:14])

    ## Compare Predictions with Actual values for RF and Linear Model
    S_LM = lm(pH ~ ., data=Soils[,6:14])
    pH_LM = predict(S_LM, Soils[,6:14])
    par(mfrow=c(1,2))
    plot(Soils$pH, pH_RF, pch=20, ylab="Predicted", main="Random Forest")
    abline(0,1)
    plot(Soils$pH, pH_LM, pch=20, ylab="Predicted", main="Linear Model")
    abline(0,1)

```

[<img src="https://i.stack.imgur.com/ieM8R.png" alt="Predicted Values vs Actuals for RF and Linear model" />](https://i.stack.imgur.com/ieM8R.png)

