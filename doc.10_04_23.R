library(tidyverse)
folder <- dirname(rstudioapi::getSourceEditorContext()$path)
                  parentFolder <- dirname(folder)

insurance <- read_csv(file = paste0(parentFolder
                                ,"/datasets/insurance.csv"))





pairs(insurance[c("age","bmi","children","charges")]
       ,pch=21,bg=c("red","green3","blue","orange")[unclass(insurance$region)])

library(psych)
pairs.panels(insurance[c("age","bmi","children","charges")]
             ,pch=21,bg=c("red","green3","blue","orange")[unclass(insurance$region)])

#predictors
predictors <- colnames(insurance)[-7]
predictors

set.seed(1)
sample.index <- sample(1:nrow(insurance)
                       ,0.7*nrow(insurance)
                       ,replace=F)

train.data <- insurance[sample.index,,drop=F]
test.data <- insurance[-sample.index,,drop=F]

m <- lm(charges ~ ., train.data)
m

summary(m)

predictions <- predict(m,test.data)
predictions

RMSE.df <- data.frame(predicted=predictions
                      ,actuak=test.data$charges
                      , RSE = sqrt((predictions-test.data$charges)^2))

sum(RMSE.df$RSE)/nrow(RMSE.df)

