library(tidyverse)
folder <-  dirname(rstudioapi::getSourceEditorContext()$path )
wall.distance <-read_csv(paste0(folder,"/dataset_wall_distance.csv"))
wall.distance2 <- read_csv(paste0(folder,"/dataset_wall_distance.csv"))
#analisis exploratorio de datos
summary(wall.distance2)
hist(wall.distance2$INFRARED,breaks = 10)
hist(wall.distance2$ULTRASONIC,breaks = 10)
wall.distance2$`DISTANCE(cm)` <- as.factor(wall.distance2$`DISTANCE(cm)`)
pairs(wall.distance2[c("INFRARED","ULTRASONIC")]
      ,pch=21,bg=c("green","blue3","gray","yellow","green3","pink","brown","black","red","orange")[unclass(wall.distance2$`DISTANCE(cm)`)])
library(psych)
pairs.panels(wall.distance2[c("INFRARED","ULTRASONIC")]
             ,pch=21,bg=c("green","blue3","gray","yellow","green3","pink","brown","black","red","orange")[unclass(wall.distance2$`DISTANCE(cm)`)])
##multilineal
predictors <- c( "INFRARED","ULTRASONIC")
sample.index <- sample(1:nrow(wall.distance)
                       ,nrow(wall.distance)*0.7
                       ,replace = F)
train.data  <-  wall.distance[sample.index
                                   ,c(predictors,"DISTANCE(cm)")
                                   ,drop=F]
test.data  <-  wall.distance[-sample.index
                                  ,c(predictors,"DISTANCE(cm)")
                                  ,drop=F]

model<- lm(`DISTANCE(cm)` ~ INFRARED + ULTRASONIC,train.data)
summary(model)
predictions <- predict(model,test.data)

RMSE.df <- data.frame(predicted = predictions
                      ,reales=test.data$`DISTANCE(cm)`
                      ,RSE = sqrt((predictions - test.data$`DISTANCE(cm)`)^2))
promedio_error <- sum(RMSE.df$RSE)/nrow(RMSE.df )




