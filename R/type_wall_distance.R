library(tidyverse)
library(caret)
folder <-  dirname(rstudioapi::getSourceEditorContext()$path )
type.wall.distance <-read_csv(paste0(folder,"/dataset_type_wall_distance.csv"))

#Exploratory Data Analysis
head(type.wall.distance)

type.wall.distance$`DISTANCE(cm)` <-as.factor(type.wall.distance$`DISTANCE(cm)`)

prop.table(table(type.wall.distance$`DISTANCE(cm)`))

plot(type.wall.distance[1:2],pch =21
     ,bg= c("red","green")[unclass(type.wall.distance$`DISTANCE(cm)`)])
hist(type.wall.distance$INFRARED,breaks = 50)
hist(type.wall.distance$ULTRASONIC,breaks = 50)

dummy<-dummyVars(" ~ TYPE",data = type.wall.distance)

newdata <- data.frame(predict(dummy,newdata = type.wall.distance))

type.wall.distance <- cbind(type.wall.distance,newdata)



