library(tidyverse)
normalise <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
folder <-  dirname(rstudioapi::getSourceEditorContext()$path )
wall.distance <-read_csv(paste0(folder,"/dataset_wall_distance.csv"))

## Exploratory Data Analysis

head(wall.distance)

summary(wall.distance)

hist(wall.distance$INFRARED,breaks = 10)
hist(wall.distance$ULTRASONIC,breaks = 10)

wall.distance$`DISTANCE(cm)` <-as.factor(wall.distance$`DISTANCE(cm)`)
pairs(wall.distance[c("INFRARED","ULTRASONIC")]
      ,pch=25,bg=c("green","blue3","gray","yellow","green3","pink","brown","black","red","orange")[unclass(wall.distance$`DISTANCE(cm)`)])

prop.table(table(wall.distance$`DISTANCE(cm)`))


##Linear Regression For Infrared Sensor
x <- wall.distance$INFRARED
y <- wall.distance$`DISTANCE(cm)`
b=cov(x,y)/var(x)
a=mean(y)-b*mean(x)
a+b*121## aca va el nuevo dataset


##Linear Regression For Ultrasonic Sensor
x <- wall.distance$ULTRASONIC
y <- wall.distance$`DISTANCE(cm)`
b=cov(x,y)/var(x)
a=mean(y)-b*mean(x)
a+b*550## aca va el nuevo dataset
