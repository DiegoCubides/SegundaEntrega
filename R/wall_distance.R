library(tidyverse)
normalise <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

folder <-  dirname(rstudioapi::getSourceEditorContext()$path )
wall.distance <-read_csv(paste0(folder,"/dataset_wall_distance.csv"))

##regresion lineal para sensor infrarojo
x <- wall.distance$INFRARED
y <- wall.distance$`DISTANCE(cm)`
b=cov(x,y)/var(x)
a=mean(y)-b*mean(x)
a+b*121


##regresion lineal para sensor ultrasonico
x <- wall.distance$ULTRASONIC
y <- wall.distance$`DISTANCE(cm)`
b=cov(x,y)/var(x)
a=mean(y)-b*mean(x)
a+b*126




head(wall.distance)

wall.distance$`DISTANCE(cm)` <-
  as.factor(wall.distance$`DISTANCE(cm)`)

plot(wall.distance[1:2],pch =21
     ,bg= c("red","green")[unclass(wall.distance$`DISTANCE(cm)`)])

prop.table(table(wall.distance$`DISTANCE(cm)`))
