library(tidyverse)
normalise <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

folder <-  dirname(rstudioapi::getSourceEditorContext()$path )
wall.distance <-read_csv(paste0(folder,"/dataset_wall_distance.csv"))

head(wall.distance)

wall.distance$`DISTANCE(cm)` <-
  as.factor(wall.distance$`DISTANCE(cm)`)
pairs(wall.distance[c("INFRARED","ULTRASONIC")]
      ,pch=21,bg=c("green","blue3","gray","yellow","green3","pink","brown","black","red","orange")[unclass(wall.distance$`DISTANCE(cm)`)])

prop.table(table(wall.distance$`DISTANCE(cm)`))



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






