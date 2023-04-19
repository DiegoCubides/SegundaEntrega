library(tidyverse)
normalise <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

folder <-  dirname(rstudioapi::getSourceEditorContext()$path )
wall.distance <-read_csv(paste0(folder,"/dataset_wall_distance.csv"))

head(wall.distance)

wall.distance$`DISTANCE(cm)` <-
  as.factor(wall.distance$`DISTANCE(cm)`)

plot(wall.distance[1:2],pch =21
     ,bg= c("red","green")[unclass(wall.distance$`DISTANCE(cm)`)])

prop.table(table(wall.distance$`DISTANCE(cm)`))
