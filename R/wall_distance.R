library(tidyverse)
normalise <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

folder <-  dirname(rstudioapi::getSourceEditorContext()$path )
parentFolder <- dirname(folder)
Social_Network_Ads <- read_csv(paste0(parentFolder ,"/R/dataset_wall_distance.csv"))
