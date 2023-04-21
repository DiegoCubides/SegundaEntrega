library(tidyverse)
library(caret)
folder <-  dirname(rstudioapi::getSourceEditorContext()$path )
type.wall.distance <-read_csv(paste0(folder,"/dataset_type_wall_distance.csv"))
prueba <- read_csv(paste0(folder,"/type_wall_distance_prueba.csv"))
#Exploratory Data Analysis
head(type.wall.distance)
hist(type.wall.distance$INFRARED,breaks = 50)
hist(type.wall.distance$ULTRASONIC,breaks = 50)

type.wall.distance$TYPE <-as.factor(type.wall.distance$TYPE)

prop.table(table(type.wall.distance$`DISTANCE(cm)`))

plot(type.wall.distance[1:2]
     ,pch=21,bg=c("green","blue3","yellow")[unclass(type.wall.distance$TYPE)])
library(psych)
pairs.panels(type.wall.distance[1:2]
             ,pch=21,bg=c("green","blue3","yellow")[unclass(type.wall.distance$TYPE)])


dummy <- dummyVars(" ~ TYPE",data = type.wall.distance)
dummy2 <- dummyVars(" ~ TYPE",data = prueba)


newdata <- data.frame(predict(dummy,newdata = type.wall.distance))
type.wall.distance <- cbind(type.wall.distance,newdata)
newdata2 <- data.frame(predict(dummy2,newdata = prueba))
prueba<- cbind(prueba,newdata2)

sample.index <- sample(1:nrow(type.wall.distance)
                       ,nrow(type.wall.distance)*0.7
                       ,replace = F)
predictors <- c("INFRARED","ULTRASONIC","TYPECONCAVA","TYPECONVEXA","TYPEPLANA")


train.data  <-  type.wall.distance[sample.index
                                   ,c(predictors,"TYPE")
                                   ,drop=F]
test.data  <-  type.wall.distance[-sample.index
                                   ,c(predictors,"TYPE")
                                   ,drop=F]
##KNN
ctrl <- trainControl(method = "cv",p=0.7) #variable de control
Knnfit <- train(TYPE ~ INFRARED+ULTRASONIC+TYPECONCAVA+TYPECONVEXA+TYPEPLANA
                ,data = train.data
                ,method = "knn", trControl = ctrl
                ,preProcess= c("range")
                ,tuneLength=20)

Knnpredict <- predict(Knnfit,newdata = prueba)## aca va el nuevo dataset

confusionMatrix(Knnpredict
                ,prueba$TYPE)
library(pROC)
kNN.roc <- roc(test.data$TYPE, as.numeric(as.character(Knnpredict)))




