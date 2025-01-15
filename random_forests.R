library(readr)
library(ggplot2)
library(randomForest)
wt24<-read_csv("racedata_wt24.csv")

#creating a random forest for number of riders at the finish 
num_finish_forest<-randomForest(num_finish~.,data=wt24,ntrees=1000,keep.forest=TRUE,importance=TRUE)
num_finish_forest
plot(num_finish_forest)
numfin_importance<-as.data.frame(importance(num_finish_forest))
num_finish_predictions<-predict(num_finish_forest,wt24)
ggplot(data=wt24, aes(x=1:nrow(wt24),y=num_finish))+geom_point()

#creating a random forest for gap to next pack
gap_forest<-randomForest(gap~.,data=wt24,ntrees=1000,importance=TRUE)
gap_forest
plot(gap_forest)
gap_importance<-as.data.frame(importance(gap_forest))

#next steps: 
#predictions for distance solo
#plots to compare model to actual data
#get best possible forests
#apply these random forests to future stage profiles
