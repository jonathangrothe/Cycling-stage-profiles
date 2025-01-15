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
numf_predictions_diff<-wt24$num_finish-num_finish_predictions
ggplot(data=wt24, aes(x=num_finish,y=num_finish_predictions))+geom_point()+geom_abline(intercept=0,slope=1)+labs(xlab="Actual number at finish",ylab="Predicted number at finish",title="Predicted versus actual number at finish")
ggplot(data=wt24, aes(x=1:nrow(wt24),y=numf_predictions_diff))+geom_point()+geom_line(y=0)+labs(xlab="Observation",ylab="Differnce between prediction and observation",title="Difference between actual and observed number of riders in group 1")

#creating a random forest for gap to next pack
gap_forest<-randomForest(gap~.,data=wt24,ntrees=1000,keep.forest=TRUE,importance=TRUE)
gap_forest
plot(gap_forest)
gap_importance<-as.data.frame(importance(gap_forest))
gap_predictions<-predict(gap_forest,wt24)
gap_diff<-wt24$gap-gap_predictions
ggplot(wt24, aes(x=gap,y=gap_predictions))+geom_point()+geom_abline(intercept=0,slope=1)+labs(x="Actual gap",y="Predicted gap",title="Predicted versus actual gap between group 1 and group 2")
ggplot(wt24, aes(x=1:nrow(wt24),y=gap_diff))+geom_point()+geom_line(y=0)+labs(x="Observation",y="Difference between predicted and actual gap",title="Difference between actual and observed gap between group 1 and group 2")


#gap seems good at predicting most aside from a few outliers which really drag down its mse
#problem stages: sprints which have surprisingly high gaps
  #probably could adjust this


#next steps: 
#predictions for distance solo
#plots to compare model to actual data
#get best possible forests
#apply these random forests to future stage profiles
