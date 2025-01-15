library(readr)
library(ggplot2)
library(randomForest)
wt24<-read_csv("racedata_wt24.csv")

#creating a random forest for number of riders at the finish 
#based on tuning it seems like we should use mtry=6
num_finish_forest<-randomForest(num_finish~.,data=wt24,mtry=6,ntrees=1000,keep.forest=TRUE,importance=TRUE)
num_finish_forest
plot(num_finish_forest)
numfin_importance<-as.data.frame(importance(num_finish_forest))
num_finish_predictions<-predict(num_finish_forest,wt24)
numf_predictions_diff<-wt24$num_finish-num_finish_predictions
ggplot(data=wt24, aes(x=num_finish,y=num_finish_predictions))+geom_point()+geom_abline(intercept=0,slope=1)+labs(xlab="Actual number at finish",ylab="Predicted number at finish",title="Predicted versus actual number at finish")
ggplot(data=wt24, aes(x=1:nrow(wt24),y=numf_predictions_diff))+geom_point()+geom_line(y=0)+labs(xlab="Observation",ylab="Differnce between prediction and observation",title="Difference between actual and observed number of riders in group 1")

#creating a random forest for gap to next pack
#based on tuning it seems mtry=5 is best
gap_forest<-randomForest(gap~.,data=wt24,mtry=5,ntrees=1000,keep.forest=TRUE,importance=TRUE)
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

#based on tuning info it looks like mtry=5 is best
solo_forest<-randomForest(distance_solo~.,data=wt24,mtry=5,ntrees=1000,keep.forest=TRUE,importance=TRUE)
solo_forest
plot(solo_forest)
solo_importance<-as.data.frame(importance(solo_forest))
solo_predictions<-predict(solo_forest,wt24)
solo_diff<-wt24$distance_solo-solo_predictions
ggplot(wt24, aes(x=distance_solo,y=solo_predictions))+geom_point()+geom_abline(intercept=0,slope=1)+labs(x="Observations",y="Predictions",title="Observed versus predicted distance solo")
ggplot(wt24, aes(x=1:nrow(wt24),y=solo_diff))+geom_point()+geom_line(y=0)+labs(x="Obsbervation number",y="Difference between prediction and actual distance solo",title="Difference between actual and observed distance solo for the winner")

#next steps: 
#get best possible forests
#try other models
#apply these random forests to future stage profiles
#look for trends in outliers