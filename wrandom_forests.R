library(readr)
library(ggplot2)
library(randomForest)

wwt24<-read_csv("wwt24.csv")

wfinish_forest<-randomForest(num_finish~.,data=wwt24,ntrees=1000,keep.forest=TRUE,importance=TRUE)
wfinish_forest
plot(wfinish_forest)
wfinish_importance<-as.data.frame(importance(wfinish_forest))
wfinish_predictions<-predict(wfinish_forest,wwt24)
wfin_predictions_diff<-wwt24$num_finish-wfinish_predictions
ggplot(data=wwt24, aes(x=num_finish,y=wfinish_predictions))+geom_point()+geom_abline(intercept=0,slope=1)+labs(xlab="Actual number at finish",ylab="Predicted number at finish",title="Predicted versus actual number at finish")
ggplot(data=wwt24, aes(x=1:nrow(wwt24),y=wfin_predictions_diff))+geom_point()+geom_line(y=0)+labs(xlab="Observation",ylab="Differnce between prediction and observation",title="Difference between actual and observed number of riders in group 1")


wgap_forest<-randomForest(gap~.,data=wwt24,ntrees=1000,keep.forest=TRUE,importance=TRUE)
wgap_forest
plot(wgap_forest)
wgap_importance<-as.data.frame(importance(wgap_forest))
wgap_predictions<-predict(wgap_forest,wwt24)
wgap_predictions_diff<-wwt24$gap-wgap_predictions
ggplot(data=wwt24, aes(x=gap,y=wgap_predictions))+geom_point()+geom_abline(intercept=0,slope=1)+labs(xlab="Observed gap",ylab="Predicted gap",title="Predicted versus gap between group 1 and group 2")
ggplot(data=wwt24, aes(x=1:nrow(wwt24),y=wgap_predictions_diff))+geom_point()+geom_line(y=0)+labs(xlab="Observation",ylab="Differnce between prediction and observation",title="Difference between actual and predicted gap between group 1 and grop 2")


#random forest absolutely does not work for gap for this


wdsolo_forest<-randomForest(distance_solo~.,data=wwt24,ntrees=1000,keep.forest=TRUE,importance=TRUE)
wdsolo_forest
plot(wdsolo_forest)
wdsolo_importance<-as.data.frame(importance(wdsolo_forest))
wdsolo_predictions<-predict(wdsolo_forest,wwt24)
wdsolo_predictions_diff<-wwt24$distance_solo-wdsolo_predictions
ggplot(data=wwt24, aes(x=distance_solo,y=wdsolo_predictions))+geom_point()+geom_abline(intercept=0,slope=1)+labs(xlab="Observed distance solo",ylab="Predicted distance solo",title="Predicted versus observed distance solo between for winner")
ggplot(data=wwt24, aes(x=1:nrow(wwt24),y=wdsolo_predictions_diff))+geom_point()+geom_line(y=0)+labs(xlab="Observation",ylab="Differnce between prediction and observation",title="Difference between actual and predicted distance solo for winner")

#distance solo does not work very well, probably because this is a smaller sample
#and that there are way less solos compared to mens world tour
