library(readr)
library(ggplot2)
library(randomForest)
wt24<-read_csv("racedata_wt24.csv")

forest_info<-function(data,variable,datacol){
  variable <- ensym(variable)
  equation <- as.formula(paste(as.character(variable),"~."))
  forest<-randomForest(formula=equation,data=data,ntrees=1000,keep.forest=TRUE,importance=TRUE)
  print(forest)
  plot(forest)
  importance<-as.data.frame(importance(forest))
  predictions<-predict(forest,data)
  predictions_diff<-datacol-predictions
  print(ggplot(data=data, aes(x=datacol,y=predictions))+geom_point()+geom_abline(intercept=0,slope=1)+labs(xlab="Actual",ylab="Predicted"))
  print(ggplot(data=data, aes(x=1:nrow(data),y=predictions_diff))+geom_point()+geom_line(y=0)+labs(xlab="Observation",ylab="Differnce between prediction and observation"))
}


num_finish_forest <- forest_info(wt24,num_finish,wt24$num_finish)
gap_forest <- forest_info(wt24,gap,wt24$gap)
solo_forest <- forest_info(wt24,distance_solo,wt24$distance_solo)


