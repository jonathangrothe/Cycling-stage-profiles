library(readr)
library(ggplot2)
library(randomForest)

#data from procyclingstats.com about each womens world tour race in 2024
wwt24<-read_csv("wwt24.csv")
#that data with an extreme outlier (Burgos stage 3) in gap from group 1 to group 2 removed
wwt24_removed<-wwt24[-32,]

#a function that makes a random forest for a variable,
#creates two plots to demonstrate how well it does predicting on the data set
#returns a data frame of the importance of each variable
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


wfinish_importance<-forest_info(wwt24,num_finish,wwt24$num_finish)
wgap_forest<-forest_info(wwt24,gap,wwt24$gap)
wgap_removed<-forest_info(wwt24_removed,gap,wwt24_removed$gap)
wdsolo_forest<-forest_info(wwt24,distance_solo,wwt24$distance_solo)


#distance solo does not work very well, probably because of the very small number of solo wins
