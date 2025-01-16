library(readr)
library(ggplot2)
library(ggfortify)
library(dplyr)

wt24<-read_csv("racedata_wt24.csv")
wwt24<-read_csv("wwt24.csv")
scaled_wt24<-as.data.frame(scale(wt24))
scaled_wwt24<-as.data.frame(scale(wwt24))

cluster_ssplot <- function(data){
  ss_vector <- numeric(10)
  for(i in 1:10){
    output <- kmeans(data, centers = i, nstart=20)
    ss_vector[i] <- output$tot.withinss
  }
  plot(ss_vector)
}

cluster_ssplot(scaled_wt24)
cluster_ssplot(scaled_wwt24)

wt_3clusters<-kmeans(wt24,centers=3,nstart=20)
wt24_withclusters<-scaled_wt24 %>% 
  mutate(clusternum=wt_3clusters$cluster)



wt24_pca<-prcomp(scaled_wt24)
wt_pca_plot<-autoplot(wt24_pca,data=wt24_withclusters,colour='clusternum',label=TRUE)
wt_pca_plot

wt24_loadings<-wt24_pca$rotation
wt24_contribution<-wt24_loadings[,1]^2/sum(wt24_loadings[,1]^2)
barplot(wt24_contribution, names.arg = colnames(scaled_wt24), 
        xlab = "Variables", ylab = "% Contribution to PC1")

hist(wt24$gap)
hist(wt24$num_finish)
hist(wt24$distance_solo)
hist(wt24$distance)
hist(wt24$vertical)
hist(wt24$profile_score)
hist(wt24$profile_score_last25k)

