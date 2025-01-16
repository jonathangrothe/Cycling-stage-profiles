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
wt24_withclusters<-wt24 %>% 
  mutate(clusternum=wt_3clusters$cluster)

wwt_5clusters<-kmeans(wwt24,centers=5,nstart=20)
wwt24_withclusters<-wwt24 %>%
  mutate(clusternum=wwt_5clusters$cluster)


wt24_pca<-prcomp(scaled_wt24)
wt_pca_plot<-autoplot(wt24_pca,data=wt24_withclusters,colour='clusternum',label=TRUE)
wt_pca_plot

wt24_loadings<-wt24_pca$rotation
wt24_contribution<-wt24_loadings[,1]^2/sum(wt24_loadings[,1]^2)
barplot(wt24_contribution, names.arg = colnames(scaled_wt24), 
        xlab = "Variables", ylab = "% Contribution to PC1")

wwt24_pca<-prcomp(scaled_wwt24)
wwt_pca_plot<-autoplot(wwt24_pca,data=wwt24_withclusters,colour='clusternum',label=TRUE)
wwt_pca_plot

wwt24_loadings<-wwt24_pca$rotation
wwt24_contribution<-wwt24_loadings[,1]^2/sum(wwt24_loadings[,1]^2)
barplot(wwt24_contribution, names.arg = colnames(scaled_wwt24), 
        xlab = "Variables", ylab = "% Contribution to PC1")

hist(wt24$gap)
hist(wt24$num_finish)
hist(wt24$distance_solo)
hist(wt24$distance)
hist(wt24$vertical)
hist(wt24$profile_score)
hist(wt24$profile_score_last25k)

hist(wwt24$gap)
hist(wwt24$num_finish)
hist(wwt24$distance_solo)
hist(wwt24$distance)
hist(wwt24$vertical)
hist(wwt24$profile_score)
hist(wwt24$profile_score_last25k)

#next steps: 
#tuning - research how to do tree tuning best
#look for trends in outliers and try fitting forests on datasets w/o outliers/extreme values
#try other models
#apply these random forests to future stage profiles
#look for trends in outliers