library(readr)
library(ggplot2)
library(ggfortify)
library(dplyr)
tdf24<-read_csv("tdf24.csv")
tdf24<-as.data.frame(scale(tdf24))

cluster_ssplot <- function(data){
  ss_vector <- numeric(10)
  for(i in 1:10){
    output <- kmeans(data, centers = i, nstart=20)
    ss_vector[i] <- output$tot.withinss
  }
  plot(ss_vector)
}

cluster_ssplot(tdf24)

tdf_3clusters<-kmeans(tdf24,centers=3,nstart=20)
tdf24_withclusters<-tdf24 %>% 
  mutate(clusternum=tdf_3clusters$cluster)

tdf24_pca<-prcomp(tdf24)
pca_plot<-autoplot(tdf24_pca,data=tdf24_withclusters,colour='clusternum',label=TRUE)
pca_plot

loadings<-tdf24_pca$rotation
contribution<-loadings[,1]^2/sum(loadings[,1]^2)
barplot(contribution, names.arg = colnames(tdf24), 
        xlab = "Variables", ylab = "% Contribution to PC1")

