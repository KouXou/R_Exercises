library('dplyr')
library('rpart')
library('ggplot2')
library('fpc')

file_path<-'./decathlon.csv'
decathlon<-read.csv(file_path, sep = ';')
# head(decathlon)

pmatrix<-decathlon[,2:11]

# Πίνακας αποστάσεων
d<-dist(pmatrix, method="euclidean")

pfit<-hclust(d, method="ward.D")
plot(pfit, labels=decathlon$X, cex= 1.0)

rect.hclust(pfit, k=5)


groups <- cutree(pfit, k=5)

df<-mutate(decathlon, Cluster=groups)
df<-df[order(df$Cluster),]
df

no_of_clusters <- 5
pclusters <- kmeans(decathlon[,2:11], no_of_clusters, nstart=100, iter.max=100)
pclusters

pclusters$centers
