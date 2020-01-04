library('dplyr')
library('rpart')
library('ggplot2')
library('fpc')


# file_path<-'~/Documents/Msc/Lessons 1st/Data Mining/Ergasia Filip/mf_lab/decathlon.csv'
file_path<-'./decathlon.csv'
decathlon<-read.csv(file_path, sep = ';')
head(decathlon)
# decathlon

# Hierarchical clustering
pmatrix<-decathlon[,2:11]

d<-dist(pmatrix, method="euclidean") # distance matrix

pfit<-hclust(d, method="ward.D")
plot(pfit, labels=decathlon$X, cex= 1.0)

rect.hclust(pfit, k=5)


groups <- cutree(pfit, k=5)
# groups
# print_clusters <- function(labels, k) {
#     for(i in 1:k) {
#         print(paste("cluster", i))
#         print(decathlon[labels==i,c("X","X100m","Long.jump","Shot.put")])
#     }
# }
# print_clusters(groups, 5)
df<-mutate(decathlon, Cluster=groups)
df<-df[order(df$Cluster),]
df

princ <- prcomp(pmatrix)
nComp <- 2
project <- predict(princ, newdata=pmatrix)[,1:nComp]
project.plus <- cbind(as.data.frame(project),
                    cluster=as.factor(groups),
                    X=decathlon$X)
ggplot(project.plus, aes(x=PC1, y=PC2)) + geom_point(aes(shape=cluster)) + geom_text(aes(label=X),hjust=0, vjust=1)

#K-Means

head(decathlon)

pclusters <- kmeans(decathlon[,2:11], 5, nstart=100, iter.max=100)
pclusters

pclusters$centers
pclusters$cluster

#  kmeansruns to pick the best k
clustering.ch <- kmeansruns(decathlon[,2:11], krange=1:10, criterion="ch") # Calinski-Harabasz Index 
clustering.ch

clustering.asw <- kmeansruns(decathlon[,2:11], krange=1:10, criterion="asw") # average silhouette width
clustering.asw


