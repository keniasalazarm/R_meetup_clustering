# Ex for k-means clustering
library(ggplot2)
library(dplyr)

set.seed(2)
x <- matrix(rnorm (50*2), ncol=2)
x[1:25,1] <- x[1:25,1]+3
x[1:25,2] <- x[1:25,2]-4

plot(x)

# K-means clustering with K = 2
km2.out <- kmeans (x,2, nstart =20)
km2.out

# Within cluster sum of squares by cluster
# inter clusters variability k2 
km2.out$tot.withinss/ km2.out$totss
# intra clusters variability k2 
km2.out$betweenss/ km2.out$totss

# Visualizing the data
km2.clusters <- km2.out$cluster 
x <- x %>% cbind(km2.clusters) %>% as.data.frame()

p1 <- ggplot(data = x, aes(x = x[,1], y = x[,2])) + 
    geom_point(color=x[,3]) + 
    labs(x='', y='', title="k-means clustering", subtitle='k=2')
p1

# Let's pretend we don't know k and asume k=3
set.seed(4)
km3.out <- kmeans (x,3, nstart =20)
km3.out

# Visualizing the data
km3.clusters <- km3.out$cluster
x <- x %>% cbind(km3.clusters) %>% as.data.frame()

p2 <- ggplot(data = x, aes(x = x[,1], y = x[,2])) + 
  geom_point(color=x[,4]) + 
  labs(x='', y='', title="k-means clustering", subtitle='k=3')
p2

# Generating multiple partitions
set.seed(6)
km1.out=kmeans (x,1, nstart =20)
set.seed(8)
km4.out=kmeans (x,4, nstart =20)
set.seed(10)
km6.out=kmeans (x,6, nstart =20)

WSS <-c(km1.out$tot.withinss, km2.out$tot.withinss, km3.out$tot.withinss, km4.out$tot.withinss, km6.out$tot.withinss)
number_k <- c(1,2,3,4,6) %>% cbind (WSS) %>% as.data.frame()

# Choosing optimal number of cluster
p3<- ggplot(number_k, aes(number_k[,1], WSS)) + geom_line() + labs(x='number of clusters')
p3

# Hierarchical clustering, comparing diferent linkage methods
par(mfrow=c(1,3))

#default euclidedan
data.dist <- dist(x)

#default grouping criteria in hclust is complete linkage
plot(hclust(data.dist) , main="Complete
       Linkage ", xlab="", sub="",ylab="")
plot(hclust(data.dist , method ="average"),
       main="Average Linkage ", xlab="", sub="",ylab="")
plot(hclust(data.dist , method ="single"),
       main="Single Linkage ", xlab="", sub="",ylab="")
# Average and complete linkage tend to yield more balanced clusters

#Asign objects for 2 clusters and complete linkage
hc.clusters <- cutree(hclust(data.dist) , 2)

# Comparing k-means and hirerchical 
table(km2.clusters ,hc.clusters )
x <- x %>% cbind(hc.clusters)

# WSS, BSS, TSS for hierarchical clustering
library(GMD)
ss <- css.hclust(data.dist, hclust(data.dist))
ss

par(mfrow=c(1,2))
plot(ss$k, ss$ev)
plot(ss$k, (1-ss$ev))
