library(ggplot2)
library(dplyr)
d1 <-read.csv('well_log.csv', stringsAsFactors = FALSE)

dim(d1)
head(d1)

# Remove any missing value
d1 <- na.omit(d1)

# First data visualization
ggplot(d1, aes(PHIE)) + geom_histogram(bins = 30)
ggplot(d1, aes(SWE)) + geom_histogram(bins = 30)
ggplot(d1, aes(VOL_WETCLAY)) + geom_histogram(bins = 30)
ggplot(d1, aes(rocktype, logk, group=rocktype)) + geom_boxplot()

#selecting variables of interest
data <- d1[,c("PHIE", "logk", "VOL_WETCLAY", "SWE")]

library(corrgram) 
corrgram(data, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="PCA Data")

# PCA 
pr.out=prcomp(data , scale=TRUE)
summary (pr.out)
pve =100*pr.out$sdev ^2/sum(pr.out$sdev ^2)
scores <- pr.out$x #new variables
eigenvectors <- pr.out$rotation
eigenvalues <- pr.out$sdev*pr.out$sdev
CFV <- eigenvectors*eigenvectors  #Contribution factor-var
CFV

# Scree plot
par(mfrow=c(1,2))
plot(pve , type="o", ylab="PVE", xlab=" Principal Component ",
        col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="
        Principal Component ", col="brown3")

# Visualizing 
library("factoextra")
# PCA
fviz_pca_var(pr.out)

# Hierarchical clustering
# Compute dissimilarity matrix
d <- dist(scores [,1:2], method = "euclidean")

# heat map, takes a lot time
# fviz_dist(d, 
#           gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Grouping criteria, Ward's method
#  min Increase in sum of squares within clusters, after fusion
res.hc <- hclust(d, method = "ward.D2" )

# Visualize
plot(res.hc, cex = 0.6) # plot tree

# color dedndrogram, takes time too 
# res <- hcut(d, k = 4, stand = TRUE)
# fviz_dend(res, rect = TRUE, cex = 0.5,
#           k_colors = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"))

# Compute WSS and BSS
library(GMD)
ss <- css.hclust(d, hclust(d))

# elbow plot
par(mfrow=c(1,2))
plot(ss$k, ss$ev)
plot(ss$k, (1-ss$ev))

# Cut into 4 clusters
h.cluster <- cutree(res.hc , 4)

# K-means clustering
km.cluster <- kmeans(data, 4, nstart = 25)
fviz_cluster(km.cluster, data = data, frame.type = "convex")+
  theme_minimal()

# more vis
data <- data %>% cbind(h.cluster) %>% cbind(km.cluster$cluster)
ggplot(data, aes(PHIE, logk)) + geom_point(color=h.cluster)
ggplot(data, aes(PHIE, logk)) + geom_point(color=km.cluster$cluster)

data <- data %>% cbind(d1$rocktype)
ggplot(data, aes(PHIE, logk)) + geom_point(color=d1$rocktype)

ggplot(data, aes(d1$rocktype, logk, group=d1$rocktype)) + geom_boxplot()
ggplot(data, aes(h.cluster, logk, group=h.cluster)) + geom_boxplot()
ggplot(data, aes(km.cluster$cluster, logk, group=km.cluster$cluster)) + geom_boxplot()

