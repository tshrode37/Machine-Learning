## Libraries ----------------------------------------------------------------

library(DataExplorer)
library("factoextra")
library(cluster)
library(caret)
library(dplyr)
library(dendextend)

## Load Dataset ----------------------------------------------------------------

wholesale_df <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00292/Wholesale customers data.csv")
head(wholesale_df)
wholesale_df$Region <- recode(wholesale_df$Region, "1" = "Lisbon", "2" = "Oporto", "3" = "Other") # 3 = Other, 2 = Oporto, 1 = Lisbon
wholesale_df$Channel <- recode(wholesale_df$Channel, "1" = "Horeca", "2" = "Retail") # 1 = Horeca, 2 = Retail
table(wholesale_df$Region)
table(wholesale_df$Channel)


## Data Exploration ----------------------------------------------------------------

str(wholesale_df)
summary(wholesale_df)
introduce(wholesale_df)
plot_intro(wholesale_df)
plot_histogram(wholesale_df)
plot_correlation(wholesale_df)
plot_boxplot(wholesale_df, by = "Region")
plot_boxplot(wholesale_df, by = "Channel")

## Dummify/Normalize Columns ----------------------------------------------------------------

dmy <- dummyVars(" ~ Region + Channel", data = wholesale_df, fullRank = T) 
trsf <- data.frame(predict(dmy, newdata = wholesale_df))
head(trsf)

#or use scale() function

#norm <- function(x) {return ((x - min(x)) / (max(x) - min(x)))} 
#wholesale_norm <- as.data.frame(lapply(wholesale_df[,c(3:8)], norm))
#head(wholesale_norm) 

wholesale_scale <- scale(wholesale_df[,c(3:8)])
wholesale_final <- cbind(wholesale_scale, trsf)
head(wholesale_final)

####### PART 1 ##########------------------------------------------------------------------

## Kmeans: 10 Clusters----------------------------------------------------------------
set.seed(789)

wholesale_k10 <- kmeans(wholesale_final, 10, nstart = 25)
str(wholesale_k10)
wholesale_k10
wholesale_k10$size # Lantz
wholesale_k10$centers # Lantz
fviz_cluster(wholesale_k10, wholesale_final) #kmeans
barplot(t(wholesale_k10$centers), beside = TRUE, xlab = "cluster", ylab="value") # Chiu, 2015: inspect center of each cluster
clusplot(wholesale_final, wholesale_k10$cluster, color = TRUE, shade = TRUE) # Chui, 2015: Bivariate cluster plot


## Determine Optimal Clusters ----------------------------------------------------------------

dev.off() # for "Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) : invalid graphics state" error
  #https://intellipaat.com/community/12988/persistent-invalid-graphics-state-error-when-using-ggplot2
fviz_nbclust(wholesale_final, kmeans, method = "wss")
fviz_nbclust(wholesale_final, kmeans, method = "silhouette")
fviz_nbclust(wholesale_final, kmeans, method = "gap")

## Final KMeans ----------------------------------------------------------------

wholesale_k2 <- kmeans(wholesale_final, 2, nstart = 25)
str(wholesale_k2)
wholesale_k2
wholesale_k2$size # Lantz
wholesale_k2$centers # Lantz
fviz_cluster(wholesale_k2, wholesale_final) #kmeans
#barplot(t(wholesale_k3$centers), beside = TRUE, xlab = "cluster", ylab="value") # Chiu, 2015: inspect center of each cluster
#clusplot(wholesale_final, wholesale_k3$cluster, color = TRUE, shade = TRUE) # Chui, 2015: Bivariate cluster plot

## Plotting ----------------------------------------------------------------

table(wholesale_df$Region, wholesale_k2$cluster) #compare region with clustering results
table(wholesale_df$Channel, wholesale_k2$cluster) #compare channel with clustering results
#plot(wholesale_final[c("Detergents_Paper", "Delicassen")], col = wholesale_k2$cluster,pch=1, cex=0.5)
#points(wholesale_k2$centers[, c("Detergents_Paper", "Delicassen")], col=1:3,pch=8, cex=5)

plot(wholesale_final[c("Milk", "Grocery")], col = wholesale_k2$cluster,pch=1, cex=0.5)
points(wholesale_k2$centers[, c("Milk", "Grocery")], col=1:3,pch=8, cex=5)

## Cluster Elements ----------------------------------------------------------------

cluster_1 <- wholesale_final[wholesale_k2$cluster == 1,]
dim(cluster_1)
cluster_2 <- wholesale_final[wholesale_k2$cluster == 2,]
dim(cluster_2)
wholesale_k2$size

####### PART 2 ##########------------------------------------------------------------------

## Hierarchical Clustering: Linkages ----------------------------------------------------------------
  # agglomerative clustering (bottom-up): hclust() or agnes() packages

d <- dist(wholesale_scale, method = "euclidean") #compute dissimilarity values with dist()

hc1 = hclust(d, method = "average") #average linkage
hc1
plot(hc1, cex = 0.6, hang = -1, main = "Cluster Dendrogram: Average Linkage") #plot dendrogram 
#hang = -1 puts labels at the same height


hc2 = hclust(d, method = "ward.D2") #average linkage
hc2
plot(hc2, cex = 0.6, hang = -1, main = "Cluster Dendrogram: Average Linkage") #plot dendrogram 
#hang = -1 puts labels at the same height


#zoom into dendrogram: first cluster
hcd_2 <- as.dendrogram(hc2)
plot(hcd_2, xlim = c(1,6), ylim = c(1,32))



  # divisive clustering (top-bottom): diana()
hc3 <- diana(wholesale_final)
pltree(hc3, cex = 0.6, hang = -1, main = "Cluster Dendrogram: Divisive Clustering using diana()")


## Determining Optimal Number of Clusters ----------------------------------------------------------------

fviz_nbclust(wholesale_final, FUN = hcut, method = "wss")
fviz_nbclust(wholesale_final, FUN = hcut, method = "silhouette")
fviz_nbclust(wholesale_final, FUN = hcut, method = "gap", k.max = 20)


## Cut Dendrogram ----------------------------------------------------------------
fit_ward = cutree(hc2, k = 2)
table(fit_ward)

plot(hc2)
rect.hclust(hc2, k=2, border = "blue")


## Code, Plots, etc. NOT Used in Assignment ----------------------------------------

#heatmap(as.matrix(wholesale_final), Rowv = as.dendrogram(hc1))

#gap_stat_hca <- clusGap(wholesale_final, FUN = hcut, nstart = 25, K.max = 20, B = 50)
#fviz_gap_stat(gap_stat_hca)

#hc2 <- agnes(wholesale_final, method = "single")
#pltree(hc2, cex = 0.6, hang = -1, main = "Cluster Dendrogram: Ward Linkage using agnes()")
#sub_grp<-cutree(as.hclust(hc2), k =7) #cut agnes into 7 groups
#table(sub_grp)
#wholesale_final %>% mutate(cluster = sub_grp) %>% head

#nodePar <- list(lab.cex =0.6, pch = c(NA, 3), cex = 0.7, col = "purple")
#plot(hcd_2, nodePar = nodePar, leaflab = "none")

#dend <- wholesale_final[1:30,] %>% scale %>% dist %>% hclust %>% as.dendrogram
#dend %>% plot

#labels(dend)
#change labels example: dend %>% set("labels", c("a", "b", "c", "d", "e")) %>% plot
#change color and size for labels

#dend %>% set("labels_col", values = c("orange", "blue"), k= 2) %>% set("labels_cex", 0.5) %>% set("branches_k_color", value = c("green", "red"),k =2)
