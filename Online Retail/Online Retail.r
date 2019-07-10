
library(purrr)
library(cluster)

online.retail <- read.csv("Online Retail.csv")

library(ggplot2)

order_wise <- na.omit(online.retail)

Amount <- order_wise$Quantity * order_wise$UnitPrice

order_wise <-  order_wise[order(order_wise$CustomerID),]
order_wise

monetary <- aggregate(Amount~CustomerID, order_wise, sum)
monetary

frequency <- order_wise[,c(7,1)]

k <- table(as.factor(frequency$CustomerID))

k <- data.frame(k)

colnames(k)[1] <- c("CustomerID")

master <- merge(monetary, k, by = "CustomerID")

recency <- order_wise[,c(7,5)]

recency$InvoiceDate <- as.Date(recency$InvoiceDate, "%d-%m-%Y %H:%M")

maximum <- max(recency$InvoiceDate)
maximum

maximum <- maximum + 1
maximum

maximum$diff <- maximum - recency$InvoiceDate
maximum$diff

recency$diff <- maximum$diff
recency

df <- aggregate(recency$diff, by = list(recency$CustomerID), FUN = "min")

colnames(df)[1] <- "CustomerID"

colnames(df)[2] <- "Recency"

RFM <- merge(monetary, k, by = ("CustomerID"))

RFM <- merge(RFM, df, by = ("CustomerID"))

RFM$Recency <- as.numeric(RFM$Recency)

box <- boxplot.stats(RFM$Amount)
box

out <- box$out

RFM1 <- RFM[!RFM$Amount %in% out,]

RFM <- RFM1

RFM

box <- boxplot.stats(RFM$Recency)

out <- box$out

RFM1 <- RFM[!RFM$Recency %in% out,]

RFM <- RFM1

RFM

box <- boxplot.stats(RFM$Freq)

out <- box$out

RFM1 <- RFM[!RFM$Freq %in% out,]

RFM <- RFM1

RFM

RFM_norm <- RFM[,-1]

RFM_norm$Amount <- scale(RFM_norm$Amount)

RFM_norm$Freq <- scale(RFM_norm$Freq)

RFM_norm$Recency <- scale(RFM_norm$Recency)

RFM_norm

clus3 <- kmeans(RFM_norm, centers = 3, iter.max = 50, nstart = 50)

r_sq <- rnorm(20)

for (number in 1:20){
    clus <- kmeans(RFM_norm, centers = number, nstart = 50)
    r_sq[number] <- clus$betweenss/clus$totss
}

plot(r_sq)

clus4 <- kmeans(RFM_norm, centers = 4, nstart = 50, iter.max = 50)

clus5 <- kmeans(RFM_norm, centers = 5, nstart = 50, iter.max = 50)

clus6 <- kmeans(RFM_norm, centers = 6, nstart = 50, iter.max = 50)

RFM_km <-cbind(RFM,clus5$cluster)

colnames(RFM_km)[5] <- "ClusterID"

library(dplyr)

km_clusters <- group_by(RFM_km, ClusterID)

tab1 <- summarise(km_clusters, Mean_amount = mean(Amount), Mean_freq = mean(Freq), Mean_recency = mean(Recency))

ggplot(tab1, aes(x = factor(ClusterID), y = Mean_amount)) + geom_bar(stat = "identity", aes(fill = factor(ClusterID)))

ggplot(tab1, aes(x = factor(ClusterID), y = Mean_freq)) + geom_bar(stat = "identity", aes (fill = factor(ClusterID)))

ggplot(tab1, aes(x = factor(ClusterID), y = Mean_recency)) + geom_bar(stat = "identity", aes(fill = factor(ClusterID)))

RFM_dist <- dist(RFM_norm)
#RFM_dist

RFM_hclust1 <- hclust(RFM_dist, method = "single")

plot(RFM_hclust1)


hclust1<- as.dendrogram(RFM_hclust1)

plot(cut(hclust1, h=0.3)$upper)

RFM_hclust2 <- hclust(RFM_dist, method = "complete")

plot(RFM_hclust2)

hclust2<- as.dendrogram(RFM_hclust2)

plot(cut(hclust2, h=2)$upper)

plot(RFM_hclust2)
rect.hclust(RFM_hclust2, k = 5, border = "red")

clusterCut <- cutree(RFM_hclust2, k=5)

avg_sil <- function(k){
    km.res <- kmeans(RFM_norm, centers = k, nstart = 25 )
    ss <- silhouette(km.res$cluster, dist(RFM_norm))
    mean(ss[, 3])
}


k.values <- 2:15

avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values, type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters K", ylab = "Average Silhouettes")


