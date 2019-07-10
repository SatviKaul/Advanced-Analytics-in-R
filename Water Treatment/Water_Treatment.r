

library(data.table)

library(ggplot2)
library(dplyr)

#install.packages("fpc")
library(fpc)

water_data <- read.table("water-treatment.data_.txt", sep = ",", header = F, na.strings = c("?"))

setDT(water_data)

head(water_data)

colSums(is.na(water_data))

for(i in colnames(water_data)[!(colnames(water_data) %in% c("V1"))])
set(x = water_data,i = which(is.na(water_data[[i]])), j = i, value = median(water_data[[i]], na.rm = T))

scaled_wd <- scale(water_data[,-c("V1"), with=F])

d <- dist(scaled_wd, method = "euclidean")

h_clust <- hclust(d, method = "ward") #clustering

plot(h_clust,labels = water_data$V1)
rect.hclust(h_clust,k=4)

pcmp <- princomp(scaled_wd)
pred_pc <- predict(pcmp, newdata=scaled_wd)[,1:2]

comp_dt <- cbind(as.data.table(pred_pc), cluster = as.factor(groups), Labels = water_data$V1)

ggplot(comp_dt,aes(Comp.1,Comp.2))+ geom_point(aes(color = cluster),size=3)

 kclust <- kmeans(scaled_wd,centers = 4,iter.max = 100)

ggplot(comp_dt,aes(Comp.1,Comp.2))+ geom_point(aes(color = as.factor(kclust$cluster)),size=3)

tunek <- kmeansruns(scaled_wd,krange = 1:10,criterion = "ch") 

tunek$bestk #3

tunekw <- kmeansruns(scaled_wd,krange = 1:10,criterion = "asw")

tunekw$bestk


