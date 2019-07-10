
fin <- read.csv("Wholesale customers data.csv", header = T)

summary(fin)

head(fin)

top.n.custs <- function (data,cols,n=5) { #Requires some data frame and the top N to remove
idx.to.remove <-integer(0) #Initialize a vector to hold customers being removed
for (c in cols){ # For every column in the data we passed to this function
col.order <-order(data[,c],decreasing=T) #Sort column "c" in descending order (bigger on top)
#Order returns the sorted index (e.g. row 15, 3, 7, 1, ...) rather than the actual values sorted.
idx <-head(col.order, n) #Take the first n of the sorted column C to
idx.to.remove <-union(idx.to.remove,idx) #Combine and de-duplicate the row ids that need to be removed
}
return(idx.to.remove) #Return the indexes of customers to be removed
}


top.custs <-top.n.custs(fin, cols = 3:8, n = 5)

length(top.custs)

fin[top.custs,]

data.rm.top <- fin[-c(top.custs),] # Remove the clusters

set.seed(76964057)

k <- kmeans(data.rm.top[,-c(1,2)], centers = 5)

k$centers

table(k$cluster)

rng <- 2:20

tries <- 100

avg.totw.ss <- integer(length(rng))

for(v in rng){
    v.totw.ss <- integer(tries)
    for(i in 1:tries){
    k.temp <- kmeans(data.rm.top, centers = v)
    v.totw.ss[i] <- k.temp$tot.withinss
}
    avg.totw.ss[v-1] <- mean(v.totw.ss)
}

plot(rng,avg.totw.ss, type = 'b', main = "Total Within SS by Various K", ylab = "Average Total within sum of squares", xlab = "Value of K")


