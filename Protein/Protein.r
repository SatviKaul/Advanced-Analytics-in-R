
protein <- read.table("protein.txt", sep = "\t", header = T)

summary(protein)

var.to.use <- colnames(protein)[-1]

pmat <- scale(protein[,var.to.use])

head(pmat)

pcent <- attr(pmat, "Scaled:center")
pscale <- attr(pmat, "Scaled:scale")

d <- dist(pmat, method = 'euclidean')

pfit <- hclust(d, method = 'ward')

plot(pfit, labels = protein$Country)
rect.hclust(pfit, k = 5)

print_clusters <- function(labels, k) {             
  for(i in 1:k) {
    print(paste("cluster", i))
    print(protein[labels==i,c("Country","RedMeat","Fish","Fr.Veg")])
  }
}

groups <- cutree(pfit, k = 5)

print_clusters(groups, 5)

library(fpc)

kbest.p <- 5

cboot.hclust <- clusterboot(pmat, clustermethod = hclustCBI, method = 'ward', k = kbest.p)

groups <- cboot.hclust$result$partition

print_clusters(groups, kbest.p)

cboot.hclust$bootmean
# The vector of cluster stabilities. 
# Values close to 1 indicate stable clusters

cboot.hclust$bootbrd

# The count of how many times each cluster was 
# dissolved. By default clusterboot() runs 100 
# bootstrap iterations. 
# Clusters that are dissolved often are unstable. 


