library(e1071)
library(mclust)
#1.Hierarchial Clustering
data("iris")
#Removing known type.
iris <- iris[,-5]
#Standardise data.
iris.sd <- apply(iris, 2, sd)
stdiris <- sweep(iris, 2, iris.sd, "/")
#Build a distance matrix using euclidean dista
#-nce measure.
d <- dist(stdiris)
#Create hierarchial clustering
cl.stdiris.average <- hclust(d, method = "average")
plot(cl.stdiris.average)
#Compute withing groups sum of squares.
WGCS <- sapply(1:10, function(k) {sum(kmeans(iris, centers = k)$withinss)})
#Elbow Plot
plot(1:10, WGCS, type = "b", xlab = "Number of Clusters", ylab = "Total Within Groups Sum of Squares")
#Mark Point
K <- 3
points(K, WGCS[K], col = 'red')
#Get Cluster Count
cl <- kmeans(iris, center = K)
table(cl$cluster)
plot(iris, col = cl$cluster)
points(cl$centers, col = 1:K, pch =8)
#Compare Clustering solutions for above K.
hcl <- cutree(hclust(dist(iris)), K)
pcl <- kmeans(iris, centers = K)
#Contingency Table
tab <- table(hcl, pcl$cluster)
tab
# FIXME: Should really compare known class to achieved clustering!

# Get the adjusted Rand Index.
crand1 <- classAgreement(tab)$crand
print("Rand Index:")
print(crand1)
crand2 <- adjustedRandIndex(hcl, pcl$cluster)
print("Adjusted Rand Index:")
print(crand2)
