
chic_f <- read.csv("Chicago-F.csv",row.names=1)
san_f <- read.csv("SanFrancisco-F.csv",row.names=1)
ny_f <- read.csv("NewYork-F.csv",row.names=1)
hous_f <- read.csv("Houston-F.csv",row.names=1)

head(chic_f)
head(san_f)
head(ny_f)
head(hous_f)

#Since all these values inside the table are numeric, we can change the data frames into matrix

chic_f <- as.matrix(chic_f)
ny_f <- as.matrix(ny_f)
hous_f <- as.matrix(hous_f)
san_f <- as.matrix(san_f)

weather <- list(Chicago = chic_f, NewYork = ny_f, Houston = hous_f, SanFrancisco = san_f)

weather

 apply(chic_f,1,mean) #Gives vector containing mean of all rows

 apply(chic_f,1,mean)  #Gives vector containing mean of all columns

# Saves time because no loops are required to be typed

# The apply family:
# apply : use on a matrix; either row or column.
# tapply : use on vector to extract subgroups and apply function to them.
# by : use on data frames. Same as GROUP BY in SQL.
# eapply : use on an Environment(E).
# lapply : apply function to all elements of list.
# sapply : a version of lapply. Can simplify (S) the result so it's not presented as a list
# vapply : has a pre-specified type of return value(V)
# replicate : run a function several times. Usually used with random variables.
# mapply : multivariate (M) version of sapply. Arguments can be recycled.
# rapply : recursive (R) version of apply.

#?apply

 apply(chic_f, 1, mean) 
 apply(chic_f, 2, sum) 

apply(chic_f, 1, max)

apply(chic_f, 1, min)

apply(chic_f, 2, min)

apply(chic_f, 2, max)

apply(chic_f, 1, mean)  
apply(ny_f, 1, mean)
apply(hous_f, 1, mean)
apply(san_f, 1, mean) 

# Find mean of every row

# 1. Via Loops

output <- NULL #Preparing an empty vector

for(i in 1:5){
   output[i] <- mean(chic_f[i,])
}

output

names(output) <- rownames(chic_f)

output

# 2. Via Apply

apply(chic_f, 1, mean)

# See difference in both

#?lapply

chic_f

t(chic_f) # Transpose

weather

# Applying transpose to each matrix and storing them in a new list

weather_1 <- lapply(weather, t)

# Adding another row to matrices

rbind ( chic_f, NewRow = 1:12) #Do for each, then store in a list

lapply( weather, rbind, NewRow = 1:12) #Easier

#?rowMeans

rowMeans(chic_f) # Identical to apply(chic_f, 1, mean)

lapply(weather, rowMeans) #Returns a named vector

#rowMeans
#colMeans
#rowSums
#colSums

weather

weather$Chicago

weather[[1]]

weather$Chicago[1,1]

weather[[1]][1]

lapply(weather, "[",1,1) # lapply already implies [[]]

# "[" means single square brackets, lapply with iterate over list using [[]], and value will be
# returned using []

# First row for every city
lapply(weather, "[", 1,)

lapply(weather, "[", ,"Mar")
#lapply(weather, "[",,3)"

lapply(weather, rowMeans)

lapply( weather, function(x) x[5,])

lapply( weather, function(x) x[,12]) #Dec for every city

lapply(weather, function(z) (round((z[1,]-z[2,])/z[2,],2))*100)

#?sapply

weather

lapply(weather, "[", 1, 7 ) #Returns a list

sapply(weather , "[", 1, 7) #Returns named vector

#avg_high for 4th quarter

lapply(weather, "[" , 1, c(10:12))

sapply(weather, "[" , 1, c(10:12)) #Puts them into a matrix

lapply(weather, rowMeans)

round(sapply(weather, rowMeans),2) #Returns a matrix #Deliverable 1

lapply(weather, function(z) (round((z[1,]-z[2,])/z[2,],2))*100)

sapply(weather, function(z) (round((z[1,]-z[2,])/z[2,],2))*100) #Deliverable 2

sapply(weather, rowMeans, simplify = F) # sapply is lapply simplified

chic_f

apply(chic_f, 1, max)

lapply(weather, apply, 1, max) #Returns max of every row in every element in R, using apply as 
                               # user defined function.

lapply(weather, function(x) apply(x, 1, max)) # Another approach

#tidyup

sapply(weather, apply, 1, max) #Deliverable 3
sapply(weather, apply, 1, min) #Deliverable 4

# We want to know what month was highest, names, not numbers
# which.max() and which.min() will help

which.max(chic_f[1,])

names(which.max(chic_f[1,])) #Nesting names,which.max()

apply(chic_f, 1, function(x) names(which.max(x))) #Nesting names,which.max() with apply()

lapply(weather, function(y) apply(y, 1, function(x) names(which.max(x))))
                                  # Nesting names,lapply,apply,which.max()

sapply(weather, function(y) apply(y, 1, function(x) names(which.max(x))))


