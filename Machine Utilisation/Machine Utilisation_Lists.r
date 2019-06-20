
# Deliver a list with following components:
#Character : Machine Name
#Vector: (min,mean,max) Utilisation for the month
#Logical : Has utilisation ever fallen below 90%
#Vector : All hours for which utilisation is unknown
#DataFrame : For this machine
#plot : All machines

util <- read.csv("P3-Machine-Utilization.csv")

head(util,12)

str(util)

summary(util)

#We need (1-percent idle) to get utilisation of machine

#Deriving utilisation column

util$Utilisation <- 1- util$Percent.Idle

head(util,12)

df <- util[!complete.cases(util),]
unknown_util <- df$Utilisation
as.vector(unknown_util)

tail(util)

#First column represents date

#For very confusing time-stamps, use this

?POSIXct #Stores time as number of seconds passed till 1970

util$POSIXTime <- as.POSIXct(util$Timestamp, format = "%d/%m/%Y %H:%M")

head(util,12)

summary(util)

#TIP : Rearranging Columns in a df

util$Timestamp <- NULL

util <- util[c(4,1,2,3)]

head(util,12)

# List can contain mix of elements and data types

summary(util)

RL1 <- util[util$Machine=="RL1",]
unknown_util <- RL1[!complete.cases(RL1),"POSIXTime"]

summary(RL1)

RL1$Machine <- factor(RL1$Machine) #Removing other machines from data frame

summary(RL1)

#Construct List

#Mean, max and min calculations

util_stats_rl1 <- c(min(RL1$Utilisation, na.rm =T),mean(RL1$Utilisation, na.rm=T),max(RL1$Utilisation, na.rm=T))

util_stats_rl1

util_under_90 <- which(RL1$Utilisation<0.90)#Which ignores NA

flag <- length(util_under_90)>0 #27 times it fell below 90

machine_list <- list("RL1", util_stats_rl1, flag)

machine_list

machine_list

names(machine_list) #checking names of list

names(machine_list) <- c("MachineName", "Statistics", "LowThreshold")

machine_list

#Another way to name lists
rm(machine_list)

list_rl1 <- list(Machine = "RL1", Stats = util_stats_rl1, LowThreshold = flag)

list_rl1

#First Method

#[]: will always return a list

#Second Method:
#[[]] will always return value
#THird Method:
#$ same as [[]] but prettier

list_rl1[1]

list_rl1[2]

list_rl1[3]

list_rl1[[1]]

list_rl1[[2]]

list_rl1[[3]]

typeof(list_rl1[2])

typeof(list_rl1[[1]])

list_rl1$Stats #Simpler way of representing

list_rl1[[2]][3] #Getting to third value of second vector

list_rl1$Stats[3]

#list_rl1[4] <- "New Information"

#Another way to add component via dollar sign
list_rl1$UnknownHours <- unknown_util

list_rl1

#Removing a Component, using the NULL method:

#list_rl1[4] <- NULL
#NOTICE NUMERATION HAS SHIFTED AUTOMATICALLY, UNLIKE DATA FRAMES

#Adding the data frame
list_rl1$DATA <- RL1

list_rl1

summary(list_rl1)

str(list_rl1)

list_rl1$UnknownHours[1]

list_rl1[[4]][1]

list_rl1[1:2] #Subsets of list

list_rl1[c(1,3)]

sub_list <- list_rl1[c("Machine", "Stats")]

sub_list[[2]][2]

#[] are for subsetting
#[[]] are for accessing elements

install.packages("ggplot2")
library(ggplot2)
p <- ggplot(data=util)

myplot <- p + geom_line(aes(x = POSIXTime, y = Utilisation, colour = Machine), size = 1.2) + facet_grid(Machine ~ .) + geom_hline(yintercept = 0.90, colour = "Black", size = 1.2, linetype =3)

myplot

list_rl1$Plot <- myplot

list_rl1

summary(list_rl1)

str(list_rl1)
