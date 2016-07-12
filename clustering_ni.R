setwd("~/Desktop/practicum/venga_practicum/")
library(dplyr)
library(ggplot2)
library(data.table)  
library(tidyr)
library(reshape2)
library(stats)
library(caret)
library(corrplot)
library(fpc)
library(cluster)
options( java.parameters = "-Xmx4g" )

user <- fread("user_view_3865_ni.csv",check.names = T,
              header = T)
#keep these for later reference merging
user.rownames <- data.frame(rows = row.names(user),loyalty_user_id = user$loyalty_user_id)

#separate sets for repeat vs non repeat. repeat defined as anyone over 1 visit

#repeat
repeat.user.ind <- with(user,num_of_repeats == 1)
repeat.user <- user[!repeat.user.ind,]

#first time
firsttime.user.ind <- with(user,num_of_repeats > 1)
firsttime.user <- user[!firsttime.user.ind,]

#get user data ready for clustering, first removing variables that cannot be made numeric

#sapply does not work with data.table so have to turn it into a data.frame
users.cluster <- as.data.frame(user)
user.numeric <- users.cluster[sapply(users.cluster,is.numeric)]
str(user.numeric)

repeat.cluster <- as.data.frame(repeat.user)
repeat.numeric <- repeat.cluster[sapply(repeat.cluster,is.numeric)]

firsttime.cluster <- as.data.frame(firsttime.user)
firsttime.numeric <- firsttime.cluster[sapply(firsttime.cluster,is.numeric)]

rm(users.cluster,repeat.cluster,firsttime.cluster)

#keep for later merging 
numeric.rownames <- data.frame(rows = row.names(user.numeric),loyalty_user_id = user$loyalty_user_id)
repeatnumeric.rownames <- data.frame(rows = row.names(repeat.numeric),loyalty_user_id = repeat.user$loyalty_user_id)
firstnumeric.rownames <- data.frame(rows = row.names(firsttime.numeric),loyalty_user_id = firsttime.user$loyalty_user_id)

# Define functions used-------------------------------
#function for scaling
scale <- function(df){
  pre_range <- preProcess(df,method = c('center','scale'))
  processed <- predict(pre_range,df)
  return(data.frame(processed))
}

#graphs for within and between sum of squares
wss_and_bss <- function(df){
  #within sum of squares
  wss <- (nrow(df)-1)*sum(apply(df,2,var))
  for (i in 1:12) wss[i] <- sum(kmeans(df, 
                                       centers=i)$withinss)
  print(plot(1:12, wss, type="b", xlab="Number of Clusters",
             ylab="Within groups sum of squares"))
  
  #between sum of squares
  bss <- (nrow(df)-1)*sum(apply(df,2,var))
  for (i in 1:12) bss[i] <- sum(kmeans(df, 
                                       centers=i)$betweenss)
  print(plot(1:12, bss, type="b", xlab="Number of Clusters",
             ylab="Between groups sum of squares"))
}

#function will return the number of clusters (n) with loyalty user id for each cluster
#user is the scaled data frame, n is number of clusters, rows is the rownames+loyalty id to merge back

kmeans.venga <- function(user,n,rows){
  fit <- kmeans(user,n)
  #get cluster means:this illustrates amount of each characteristic in each cluster
  aggregate <- aggregate(user,by=list(fit$cluster), FUN=mean)
  #append cluster assignment
  cluster_assignment <- data.frame(rows = row.names(user),user, cluster_number = fit$cluster)
  #merge in loyalty user id; this gives us the user id by cluster that we can then merge into main data set
  cluster_assignment.user <- merge(cluster_assignment,rows,by = 'rows')
  wss <- fit$withinss
  bss <- fit$betweenss
  list <- list(cluster_assignment.user,wss,bss,fit)
  return(list)
}

# I will first cluster all the data together, then repeat guests vs. non repeat

#1. All users : using user.numeric data set. --------------------------------------------------


#lot of variables, remove near zero variance and redundant (high collineariy vars), no NAs

set.seed(1234)
nzv <- nearZeroVar(user.numeric)
user.numeric <- user.numeric[, -nzv]
dim(user.numeric)

#look at correlation 
userCor <- cor(user.numeric)
corrplot(userCor, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))

#remove highly correlated values
highlyCorrelated <- findCorrelation(userCor, cutoff=0.7)
highlyCorCol <- colnames(user.numeric)[highlyCorrelated]
user.numeric.clean <- user.numeric[,-which(colnames(user.numeric) %in% highlyCorCol)]

#keep the names of the columns that were used for the clustering
reduced.names <- names(user.numeric.clean)

#scale the set; i will use user.numeric and user.numeric.clean (with highly correlated columns taken out)

#user.numeric - clustering
scaled.user.numeric <- scale(user.numeric)

#wss and bss plot to see optimal number of clusters
dev.off()

wss_and_bss(scaled.user.numeric)
#maybe 6, but optimal looks like 10


user.numeric.cluster <- kmeans.venga(scaled.user.numeric,6,numeric.rownames)
names <- c("user.cluster","user.wss","user.bss","user.fit")
names(user.numeric.cluster) <- names
names(user.numeric.cluster)
list2env(user.numeric.cluster,environment())

#build final data frame with all user observations and cluster assignments for each
user.final <- user.cluster %>% select(cluster_number,loyalty_user_id) 
user.final <- merge(user,user.final,by = 'loyalty_user_id')

#number of observations in each cluster
table(user.cluster$cluster_number)



#############DO NOT RUN, NEEDS TO BE FIXED########################
#plot by cluster
#plotcluster(scaled.user.numeric.clean, user.fit$cluster)
#clusplot(userclean.cluster,user.fit$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)
#only explain 38.19% variability, also a lot of variables. try and narrow down
###################################################################



scaled.user.numeric.clean <- scale(user.numeric.clean)

#using the cleaned user data with 15 variables
wss_and_bss(scaled.user.numeric.clean)
#7 clusters

user.numeric.clean <- kmeans.venga(scaled.user.numeric.clean,7,numeric.rownames)
names <- c("userclean.cluster","userclean.wss","userclean.bss")
names(user.numeric.clean) <- names
names(user.numeric.clean)
list2env(user.numeric.clean,environment())

#build final data frame with all user observations and cluster assignments for each
user.final <- userclean.cluster %>% select(cluster_number,loyalty_user_id) 
user.final <- merge(user,user.final,by = 'loyalty_user_id')

table(userclean.cluster$cluster_number)




#2. First time users ----------------------------------------------

set.seed(1234)
nzv <- nearZeroVar(firsttime.numeric)
firsttime.numeric <- firsttime.numeric[, -nzv]
dim(firsttime.numeric)

#look at correlation 
firstuserCor <- cor(firsttime.numeric)
corrplot(firstuserCor, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))

#remove highly correlated values
highlyCorrelated.first <- findCorrelation(firstuserCor, cutoff=0.7)
highlyCorCol.first <- colnames(firsttime.numeric)[highlyCorrelated]
firsttime.numeric.clean <- firsttime.numeric[,-which(colnames(firsttime.numeric) %in% highlyCorCol)]

#scale the set; i will use firsttime.numeric.clean (with highly correlated columns taken out)

scaled.first.numeric <- scale(firsttime.numeric.clean)

dev.off()
wss_and_bss(scaled.user.numeric)
#8 clusters

firstuser.numeric.cluster <- kmeans.venga(scaled.first.numeric,8,firstnumeric.rownames)
names <- c("first.cluster","first.wss","first.bss","first.fit")
names(firstuser.numeric.cluster) <- names
names(firstuser.numeric.cluster)
list2env(firstuser.numeric.cluster,environment())

#build final data frame with all user observations and cluster assignments for each
firstuser.final <- first.cluster %>% select(cluster_number,loyalty_user_id) 
firstuser.final <- merge(user,firstuser.final,by = 'loyalty_user_id')


#number of observations in each cluster
table(first.cluster$cluster_number)
#interesting clusters to look at: 6- outliers, 4,7,2 - biggest groups

six <- firstuser.final %>% filter(cluster_number == 6)
#cluster six: all events

four <- firstuser.final %>% filter(cluster_number == 4)


#3. Repeat users ----------------------------

set.seed(1234)
nzv <- nearZeroVar(repeat.numeric)
repeat.numeric <- repeat.numeric[, -nzv]
dim(repeat.numeric)

#look at correlation 
repeatuserCor <- cor(repeat.numeric)
corrplot(repeatuserCor, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))

#remove highly correlated values
highlyCorrelated.repeat <- findCorrelation(repeatuserCor, cutoff=0.7)
highlyCorCol.repeat <- colnames(repeat.numeric)[highlyCorrelated]
repeat.numeric.clean <- repeat.numeric[,-which(colnames(repeat.numeric) %in% highlyCorCol)]

#scale the set; i will use firsttime.numeric.clean (with highly correlated columns taken out)

scaled.repeat.numeric <- scale(repeat.numeric.clean)

dev.off()
wss_and_bss(scaled.repeat.numeric)
#6 or 7 clusters

repeat.numeric.cluster <- kmeans.venga(scaled.repeat.numeric,6,repeatnumeric.rownames)
names <- c("repeat.cluster","repeat.wss","repeat.bss","repeat.fit")
names(repeat.numeric.cluster) <- names
names(repeat.numeric.cluster)
list2env(repeat.numeric.cluster,environment())


#number of observations in each cluster
table(repeat.cluster$cluster_number)
#interesting clusters to look at:4- most obs


#build final data frame with all user observations and cluster assignments for each
repeat.final <- repeat.cluster %>% select(cluster_number,loyalty_user_id) 
repeat.final <- merge(user,repeat.final,by = 'loyalty_user_id')

#4.Final analysis, on user,first and repeat final tables---------------------------

#only keep the final analysis tables
rm(list= ls()[!(ls() %in% c('repeat.final','firstuser.final','user','user.final','reduced.names'))]) 





