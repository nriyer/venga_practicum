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
library(RColorBrewer)
options(scipen=999)
options( java.parameters = "-Xmx4g" )

#Final analysis based on three groups; first,repeat, and all users

#This analysis is using the clusters built from the 15 variable reduced set.
#These are the columns that built the actual clusters, after dimension reduction:
reduced.names

#Basing the columns off of segmentation categories

#functions for analysis

total_rev_cluster <- function(df){
  total_rev <- c()
  for (i in 1:max(df$cluster_number)){
    total_rev[i] <- df %>% filter(cluster_number == i) %>% summarise(total_revenue = sum(avg_spend))
  }
  total_rev.cluster <- data.frame(cluster.number = 1:max(df$cluster_number),total_revenue = t(as.data.frame(total_rev)))
  return(total_rev.cluster)
}

hour_month <- function(df){
  df$hour <- substring(df$first_visit_date,12,13)
  df$month <- substring(df$first_visit_date,6,7)
  return(df)
}

highest_lowest <- function(h,l,df,n){
  #highest
  high <- df %>% filter(cluster_number == h)
  foodvswine.high <- high %>% select(avg_Food_spend,avg_Wine_spend,avg_Liquor.Beer_spend) %>% 
    summarise(food = sum(avg_Food_spend), wine = sum(avg_Wine_spend), liquor = sum(avg_Liquor.Beer_spend))
  
  #lowest
  low <- df %>% filter(cluster_number == l)
  foodvswine.low <- low %>% select(avg_Food_spend,avg_Wine_spend,avg_Liquor.Beer_spend) %>% 
    summarise(food = sum(avg_Food_spend), wine = sum(avg_Wine_spend), liquor = sum(avg_Liquor.Beer_spend))
  
  #looking at the hours and total covers
  covers.high <- high %>% select(max_cover_count,first_visit_date) %>%
    summarise(covers= mean(max_cover_count)) 
  #add hour to see if there is a pattern in hour by cluster
  time.high <- high %>% select(hour) %>% group_by(hour) %>% summarise(n = n())
  
  covers.low<- low %>% select(max_cover_count,first_visit_date) %>%
    summarise(covers= mean(max_cover_count)) 
  #add hour to see if there is a pattern in hour by cluster
  time.low <- low %>% select(hour) %>% group_by(hour) %>% summarise(n = n())
  
  highlow <- list(foodvswine.high,foodvswine.low,covers.high,covers.low,time.high,time.low)
  names(highlow) <- c(paste0("foodvswine.high",n),paste0("foodvswine.low",n),paste0("covers.high",n),
                       paste0("covers.low",n),paste0("time.high",n),paste0("time.low",n))
  return(highlow)
}


month_hour <- function(df,title){
  bymonth <- df %>% select(cluster_number,month) %>% count(cluster_number,month) %>% arrange(cluster_number,month)
  
  print(ggplot(data = bymonth, aes(x=factor(month), y=n)) + geom_bar(stat="identity") + ggtitle(title)+facet_wrap(~cluster_number,scales="free")) 
  
  
  #look at number of people by time - by cluster
  byhour <- df %>% select(cluster_number,hour,first_weekend_visit) %>% count(cluster_number,hour,first_weekend_visit) %>% arrange(cluster_number,hour)
  
  #this looks at number of guests, by hour and if they were weekend (blue) or not.
  print(ggplot(data = byhour, aes(x=factor(hour), y=n, color = factor(first_weekend_visit))) +  
          geom_bar(stat="identity") + 
          ggtitle(title) +
          facet_wrap(~cluster_number,scales="free"))
}

holidays <- function(df){
    holidays.cluster <- df %>% select(first_newyears_visit,first_christmas_visit,first_valentine_visit,cluster_number)
  
  new_years <-holidays.cluster %>% 
    count(cluster_number,first_newyears_visit) %>% 
    filter(first_newyears_visit == 1) %>%
    arrange(first_newyears_visit)
  
  christmas <- holidays.cluster %>% 
    count(cluster_number,first_christmas_visit) %>% 
    filter(first_christmas_visit == 1) %>%
    arrange(first_christmas_visit)
  
  valentine <- holidays.cluster %>% 
    count(cluster_number,first_valentine_visit) %>% 
    filter(first_valentine_visit == 1) %>%
    arrange(first_valentine_visit)
  
  new_years$first_newyears_visit <- new_years$n
  new_years$n <- NULL
  christmas$first_christmas_visit <- christmas$n
  christmas$n <- NULL
  valentine$first_valentine_visit <- valentine$n
  valentine$n <- NULL
  
  all.holidays <- merge(christmas,new_years,by = "cluster_number",all= TRUE)
  all.holidays <- merge(all.holidays,valentine, by = "cluster_number",all=TRUE)
  all.holidays <- as.data.frame(all.holidays)
  
  for (i in 2:length(all.holidays)){
    holidays.plot <- ggplot(data = all.holidays, aes(x=factor(cluster_number), y=all.holidays[,i])) +  
      geom_bar(stat="identity") + labs(y = names(all.holidays)[i]) 
    print(holidays.plot)
  }
}

discounts <- function(df){
  discounts.cluster <- df %>% select(max_party_size,max_discount,max_spend,cluster_number)
  
  discounts.bycluster <- discounts.cluster %>% group_by(cluster_number,max_party_size) %>% 
    summarise(total_discounts = sum(max_discount)) %>%
    arrange(cluster_number,max_party_size)
  
  
  discounts.patterns <- discounts.cluster %>% group_by(cluster_number,max_party_size) %>% 
    summarise(total_check_withdiscounts = sum(max_spend) - sum(max_discount)) %>%
    arrange(cluster_number,max_party_size)
  
  discount.bycluster.plot <- ggplot(data = discounts.bycluster, aes(x=factor(max_party_size), y=abs(total_discounts) )) +  
    geom_bar(stat="identity") + facet_wrap(~cluster_number,scales="free")
  print(discount.bycluster.plot)
  
  discounts.pattern.plot <- ggplot(data = discounts.patterns, aes(x=factor(max_party_size), y=abs(total_check_withdiscounts) )) +  
    geom_bar(stat="identity") + facet_wrap(~cluster_number,scales="free")
  print(discounts.pattern.plot)
}


#1.Total Revenue ---------------

#adding 'hour' and 'month' columns

firstuser.final <- hour_month(firstuser.final)
repeat.final <- hour_month(repeat.final)
user.final <- hour_month(user.final)


#Total revenue by cluster
first.users.revenue <- total_rev_cluster(firstuser.final)
return.users.revenue <- total_rev_cluster(repeat.final)
users.revenue <- total_rev_cluster(user.final)


#First time results
ggplot(first.users.revenue, aes(x = factor(cluster.number), y = total_revenue)) + 
  geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + ggtitle("Total Revenue Non-Repeat") 

#Cluster 3 has the most revenue, 2 is lowest. 

#Repeat time results
ggplot(return.users.revenue, aes(x = factor(cluster.number), y = total_revenue)) + 
  geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + ggtitle("Total Revenue Repeat")

#Cluster 6 has the most revenue, 3 is lowest. 

#All Users
ggplot(users.revenue, aes(x = factor(cluster.number), y = total_revenue)) + 
  geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + ggtitle("Total Revenue All Customers")

#Cluster 6 has the most revenue, 2 is lowest. 

#Look at highest and lowest revenue------------------------------

#First time users
first.list <- highest_lowest(3,2,firstuser.final,"first")
list2env(first.list,environment())
#high 
foodvswine.highfirst
ggplot(data=time.highfirst,aes(x=factor(hour), y=n)) + geom_bar(stat="identity")
#low
foodvswine.lowfirst
ggplot(data=time.lowfirst,aes(x=factor(hour), y=n)) + geom_bar(stat="identity")

#Repeat users
repeat.list <- highest_lowest(6,3,repeat.final,"repeat")
list2env(repeat.list,environment())
#high 
foodvswine.highrepeat
ggplot(data=time.highrepeat,aes(x=factor(hour), y=n)) + geom_bar(stat="identity")
#low
foodvswine.lowrepeat
ggplot(data=time.lowrepeat,aes(x=factor(hour), y=n)) + geom_bar(stat="identity")

#All users
users.list <- highest_lowest(6,2,user.final,"all")
list2env(users.list,environment())
#high 
foodvswine.highall
ggplot(data=time.highall,aes(x=factor(hour), y=n)) + geom_bar(stat="identity")
#low
foodvswine.lowall
ggplot(data=time.lowall,aes(x=factor(hour), y=n)) + geom_bar(stat="identity")


#look at number of people by month - by cluster

month_hour(firstuser.final,"first_user")

#Repeat
month_hour(repeat.final, "Repeat")

#All
month_hour(user.final, "All_Users")


#definite clusters for dinner/brunch and happy hour - can we further define these clusters? 

#cluster 2- happy hour with a lot of covers per check?
#cluster 3 - weekday dinner
#cluster 4- weekday breakfast
#cluster 5 - brunch (weekend)
#cluster 6 - weekday dinner
#cluster 7 - spread out but mostly weekday diners
#cluster 8 - weekend dinner (second highest revenue to 3)

#weekday dinner revenue is higher than weekend but with day to revenue ratio; we should be targeting weekend customers (5 and 8)
#-------

#look at holidays by cluster

holidays(firstuser.final)

holidays.cluster <- firstuser.final %>% select(first_newyears_visit,first_christmas_visit,first_valentine_visit,cluster_number)

new_years <-holidays.cluster %>% 
  count(cluster_number,first_newyears_visit) %>% 
  filter(first_newyears_visit == 1) %>%
   arrange(first_newyears_visit)

christmas <- holidays.cluster %>% 
  count(cluster_number,first_christmas_visit) %>% 
    filter(first_christmas_visit == 1) %>%
    arrange(first_christmas_visit)

valentine <- holidays.cluster %>% 
  count(cluster_number,first_valentine_visit) %>% 
  filter(first_valentine_visit == 1) %>%
  arrange(first_valentine_visit)

new_years$first_newyears_visit <- new_years$n
new_years$n <- NULL
christmas$first_christmas_visit <- christmas$n
christmas$n <- NULL
valentine$first_valentine_visit <- valentine$n
valentine$n <- NULL

all.holidays <- merge(christmas,new_years,by = "cluster_number",all= TRUE)
all.holidays <- merge(all.holidays,valentine, by = "cluster_number",all=TRUE)
all.holidays <- as.data.frame(all.holidays)

for (i in 2:length(all.holidays)){
  holidays.plot <- ggplot(data = all.holidays, aes(x=factor(cluster_number), y=all.holidays[,i])) +  
  geom_bar(stat="identity") + labs(y = names(all.holidays)[i]) 
  print(holidays.plot)
}

holidays(repeat.final)

#8 has the highest christmas visitors and also highest revenue by days. 
#cluster 3
total_rev_weekday_dinners <- total_rev.cluster[3,2]/5
#cluster 8
total_rev_weekend_dinners <- total_rev.cluster[8,2]/2

#Target new guests around christmas time to come in on the weekends near christmas.

#Discounts

discounts.cluster <- firstuser.final %>% select(max_party_size,max_discount,max_spend,cluster_number)

discounts.bycluster <- discounts.cluster %>% group_by(cluster_number,max_party_size) %>% 
  summarise(total_discounts = sum(max_discount)) %>%
  arrange(cluster_number,max_party_size)


discounts.patterns <- discounts.cluster %>% group_by(cluster_number,max_party_size) %>% 
  summarise(total_check_withdiscounts = sum(max_spend) - sum(max_discount)) %>%
  arrange(cluster_number,max_party_size)

discount.bycluster.plot <- ggplot(data = discounts.bycluster, aes(x=factor(max_party_size), y=abs(total_discounts) )) +  
  geom_bar(stat="identity") + facet_wrap(~cluster_number,scales="free")
discount.bycluster.plot

discounts(firstuser.final)

#most discounts also given during the two shifts that make most revenue and in 2 --> seems to be a strange cluster
#5 is a high revenue but least discount cluster. target for weekend brunch




