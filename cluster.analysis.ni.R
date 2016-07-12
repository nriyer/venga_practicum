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
  total_rev.cluster <- data.frame(cluster.number = 1:8,total_revenue = t(as.data.frame(total_rev)))
  return(total_rev.cluster)
}



#1. First time users

#adding 'hour' and 'month' columns

firstuser.final$hour <- substring(firstuser.final$first_visit_date,12,13)
firstuser.final$month <- substring(firstuser.final$first_visit_date,6,7)

#total revenue by cluster
first.users.revenue <- total_rev_cluster(firstuser.final)
#plot results
ggplot(first.users.revenue, aes(x = factor(cluster.number), y = total_revenue)) + 
  geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + ggtitle("Total Revenue Non-Repeat")

#clusters 3 has the most revenue, 2 is a lowest. 

#cluster 3
cluster3 <- firstuser.final %>% filter(cluster_number == 3)
foodvswine.3 <- cluster3 %>% select(avg_Food_spend,avg_Wine_spend,avg_Liquor.Beer_spend) %>% 
  summarise(food = sum(avg_Food_spend), wine = sum(avg_Wine_spend), liquor = sum(avg_Liquor.Beer_spend))


#cluster 2
cluster2 <- firstuser.final %>% filter(cluster_number == 2)
foodvswine.2 <- cluster2 %>% select(avg_Food_spend,avg_Wine_spend,avg_Liquor.Beer_spend) %>% 
  summarise(food = sum(avg_Food_spend), wine = sum(avg_Wine_spend), liquor = sum(avg_Liquor.Beer_spend))

# the lowest revenue comes from a group that seems to be more focused around happy hour. Highest
# are diners. we can verify by looking at the hours and total covers

covers.3 <- cluster3 %>% select(max_cover_count,first_visit_date) %>%
  summarise(covers= mean(max_cover_count)) 
#add hour to see if there is a pattern in hour by cluster
time.3 <- cluster3 %>% select(hour) %>% group_by(hour) %>% summarise(n = n())


covers_time.2 <- cluster2 %>% select(max_cover_count,max_diffmins_rsvp_open,first_visit_date) %>%
  summarise(covers= mean(max_cover_count), time_spent = mean(max_diffmins_rsvp_open)) 
#really strange, extremely high cover counts (bar?)
#add hour to see if there is a pattern in hour by cluster
cluster2$hour <- substring(cluster2$first_visit_date,12,13)
time.2 <- cluster2 %>% select(hour) %>% group_by(hour) %>% summarise(cover = n())


#look at number of people by month - by cluster

bymonth <- firstuser.final %>% select(cluster_number,month) %>% count(cluster_number,month) %>% arrange(cluster_number,month)

months.plot <- ggplot(data = bymonth, aes(x=factor(month), y=n)) + geom_bar(stat="identity") + facet_wrap(~cluster_number) 
months.plot

#does not look to be any difference in seasonality. all clusters have max users in december

#look at number of people by time - by cluster
byhour <- firstuser.final %>% select(cluster_number,hour,first_weekend_visit) %>% count(cluster_number,hour,first_weekend_visit) %>% arrange(cluster_number,hour)

#this looks at number of guests, by hour and if they were weekend (blue) or not.
hour.plot <- ggplot(data = byhour, aes(x=factor(hour), y=n, color = factor(first_weekend_visit))) +  
  geom_bar(stat="identity") + 
  facet_wrap(~cluster_number,scales="free")
hour.plot

#definite clusters for dinner/brunch and happy hour - can we further define these clusters? 

#cluster 2- happy hour with a lot of covers per check?
#cluster 3 - dinner
#cluster 4- brunch
#cluster 5 - lunch
#cluster 7 - spread out (look for something else that differentiates these people)
#cluster 8 - dinner (second highest revenue to 3)


#look at holidays by cluster

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

holidays <- cbind(new_years,valentine,christmas)


