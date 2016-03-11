#set working directory to box folder
setwd("~/Dropbox/GWU Data")
options( java.parameters = "-Xmx4g" )
library(ggplot2)
library(dplyr)
library(magrittr)
library(Hmisc)
library(caret)
library(ff)
library(data.table)
options(scipen=999)
POS <- fread("3865_pos.csv")
#clean up data set and make sure each variable is according to metadata
## why would you ignore the check number? could that provide added information?
POS <- as.data.frame(POS)
head(POS)
POSclean <- POS[,-c(1,3,9)]
str(POSclean)

#convert variables to the correct data type

######trying to use lapply
make_factor <- function(x) {
  as.factor(names(x))
  x
}
names(POSclean[,4:6])
as.factor(POSclean[,4])
POSclean[,4:6] <- lapply(POSclean[,4:6], make_factor)
str(POSclean)
########### why doesnt that work (above)


POSclean$loyalty_visit_id <- as.factor(POSclean$loyalty_visit_id)
POSclean$loyalty_reservation_id <- as.factor(POSclean$loyalty_reservation_id)
POSclean$loyalty_user_id <- as.factor(POSclean$loyalty_user_id)
POSclean$parent_venue_id <- as.factor(POSclean$ordinal)
POSclean$server_id <- as.factor(POSclean$server_id)
POSclean$server_name <- as.factor(POSclean$server_name)
str(POSclean)

#look at distribution of variables
#POS cover count
head(POSclean)
#summary of all variables
#what is the format of date
#why is gratuity all 0 --> no gratuity for this restaurant
#what does a positive discount signify?
summary(POSclean)
#take out all ordinals that are not 1
POSordinal <- POSclean[-which(POSclean$ordinal != 1),]

#ordinal date
#convert to date
POSordinal$open_date <- as.POSIXct(POSordinal$open_date,origin="1970-01-01",tz="GMT")
summary(POSordinal$open_date)

POSordinal$visit_date <- as.POSIXct(POSordinal$visit_date,origin="1970-01-01",tz="GMT")
summary(POSordinal$visit_date)

#distribution cover count
ggplot(POSclean, aes(x = factor(cover_count))) + geom_bar(stat = "count")
mean(POSclean$cover_count)
range(POSclean$cover_count)

#distinct counts using dplyr
#look at frequency of aggregate variables

#cover_count
#cover_count already in descending order
counts.cover <- as.data.frame(count(POSclean,cover_count))
summary(counts.cover)
#this data could be skewed based on server error

#loyalty_visit_id - it is 1.1 relationship and this checks out
counts.visit_id <- arrange(as.data.frame(count(POSclean,loyalty_visit_id)),n)

#loyalty_reservation_id
#why do reservation ids repeat, different price and different visit ids
summary(POSordinal$loyalty_reservation_id)
counts.res <- arrange(as.data.frame(count(POSordinal,loyalty_reservation_id)),desc(n))



#loyalty_user_id
summary(POSordinal$loyalty_user_id)
counts.user <- arrange(as.data.frame(count(POSordinal,loyalty_user_id)),desc(n))

#discount
summary(POSordinal$discount)
counts.discount <- arrange(as.data.frame(count(POSordinal,discount)),desc(n))
#small amount of distinct discount items, can join this back to POS items to see if there is 
#reason for discounts on these items 

#opendate-- could possibly use these counts to look at patterns and for seasonality
#also worth breaking in to day/month/day part to look at this in detail.
summary(POSordinal$open_date)
counts.open_date <- arrange(as.data.frame(count(POSordinal,open_date)),desc(n))

#visitdate ???
summary(POSordinal$visit_date)
counts.visit_date <- arrange(as.data.frame(count(POSordinal,visit_date)),desc(n))

#server_id,server_name 
counts.server_info <- POSordinal %>%
  select(server_id,server_name,spend) %>%
  count(server_id,server_name) %>%
  arrange(desc(n)) 

#some servers do not have IDs....these also seem to be the servers with name showing up repeat times in data
server.total_amount <- POSordinal %>% 
  select(server_id,server_name,spend) %>%
  group_by(server_name) %>%
  summarise(total_sold = sum(spend)) %>%
  arrange(-total_sold)



#aggregates for more information about POS data using DPLYR

#server_id
summary(POSordinal$server_id)
#find total amount of sales for each server

server.total_amount <- POSordinal %>% 
  select(server_id,server_name,spend) %>%
  group_by(server_name) %>%
  summarise(total_sold = sum(spend)) %>%
  arrange(-total_sold)

#server with most covers
server.totalamount <- POSordinal %>% 
  select(server_id,server_name,spend) %>%
  group_by(server_name) %>%
  summarise(total_sold = sum(spend)) %>%
  arrange(-total_sold)


#spend
str(POSordinal$spend)
#avg per person
spend.total_amount <- POSordinal %>% 
  select(cover_count,spend) %>%
  group_by(as.factor(cover_count)) %>%
  summarise(total_sold = sum(spend),mean(spend),per_person = mean(spend)/4) 
