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
#merged all pos item csv files into one in bash
POSitems_sample <- read.csv('3865_pos_items_2013.csv')
POSitems <- fread('pos_items_final.csv')
head(POSitems_sample)
str(POSitems_sample)

#look at each variable
#amount - total amount (count) of each item 
######little confused about what this is
hist(POSitems_sample$amount)
#amount_of_item
POSitems$amount_per_unit <- as.integer(POSitems$amount_per_unit)
hist(POSitems_sample$amount_per_unit)

#sort amount by count
a <- POSitems_sample %>%
  select(amount,amount_per_unit) %>%
  group_by(amount_per_unit) %>%
  count(amount) %>%
  arrange(desc(n))

#discount -- all 0 in this set
summary(POSitems_sample$discount)
#this would sort discount descending by amount of item, in this first set all discounts 
#re 0 so invalid.
d <- POSitems_sample %>%
  select(amount,discount) %>%
  group_by(amount) %>%
  arrange(desc(discount))

#loyalty visit id -- look at loyalty visit ids with most
str(POSitems_sample$loyalty_visit_id)
hist(POSitems_sample$loyalty_visit_id)
loyalty <- POSitems_sample %>%
  select(loyalty_visit_id) %>%
  count(loyalty_visit_id) %>%
  arrange(desc(n))

#item_name (messy but worth trying to roll up into a few levels? )
summary(POSitems_sample$item_category_name)
table(POSitems_sample$item_name,useNA = "always")
##USE THIS CODE FOR ROLLING UP LEVELS
##levels(own_exploration_all$coalesce_invests_in_mutual_funds_annuities) <- c("U","N","Y","U")##


#summarise(df3, cnt = n(), n_distinct(col4), mean(col7)) – 
#We then summarise the data frame created (df3) above to calculate the count of columns 1, 
#distinct count of elements in column 4 and mean of the values of column 7

#item_category_name
str(POSitems_sample$item_category_name)
table(POSitems_sample$item_category_name, useNA= "always")
items <- POSitems_sample %>%
  select(item_category_name,amount,quantity) %>%
  group_by(item_category_name)
distinct_items <- summarise(items, cnt=n(), sum(amount),sum(quantity))
  


#loyalty_category_name, can we bin these or roll up levels on factors? 
#by category?
str(POSitems_sample$loyalty_category_name)
sum(is.na(POSitems_sample$loyalty_category_name))
#no NAs
popular_items <- POSitems_sample %>%
  select(loyalty_category_name) %>%
  count(loyalty_category_name) %>%
  group_by(loyalty_category_name) %>%
  arrange(desc(n))
plot(popular_items,)

#loyalty_reservation_id
str(POSitems_sample$loyalty_reservation_id)
sum(is.na(POSitems_sample$loyalty_reservation_id))
#no NAs
reservation_id_count <- POSitems_sample %>%
  select(loyalty_reservation_id) %>%
  count(loyalty_reservation_id) %>%
  arrange(desc(n))
##?? they are all 0 i think this is in parking lot

#loyalty_visit_item_id
str(POSitems_sample$loyalty_visit_item_id)
sum(is.na(POSitems_sample$loyalty_visit_item_id))
#shouldnt be any NAs or repeats
reservation_id_count <- POSitems_sample %>%
  select(loyalty_reservation_id) %>%
  count(loyalty_reservation_id) %>%
  arrange(desc(n))

#parent_venue_id - all 3864
str(POSitems_sample$parent_venue_id)
parent_id <- POSitems_sample %>%
  select(parent_venue_id) %>%
  count(parent_venue_id)%>%
  arrange(desc(n))

#quantity of item
str(POSitems_sample$quantity)
quant_of_item <- POSitems_sample %>%
  select(itemquantity,amount)%>%
  group_by(quantity)
quant_distinct <- summarise(quant_of_item, cnt=n(),sum(amount))

#####next are foreign keys###

#venue_id - check there is only one
str(POSitems_sample$venue_id)
count_venue <- POSitems_sample %>%
  select(venue_id, parent_venue_id) %>%
  count(venue_id)


