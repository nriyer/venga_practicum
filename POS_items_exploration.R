#set working directory to box folder
setwd("~/Dropbox/GWU Data")
library(ggplot2)
library(dplyr)
library(magrittr)
options(scipen=999)
#merged all pos item csv files into one in bash
POSitems_sample <- read.csv('3865_pos_items_2013.csv')
POSitems <- read.csv('pos_items_final.csv')
head(POSitems_sample)
str(POSitems_sample)
table(POSitems)
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
##??
barplot(loyalty$loyalty_visit_id,loyalty$n)
  