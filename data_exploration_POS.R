#set working directory to box folder
setwd("~/Dropbox/GWU Data")
library(ggplot2)
library(plyr)
library(dplyr)
library(magrittr)
options(scipen=999)
POS <- as.data.frame(read.csv("3865_pos.csv"))
#clean up data set and make sure each variable is according to metadata
## why would you ignore the check number? could that provide added information?
str(POS)
POSclean <- POS[,-c(1,3,9)]
str(POSclean)
#convert variables to the correct data type
POSclean$loyalty_visit_id <- as.factor(POSclean$loyalty_visit_id)
POSclean$loyalty_reservation_id <- as.factor(POSclean$loyalty_reservation_id)
POSclean$loyalty_user_id <- as.factor(POSclean$loyalty_user_id)
POSclean$parent_venue_id <- as.factor(POSclean$ordinal)
POSclean$server_id <- as.factor(POSclean$server_id)
POSclean$server_name <- as.factor(POSclean$server_name)
#convert date time variables
POSclean$open_date <- as.Date(POSclean$open_date, "%m/%d/%Y")
#look at distribution of variables
#POS cover count
head(POSclean)
#summary of all variables
#what is the format of date
#why is gratuity all 0
#what does a positive discount signify?
summary(POSclean)
#take out all ordinals that are not 1
POSordinal <- POSclean[-which(POSclean$ordinal != 1),]
#distribution cover count
ggplot(POSclean, aes(x = factor(cover_count))) + geom_bar(stat = "count")
mean(POSclean$cover_count)
range(POSclean$cover_count)

#discount
#look at frequency
hist(POSclean$discount)
counts <- as.data.frame(count(POSclean,vars = "discount"))
summary(counts)
#order counts descending
counts_sorted <- counts[with(counts, order(-freq)), ]

#loyalty_visit_id
summary(POSclean$loyalty_visit_id)
counts <- as.data.frame(count(POSclean,vars = "loyalty_visit_id"))
counts_loyalty <- counts[with(counts, order(-freq)), ]
summary(POSordinal$loyalty_visit_id)

#loyalty_reservation_id
#why do reservation ids repeat, different price and different visit ids
summary(POSordinal$loyalty_reservation_id)
counts.r <- as.data.frame(count(POSordinal,vars = "loyalty_reservation_id"))
counts_loyalty_res <- counts.r[with(counts.r, order(-freq)), ]
check <- as.data.frame(POSordinal[which(POSordinal$loyalty_reservation_id=='63561364'),])

#loyalty_user_id
summary(POSordinal$loyalty_user_id)
counts.u <- as.data.frame(count(POSordinal,vars = "loyalty_user_id"))
counts_loyalty_user <- counts.u[with(counts.u, order(-freq)), ]

#ordinal date
#convert to date
POSordinal$open_date <- as.POSIXct(POSordinal$open_date,origin="1970-01-01",z="GMT")
summary(POSordinal$open_date)

##EXAMPLE OF DPLYR
#server_id
summary(POSordinal$server_id)
##dplyr like sql
server.totalamount <- POSordinal %>% 
  select(server_id,server_name,spend) %>%
  group_by(server_name) %>%
  summarise(total_sold = sum(spend)) %>%
  arrange(-total_sold)


