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

res <- fread("3988_reservations.csv")
res$created_on_utc <- NULL
res$modified_on_utc <- NULL

#code - reservation code for unique event
str(res$code)
res$code <- factor(res$code, exclude = NULL)
table(res$code, useNA = "always")
distinct_code <- res %>%
  select(code,party_size) %>%
  count(code)

#loyalty_reservation_id
str(res$loyalty_reservation_id)
table(res$loyalty_reservation_id)
hist(res$loyalty_reservation_id,decreasing = TRUE)

distinct_res <- res %>%
  select(loyalty_reservation_id,party_size) %>%
  group_by(loyalty_reservation_id)  

mean_party_per_res_id <- summarise(distinct_res, cnt=n(), mean(party_size))

##????###
mean <- distinct_res %>%
        summarise(cnt=n(), mean(party_size))) %>%
        arrange(desc(cnt))

#is_walkin ---non existent and important
count_ordinal <- res %>%
  select(ordinal, loyalty_user_id) %>%
  count(ordinal) %>%
  arrange(desc(n))

loy_ord <- res %>%
  select(ordinal, loyalty_user_id) %>%
  count(loyalty_user_id) %>%
  arrange(desc(n))

count_loyalty <- summarise(loy_ord,cnt=n(),count(ordinal))

#loyalty_user_id - 1.1 , look at relationship with loyalty reservation id
str(res$loyalty_user_id)
check_distinct <- res %>%
  select(loyalty_user_id,loyalty_reservation_id) %>%
  count(loyalty_user_id,loyalty_reservation_id) %>%
  arrange(desc(n))

#example of count dplyr: flights %>% count(month, sort=TRUE)
user_to_res <- summarise(check_distinct, count(loyalty_reservation_id, sort=TRUE))

#party_size - top are 2,4,3,5,6 and then 1 (is there a way to differntiate b/w bar and table customers)
str(res$party_size)
hist(res$party_size)
top_party_sizes <- res %>%
  select(party_size) %>% count(party_size) %>%
  arrange(desc(n))

#server_name
str(res$server_name)
res$server_name <- as.factor(res$server_name)
levels(res$server_name)
##total covers for server....in order from most to least and average party size
tables <- res %>%
  select(server_name,party_size) %>%
  group_by(server_name) %>%
  summarise(cnt = n(),mean(party_size)) %>%
  arrange(desc(cnt))

server_count <- res %>%
  select(server_name) %>%
  count(server_name) %>%
  arrange(desc(n)) %>%
  top_n(10)
##plot top 10
  qplot(server_count$server_name,server_count$n, geom="bar",stat=)
str(server_count$n)

#state - no show,paid
str(res$state)
res$state <- factor(res$state, exclude=NULL)
table(res$state,useNA = "always")

#table name -name of table in res system
str(res$table_name)
hist(as.numeric(res$table_name))
str(as.factor(res$table_name))

count_tables <- res %>%
  select(table_name,party_size) %>%
  summarise(cnt=n_distinct(table_name),mean(party_size))

tables_count <- res %>%
  select(table_name,party_size) %>%
  count(table_name) %>%
  arrange(desc(n))
  