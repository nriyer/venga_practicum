setwd("~/Dropbox/GWU Data")
library(dplyr)
library(ggplot2)
library(data.table)  
library(tidyr)
library(reshape2)
options( java.parameters = "-Xmx4g" )


reservations <- fread("3865_reservations.csv",check.names = T,
                      header = T)
reservations$loyalty_user_id <- factor(reservations$loyalty_user_id)
reservations$loyalty_reservation_id <-factor(reservations$loyalty_reservation_id)
reservations$made_on <- as.POSIXct(reservations$made_on,origin="1970-01-01",tz="GMT")
reservations$reservation_date <-as.POSIXct(reservations$reservation_date,
                                           origin="1970-01-01",tz="GMT")

pos <- fread("3865_pos.csv",check.names = T,header=T)
pos$created_on_utc <- as.POSIXct(pos$created_on_utc, origin="1970-01-01",tz="GMT")
pos$modified_on_utc <- as.POSIXct(pos$modified_on_utc, origin="1970-01-01",tz="GMT")
pos$open_date <- as.POSIXct(pos$open_date,origin = "1970-01-01", tz = "GMT")
pos$loyalty_visit_id <- as.factor(pos$loyalty_visit_id)
pos$loyalty_reservation_id <- as.factor(pos$loyalty_reservation_id)
pos$loyalty_user_id <- as.factor(pos$loyalty_user_id)
pos$server_id <- as.factor(pos$server_id)

pos_items <- fread("pos_items_final.csv",check.names = T, header = T)
pos_items$amount <- as.numeric(pos_items$amount)
pos_items$amount_per_unit <- as.numeric(pos_items$amount_per_unit)
pos_items$discount <- as.numeric(pos_items$discount)
pos_items$quantity <- as.numeric(pos_items$quantity)

pos_items$loyalty_visit_id <- as.factor(pos_items$loyalty_visit_id)
pos_items$loyalty_reservation_id <- as.factor(pos_items$loyalty_reservation_id)
pos_items$loyalty_visit_item_id <- as.factor(pos_items$loyalty_visit_item_id)

pos_items$item_category_name <- as.factor(pos_items$item_category_name)
pos_items$item_name <- as.factor(pos_items$item_name)
pos_items$item_subcategory_name <- as.factor(pos_items$item_subcategory_name)


#2. Merging Reservations with POS--------------
cmb_rsvp_pos <- merge(reservations,pos,by="loyalty_reservation_id")
cmb_rsvp_pos$loyalty_user_id.y <- NULL
names(cmb_rsvp_pos)[4] <- "loyalty_user_id"

#why not just use distinct?
duplicates_flag <-duplicated(cmb_rsvp_pos,by=c("loyalty_reservation_id","loyalty_user_id",
                                               "discount","loyalty_visit_id","spend"))
cmb_rsvp_pos <- cmb_rsvp_pos[!duplicates_flag,]
cmb_rsvp_pos <- cmb_rsvp_pos %>% 
  filter(!loyalty_user_id %in% c('12484585','14324743','0') & 
           loyalty_reservation_id != '0')

#3.Creating Fact Table from joined RSVP & POS------------------
rsvp_pos_fact <- cmb_rsvp_pos %>%
  group_by(loyalty_user_id,loyalty_reservation_id) %>%
  summarise(party_size=min(party_size),
            pos_server_name=min(server_name.y),
            pos_table=min(table_name.y),
            state=min(state),
            cover_count=max(cover_count),
            discount=sum(discount),   
            spend=sum(spend),
            made_on=min(made_on),
            rsvp_date=min(reservation_date),
            open_date=min(open_date))

rsvp_pos_fact$elapsed_days_madeon_open <- round(with(rsvp_pos_fact,
                                                     difftime(open_date,made_on,
                                                              units = "days")),4)
rsvp_pos_fact$elapsed_mins_rsvp_open <-round(with(rsvp_pos_fact,
                                                  difftime(open_date,rsvp_date,
                                                           units="mins")),2)

rsvp_pos_fact$day_visit <- factor( weekdays(rsvp_pos_fact$open_date),
                                   levels = c("Sunday","Monday","Tuesday", "Wednesday",
                                              "Thursday","Friday","Saturday"))

rsvp_pos_fact$year_visit <- year(rsvp_pos_fact$open_date)

rsvp_pos_fact$month_visit <- month(rsvp_pos_fact$open_date)

rsvp_pos_fact$day_of_month_visit <- as.integer(format(rsvp_pos_fact$open_date,"%d"))

rsvp_pos_fact$weekday_visit <- with(rsvp_pos_fact,ifelse(day_visit %in% c("Monday","Tuesday",
                                                                          "Wednesday","Thursday"), 1,0))
rsvp_pos_fact$weekend_visit <- with(rsvp_pos_fact,ifelse(day_visit %in% c("Friday","Saturday",
                                                                          "Sunday"), 1,0))

rsvp_pos_fact$hour_visit <- as.numeric(format(rsvp_pos_fact$open_date,"%H"))

rsvp_pos_fact$breakfast_visit <-with(rsvp_pos_fact,ifelse(!(day_visit %in% c("Saturday","Sunday")) & 
                                                            between(hour_visit,8,11),1,0))
rsvp_pos_fact$lunch_visit <-with(rsvp_pos_fact,ifelse(!(day_visit %in% c("Saturday","Sunday")) & 
                                                        between(hour_visit,11,15),1,0))

rsvp_pos_fact$dinner_visit <-with(rsvp_pos_fact,ifelse(between(hour_visit,16,23),1,0))

rsvp_pos_fact$brunch_visit <-with(rsvp_pos_fact,ifelse(day_visit %in% c("Saturday","Sunday") & 
                                                         between(hour_visit,11,15),1,0))

rsvp_pos_fact$happy_hour_visit <-with(rsvp_pos_fact,ifelse(!(day_visit %in% c("Friday","Saturday","Sunday")) & 
                                                             between(hour_visit,17,18),1,0))


# 4. Adding holiday flags to Master Fact Table------------------
#Christmas is any day +/- 5 from December 25th
#New Year's is December 31st or January 1st
#Valentine's is any day  +/- 5 from February 14th
#Mother's Day is the Second Saturday or Sunday in May


#takes years in the dataset as input and returns Mother's Day for each of the year
fun_mothers_day <- function(years){
  may_first <- as.Date(paste0(years,"-05-01"))
  mothers_day <- c()
  for(i in 1:length(may_first)){
    days <- seq(from=may_first[i],by="day",length.out = 14)
    sundays <- days[weekdays(days)=="Sunday"]
    mothers_day[i] <- as.character(max(sundays))
  }
  return(mothers_day)
}

#so is 'with' used to go through entire df and apply if else to various columns?
rsvp_pos_fact$christmas_flag <-with(rsvp_pos_fact,
                                    ifelse(month_visit==12 & between(day_of_month_visit,24,26),1,0))

rsvp_pos_fact$newyears_flag <- ifelse((rsvp_pos_fact$month_visit == 12 & 
                                         rsvp_pos_fact$day_of_month_visit == 31) |
                                                           (rsvp_pos_fact$month_visit == 1 & rsvp_pos_fact$day_of_month_visit == 1),1,0)
rsvp_pos_fact$valentine_flag <-with(rsvp_pos_fact,
                                    ifelse(month_visit==2 & between(day_of_month_visit,9,19),1,0))

years <- levels(as.factor(rsvp_pos_fact$year_visit))
all_mothers_days <- as.Date(fun_mothers_day(years))

rsvp_pos_fact$mother_day <- 0

for(i in 1:length(all_mothers_days))
  rsvp_pos_fact$mother_day <- with(rsvp_pos_fact,ifelse(year_visit ==year(all_mothers_days[i]),
                                                        mday(all_mothers_days[i]),mother_day))
rsvp_pos_fact$mother_day_flag <- with(rsvp_pos_fact,ifelse(month_visit==5 & 
                                                             between(day_of_month_visit,mother_day-1,
                                                                     mother_day),1,0))
rsvp_pos_fact$mother_day <- NULL
rm(fun_mothers_day,years,all_mothers_days)

# 5. Creating aggregates by user and extracting first visit info--------------------------

aggr_rsvp_pos <- rsvp_pos_fact %>% 
  group_by(loyalty_user_id) %>%
  summarise(num_of_repeats = n(),
            min_party_size = min(party_size), 
            max_party_size = max(party_size),
            avg_party_size = mean(party_size),
            min_cover_count = min(cover_count), 
            max_cover_count = max(cover_count),
            avg_cover_count = mean(cover_count),
            min_discount = min(discount), 
            max_discount = max(discount),
            avg_discount = mean(discount),                      
            min_spend = min(spend), 
            max_spend = max(spend),
            avg_spend = mean(spend), 
            min_diffdays_madeon_open = min(elapsed_days_madeon_open), 
            max_diffdays_madeon_open = max(elapsed_days_madeon_open),
            avg_diffdays_madeon_open = mean(elapsed_days_madeon_open),
            min_diffmins_rsvp_open = min(elapsed_mins_rsvp_open), 
            max_diffmins_rsvp_open = max(elapsed_mins_rsvp_open),
            avg_diffmins_rsvp_open = mean(elapsed_mins_rsvp_open),
            avg_weekday_visits = mean(weekday_visit),
            avg_weekend_visits = mean(weekend_visit),
            avg_breakfast_visits= mean(breakfast_visit),
            avg_lunch_visits = mean(lunch_visit),
            avg_dinner_visits = mean(dinner_visit),
            avg_brunch_visits = mean(brunch_visit),
            avg_christmas_visits = mean(christmas_flag),
            avg_newyears_visits = mean(newyears_flag),
            avg_valentine_visits = mean(valentine_flag),
            avg_mothers_day_visits = mean(mother_day_flag)
  )
aggr_rsvp_pos$has.repeated <- ifelse(aggr_rsvp_pos$num_of_repeats > 1,"yes","no")

first_rsvp_pos<- rsvp_pos_fact %>% 
  group_by(loyalty_user_id) %>%
  filter(open_date == min(open_date)) %>%
  summarise(first_party_size = party_size,
            first_server_name = pos_server_name,
            first_table = pos_table,
            first_discount = discount,
            first_spend = spend,
            first_visit_date = open_date,
            first_diffdays_madeon_visit = elapsed_days_madeon_open,
            first_diffmins_rsvp_open = elapsed_mins_rsvp_open,
            first_weekday_visit = weekday_visit,
            first_weekend_visit = weekend_visit,
            first_breakfast = breakfast_visit,
            first_lunch_visit = lunch_visit,
            first_dinner_visit = dinner_visit,
            first_brunch_visit = brunch_visit,
            first_christmas_visit = christmas_flag,
            first_newyears_visit = newyears_flag,
            first_valentine_visit = valentine_flag,
            first_mother_day_flag = mother_day_flag)

#6. Creating dimensions about the server from RSVP & POS items----------------------
server_source <- rsvp_pos_fact %>% 
  group_by(loyalty_user_id,pos_server_name) %>%
  summarise(n = n()) %>%
  mutate(max_same_server = max(n)) 

server_repeats <- server_source %>%
  select(loyalty_user_id, max_same_server) %>%
  distinct(loyalty_user_id,
           max_same_server)

#added by nisha- accounts for which server served each guest the max amount
server_favorites_source <- rsvp_pos_fact %>% 
  group_by(loyalty_user_id,pos_server_name) %>%
  summarise(n = n()) %>%
  mutate(max_same_server = max(n)) %>%
  filter(max_same_server == n)

server_favorites <- server_favorites_source %>%
  select(loyalty_user_id,pos_server_name) %>%
  distinct(loyalty_user_id,pos_server_name)

#original, counts different servers
server_counts <- server_source %>%
  group_by(loyalty_user_id) %>%
  summarise(num_of_different_servers=n())

fact_server <- merge(server_counts,server_repeats,by="loyalty_user_id")

fact_server_faves <- merge(fact_server,server_favorites,by="loyalty_user_id")
fact_server_faves <- distinct(fact_server_faves)

rm(server_source,server_repeats,server_counts,server_favorites,server_favorites_source)

#7. Creating dimensions about the table from RSVP & POS items---------------------------

table_source <- rsvp_pos_fact %>% 
  group_by(loyalty_user_id,pos_table) %>%
  summarise(n = n()) %>%
  mutate(max_same_table = max(n))

table_repeats <- table_source %>%
  select(loyalty_user_id,max_same_table) %>%
  distinct(loyalty_user_id,max_same_table)

#added by nisha- accounts for which table the guest was at the max amount
table_favorites_source <- rsvp_pos_fact %>% 
  group_by(loyalty_user_id,pos_table) %>%
  summarise(n = n()) %>%
  mutate(max_same_table = max(n)) %>%
  filter(max_same_table == n)

table_favorites <- table_favorites_source %>%
  select(loyalty_user_id,pos_table) %>%
  distinct(loyalty_user_id,pos_table)


table_counts <- table_source %>%
  group_by(loyalty_user_id) %>%
  summarise(num_of_different_tables=n())

fact_table <- merge(table_counts,table_repeats,by="loyalty_user_id")
#fact table with favorite table of each loyalty user
fact_table_favorites <- merge(fact_table,table_favorites, by = "loyalty_user_id")
fact_table_favorites <- distinct(fact_table_favorites)

rm(table_source,table_repeats,table_counts,table_favorites,table_favorites_source)

#8. Creating aggregate view by Loyalty User -------------------------

view_aggr_rsvp_pos <- Reduce(function(...) merge(...,by="loyalty_user_id"),
                             list(aggr_rsvp_pos,first_rsvp_pos,fact_server,fact_table))

view_aggr_rsvp_pos_ni <- Reduce(function(...) merge(...,by="loyalty_user_id"),
                             list(aggr_rsvp_pos,first_rsvp_pos,fact_server_faves,fact_table_favorites))

rm(aggr_rsvp_pos,first_rsvp_pos,fact_server,fact_table,fact_table_favorites,fact_server_faves)

#9. Creating POS Items Dimensions---------------------
sum_pos_items <- pos_items %>% 
  filter(loyalty_reservation_id != "0" & 
           !(item_category_name %in% c("","item_category_name"))) %>%
  group_by(loyalty_reservation_id,
           item_category_name) %>%
  summarise(amount = sum(amount),
            qty = sum(quantity))

#transposing to extract the amount spent
amount_items <- select(sum_pos_items,-qty) %>%
  spread(item_category_name,amount)
setnames(amount_items, make.names(colnames(amount_items)))
names(amount_items)[-1] <- paste0(names(amount_items)[-1],"_spend")
amount_items[is.na(amount_items)] <- 0

#transposing to extract the quantity of each category
qty_items <- select(sum_pos_items,-amount) %>%
  spread(item_category_name,qty)
setnames(qty_items, make.names(colnames(qty_items)))
names(qty_items)[-1] <- paste0(names(qty_items)[-1],"_qty")
qty_items[is.na(qty_items)] <- 0

#merging amount and qty 
items_rsvp <- merge(amount_items,qty_items,by="loyalty_reservation_id") 

#merging loyalty user id from reservation/pos fact table in order to aggregate by user id
items_user <- merge(select(rsvp_pos_fact,loyalty_reservation_id,loyalty_user_id),
                    items_rsvp,by="loyalty_reservation_id") 

items_aggregates <- items_user %>%
  group_by(loyalty_user_id) %>%
  summarise_each(funs(mean(.,na.rm=TRUE)),3:ncol(items_user))
names(items_aggregates)[-1] <- paste0("avg_",names(items_aggregates)[-1])

rm(items_user,sum_pos_items,amount_items, qty_items)

#10. Creating dimensions from Code and State---------------------------------
#code by reservation
code <- cmb_rsvp_pos %>%
  group_by(loyalty_user_id,loyalty_reservation_id) %>%
  summarise(code = min(code)) %>%
  mutate(code=ifelse(code %in% c("Anniversary","Birthday"),
                     code,"Other"))
#code by loyalty user
rsvp_code <- cmb_rsvp_pos %>%
  group_by(loyalty_user_id,loyalty_reservation_id) %>%
  summarise(code = min(code)) %>%
  mutate(code=ifelse(code %in% c("Anniversary","Birthday"),
                     code,"Other"),
         value = 1) %>%
  group_by(loyalty_user_id,code) %>%
  summarise(value = sum(value)) %>%
  spread(code,value)
rsvp_code[is.na(rsvp_code)] <- 0
names(rsvp_code)[-1] <- paste0(names(rsvp_code)[-1],"_visits")

rsvp_state <- reservations %>%
  filter(ordinal==1 & state %in% c("Cancelled","No Show")) %>%
  dplyr::select(loyalty_user_id,loyalty_reservation_id,state) %>%
  mutate(value=1) %>%
  group_by(loyalty_user_id,state) %>%
  summarise(value=sum(value)) %>%
  spread(state,value)
setnames(rsvp_state, make.names(colnames(rsvp_state)))
names(rsvp_state)[-1] <- paste0(names(rsvp_state)[-1],"_visits")

rm(cmb_rsvp_pos)

#11. Creating Repeat Visits and Average Days b/t Visis dimensions----------------------

#visits between reservations
bt_visits <- rsvp_pos_fact %>% 
  arrange(open_date) %>%
  select(loyalty_user_id,loyalty_reservation_id,open_date,
         month_visit,day_of_month_visit) %>%
  mutate(cmb_month_day = as.integer(paste0(month_visit,day_of_month_visit))) %>%
  group_by(loyalty_user_id) %>%
  mutate(days_bt_visits = difftime(open_date,lag(open_date,default=NA),
                                   units="days")) %>%
  select(-month_visit,-day_of_month_visit)
bt_visits[is.na(bt_visits)] <- 0

bt_visits$which_visit <- ave(bt_visits$loyalty_user_id==bt_visits$loyalty_user_id,
                             bt_visits$loyalty_user_id,FUN=cumsum)

#avg visit by reservations
avg_bt_visit <- bt_visits %>%
  group_by(loyalty_user_id) %>%
  dplyr::summarise(avg_days_bt_visits = 
                     round(mean(as.double(days_bt_visits),
                                na.rm = T),3))
#repeat visit on the same day
flag_same_day <-duplicated(bt_visits,by=c("loyalty_user_id","cmb_month_day"))
same_day_visit <- bt_visits[flag_same_day,]   %>% 
  group_by(loyalty_user_id) %>%
  summarise(same_day_visits =n())

repeat_time_bt_visits <- merge(avg_bt_visit,same_day_visit,
                               by="loyalty_user_id",all.x = T,all.y = F)
repeat_time_bt_visits[is.na(repeat_time_bt_visits)] <- 0

#removing cmb_month_day variable b/c it's no longer needed
bt_visits$cmb_month_day <- NULL

rm(avg_bt_visit,flag_same_day,same_day_visit)

#12. Creating Views by Loyalty User and Reservation ID--------------------
user_view <- merge(view_aggr_rsvp_pos_ni,items_aggregates,by="loyalty_user_id")
user_view <- merge(user_view,rsvp_code,by="loyalty_user_id")
user_view <- merge(user_view,rsvp_state,all.x = TRUE,
                   all.y=FALSE,by="loyalty_user_id")
user_view <- merge(user_view,repeat_time_bt_visits,all.x = TRUE,
                   all.y=FALSE,by="loyalty_user_id")
user_view[is.na(user_view)] <- 0

#removing duplicate fields before combining
code$loyalty_user_id <- NULL
bt_visits$open_date <- NULL
bt_visits$loyalty_user_id <- NULL

rsvp_view <- Reduce(function(...) merge(...,by="loyalty_reservation_id"),
                    list(rsvp_pos_fact,code,items_rsvp,bt_visits))

rsvp_view <- merge(rsvp_view,select(user_view,loyalty_user_id,has.repeated),
                   all.x = TRUE, all.y= FALSE,by="loyalty_user_id")

#adding column to identify visits with a repeating guest

setwd("~/Desktop/practicum/venga_practicum/")
write.csv(user_view,row.names = FALSE,"user_view_3865_ni.csv")
write.csv(rsvp_view,row.names = FALSE,"rspv_view_3865.csv")
