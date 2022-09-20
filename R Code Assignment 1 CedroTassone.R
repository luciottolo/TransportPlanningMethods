##############################################################################################-
# SKELETON PART 1: DESCRIPTIVE ANALYSIS
##############################################################################################-

# Set up ------------------------------------------------------------------

require("lubridate") # great for dealing with dates if needed
require("tidyverse") # basek of great packages: e.g. dplyr which makes data wrangling more readable and easier
require("sf") # needed for plotting of spatial polygons and calculations with it


# 1 Import data -----------------------------------------------------------

# use load and adjust the path to where you have saved the data file
load(paste0("C:/Users/Lucio/OneDrive/Desktop/TPM/Assignment1/","data_assignment.RData"))


# 2 Cleaning --------------------------------------------------------------

# 1. get rid of odd naming
# if you use the dplyr package (part of tidyverse):
# use ?mutate() and ?str_remove() to check how to apply these two functions on the trips data set

# 2. floor numbers
# use ?mutate() and ?floor() to check how to use these two functions

trips <- trips %>%
  mutate(mode = str_remove_all(mode, "Mode::"),
         mode_agg = str_remove_all(mode_agg, "Mode::"),
         length = floor(length),
         duration = floor (duration))


# 3. select the legs of each trip that is the longest of all legs in the same trip
trips_mainmode <- trips %>%
  group_by(trip_id,) %>%
  filter(length == max(length)) %>% 
  select(leg_id) %>%
  mutate(trip_mainmode = TRUE)

# left-join the trips_mainmode df to the existing df
# ?left_join()
trips <- trips %>%
  left_join(trips_mainmode, by = c("trip_id","leg_id"))


# 3 travel behaviour ------------------------------------------------------

# only look at main mode legs of the trips
trips <- trips %>%
  filter(trip_mainmode == TRUE)

# join the datasets 
trips <- trips %>%
  left_join(participants %>%
   select(participant_id,household_size,income), by = c("participant_id"))

#a quick analysis of the variables used will follow:
#still we will not clean them (a.i. for income), as we do not want to remove
#rows that could be analysed using other variables
#a.i. it do not make sense to remove income NA and 99 rows now, as we are still
#analysing for example travel frequency in the days of the week

summary(trips$weekday)

summary(trips$income)
sd(trips$income, na.rm = T)
hist(trips$income)
boxplot(trips$income)

summary(trips$household_size)
sd(trips$household_size, na.rm = T)
hist(trips$household_size)
boxplot(trips$household_size)

summary(trips$length)
sd(trips$length, na.rm = T)
hist(trips$length)
boxplot(trips$length) #for length we decided not to clean the outliers 
#as travel behaviour we consider it could significantly vary from person to person

summary(trips$duration)
sd(trips$duration, na.rm = T)
hist(trips$duration)
boxplot(trips$duration) #as for length we see the same for duration.
#we analysed only by length as duration is strictly linked to it.

summary(trips$mode)

summary(participants$education)
sd(participants$education, na.rm = T)
hist(participants$education)
boxplot(participants$education)

summary(participants$age)
sd(participants$age, na.rm = T)
hist(participants$age)
boxplot(participants$age)

summary(participants$sex)
sd(participants$sex, na.rm = T)
hist(participants$sex)
boxplot(participants$sex)

summary(participants$employment_1)
sd(participants$employment_1, na.rm = T)
hist(participants$employment_1)
boxplot(participants$employment_1)

summary(participants$employment_2)
sd(participants$employment_2, na.rm = T)
hist(participants$employment_2)
boxplot(participants$employment_2)

# Number of trips per weekday:
day_of_week <- trips %>%
            select(weekday) %>% 
            count(weekday,sort=TRUE)

# Ordering weekdays:
day_of_week$weekday <- factor(day_of_week$weekday,levels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
day_of_week[order(day_of_week$weekday),]

# Graph
ggplot(day_of_week,aes(weekday,n))+
  geom_col(fill="navyblue")+
  xlab("Day of the Week")+ylab("Number of Trips")+
  ggtitle("Number of Trips per Weekday") +
  labs(
     subtitle = "barplot that shows trip distribution per day of the week",
     caption = "From MOBIS data set. https://ivtmobis.ethz.ch/mobis/en/ ") 


# Number of trips per income
## Determine how many participants per income class
part_by_income <- trips %>% 
  select (participant_id,income) %>%
  distinct() %>%
  group_by(income) %>%
  count(income)

## Determine how many trips per income class
by_income <- trips %>% 
  select(income) %>% 
  count(income,sort=FALSE) %>% 
  filter (income != "99") %>% 
  left_join(part_by_income, by = c("income"))

## Normalized Graph
ggplot(by_income,aes(income,n.x/n.y))+
  geom_col(fill="navyblue")+
  xlab("Income class")+
  ylab("Number of Trips")+
  ggtitle("Income and Number of Trips")+
  labs(
    subtitle = "barplot that shows trip distribution per income class",
    caption = "From MOBIS data set. https://ivtmobis.ethz.ch/mobis/en/ ") 


## Number of trips per income and household size
## Determine how many participants per household size
part_by_hhs <- trips %>% 
  select (participant_id,household_size) %>%
  distinct() %>%
  group_by(household_size) %>%
  count(household_size)

## How many trips per income (normalized)
df_inc_hh <- trips %>% 
  select(income, trip_id, household_size) %>% 
  filter(income %in% c(1, 2, 3, 4, 5)) %>% 
  count(income, household_size, sort = FALSE) %>%  
  left_join(part_by_income, by = c("income")) 

## Normalized Graph by income
ggplot(df_inc_hh, aes(x = income, y = n.x/n.y, fill = household_size)) +
  geom_col() + 
  ggtitle("Household Size and Income") +   
  theme(plot.title = element_text(hjust = 0.5))+
  labs(fill="Household size")+
  labs(x = "Income Class", y = "Number of Trips",
     title ="Household Size and Income",
       subtitle = "barplot that shows variation in number of trips per income class and per household size",
       caption = "From MOBIS data set. https://ivtmobis.ethz.ch/mobis/en/ ") +
  theme(
     legend.position = c(1, 1.2),
     legend.justification = c("right", "top"),
     legend.box.just = "right",
     legend.margin = margin(6, 6, 6, 6)
    )

## How many trips per household size (normalized)
df_inc_hs <- trips %>% 
  select(income, trip_id, household_size) %>% 
  filter(income %in% c(1, 2, 3, 4, 5)) %>% 
  count(income, household_size, sort = FALSE) %>%  
  left_join(part_by_hhs, by = c("household_size")) 

## Normalized Graph by Household size
ggplot(df_inc_hs, aes(x = household_size, y = n.x/n.y, fill = income)) + 
  geom_col() + 
  ggtitle("Household Size and Income") +   
  theme(plot.title = element_text(hjust = 0.5))+
  labs(fill="Income class")+
  labs(x = "Household size", y = "Number of Trips",
       title ="Household Size and Income",
       subtitle = "barplot that shows variation in number of trips per income class and per household size",
       caption = "From MOBIS data set. https://ivtmobis.ethz.ch/mobis/en/ ") +
  theme(
    legend.position = c(.95, 1),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )


# Length vs income?

by_length <- trips %>% 
  select(income,length) %>%  
  group_by(income) %>% summarise(n=sum(length)/1000)%>%
  left_join(part_by_income, by = c("income")) %>% 
  filter (income!=99) 


  ggplot(by_length,aes(income,n.x/n.y))+
    geom_col(fill = "blue")+
    xlab("Income class")+
    ylab("Distance travelled [km]")+
    ggtitle("Distance Travelled and Income")+
    
    labs(x = "Income Class", y = "Distance travelled [km]",
         title ="Distance travelled and Income",
         subtitle = "barplot that shows variation in distance travelled per income class",
         caption = "From MOBIS data set. https://ivtmobis.ethz.ch/mobis/en/ ") +
    theme(
      legend.position = c(.95, .95),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6)
    )
  


# Mode choice
  
## Number of trips by mode choice  
df <- trips %>%
  select(mode , weekday) %>%
  count (mode)

## Graph
ggplot(df,aes(mode,n))+
  geom_col(fill = "blue")+
  labs(x = "Travel Mode", y = "Number of trips",
       title ="Preferred Mode of Travel ",
       subtitle = "barplot that shows preferences of travel mode by the participants",
       caption = "From MOBIS data set. https://ivtmobis.ethz.ch/mobis/en/ ") +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )


## Distance travelled by mode choice
df <- trips %>%
  select(mode , length) %>%
  mutate (length = length/1000) 

## Graph
ggplot(df,aes(mode,length))+
  geom_col(fill = "blue")+
  labs(x = "Travel Mode", y = "Distance travelled [km]",
       title ="Preferred Mode of Travel ",
       subtitle = "barplot that shows preferences of travel mode by the participants",
       caption = "From MOBIS data set. https://ivtmobis.ethz.ch/mobis/en/ ") +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

## Mode choice per weekday
df <- trips %>%
  select(mode , weekday, length) %>%
  group_by(mode)%>%
  count(weekday) 
  df$weekday <- factor(df$weekday,levels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
df[order(df$weekday),]

## Graph
ggplot(df, aes(x = weekday, y = n, fill = mode)) + 
  geom_col() + 
  ggtitle("Mode choice per weekday") +   
  theme(plot.title = element_text(hjust = 0.5))+
  labs(fill="Travel Mode")+
  labs(x = "Day of the Week", y = "Number of Trips",
       subtitle = "barplot that shows variation in number of trips per day of the week and travel mode",
       caption = "From MOBIS data set. https://ivtmobis.ethz.ch/mobis/en/ ") 
  

# Peak Hour

start <- ymd_hms(trips$started_at)
end <- ymd_hms(trips$finished_at)

## Convert to seconds
start_time <- period_to_seconds(hms(format(start, format = "%H:%M:%S")))
end_time <- period_to_seconds(hms(format(end, format = "%H:%M:%S")))

## Chose time slots we look at, 30 min intervals
time_slot_start <- rep(NA, 48)
time_slot_end <- rep(NA, 48)
for(i in 1:48){
  time_slot_start[i] <- (i - 1) * 30 * 60
  time_slot_end[i] <- i * 30 * 60
}
n_times <- length(start_time)
trips_interval <- rep(0, 48)

## Check for each trip which timeslots it intersects
for(i in 1:n_times){
  tmp <- rep(0, 48)
  ### If trip is over 2 days split in 2 intervals
  if(start_time[i] > end_time[i]){
    interval1 <- c(start_time[i], time_slot_end[48])
    interval2 <- c(0, end_time[i])
    for(j in 1:48){
      #### check which intervals trip is in
      if((interval1[1] < time_slot_end[j] & time_slot_start[j] < interval1[2]) | (interval2[1] < time_slot_end[j] & time_slot_start[j] < interval2[2])){
        tmp[j] <- 1
      }
    }
  }else{
    for(j in 1:48){
      #### Check which intervals trip is in
      if(start_time[i] < time_slot_end[j] & time_slot_start[j] < end_time[i]){
        tmp[j] <- 1
      }
    }
  }
  ### Update number of trips per interval
  trips_interval <- trips_interval + tmp
}
Time <- seq(from = 0, to = 23.5, by = 0.5)
Rush <- rep("No Rush Hour", 48)
Rush[15:18] <- "07:00 - 09:00 RH"
Rush[33:37] <- "16:00 - 18:30 RH"

## Dataframe
df_rush <- data.frame(Trips = trips_interval, Slot = Time, Rush_Hour = Rush)

ggplot(df_rush, aes(x = Time, y = Trips, fill = Rush_Hour)) + 
       geom_col() +
       ggtitle("Trips per Time Interval") +   
       theme(plot.title = element_text(hjust = 0.5)) +
       scale_x_continuous(breaks = round(seq(min(Time), max(Time), by = 2),1))+
      labs(fill="Rush Hour")+
      labs(x = "Time [per half hour]", y = "Number of Trips",
      title ="Trips per time interval",
     subtitle = "barplot that shows variation in number of trips per day hour",
     caption = "From MOBIS data set. https://ivtmobis.ethz.ch/mobis/en/ ") 
 

# 4 socio-demographics ----------------------------------------------------

# Age distribution

age_dis <- participants %>% select(age,sex)

 
## Age Intervals
# <=10 1
# >10 && <=25 2
# >25 && <=40 3
# >40 && <=55 4
# >55 && <=70 5


age_dis$age_int=ifelse(age_dis$age<=10,1,ifelse(age_dis$age<=25,2,ifelse(age_dis$age<=40,3,ifelse(age_dis$age<=55,4,5))))

## Count number of sex=1 for each interval

age_dis <- age_dis %>%
  group_by(age_int) %>% 
  count(sex)

##Graph
ggplot(age_dis,aes(fill=factor(sex),y=n,x=age_int))+
  geom_bar(position="stack", stat="identity")+
  ggtitle("Age, sex and Number of Trips")+
  xlab("Age Interval")+
  ylab("Number of Trips")+
labs(subtitle = "barplot that shows variation in number of trips per age class and sex.",
     caption = "From MOBIS data set. https://ivtmobis.ethz.ch/mobis/en/ ") +
  theme(
    legend.position = c(.95, 1.1),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

#Education

education <- participants %>% 
  select (education) %>% 
  group_by (education) %>% 
  count (education)
ggplot(education, aes(x=education,y=n))+
  geom_col(fill="steelblue")+
  ggtitle("Distribution Education Levels")+
  xlab("Education level")+
  ylab("Number of Participants")+
labs(
     subtitle = "barplot that shows the share of education level of the participants",
     caption = "From MOBIS data set. https://ivtmobis.ethz.ch/mobis/en/ "
     ) +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

# Education and employment type
education <- participants %>% 
  select(education, employment_1,employment_2) %>%
  group_by (employment_1) %>%
  count (education) 

ggplot(education,aes(fill=factor(education),y=n,x=employment_1))+
  geom_bar(position="stack", stat="identity")+
  ggtitle("Education Level and Employment Status")+
  xlab("Employment type")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7))+
  ylab("Number of participants")+
  labs(
       subtitle = "barplot that shows number of participant per employment type and education ",
       caption = "From MOBIS data set. https://ivtmobis.ethz.ch/mobis/en/ ") +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

# Trips by income
trips_by_income <- participants %>%
  select (participant_id,income)%>% 
  count(income) %>% 
  filter (income != 99)

ggplot(df_inc_hh, aes(x = income, y = n.x/n.y, fill = household_size)) +
  geom_col() + 
  ggtitle("Household Size and Income") +   
  theme(plot.title = element_text(hjust = 0.5))+
  labs(fill="Household size")+
  labs(x = "Income Class", y = "Number of Trips",
     title ="Household Size and Income",
       subtitle = "barplot that shows variation in number of trips per income class and per household size",
       caption = "From MOBIS data set. https://ivtmobis.ethz.ch/mobis/en/ ") +
  theme(
     legend.position = c(1, 1.2),
     legend.justification = c("right", "top"),
     legend.box.just = "right",
     legend.margin = margin(6, 6, 6, 6)
    )
#simple LM to assess income influence on number of trips per participant
df <- trips %>%
  count(participant_id  )%>%
  left_join(participants %>%
              select(participant_id,household_size,income), by = c("participant_id"))

df
lm1 <- lm(n  ~  income,data = df)

#adding control variable household_size

lm2 <- lm(n  ~  income + household_size,data = df)
stargazer(lm1, lm2)

