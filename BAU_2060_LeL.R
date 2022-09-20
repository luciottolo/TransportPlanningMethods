# Set up ------------------------------------------------------------------

require("lubridate") # great for dealing with dates if needed
require("tidyverse") # basek of great packages: e.g. dplyr which makes data wrangling more readable and easier
require("ggpubr")
require("xtable")

#load("C:/Users/Lucio Tassone/Downloads/data_assignment.RData")

# CBA -----------------------------------------------------
#General modifications 
trips <- trips %>%
  mutate(mode = str_remove_all(mode, "Mode::"),
         mode_agg = str_remove_all(mode_agg, "Mode::"),
         length = floor(length),
         duration = floor (duration))

## calculate main mode of trip
trips_mainmode <- trips %>%
  group_by(participant_id, trip_id) %>%
  filter(length == max(length)) %>%
  select(participant_id, leg_id) %>%
  mutate(trip_mainmode = TRUE) %>%
  ungroup()

trips <- trips %>%
  left_join(trips_mainmode, by = c("participant_id","trip_id","leg_id")) %>%
  # changing the order of variables
  relocate(trip_mainmode, .after = mode_agg) %>%
  filter (trip_mainmode == TRUE)
rm(trips_mainmode)

trips_zones <- trips %>%
  select(start_bznr,start_bzname,end_bznr,end_bzname) %>%
  group_by(start_bzname,end_bzname) %>%
  count(start_bzname,end_bzname)

# Compute number of produced and attracted trips for each zone

trips_zones <- trips_zones %>%
  select (start_bzname,end_bzname,n) %>%
  group_by(start_bzname) %>%
  mutate(trips_prod=sum(n)) %>%
  ungroup() %>%
  group_by(end_bzname) %>%
  mutate(trips_attr=sum(n)) %>% 
  mutate (trips_prod = trips_prod*1.426, #in 2060 +42,6% population, that will be reflected by +42,6% of trips attraction and production
          trips_attr = trips_attr*1.426)%>%
  filter (start_bzname == end_bzname)%>%
  ungroup()

mean_tt <- trips %>%
  select (duration, start_bzname, end_bzname) %>%
  group_by(start_bzname,end_bzname) %>%
  mutate(mean_duration=mean(duration/60),mean_duration=floor(mean_duration), duration = NULL) %>%
  distinct() %>%
  ungroup()

# Start building matrix for Furness method with impedance function

trip_dis <- mean_tt %>%
  select (start_bzname,end_bzname,mean_duration) %>%
  mutate (fc = 1/(mean_duration)^2)

# Set the total number of produced trips per zone as the production and the total
# number of attracted trips as the attraction

trip_prod <- trips_zones %>%
  select (end_bzname,trips_prod,trips_attr) 

# Add all the variables needed for Furness method to the matrix

trip_dis <- trip_dis %>%
  left_join (trips_zones %>% select (start_bzname,trips_prod), by = c("start_bzname")) %>%
  left_join (trips_zones %>% select (end_bzname,trips_attr), by = c("end_bzname")) %>%
  mutate (production_i=0,attraction_i=0,alphas=0,betas=1,previous_betas=0,error=0,alphas_calc=0,betas_calc=0) 

sum_error <- 1

# Loop that executes Furness method and stops when the error is smaller than 0.0001%

while (sum_error>0.000001) {
  trip_dis$previous_betas <- trip_dis$betas
  trip_dis <- trip_dis %>%
    group_by(end_bzname) %>%
    mutate (alphas_calc = betas*trips_attr*fc) %>%
    ungroup() %>%
    group_by(start_bzname) %>%
    mutate (alphas=1/sum(alphas_calc)) %>%
    ungroup() %>%
    mutate (mean_duration = floor(alphas*trips_prod*betas*trips_attr*fc)) %>%
    group_by(start_bzname) %>%
    mutate (production_i=sum(mean_duration)) %>%
    ungroup() %>%
    group_by(end_bzname) %>%
    mutate (attraction_i=sum(mean_duration)) %>%
    ungroup() %>%
    group_by(start_bzname) %>%
    mutate (betas_calc=alphas*trips_prod*fc) %>%
    ungroup() %>%
    group_by(end_bzname) %>%
    mutate(betas=1/sum(alphas*trips_prod*fc),
           error=(abs(betas-previous_betas))/betas)
  sum_error <- sum(trip_dis$error)
}

#Calculate number of trips
n_trips_tts <- trip_dis %>%
  filter(start_bzname ==  "Zürich" | end_bzname == "Zürich") %>%
  filter(start_bzname != end_bzname)

trips_tt <- sum(n_trips_tts$mean_duration)

# Construct heatmap to show trip distribution between zones from the data 
zone_112 <- trip_dis %>%
  select(start_bzname,end_bzname,mean_duration) %>%
  filter(start_bzname ==  "Zürich" | end_bzname == "Zürich") %>%
  filter(start_bzname != end_bzname) 

# Heatmap of the matrix resulted from model
ggplot(zone_112, aes(end_bzname, start_bzname, fill= mean_duration)) + 
  geom_tile() +
  ggtitle("Heatmap origin-destination matrix") +   
  theme(plot.title = element_text(hjust = 0.5))+
  labs(fill="Number of trips")+
  labs(x = "Destination", y = "Origin",
       title ="Origin-destination matrix - BAU 2060",
       subtitle = "heatmap that shows model results of trip distribution",
       caption = "From MOBIS data set. https://ivtmobis.ethz.ch/mobis/en/ ") +
  theme(
    legend.position = c(.75, 0.65),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6) 
  ) +
  scale_fill_stepsn(breaks = c(0,250,500,750,1000,1250,1500,1750),
                    colours = rev(heat.colors(7,alpha=0.8)),
                    values = scales::rescale(c(0,250,500,750,1000,1250,1500,1750)),
                    show.limits=T)

