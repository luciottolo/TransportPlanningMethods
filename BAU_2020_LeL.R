#BAU 2020
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
  relocate(trip_mainmode, .after = mode_agg) %>%
  filter (trip_mainmode == TRUE)
rm(trips_mainmode)

#Calculate number of trips with PT in our scope
n_trips_tts <- trips %>%
  filter (mode_agg == "PT") %>%
  filter(start_bznr ==  "112" | end_bznr == "112") %>%
  filter(start_bznr != end_bznr)

# Calculate total km for PT
tot_km_pt <- sum(n_trips_tts$length/1000)

#Calculate number of trips with car in our scope
n_trips_tts_car <- trips %>%
  filter (mode_agg == "Car") %>%
  filter(start_bznr ==  "112" | end_bznr == "112") %>%
  filter(start_bznr != end_bznr)

mean_tt_car <- trips %>%
  filter (mode_agg == "Car") %>%
  mutate (duration = duration) %>%
  select (duration, start_bzname, end_bzname) %>%
  group_by(start_bzname,end_bzname) %>% 
  mutate (mean_duration=mean(duration/60),mean_duration=floor(mean_duration), duration = NULL) %>%
  distinct() %>%
  ungroup() %>%
  filter(start_bzname ==  "Zürich" | end_bzname == "Zürich") %>%
  filter(start_bzname != end_bzname) 

mean_car <- mean(mean_tt_car$mean_duration)

mean_tt_pt <- trips %>%
  filter (mode_agg == "PT") %>%
  mutate (duration = duration) %>%
  select (duration, start_bzname, end_bzname) %>%
  group_by(start_bzname,end_bzname) %>% 
  mutate (mean_duration=mean(duration/60),mean_duration=floor(mean_duration), duration = NULL) %>%
  distinct() %>%
  ungroup() %>%
  filter(start_bzname ==  "Zürich" | end_bzname == "Zürich") %>%
  filter(start_bzname != end_bzname) 

mean_pt <- mean(mean_tt_pt$mean_duration)


#create a subset of the dataset w  trips that pass trhough zone 112 only
zone_112_tab <- trips %>%
  select(start_bznr,start_bzname,end_bznr,end_bzname, duration) %>%
  filter(start_bznr ==  "112" | end_bznr == "112") %>%
  filter(start_bznr != end_bznr)

# Construct heatmap to show trip distribution between zones from the data 
zone_112 <- trips %>%
  select(start_bznr,start_bzname,end_bznr,end_bzname) %>%
  filter(start_bznr ==  "112" | end_bznr == "112") %>%
  filter(start_bznr != end_bznr) %>%
  group_by(start_bzname,end_bzname) %>%
  count(start_bzname,end_bzname)


# Heatmap
ggplot(zone_112, aes(end_bzname, start_bzname, fill= n)) + 
  geom_tile() +
  scale_fill_gradient(low="yellow", high="red") +
  ggtitle("Heatmap origin-destination matrix - BAU 2020") +   
  theme(plot.title = element_text(hjust = 0.5))+
  labs(fill="Number of trips")+
  labs(x = "Destination", y = "Origin",
       title ="Origin-destination matrix - BAU 2020",
       subtitle = "heatmap that shows frequency of travel between two zones",
       caption = "From MOBIS data set. https://ivtmobis.ethz.ch/mobis/en/ ") +
  theme( 
    legend.position = c(.75, 0.65),
    legend.justification = c("right", "top"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6))

