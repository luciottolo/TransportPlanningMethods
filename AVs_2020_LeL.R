#AV scenario 2020
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

trips_zones <- trips %>%
  select(start_bznr,start_bzname,end_bznr,end_bzname) %>%
  group_by(start_bzname,end_bzname) %>%
  count(start_bzname,end_bzname)

# Case for year 2020 with autonomous vehicles

trips_zones <- trips_zones %>%
  select (start_bzname,end_bzname,n) %>%
  group_by(start_bzname) %>%
  mutate(trips_prod=sum(n)) %>%
  ungroup() %>%
  group_by(end_bzname) %>%
  mutate(trips_attr=sum(n)) %>%
  filter (start_bzname == end_bzname)%>%
  ungroup()

mean_tt <- trips %>%
  select (duration, start_bzname, end_bzname,mode_agg) %>%
  mutate (duration = ifelse((end_bzname == "Zürich" | start_bzname == "Zürich") & (end_bzname != start_bzname), duration*1.3,duration)) %>%
  mutate (duration = ifelse((mode_agg=="Car"|mode_agg=="PT"),duration,duration/1.3)) %>%
  group_by(start_bzname,end_bzname) %>% 
  mutate (mean_duration=mean(duration/60),mean_duration=floor(mean_duration), duration = NULL) %>%
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

trip_dis

#Calculate number of trips
n_trips_tts <- trip_dis %>%
  filter(start_bzname ==  "Zürich" | end_bzname == "Zürich") %>%
  filter(start_bzname != end_bzname)

trips_tt <- sum(n_trips_tts$mean_duration)


mean_tt_car <- trips %>%
  filter (mode_agg == "Car") %>%
  mutate (duration = duration*1.3) %>%
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
  mutate (duration = duration*1.3) %>%
  select (duration, start_bzname, end_bzname) %>%
  group_by(start_bzname,end_bzname) %>% 
  mutate (mean_duration=mean(duration/60),mean_duration=floor(mean_duration), duration = NULL) %>%
  distinct() %>%
  ungroup() %>%
  filter(start_bzname ==  "Zürich" | end_bzname == "Zürich") %>%
  filter(start_bzname != end_bzname) 

mean_pt <- mean(mean_tt_pt$mean_duration)


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
       title ="Origin-destination matrix - AV 2020",
       subtitle = "heatmap that shows model results of trip distribution",
       caption = "From MOBIS data set. https://ivtmobis.ethz.ch/mobis/en/ ") +
  theme(
    legend.position = c(.75, 0.65),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6) 
  ) +
  scale_fill_stepsn(breaks = c(0,15,30,55,70,85,100,115,130),
                    colours = rev(heat.colors(7,alpha=0.8)),
                    values = scales::rescale(c(0,15,30,55,70,85,100,115,130)),
                    show.limits=T)


#Find out 2060 flee size based on number of future PT trips
fleet_size <- trip_dis %>%
  filter(start_bzname ==  "Zürich" | end_bzname == "Zürich") %>%
  filter(start_bzname != end_bzname) %>%
  mutate(mean_duration = 0.555*mean_duration)

future_fleet_size <- sum(fleet_size$mean_duration)

# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Thank you to Prof. Hess from Leeds!

### Clear memory
#rm(list = ls())

### Load libraries
# install.packages("apollo")
getwd()
setwd("C:/Scuola/Master/HS21/Transport Planning Methods/Exercise 2")
library(apollo)
library(tidyverse)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       ="MNL Mobis",
  modelDescr      ="MNL model on mode choice for the Mobis dataset",
  indivID         ="participant_id"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

# database = (here::here("C:/Scuola/Master/HS21/Transport Planning Methods/Exercise 2","data_assignment_modechoice"))
load("C:/Scuola/Master/HS21/Transport Planning Methods/Exercise 2/data_assignment_modechoice.RData")

# Remove all NA observation
database <- modechoicetrips %>%
            filter_at(vars(tt_walk, tt_car,cost_car,tt_pt,accegr_t_pt,freq_pt,trans_t_pt,trans_nr_pt,cost_pt),all_vars(!is.na(.))) 
            
          

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation

# here only alternative specific constants and generic parameters!

apollo_beta=c(asc_car       = 0,
              asc_w         = 0,
              asc_bike      = 0,
              asc_pt        = 0,
              b_tt_w        = 0,
              b_tt_bike     = 0,
              b_tt_car      = 0,
              b_tt_pt       = 0,
              b_dist_pt     = 0,
              b_accegr_t_pt = 0,
              b_freq_pt     = 0,
              b_trans_t_pt  = 0,
              b_trans_nr_pt = 0,
              b_cost_pt     = 0,
              b_cost        = 0)

### Car-specific constant and beta tt for walking to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_car","b_tt_w")

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

# runs a number of checks and produces a consolidated list of model inputs (see manual for further information)

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

# core part of code! defined by modeller! function is used for estimation
# function returns probabilities, depends on functionality, three inputs: betas, inputs and functionality

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create empty list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['walk']] = asc_w    + b_tt_w    * tt_walk                         
  V[['bike']] = asc_bike + b_tt_bike * tt_bike                        
  V[['car']]  = asc_car  + b_tt_car  * tt_car  + b_cost    * cost_car                        
  V[['PT']]   = asc_pt   + b_tt_pt   * tt_pt   + b_cost_pt * cost_pt     + b_dist_pt * distance_pt + 
               b_accegr_t_pt * accegr_t_pt + b_freq_pt * freq_pt + b_trans_t_pt * trans_t_pt + b_trans_nr_pt * trans_nr_pt

  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(car=1, PT=2, bike=3, walk=4), 
    avail         = list(car=avail_car, PT=avail_pt, bike=avail_bike, walk=avail_walk), 
    choiceVar     = choice, # name in database
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  # only to be used in the presence of multiple observations per individual (see presentation)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

# actual estimation using log-likelihood approach
# outcomes of model estimation are saved in a list called model
# most important output in list are the estimates of beta
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

database <- database %>%
  mutate(cost_car = cost_car*1.2,
         cost_pt = cost_pt*0.45,
         trans_nr_pt = 0,
         trans_t_pt = 0,
         accegr_t_pt = 0) %>%
  mutate(tt_car = ifelse ((end_bzname == "Zürich" | start_bzname == "Zürich") & (start_bzname != end_bzname), 1.3*tt_car,tt_car)) %>%
  mutate(tt_pt = ifelse ((end_bzname == "Zürich" | start_bzname == "Zürich") & (start_bzname != end_bzname), 1.3*tt_pt,tt_pt))

### Rerun apollo validate inputs
apollo_inputs = apollo_validateInputs()

### Rerun predictions with the new data, and save into a separate matrix
predictions_new = apollo_prediction(model, 
                                    apollo_probabilities, 
                                    apollo_inputs,
                                    prediction_settings=list())
predictions_new

### Look at a summary of the predicted choice probabilities
summary(predictions_new)

xtable(summary(predictions_new))



