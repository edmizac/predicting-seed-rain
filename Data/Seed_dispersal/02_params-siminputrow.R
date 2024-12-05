# Script name: 01_filter-data-siminputrow.R
# Script purpose: derive empirical values for parameterizing the model to run in 
# the nine situations assigned in BLT_groups_data_summary_aftercleaning.csv in 
# Data/Movement/Curated.

# Date created: 2022-11-03d
# Author: Eduardo Zanette

## Notes --------------------------- 
#
#

## Options -------------------------
# (plotting, memory limit, decimal digits)
# 

## Packages -------------------------
library("here")
library("dplyr")
library("ggplot2")
library("readxl")
library("lubridate")
library("hms")


#### Siminputrow matrix  -------------------------

# Load siminputrow matrix from movement data -------------------------
siminputmatrix <- read.csv(here("Data", "Movement", "Curated", "Param_Simulation-time",  "BLT_groups_data_summary_siminputrow.csv"),
                           sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) # to match all other datasets
siminputmatrix %>% str()

# groups <- siminputmatrix$group
# months <- siminputmatrix$id_month



# Load movement dataset to gather the time tamarins wake up and mean time tamarins go to sleep 
dat.all.mv <- read.csv(here("Data", "Movement", "Curated", "BLT_groups_data.csv"),
                           sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  mutate(group = recode(group, "Guarei" = "Guareí"))# %>% 
  # rename(datetime = POSIXct)

# dat.all.mv %>% str
dat.all.mv[ , c("group", "id_month") ] %>% distinct() # This dataset is already filtered by siminputrow!


mv <- dat.all.mv %>%
# get HH:mm:ss of wakeup and sleeping times
  mutate(
    datetime = lubridate::as_datetime(datetime),
    date = lubridate::as_date(datetime),
    time = hms::as_hms(datetime)
  ) 

# Getting mean weaking hours by days, group and timeframe
mv.days <- mv %>%
  group_by(group, id_month, date) %>% 
  
  # wake up and sleepint time
  summarise(
    time_wakeup = round(seconds_to_period(min(seconds(lubridate::hms(time))))),
    time_sleep = round(seconds_to_period(max(seconds(lubridate::hms(time)))))
  ) %>% 
  
  # differente between wakeup and sleeping time (=activity time)
  mutate(
    time_wakesleep_diff = round(seconds_to_period(seconds(time_sleep) - seconds(time_wakeup)))
  ) %>% 
  
  # Get all back to hms
  mutate_if(is.period, period_to_seconds) %>%
  mutate_at(vars(matches("time")), as_hms)


# # Write csv
# mv.days %>% write.csv(here("Data", "Seed_dispersal", "Curated", "Param_siminputrow", "Siminputrow_activity-time_days.csv"),
#                       row.names = FALSE)


# Getting mean weaking hours by group and timeframe  
mv.summary <- mv.days %>%
  group_by(group, id_month) %>% 
  summarise(
    time_wakeup_mean = round(seconds_to_period(mean(seconds(lubridate::hms(time_wakeup))))),
    time_sleep_mean = round(seconds_to_period(mean(seconds(lubridate::hms(time_sleep))))),
    time_wakesleep_mean_diff = round(seconds_to_period(seconds(time_sleep_mean) - seconds(time_wakeup_mean)))
  )

# mv.summary %>% str()
  
# Get all back to hms
mv.summary <- mv.summary %>%
  mutate_if(is.period, period_to_seconds) %>%
  mutate_at(vars(matches("time")), as_hms)

# Merge to siminputmatrix
siminputmatrix <- left_join(siminputmatrix, mv.summary, by = c("group", "id_month"))

# Make it POSIXct to get difftime (= gut_transit_time in 00_prepare-data.R)
foo <- function(x) {
  posixct <- paste(Sys.Date(), x)
  return(posixct)
}
mv.summary <- mv.summary %>%
  mutate_at(vars(matches("time")), foo) %>% 
  mutate_at(vars(matches("time")), ymd_hms)
  
# Get mean difftime in minutes and timesteps
mv.summary <- mv.summary %>%
  mutate(
    time_wakesleep_diff_minutes = round(difftime(time_sleep_mean, time_wakeup_mean, unit = "mins"), digits = 0),
    timesteps_wakesleep_diff = round(as.numeric(time_wakesleep_diff_minutes / 5, digits = 0))
  )

# Drop unuseful columns
mv.summary <- mv.summary %>%
  select(-c(time_sleep_mean, time_wakeup_mean, time_wakesleep_mean_diff))

# Merge into siminputmatrix
siminputmatrix <- dplyr::left_join(siminputmatrix, mv.summary)

0# Write csv
# siminputmatrix %>% write.csv(here("Data", "Seed_dispersal", "Curated", "Param_siminputrow",  "Siminputrow_activity-time_means.csv"),
#                       row.names = FALSE)

# Now we can check how long is the maximum in timesteps it takes for tamarins to defecate seeds after the start of the day.
# timesteps_wakesleep_mean_diff matches mean_timesteps, this means my calculations are correct


  
# Empirical seed dispersal data for summary:  -------------------------
## Load data
dat.all.sd <- read.csv(here("Data", "Seed_dispersal", "Curated", "All-areas_SeedDispersal.csv"),
                    sep = ",", stringsAsFactors = TRUE)
# dat.all.sd %>% str()








# 
# ## Make seed dispersal data match siminputrow/movement data -------------------------
# target <- siminputmatrix[ , 1:2]
# 
# dat.all.sd <- dat.all.sd %>%
#   inner_join(target) %>%  # FILTER IS WRONG. YOU HAVE TO USE INNER_JOIN()
#   # dplyr::filter(group %in% siminputmatrix$group & id_month %in% siminputmatrix$id_month) %>%
#   droplevels()
# 
gm <- dat.all.sd %>% group_by(group, id_month) %>%
  summarise(
    count = n()
  ) ## SANTA MARIA APRIL DOES NOT HAVE SEED DISPERSAL DATA!
# 










### Wrangle seed dispersal data  -------------------------
a <- dat.all.sd %>%
  # make datetime POSIXct
  mutate(
    def_datetime = lubridate::as_datetime(def_datetime), #, tz = "America/Sao_Paulo"),
    feed_datetime = lubridate::as_datetime(feed_datetime) #, tz = "America/Sao_Paulo"),
  ) %>% 
  
  # gather yyyy/mm/dd as id
  mutate(
    date = lubridate::as_date(def_datetime),
  ) %>% 
  
  # Defining each dispersal event as in the same day or next day
  mutate(disp_day = ifelse(lubridate::day(def_datetime) == lubridate::day(feed_datetime), # use case_when for a dplyr-er solution: https://stackoverflow.com/questions/22337394/dplyr-mutate-with-conditional-values
                           "same day",
                           "next day"),
         disp_day = as.factor(disp_day)
  ) %>% 
  
  # get HH:mm:ss of feeding and defecation
  mutate(
    def_time = as_hms(def_datetime),
    feed_time = as_hms(feed_datetime)
  ) %>% 
  
  # # get time of feeding and defecation in period
  # mutate(
  #   def_time_per = lubridate::seconds_to_period(def_time),
  #   feed_time_per = lubridate::seconds_to_period(feed_time)
  # ) %>%

  # # get duration of feeding and defecation in seconds
  # mutate(
  #   def_time_sec = lubridate::period_to_seconds(def_time_per),
  #   feed_time_sec = lubridate::period_to_seconds(feed_time_per)
  # ) %>%
  
  # Order group levels and drop NA
  dplyr::filter(!is.na(group)) %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "Santa Maria", "Taquara")) %>%
  mutate(id_month = forcats::fct_relevel(id_month, "Jan", "Mar", "Apr", "May", 
                                         "Jun", "Jul", "Aug", "Sep", "Dec"))

# dat.all.sd$id_month %>% levels()
a %>% str()
# a$feed_time_per %>% sum()

### Merge wake up and sleep times to seed dispersal data
a <- left_join(a, mv.days) #, by = c("group", "id_month", "date"))

# colnames(a)
# colnames(mv.days)
# colnames(mv.days) %in% colnames(a)

### Define complete days (for which we have wake up and sleeping time according to timediff collum)
a <- a %>% 
  mutate(full_day = case_when(is.na(time_wakeup) ~ "incomplete",
                              TRUE ~ "complete"
                              ))

### Calculate GTT
a_summary <- a %>%
  group_by(group, id_month, disp_day) %>%
  summarise(
    GTT_timesteps_mean = round(mean(gut_transit_time, na.rm = TRUE) / 5, digits = 0), # 1 timestep = 5 mins
    GTT_timesteps_sd = round(sd(gut_transit_time, na.rm = TRUE) / 5, digits = 0),

  )

### Diminish the start hour from the defecation hour
str(a)
b <- a  %>%
  # filter NAs
  dplyr::filter(!is.na(time_wakeup))

### Make it POSIXct to get difftime (= gut_transit_time in 00_prepare-data.R)
  foo <- function(x) {
    posixct <- paste(Sys.Date(), x)
    return(posixct)
  }
b <- b %>%
    mutate_at(c("def_time", "time_wakeup"), foo) %>% 
    mutate_at(c("def_time", "time_wakeup"), ymd_hms)

### Get mean difftime in minutes and timesteps
b <- b %>%
  mutate(
    time_wakeup_to_def = round(difftime(def_time, time_wakeup, unit = "mins"), digits = 0),
    timesteps_wakeup_to_def = round(as.numeric(time_wakeup_to_def / 5, digits = 0))
  )


### Make summary of timesteps for defecation by disp_day:  -------------------------  
c <- b %>%
  group_by(group, id_month, disp_day) %>% 
  summarise(
    # Quantos timesteps se passam em média até os micos terem a última defecaçao do dia?
    timesteps_wakeup_to_def_mean = mean(timesteps_wakeup_to_def),
    timesteps_wakeup_to_def_sd = sd(timesteps_wakeup_to_def)
  )

### Get the latest morning defecation
e <-  c %>% 
  dplyr::filter(disp_day == "next day") %>%
  summarise(
    max_timestep = max(timesteps_wakeup_to_def_mean + timesteps_wakeup_to_def_sd)
  )
hline2 <- max(e$max_timestep)
hline3 <- mean(e$max_timestep)
### From this we can see that the timesteps taken to morning defecation after tamarins wake up is very small (from 1 to 12)  


### Plot it
c %>% 
  ggplot(
    aes(x = group, y = timesteps_wakeup_to_def_mean, 
        
        #group = interaction(disp_day, id_month), 
        color = id_month, 
        #shape = disp_day,
        ymin = timesteps_wakeup_to_def_mean - timesteps_wakeup_to_def_sd,
        ymax = timesteps_wakeup_to_def_mean + timesteps_wakeup_to_def_sd)
  ) +
  # geom_boxplot() +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  
  # Add horizontal lines:
  geom_hline(yintercept = 0, col = "black") +
  geom_hline(yintercept = hline2, col = "red") +
  geom_hline(yintercept = hline3, col = "blue") +
  # coord_cartesian(clip = "off") + # this allows plotting text outside of plot
  annotate("text", label = paste("max morning defecation time ( = ", hline2, ")"),  x = 2.5, y = hline2 + 2.5, color = "red") +
  annotate("text", label = paste("mean =", hline3),  x = 2.5, y = hline3 + 2.5, color = "blue") +
  annotate("text", label = "wake up time",  x = 2.5, y = 0 - 2, color = "black") +
  
  ylab("timesteps (5 min)") +
  ggtitle("timesteps from waking up to defecation (siminputrows only)") +
  theme_bw(base_size = 15) +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(
          hjust = 0.3  ,
          size = 12
                                  )) +
  scale_color_viridis_d()

#### Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", "Param_siminputrow",
#             'Plot_GTT_disp-day_morning-defecation.png'), height = 7, width = 5)

##### Write csv
# c %>%
#   write.csv(here("Data", "Seed_dispersal", "Curated", "Param_siminputrow", "Siminputrow_disp-day.csv"),
#             row.names = FALSE)

#### Write csv for next day events (same day values are not entering the siminputmatrix as this is estimated by GTT in 00_prepare-data.R already)
c2 <- left_join(c, a_summary) # Santa Maria April lacking

d1 <- c2 %>% 
  dplyr::filter(disp_day == "next day") %>% 
  ungroup()

d2 <- c2 %>% 
  dplyr::filter(disp_day == "same day") %>% 
  ungroup()

to_app1 <- "_next_day"
to_app2 <- "_same_day"
cols1 <- d1 %>% dplyr::select(-c(1:3)) %>% colnames()
cols2 <- d2 %>% dplyr::select(-c(1:3)) %>% colnames()

d1 <- d1 %>% 
  rename_with(., ~paste0(cols1, to_app1), all_of(cols1)) %>% 
  # tidyr::pivot_wider()
  select(-disp_day)

d2 <- d2 %>% 
  rename_with(., ~paste0(cols2, to_app2), all_of(cols2)) %>% 
  # tidyr::pivot_wider()
  select(-disp_day)

siminputmatrix <- siminputmatrix %>% 
  dplyr::select(1:5)

### Join and save
siminputmatrix_nextday_seeddispersal <- left_join(siminputmatrix, d1)
siminputmatrix_nextday_seeddispersal <- left_join(siminputmatrix_nextday_seeddispersal, d2, by = c("group", "id_month"))

# siminputmatrix_nextday_seeddispersal %>%
#   write.csv(here("Data", "Seed_dispersal", "Curated", "Param_siminputrow", "Siminputrow_disp-day_nex-day_params.csv"),
#             row.names = FALSE)

### From this we can see that the timesteps taken to morning defecation after tamarins wake up is very small (from 1 to 12, but mean = 6.3)  



#### n defecations per day ####
# By group
dat.all.summary <- dat.all.sd %>% 
  mutate(
    day = lubridate::ymd(as_date(def_datetime)), 
    month = lubridate::month(as_date(def_datetime))
  ) %>% 
  group_by(group, day) %>% 
  summarize(
    defecation_events = n()
    # mean_SDD = mean(SDD),
    # sd_SDD = sd(SDD)
  )

dat.all.summary <- dat.all.summary %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "SantaMaria", "Taquara"))

# Relevel all fragment names to size categories
dat.all.summary <- dat.all.summary %>% 
  mutate(group = recode(group,
                        "Santa Maria" = "SantaMaria"
  )) %>% 
  mutate(
    fragment = case_when(
      group == "Suzano" ~ "Riparian",
      group == "Guareí" ~ "Small",
      group == "SantaMaria" ~ "Medium",
      group == "Taquara" ~ "Continuous",
      TRUE ~ "check"
    )
  ) %>% 
  mutate(
    fragment = forcats::fct_relevel(fragment, "Riparian", "Small", "Medium", "Continuous")
  )

### Write csv
# dat.all.summary %>% 
#   write.csv(here("Data", "Seed_dispersal", "Curated", "Param_all-data", "Table_n_defecations.csv"),
#             row.names = FALSE)

# dat.all.summary %>% str()

dat.all.summary %>% 
  ggplot() +
  geom_boxplot(aes(x = group, y = defecation_events, fill = group),
               notch = TRUE) +
  geom_point(aes(x = group, y = defecation_events, fill = group),
             position = position_jitter(width = .05)) + # position_dodge() does not work for geom_point: https://stackoverflow.com/questions/17281027/dodge-not-working-when-using-ggplot2
  # geom_dotplot(aes(x = group, y = defecation_events, fill = group)) + # didnt work: https://stackoverflow.com/questions/17281027/dodge-not-working-when-using-ggplot2
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 11)
  ) +
  ylab("number of defecations per day") #+
# facet_wrap(~disp_event, nrow = 2)

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", "Param_siminputrow", 'n-defecations-per-day_By-group.png'), height = 5, width = 7)


# By group
dat.all.summary %>% 
  mutate(
    fragment = forcats::fct_relevel(fragment, "Riparian", "Small", "Medium", "Continuous")
  ) %>% 
  ggplot() +
  geom_boxplot(aes(x = fragment, y = defecation_events, fill = fragment),
               notch = TRUE) +
  geom_point(aes(x = fragment, y = defecation_events, fill = fragment),
             position = position_jitter(width = .05)) + # position_dodge() does not work for geom_point: https://stackoverflow.com/questions/17281027/dodge-not-working-when-using-ggplot2
  # geom_dotplot(aes(x = group, y = defecation_events, fill = group)) + # didnt work: https://stackoverflow.com/questions/17281027/dodge-not-working-when-using-ggplot2
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 11)
  ) +
  ylab("number of defecations per day") #+
# facet_wrap(~disp_event, nrow = 2)

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", "Param_siminputrow", 'n-defecations-per-day_By-fragment.png'), height = 5, width = 7)




# By month
dat.all.summary <- dat.all.sd %>% 
  mutate(
    day = lubridate::ymd(as_date(def_datetime)), 
    month = lubridate::month(as_date(def_datetime), label = T, abbr = T)
  ) %>% 
  group_by(group, month, day) %>% 
  summarize(
    defecation_events = n()
    # mean_SDD = mean(SDD),
    # sd_SDD = sd(SDD)
  )
dat.all.summary <- dat.all.summary %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "SantaMaria", "Taquara"))

# Relevel all fragment names to size categories
dat.all.summary <- dat.all.summary %>% 
  mutate(group = recode(group,
                        "Santa Maria" = "SantaMaria"
  )) %>% 
  mutate(
    fragment = case_when(
      group == "Suzano" ~ "Riparian",
      group == "Guareí" ~ "Small",
      group == "SantaMaria" ~ "Medium",
      group == "Taquara" ~ "Continuous",
      TRUE ~ "check"
    )
  ) %>% 
  mutate(
    fragment = forcats::fct_relevel(fragment, "Riparian", "Small", "Medium", "Continuous")
  )


dat.all.summary %>% 
  mutate(
    fragment = forcats::fct_relevel(fragment, "Riparian", "Small", "Medium", "Continuous")
  ) %>% 
  ggplot() +
  geom_boxplot(aes(x = month, y = defecation_events, fill = month),
               notch = F) +
  geom_point(aes(x = month, y = defecation_events, fill = month), size = 2,
             position = position_jitter(width = .05)) + # position_dodge() does not work for geom_point: https://stackoverflow.com/questions/17281027/dodge-not-working-when-using-ggplot2
  # geom_dotplot(aes(x = group, y = defecation_events, fill = month)) + # didnt work: https://stackoverflow.com/questions/17281027/dodge-not-working-when-using-ggplot2
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 11)
  ) +
  ylab("number of defecations per day") #+
# facet_wrap(~disp_event, nrow = 2)

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", "Param_siminputrow", 'n-defecations-per-day_By-month.png'), height = 5, width = 7)



getwd()
# ---------



  

# Continue with the siminputmatrix
b <- b %>% 
  dplyr::filter(disp_day == "same day")


  mutate(
    # time_def_mean = (time_def_mean)
    # max_time_seeds_mean = seconds_to_period(max_time_seeds_mean), # divided by 60 (= minutes)
      # max_time_seeds_sd = seconds_to_period(max_time_seeds_sd)     # divided by 60 (= minutes) 
      )

b %>% str()
dat.all.sd %>% str()

