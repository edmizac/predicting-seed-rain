# Script name: 04_Feedingbout.R
# Script purpose: summarize feedingbout (siminputmatrix) for model parameterization 
# and sensitivity-analysis (December2022 folder)

# Date created: 2022-12-23d
# Author: Eduardo Zanette

## Notes --------------------------- 
# 
#

## Options -------------------------
# (plotting, memory limit, decimal digits)

# ggplot theme
theme_set(theme_bw(base_size = 15))

## Packages -------------------------
library("here")
library("dplyr")
library("ggplot2")
library("readxl")
library("lubridate")
library("hms")


# # Load siminputrow matrix  -------------------
# siminputmatrix <- read.csv(here("Data", "Movement", "Curated", "Param_Simulation-time",
#                                 "BLT_groups_data_summary_siminputrow.csv"),
#                            sep = ",", dec = ".", stringsAsFactors = TRUE) %>%
#   mutate(group = recode(group, "Guarei" = "Guareí")) # to match all other datasets


# Read siminputrow movement data
mv <- read.csv(here("Data", "Movement", "Curated", "Param_siminputrow", 
                            "Siminputrow_movement-data.csv") #"BLT_groups_data_siminputrow.csv"),
                       #row.names = FALSE
) %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) %>%  # to match all other datasets
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "Santa Maria", "Taquara")) %>% 
  mutate(id_month = forcats::fct_relevel(id_month, "Jan", "Mar", "Apr", "May", 
                                         "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  # mutate(
  #   datetime = ymd_hms(datetime),
  #   date = lubridate::date(datetime)
  # ) %>% 
  rename(
    month = id_month
  ) %>% 
  mutate(
    datetime = lubridate::ymd_hms(datetime)
  )



a <- mv %>% 
  dplyr::filter(id_tree != is.na(id_tree)) %>% 
  dplyr::filter(behavior == "Frugivory") %>% 
  # group_by(group, month, date, id_tree) %>%
  mutate(
    datetimenum = as.numeric(datetime) / 60
    # datetimenum = lubridate::period_to_seconds(datetime),
    # dummy = c(0, diff(datetimenum))
    )


a$difftime <- NA
a$feedingbout_id <- NA

for (n in 2:nrow(a)) {
  a$difftime[n] <- as.numeric(a$datetime[n] -
                                 a$datetime[n - 1],
                               units = "mins")
}

a %>% str()


## Define feedingbouts based on time differences of 30 minutes (6 timesteps): ----
feedingbout_interval <- 30 # if the tamarins stop eating for 30 min and then come back to the same tree, it is counted as another feedingbout

t <- 1
for(n in 2:nrow(a)) {
  if (abs(a$difftime[n]) > feedingbout_interval) { # use abs otherwise the order of the rbind of datasets will give a negative value
    t <- t + 1
  }
  a$feedingbout_id[n] <- t
}
a$feedingbout_id[1] <- 0
a$feedingbout_id <- paste0("fb_", a$feedingbout_id)

# Filter large values and generate feedingbout values
b <- a %>% 
  # group_by(group, month, date, id_tree) %>%
  group_by(group, month, date, id_tree, feedingbout_id) %>%
    summarise(
      feedingbout_dur = sum(dt) / 60 # in minutes
    )

b %>% 
  ggplot() +
  geom_density(aes(x = feedingbout_dur, fill = group), alpha = 0.5) +
  xlab("Feeding bout duration (min)")

# b %>% 
#   ggplot() +
#   geom_density(aes(x = mean, fill = group), alpha = 0.5) +
#   xlab("Feeding bout duration (min)")

  
b %>% 
  ggplot() +
  geom_histogram(aes(x = feedingbout_dur, y=..density.., fill = group)
                 , alpha = 0.8
                 , position = "dodge"
                 ) +
  geom_density(aes(x = feedingbout_dur, fill = group), alpha = 0.5)

# some feedbingbouts are too big (> 100 mins)


## Figure 2 (Felipe) -----
c <- dplyr::left_join(mv, a)
c <- dplyr::inner_join(c, b)

c %>% str()

c <- c %>% 
  dplyr::filter(!is.na(feedingbout_dur)) %>% 
  dplyr::filter(behavior == "Frugivory") %>% 
  dplyr::select(-c(1:9, datetime, datetimenum, difftime)) %>% 
  dplyr::distinct() %>% 
  # group_by(group, month, species) %>% 
  group_by(group, month) %>% 
  mutate(
    richness = n_distinct(species)
  ) %>% 
  # add_tally() %>% 
  ungroup()

c <- c %>% 
  mutate(species = case_when(species == "inch_pass" ~ NA_character_,
                             species == NA ~ NA_character_,
                             TRUE ~ species,
                             is.na(species)  ~ NA_character_,
  )
  ) %>% 
  mutate(
    species = forcats::fct_reorder(species, richness, .desc = TRUE)
  )

c$species %>% unique()


c <- c %>% 
  mutate(
    feedingbout_dur_timestep = feedingbout_dur / 5 #convert to timesteps
  )

# cel.igu <- c %>% 
#   dplyr::filter(species == "Celtis iguanaea")

# # Write csv
# c %>%
#   write.csv(here("Data", "Movement", "Curated", "Param_siminputrow", "Siminputrow_feedingbout-data.csv"),
#             row.names = FALSE)

d <- c %>% 
  # group_by(group, month, species) %>%
  group_by(group, species) %>%
  summarise(
    mean_min = mean(feedingbout_dur, na.rm = TRUE),
    sd_min = sd(feedingbout_dur, na.rm = TRUE),
    sqrt_min = sqrt(length(feedingbout_dur)),
    error_min = sd_min / sqrt_min,
    mean_timestep = mean(feedingbout_dur_timestep, na.rm = TRUE),
    sd_timestep = sd(feedingbout_dur_timestep, na.rm = TRUE),
    
  ) %>% 
  ungroup()

# # Write csv
# d %>%
#   write.csv(here("Data", "Movement", "Curated", "Param_siminputrow", "Siminputrow_feedingbout-summary.csv"),
#             row.names = FALSE)


# Option 1: with errorbars
ggplot() +
  # geom_bar(position = "dodge", stat = "identity") +
  geom_bar(
    data = d, aes(x = factor(species), y = mean, fill = group),
    position = position_dodge(.9), stat = "identity", alpha = 0.6
  ) +
  # geom_bar(position = position_dodge(.9), stat = "identity", color = "black") +
  # geom_bar(stat = "identity", color = "black") +
  geom_point(data = c, aes(x = factor(species), y = feedingbout_dur, color = group),
             position = position_jitterdodge(0.9)) +
  geom_errorbar(
    data = d, aes(x = factor(species), ymin=mean-error, ymax=mean+error, fill = group),
    position = position_dodge(.9),
    # position = "dodge",
    width = 0.3
  ) +
  ylab("Mean feeding bout duration (min)") +
  # facet_wrap(~group, nrow = 2) +
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text.x = element_text(size = 9,
                               angle = 45,
                               # vjust = 0.3,
                               hjust = 1,
    ),
    # axis.text.x = element_text(size = 9,
    #                            angle = 90,
    #                            vjust = 0.3,
    #                            hjust = 1,
    # ),
    axis.title.x = element_blank(),
    title = element_text(size = 13),
    legend.position = "bottom"
  ) #+ 
  # scale_fill_viridis_d()
  # scale_fill_viridis_d(option = "inferno") #+
  # scale_color_viridis_d(option = "inferno")
  
# Save plot
ggsave(filename = here("Data", "Movement", "Curated", "Param_siminputrow",
                       "Siminputrow_Feedingbout_errorbar_byrichness.png"),  # drop fct_reorder to see plant species by alphabetical order
       dpi = 300, width = 15, height = 12, units = "cm")


# Option 1: with sd
ggplot() +
  # geom_bar(position = "dodge", stat = "identity") +
  geom_bar(
    data = d, aes(x = factor(species), y = mean, fill = group),
    position = position_dodge(.9), stat = "identity", alpha = 0.6
  ) +
  # geom_bar(position = position_dodge(.9), stat = "identity", color = "black") +
  # geom_bar(stat = "identity", color = "black") +
  geom_point(data = c, aes(x = factor(species), y = feedingbout_dur, color = group),
             position = position_jitterdodge(0.9)) +
  geom_errorbar(
    data = d, aes(x = factor(species), ymin=mean-sd, ymax=mean+sd, fill = group),
    position = position_dodge(.9),
    # position = "dodge",
    width = 0.3
  ) +
  ylab("Mean feeding bout duration (min)") +
  # facet_wrap(~group, nrow = 2) +
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text.x = element_text(size = 9,
                               angle = 45,
                               # vjust = 0.3,
                               hjust = 1,
    ),
    # axis.text.x = element_text(size = 9,
    #                            angle = 90,
    #                            vjust = 0.3,
    #                            hjust = 1,
    # ),
    axis.title.x = element_blank(),
    title = element_text(size = 13),
    legend.position = "bottom"
  ) #+ 
# scale_fill_viridis_d()
# scale_fill_viridis_d(option = "inferno") #+
# scale_color_viridis_d(option = "inferno")

# Save plot
ggsave(filename = here("Data", "Movement", "Curated", "Param_siminputrow",
                       "Siminputrow_Feedingbout_sd_byreachness.png"), # drop fct_reorder to see plant species by alphabetical order
       dpi = 300, width = 15, height = 12, units = "cm")
  
  
  
  
  
  

