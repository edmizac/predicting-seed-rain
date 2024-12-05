# Script name: 02_params-siminputrow.R
# Script purpose: derive empirical values of Movement parameters for parameterizing 
# the model to run on the nine situations assigned in BLT_groups_data_summary_aftercleaning.csv in 
# Data/Movement/Curated.

# Date created: 2022-11-16d
# Author: Eduardo Zanette

## Notes --------------------------- 
# Code adapted from "asltraj_script_2022-06-02d.R" and 
# "TurningAnglesScript_EMZ6_2022-07-27d.R" in BLT-Movement-Patterns github repository


## Options -------------------------
# (plotting, memory limit, decimal digits)
# 
our_crs <- "+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
validation_pallete <- readRDS(here("Data", "pallete_validation4c.rds"))


## Packages -------------------------
library("here")
library("lubridate")
# library("hms")
library("dplyr")
library("tidyr")
library("readxl")
# library("sf")
library("sp")
library("adehabitatLT")
library("circular")
library("ggplot2")


# Read data (filtered by siminputrow on CurateData.R)
dat.all <- read.csv(here("Data", "Movement", "Curated", "BLT_groups_data_siminputrow.csv")
                    #, stringsAsFactors = TRUE
                    )  %>% 
  mutate(group = recode(group, 
                        "Guarei" = "Guareí",
                        "Santa Maria" = "SantaMaria"
                        )) %>%  # to match all other datasets
  rename(
    # datetime = POSIXct,
    # id_tree = id
    month = id_month
    )

str(dat.all)
# dat.all$id %>% levels()
dat.all$id_day_all %>% levels()
dat.all$month %>% levels()
dat.all$group %>% unique()

# Make POSIXct
dat.all <- dat.all %>% 
  mutate(datetime = ymd_hms(datetime, tz = "America/Sao_Paulo"))

# Transforming to ltraj
dat.all.ltraj <- adehabitatLT::as.ltraj(xy = dat.all[, c("x", "y")], 
                                       date = dat.all$datetime, 
                                       id = dat.all$group,
                                       burst = lubridate::as_date(dat.all$datetime), # bursts as date because all sampling days are complete (from sleepint tree to sleeping tree)
                                       proj4string = sp::CRS(our_crs),
                                       typeII = TRUE,
                                       infolocs = dat.all[ , c(1, 3:(ncol(dat.all)))] #only non-used collumns
)

# Check ltraj objetct
dat.all.ltraj # type II # no NAs, Regular. Bursts = days. Day 2019-08-19 is too short **CHECK WITH FELIPE


# convert to df and save as csv
dat.all.ltraj.df <- adehabitatLT::ld(dat.all.ltraj)

# Remove repeated columns
dat.all.ltraj.df <- dat.all.ltraj.df %>% 
  dplyr::select(!c(y.1,date)) %>% 
  rename(date = burst) %>% 
  mutate_if(is.character, as.factor)

# dat.all.ltraj.df %>% colnames()
dat.all.ltraj.df %>% str()
dat.all.ltraj.df$dist %>% summary()

# a %>% str()

# # Write csv
# dat.all.ltraj.df %>%
#   write.csv(here("Data", "Movement", "Curated", "Param_siminputrow", "Siminputrow_movement-data.csv"),
#             row.names = FALSE)


# a <- dat.all.ltraj.df$rel.angle %>%
#   rad2deg(
#   # circular::circular(
#     # units='degrees'
#                      # , rotation='clock', 
# # ,zero=pi/2,
# # modulo='2pi'
# ) %>% 
#   plot()
# 
# a$zero
# a$rotation
# a$next.points





## Summary  -------------------

# Transform rel.ang to circular
# dat.all.ltraj.df <- dat.all.ltraj.df %>% 
  # mutate(rel.angle = circular::as.circular(rel.angle))

# Convert rel.angles to degrees
# dat.all.ltraj.df$rel.angle %>% max(na.rm = TRUE) # 3.1415 = 180°. Why is not relative angles bigger than 180°? Because it is to the left or to the right! # adehabitatLT: "abs.angle,rel.angle are expressed in radians"
# dat.all.ltraj.df$rel.angle %>% circular() %>% plot() # https://www.google.com/search?q=convert+pi+to+degrees&oq=converting+pi+&aqs=chrome.1.69i57j0i13i19i512l9.7254j0j4&sourceid=chrome&ie=UTF-8
dat.all.ltraj.df <- dat.all.ltraj.df %>%
  mutate(
    rel.angle.degree = rel.angle * 180 / pi,
    abs.angle.degree = abs.angle * 180 / pi
  )


# Relevel
dat.all.ltraj.df <- dat.all.ltraj.df %>%
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
  ) %>% 
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec"))



# Absolute angles For Ronald (11d/08/2023) -----

### With amt ------
library("amt")
trk <- dat.all.ltraj.df %>% 
  amt::make_track(.x=x, .y=y, id = c(fragment, month, date),
                  .t = datetime,
                      crs = our_crs,
                      all_cols = TRUE)

# summarize_sampling_rate(trk)

# # Resample tracks to create bursts:
trk <- trk %>%
  track_resample(rate = minutes(5), tolerance = minutes(2))

trk_b <- trk %>% 
  steps_by_burst()

trk$t_
trk_b$t1_

trk <- left_join(trk, trk_b, by = join_by(t_ == t1_))

trk <- trk %>%
  mutate(
    # IU = intensity_use(.),
    # sl = step_lengths(.),
    # ta = amt::direction_rel(.) %>%  amt::as_degree(.),
    # ta_abs = amt::direction_abs(.) %>%  amt::as_degree(.),
    ta = amt::as_degree(ta_),
    ta_abs = amt::as_degree(direction_p)
  )
# 
trk$sl_ %>% summary() # it looks right now. (before resampling: there was something wrong. I should define bursts with track_resample() but I think this funciton changes the x and y locations)
# 
# trk %>% 
#   dplyr::filter(sl > 150) %>% 
#   View()


# Plot
trk %>%
  ggplot() +
  geom_histogram(aes(ta, fill = fragment)) +
  geom_histogram(aes(ta_abs, fill = NA), alpha = 0.6) +
  scale_y_sqrt(
    breaks = c(0, 10, 25, 50, 120)
    ) +
  ylab("sqrt(count)") +
  theme_bw() +
  facet_grid(fragment ~ month) +
  scale_fill_manual(values = rev(validation_pallete),
                    breaks = c("Riparian", "Small", "Medium", "Continuous")
                    ) +
  scale_x_continuous(limits = c(-180, 180), n.breaks = 4)


# This one has less data than with adehabitatLT. Thus, I'll continue with adehabitatLT and ignore this one
# # Save
# ggsave(filename = here("Data", "Movement","Curated", "Param_siminputrow",
#                        "02_angles_amt.png"),
#        dpi = 300,  width = 14, height = 10)





### With adehabitatLT ------
par(mfrow = c(2,1))
hist(dat.all.ltraj.df$abs.angle.degree)
hist(dat.all.ltraj.df$rel.angle.degree)
par(mfrow = c(1,1))

dat.all.ltraj.df$group
dat.all.ltraj.df$month


# Same number of observations?
n_min <- dat.all.ltraj.df %>% 
  group_by(fragment, month) %>% 
  summarise(
    n = n()
  ) %>% 
  ungroup() %>% 
  slice_min(n, n=1) %>% 
  dplyr::select(n) %>% 
  pull()

# Plot
dat.all.ltraj.df %>% 
  
  # Same number of observations :
  group_by(fragment, month) %>% 
  slice_sample(n = n_min) %>% 

  ggplot() +
  geom_histogram(aes(rel.angle.degree, fill = fragment)) +
  geom_histogram(aes(abs.angle.degree, fill = NA), alpha = 0.6) +
  scale_y_sqrt(
    # breaks = c(0, 10, 25, 50, 100, 120)
    breaks = c(0, 10, 25, 50, 75)
  ) +
  ylab("sqrt(count)") +
  theme_bw() +
  facet_grid(fragment ~ month) +
  scale_fill_manual(values = rev(validation_pallete), 
                    breaks = c("Riparian", "Small", "Medium", "Continuous")
  ) +
  scale_x_continuous(limits = c(-180, 180), n.breaks = 4)


# # Save
# ggsave(filename = here("Data", "Movement","Curated", "Param_siminputrow",
#                        "02_angles_adehabitat_rarefied.png"),
#        dpi = 300,  width = 14, height = 10)


# Summarize step length and turning angles for all data and for travel/foraging behavior separetely
dat.mv.summary.all <- dat.all.ltraj.df %>% 
  # droplevels() %>% 
  group_by(group, month) %>%
  summarise(
    step_len_median = median(dist, na.rm = TRUE),
    step_len_mean = mean(dist, na.rm = TRUE),
    step_len_sd = sd(dist, na.rm = TRUE),
    rel_angle_mean = mean(rel.angle.degree, na.rm = TRUE), # rad to degrees: * 180 / pi,
    rel_angle_sd = sd(rel.angle.degree, na.rm = TRUE), # rad to degrees: * 180 / pi,
    # rel_angle_mean = mean(circular(rel.angle, type="angles", units="degrees",
    #                                modulo="pi", template='geographics'),
    #                       na.rm = TRUE),
    # rel_angle_sd = sd(circular(rel.angle, type="angles", units="degrees",
    #                            modulo="pi", template='geographics'),
    #                   na.rm = TRUE),
    # Although the mean is output in degreees, sd is not # https://stackoverflow.com/questions/55870751/is-the-standard-deviation-in-the-circular-package-in-r-correct
    # rel_angle_sd = rel_angle_sd * 180 / pi,
    
    # Max angle quantiles
    max_random_angle_95q = quantile(
      Mod(rel.angle.degree), 
      0.95, na.rm = TRUE
    ),
    max_random_angle_75q = quantile(
      Mod(rel.angle.degree), 
      0.75, na.rm = TRUE
    )
  )


target_behav <- c("Travel", "Foraging")

dat.mv.summary.behav <- dat.all.ltraj.df %>% 
  dplyr::filter(behavior %in% target_behav) %>% # or (after group_by) dplyr::filter(behavior == "Travel") %>% # Try "Frugivory", "Travel" and "Foraging"
  # droplevels() %>% 
  group_by(group, month, behavior) %>%
  summarise(
    step_len_median = median(dist, na.rm = TRUE),
    step_len_mean = mean(dist, na.rm = TRUE),
    step_len_sd = sd(dist, na.rm = TRUE),
    rel_angle_mean = mean(rel.angle.degree, na.rm = TRUE), # rad to degrees: * 180 / pi,
    rel_angle_sd = sd(rel.angle.degree, na.rm = TRUE), # rad to degrees: * 180 / pi,
    # rel_angle_mean = mean(circular(rel.angle, type="angles", units="degrees",
    #                                          modulo="pi", template='geographics'),
    #                                 na.rm = TRUE),
    # rel_angle_sd = sd(circular(rel.angle, type="angles", units="degrees",
    #                              modulo="pi", template='geographics'),
    #                     na.rm = TRUE),
    # # Although the mean is output in degreees, sd is not # https://stackoverflow.com/questions/55870751/is-the-standard-deviation-in-the-circular-package-in-r-correct
    # rel_angle_sd = rel_angle_sd * 180 / pi,
    
    # Max angle quantiles
    max_random_angle_95q = quantile(
      Mod(rel.angle.degree), 
      0.95, na.rm = TRUE
    ),
    max_random_angle_75q = quantile(
      Mod(rel.angle.degree), 
      0.75, na.rm = TRUE
    )
  )

# pivor wider
dat.mv.summary.behav <- dat.mv.summary.behav %>% 
  tidyr::pivot_wider(id_cols = c(group, month), names_from = "behavior", 
                     values_from = c(step_len_mean:max_random_angle_75q)
  )

# Mod(dat.all.ltraj.df$rel.angle)


# Join
dat.mv.summary <- dplyr::left_join(dat.mv.summary.all, dat.mv.summary.behav)

# # Write csv
# dat.mv.summary %>%
#   write.csv(here("Data", "Movement", "Curated", "Param_siminputrow", "Siminputrow_sl_ta.csv"),
#             row.names = FALSE)



## Summarize activity budget for calculating p_foraging_while_traveling -----
dat.all.ltraj.df$behavior %>% levels()
dat.all.ltraj.df$behavior %>% unique()

# First make idle = resting (the model does not distinguish them)
dat.all.ltraj.df <- dat.all.ltraj.df %>% 
  mutate(
    behavior = case_when(
      behavior == "Inactive" ~ "Resting",
      TRUE ~ behavior
    )
  )
# Check:
dat.all.ltraj.df$behavior %>% unique()

# Select target behaviors:
target_behav <- c("Frugivory", "Travel", "Foraging", "Resting")

# By day
dat.ab.summary.day <- dat.all.ltraj.df %>% 
  group_by(group, month, date, behavior) %>%
  summarise(
    n = n()
  ) %>% 
  mutate(
    perc_behavior = 100 * (n / sum(n))
  ) %>% 
  
  # filter only behaviors of interest of the model
  dplyr::filter(behavior %in% target_behav
  )

# # Write csv
# dat.ab.summary.day %>%
#   write.csv(here("Data", "Movement", "Curated", "Param_siminputrow", "Siminputrow_Activity-budget_By-day.csv"),
#             row.names = FALSE)

# By month (first summarize by day and get average of percentages)
dat.ab.summary <- dat.ab.summary.day %>% 
  ungroup() %>% 
  group_by(group, month, behavior) %>% 
  summarise(
    perc_behavior_mean = mean(perc_behavior),
    perc_behavior_sd = sd(perc_behavior)
  )

# # Write csv
# dat.ab.summary %>%
#   write.csv(here("Data", "Movement", "Curated", "Param_siminputrow", "Siminputrow_Activity-budget_By-month.csv"),
#             row.names = FALSE)



## Derive p_foraging_while_traveling based on Ronald's suggestion: -----
## p_foraging_while_traveling = foraging / (foraging + traveling)
target_behav <- c("Travel", "Foraging")
dat.ab.summary <- dat.ab.summary %>% 
  dplyr::filter(behavior %in% target_behav) %>%  # only behaviors of interest 
  tidyr::pivot_wider(values_from = c(perc_behavior_mean, perc_behavior_sd), names_from = "behavior")

dat.ab.summary.p_travel <- dat.ab.summary %>% 
  group_by(group, month) %>%
  # dplyr::filter(behavior %in% target_behav) %>% 
  mutate(
    p_foraging_while_traveling = perc_behavior_mean_Foraging / (perc_behavior_mean_Foraging + perc_behavior_mean_Travel)
  ) 

# # Write csv
# dat.ab.summary.p_travel %>%
#   write.csv(here("Data", "Movement", "Curated", "Param_siminputrow", "Siminputrow_Activity-budget_p-foraging.csv"),
#             row.names = FALSE)
















## Load siminputrow matrix from simulation time data -------------------
siminputmatrix <- read.csv(here("Data", "Movement", "Curated", "Param_Simulation-time", 
                                "BLT_groups_data_summary_siminputrow.csv"),
                           sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  mutate(group = recode(group,                 # to match all other datasets
                        "Guarei" = "Guareí",
                        "Santa Maria" = "SantaMaria"
                        )) %>% 
  rename(month = id_month)
siminputmatrix %>% str()


## Merge parameters data derived here to siminputrow matrix -------------------
siminputmatrix_mov <- left_join(siminputmatrix, dat.mv.summary)
siminputmatrix_mov <- left_join(siminputmatrix_mov, dat.ab.summary.p_travel)

# # Write csv
# siminputmatrix_mov %>%
#   write.csv(here("Data", "Movement", "Curated", "Param_siminputrow", "Siminputrow_parameters_movement.csv"),
#             row.names = FALSE)


