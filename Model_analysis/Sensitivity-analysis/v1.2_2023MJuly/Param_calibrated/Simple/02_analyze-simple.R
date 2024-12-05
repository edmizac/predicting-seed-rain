# Script name: 02_analyze-simple.R
# Script purpose: create validation plots with calibrated parameters 
# to compare with v1.2 results

# Date created: 2023-04-23d
# Last update: 2023-10-31d
# Author: Eduardo Zanette

## Notes --------------------------- 
#
#

## Options -------------------------
# (plotting, memory limit, decimal digits)
# 

## Packages -------------------------
library("here")
library("nlrx")
library("dplyr")
library("readr")
library("ggplot2")
library("ggridges")
library("ggspatial")
library("sf")
library("purrr")

path <- here("Model_analysis", "Sensitivity-analysis",
             "v1.2_2023MJuly", "Param_calibrated", "Simple", "temp")


# ggplot theme
theme_set(theme_bw(base_size = 15))
theme_update(
  axis.text.x = element_text(size = 11)
)



# ggplot theme
theme_set(theme_bw(base_size = 15))
theme_update(
  axis.text.x = element_text(size = 11)
)

validation_pallete <- readRDS(here("Data", "pallete_validation4c.rds"))



## SDD -------------------------

# rm(group)

# Load empirical data
obs <- read.csv(here("Data", "Seed_dispersal", "Curated", "Validation", "Siminputrow_SDD.csv"),
                sep = ",", stringsAsFactors = TRUE)  %>%  
  # mutate(group = recode(group, "Guarei" = "Guareí")) #%>%  # to match all other datasets
  mutate(source = as.factor("observed")) %>% 
  mutate(group = recode(group,
                        "Santa Maria" = "SantaMaria"
  )) %>% 
  rename(
    month = id_month,
    SDD_seeds = SDD
  ) %>% 
  group_by(group, month) %>% 
  
  # mutate(
  #   SDD = mean(SDD_seeds, na.rm = TRUE),
  #   SDD_sd = sd(SDD_seeds, na.rm = TRUE)
  #   ) %>%
  # rename(
  # SDD = SDD_seeds
  # ) %>% 
  
  # group_by(group, month, disp_day) %>% 
  # mutate(
#   SDD_sameday = SDD_seeds
# )
ungroup() %>% 
  as.data.frame() %>% 
  mutate_if(is.character, as.factor)
obs %>% str()

# Simulated data
db_sd_raw <- read.csv(paste0(path, "/", "02_Simoutput-simple-all.csv"))
db_sd_raw %>% str()


# Filter unviable runs
dead_runs <- db_sd_raw %>% dplyr::filter(`survived.` == "no")
# save 
dead_runs %>% 
  write.csv(paste0(path, "/", "02_Simoutput-simple_dead.csv"),
                        row.names = FALSE)

dead_runs$group %>% unique()
dead_runs %>% summary()

db_sd_raw <- db_sd_raw %>% 
  dplyr::filter(`survived.` == "yes")

# Remove runs that presented issues: 
# db_sd <- db_sd %>% dplyr::filter(DPL > 100) # most of them Guareí May and Suzano Sep

db_sd_raw$X.run_number. %>% unique()
db_sd_raw$random_seed %>% unique()

# Filter seed data
db_sd_raw <- db_sd_raw %>% 
  # rename(group = study_area) %>% 
  dplyr::filter(breed == "seeds") %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(source = "simulated") %>% 
  group_by(group, random_seed) %>% # each random seed = 1 run (~ run number)
  mutate(day = as.factor(day),
         disp_day = recode(disp_day,
                           "sameday" = "same day",
                           "nextday" = "next day"
         ) %>% as.factor()
  ) %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "SantaMaria", "Taquara")) %>% 
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  mutate(source = forcats::fct_relevel(source, "observed", "simulated")) %>% 
  mutate(disp_day = forcats::fct_relevel(disp_day, "same day", "next day")) %>% 
  ungroup()

# Relevel all fragment names to size categories
db_sd_raw <- db_sd_raw %>% 
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

db_sd_raw$disp_day %>% str()
db_sd_raw$source %>% str()
db_sd_raw$group %>% levels()
db_sd_raw$month %>% levels()
db_sd_raw$fragment %>% levels()
# db1_sdd <- db1_sdd %>% 
#   filter(SDD > 0)
# db1_sdd <- db1_sdd %>%
#   dplyr::filter(disp_day == "sameday") # next_day SDD is all 0 (check model)


# Summarise mean SDD values as we always compare distribution of means with all observetions of one empirical case
db_sd <- db_sd_raw %>% 
  group_by(group, random_seed) %>% # each random seed = 1 run (~ run number)
  mutate(
    SDD = mean(SDD_seeds, na.rm = TRUE),
    SDD_sd = sd(SDD_seeds, na.rm = TRUE)
  ) %>% 
  dplyr::select(-c(x, y, SDD_seeds, id_seed, species, mother_tree)) %>% 
  distinct()# %>% 
# ungroup()

obs2 <- obs %>% 
  rename(
    SDD = SDD_seeds
  )


# Merge obserded data into db1_sd
db_sd <- db_sd %>% dplyr::bind_rows(obs2) # summarized (with mean SDD of simulated)

db_sd_raw <- db_sd_raw %>% dplyr::bind_rows(obs) # not summarized (with mean SDD of simulated = used to plot examples)

db_sd <- db_sd %>% 
  mutate(source = as.factor(source)) %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "SantaMaria", "Taquara")) %>% 
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  mutate(source = forcats::fct_relevel(source, "observed", "simulated")) %>% 
  mutate(disp_day = forcats::fct_relevel(disp_day, "same day", "next day"))

# mutate_if(is.character, as.factor)

db_sd$disp_day %>% str()
db_sd$disp_day %>% levels()
db_sd$source %>% levels()
db_sd$group %>% levels()
length(db_sd$disp_day %>% is.na(.)) # one NA?
a <- db_sd[db_sd$disp_day %>% is.na(.), ] # whatever

# db_sd$group %>% str()
## Density Option 1: by day of dispersal
# By group

db_sd <- db_sd %>% 
  dplyr::filter(!is.na(SDD)) %>% 
  dplyr::filter(!is.na(disp_day)) %>% 
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  mutate(source = forcats::fct_relevel(source, "observed", "simulated")) %>% 
  mutate(disp_day = forcats::fct_relevel(disp_day, "same day", "next day"))

db_sd %>% str()
db_sd$source %>% summary()


# Relevel all fragment names to size categories
db_sd <- db_sd %>% 
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

#check:
db_sd$fragment %>% unique()

db_sd$source %>% unique()



### Plots ------
theme_update(
  axis.text.x = element_text(size = 11)
)

#### Mean SDD By group ####
# density
db_sd %>%
  # a <- db_sd %>%
  # Santa Maria April is missing from observed data (not enough observations), so we drop the simulations
  dplyr::filter(
    group != "SantaMaria" | month != "Apr"
  ) %>%
  # dplyr::select(fragment, group, month, source, seed, disp_day, SDD) %>%
  # dplyr::distinct() %>%
  # droplevels() %>%
  ggplot(
    # aes(x = SDD, fill = group, group = group)
    aes(x = SDD, group = fragment, fill = fragment)
  ) +
  geom_density(
    alpha = 0.5
    # , adjust = 1.5
  ) +
  xlab("Mean SDD (m)") +
  # facet_grid(. ~ disp_day, rows = 2)
  # facet_wrap(vars(source, disp_day), nrow = 2) +
  # facet_grid(disp_day ~ source) +
  facet_wrap(~source) +
  scale_fill_manual(values = validation_pallete) +
  # ggtitle("Density plot with adjustment = 2") +
  
  # others
  theme(axis.text = element_text(size = 9))

# Save plot
# ggsave(paste0(path, "/", '02_simple_SDD_disp_day_density.png'), height = 5, width = 7)


# boxplot
theme_update(
  axis.text.x = element_text(size = 8)
)
db_sd %>%
  # Santa Maria April is missing from observed data (not enough observations), so we drop the simulations
  dplyr::filter(
    group != "SantaMaria" | month != "Apr"
  ) %>%
  # nrow() 
  # droplevels() %>% 
  ggplot() +
  # aes(x = group, y = SDD, fill = group) +
  aes(x = fragment, y = SDD, fill = fragment) +
  geom_violin() +
  # see::geom_violinhalf() +
  geom_boxplot(width = 0.3, #alpha = 0.8) + 
               fill = "white", alpha = 0.5) +
  # Half violin half dotplot option 1:
  # geom_dotplot(
  #   binaxis = "y",
  #   stackdir = "center",
  #   stackratio = 0.3,
  #   position = position_jitter(width = 0.01),
  #   dotsize = 0.5
  # ) +
  geom_point(
    size = 0.7,
    shape = 21,
    color = "black",
    # position = position_jitterdodge(jitter.width = .65) # only way of dodging the points and jitter it
    position = position_jitterdodge(seed = 41, dodge.width = 0.9),
  ) +
  # # Half violin half dotplot option 2:
  # geom_violindot(
  #   dots_size = 1.2
  # ) +
  
  scale_fill_manual(values = validation_pallete) +
  # scale_fill_brewer(palette="Spectral") +
  # scale_fill_grey(start = 0.20)+
  theme(axis.title.x = element_blank()) +
  facet_wrap(~disp_day, nrow = 2) +
  ylab("Mean SDD (m)") +
  ylim(0, 800) +
  ggtitle("Mean seed dispersal distance by fragment") +
  # facet_wrap(vars(disp_day, source), nrow = 2) +
  facet_grid(disp_day ~ source) +
  
  # Mean
  stat_summary(fun=mean, geom="point", shape = 23, #shape=18,
               size=1.5, color="white", fill = "red") +
  # others
  theme(
    axis.text = element_text(size = 10), 
    legend.position="bottom"
    # legend.position="none"
  )
# geom_point(position = position_jitterdodge(jitter.width = 0.7))


# Save plot
ggsave(paste0(path, "/", '02_simple_SDD_disp_day_violin.png'), height = 5, width = 7)



#### Mean SDD By group and month ####

# density
theme_update(
  axis.text.x = element_text(size = 11)
)
db_sd %>% 
  # Santa Maria April is missing from observed data (not enough observations), so we drop the simulations
  dplyr::filter(
    group != "SantaMaria" | month != "Apr"
  ) %>%
  dplyr::filter(disp_day == "same day") %>% 
  ggplot(
    # aes(x = SDD, y = group, fill = month) #, height = ..density..)
    aes(x = SDD, y = fragment, fill = month) #, height = ..density..)
  ) +
  geom_density_ridges( 
    # bandwidth = 5, # specify global bandwidth. Problem: we have more simulation data
    #alpha = 0.4,
    # adjust = 5,
    # position = "stack"
    # stat = "identity",
    # trim = TRUE,
    bandwidth = 20,
    scale = 1.2, # heigth
    jittered_points = TRUE,
    point_shape = "|", point_size = 2,
    position = position_points_jitter(width = 0.1, height = 0)
  ) +
  xlab("Mean SDD (m)") +
  # xlim(0, 800) +
  # facet_grid(source ~ disp_day) +
  facet_grid(rows = vars(source)) +
  scale_fill_viridis_d() +
  ggtitle("Mean distance of seeds dispersed on the same day") +
  theme(plot.title = element_text(size = 14)) #+
# theme_ridges()

# # Save plot
ggsave(paste0(path, "/", '02_simple_SDD_same-day_density_ridges.png'), height = 5, width = 7)


db_sd %>% 
  # Santa Maria April is missing from observed data (not enough observations), so we drop the simulations
  dplyr::filter(
    group != "SantaMaria" | month != "Apr"
  ) %>%
  dplyr::filter(disp_day == "next day") %>% 
  ggplot(
    # aes(x = SDD, y = group, fill = month) #, height = ..density..)
    aes(x = SDD, y = fragment, fill = month) #, height = ..density..)
  ) +
  geom_density_ridges( #alpha = 0.4,
    # adjust = 5,
    # position = "stack"
    # stat = "identity",
    # trim = TRUE,
    bandwidth = 20,
    scale = 1.2, # heigth
    jittered_points = TRUE,
    point_shape = "|", point_size = 2,
    position = position_points_jitter(width = 0.1, height = 0)
  ) +
  xlab("Mean SDD (m)") +
  # xlim(0, 800) +
  # facet_grid(source ~ disp_day) +
  facet_grid(rows = vars(source)) +
  scale_fill_viridis_d() +
  ggtitle("Mean distance of seeds dispersed on the next day") +
  theme(plot.title = element_text(size = 14))

# Save plot
ggsave(paste0(path, "/", '02_simple_SDD_next-day_density_ridges.png'), height = 5, width = 7)


db_sd %>% 
  # Santa Maria April is missing from observed data (not enough observations), so we drop the simulations
  dplyr::filter(
    group != "SantaMaria" | month != "Apr"
  ) %>%
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  ggplot(
    aes(x = SDD, y = month, fill = month) #, height = ..density..)
  ) +
  geom_density_ridges( #alpha = 0.4,
    # adjust = 5,
    # position = "stack"
    # stat = "identity",
    # trim = TRUE,
    scale = 1.5, # heigth
    jittered_points = TRUE,
    point_shape = "|", point_size = 2,
    position = position_points_jitter(width = 0.5, height = 0)
  ) +
  xlab("Mean SDD (m)") +
  # facet_grid(source ~ disp_day) +
  facet_grid(source ~ disp_day) +
  scale_fill_viridis_d()

# # Save plot
ggsave(paste0(path, "/", '02_simple_SDD_disp_day_density_ridges1.png'), height = 5, width = 10)


# ggrides with boxplot
db_sd %>% 
  # Santa Maria April is missing from observed data (not enough observations), so we drop the simulations
  dplyr::filter(
    group != "SantaMaria" | month != "Apr"
  ) %>%
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  ggplot(
    aes(x = SDD, y = month, fill = month) #, height = ..density..)
  ) +
  geom_density_ridges( #alpha = 0.4,
    # adjust = 5,
    # position = "stack"
    # stat = "identity",
    # trim = TRUE,
    bandwidth = 20,
    scale = 0.9, # heigth
    jittered_points = TRUE,
    point_shape = "|", point_size = 2,
    position = position_points_jitter(width = 0.5, height = 0)
  ) +
  geom_boxplot(
    width = .20, position = position_nudge(y = -.25) #, outlier.shape = NA
  ) +
  xlab("Mean SDD (m)") +
  # facet_grid(source ~ disp_day) +
  facet_grid(source ~ disp_day) +
  scale_fill_viridis_d()

# # Save plot
ggsave(paste0(path, "/", '02_simple_SDD_disp_day_density_ridges1_withboxplot.png'), height = 5, width = 10)




db_sd %>% 
  # Santa Maria April is missing from observed data (not enough observations), so we drop the simulations
  dplyr::filter(
    group != "SantaMaria" | month != "Apr"
  ) %>%
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  ggplot(
    aes(x = SDD, y = month, fill = month) #, height = ..density..)
  ) +
  geom_density_ridges( #alpha = 0.4,
    # adjust = 5,
    # position = "stack"
    # stat = "identity",
    # trim = TRUE,
    scale = 1.5, # heigth
    jittered_points = TRUE,
    point_shape = "|", point_size = 2,
    position = position_points_jitter(width = 0.5, height = 0)
  ) +
  xlab("Mean SDD (m)") +
  # facet_grid(source ~ disp_day) +
  facet_grid(~source) +
  scale_fill_viridis_d() +
  ggtitle("Mean SDD all events (same and next day)") +
  theme(plot.title = element_text(size = 16))

# # Save plot
ggsave(paste0(path, "/", '02_simple_SDD_disp_day_density_ridges2.png'), height = 5, width = 7)



## ggrides with boxplot
db_sd %>% 
  # Santa Maria April is missing from observed data (not enough observations), so we drop the simulations
  dplyr::filter(
    group != "SantaMaria" | month != "Apr"
  ) %>%
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  ggplot(
    aes(x = SDD, y = month, fill = month) #, height = ..density..)
  ) +
  geom_density_ridges( #alpha = 0.4,
    # adjust = 5,
    # position = "stack"
    # stat = "identity",
    # trim = TRUE,
    bandwidth = 20,
    scale = 0.9, # heigth
    jittered_points = TRUE,
    point_shape = "|", point_size = 2,
    position = position_points_jitter(width = 0.5, height = 0)
  ) +
  geom_boxplot(
    width = .15, position = position_nudge(y = -.15) #, outlier.shape = NA
  ) +
  xlab("Mean SDD (m)") +
  # facet_grid(source ~ disp_day) +
  facet_grid(~source) +
  scale_fill_viridis_d() +
  ggtitle("Mean SDD all events (same and next day)") +
  theme(plot.title = element_text(size = 16))

# # Save plot
ggsave(paste0(path, "/", '02_simple_SDD_disp_day_density_ridges2_withboxplot.png'), height = 5, width = 7)



# boxplot
theme_update(
  axis.text.x = element_text(size = 8)
)
db_sd %>% 
  # Santa Maria April is missing from observed data (not enough observations), so we drop the simulations
  dplyr::filter(
    group != "SantaMaria" | month != "Apr"
  ) %>%
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  ggplot(
    # aes(x = group, y = SDD, color = month)
    aes(x = fragment, y = SDD, group = month, color = month, fill = month)
  ) +
  # # Option 1:
  # geom_boxplot() +
  # geom_point(
  #   size = 0.5,
  #   # position = position_jitterdodge(jitter.width = .65) # only way of dodging the points and jitter it
  #   position = position_dodge(0.9)
  # ) +
  # Option 2:
  geom_violin(lwd = 0.65, position = position_dodge(0.9)
              , fill = "white"
  ) +
  # see::geom_violinhalf() +
  # geom_boxplot(width = 0.3, #alpha = 0.8) + 
  #              fill = "white", alpha = 0.5,
  #              position = position_dodge(0.9)
  #              ) +
  # Half violin half dotplot option 1:
  # geom_dotplot(
  #   binaxis = "y",
  #   stackdir = "center",
  #   stackratio = 0.3,
  #   # position = position_jitter(width = 0.01),
#   position = position_dodge(0.9),
#   dotsize = 0.5
# ) +
geom_point(
  size = 0.6,
  shape = 21,
  color = "black",
  # position = position_jitterdodge(jitter.width = .65) # only way of dodging the points and jitter it
  position = position_jitterdodge(seed = 41, dodge.width = 0.9),
) +
  # Mean
  stat_summary(fun=mean, geom="point", shape = 23, #shape=18,
               size=1.5, color="white", fill = "red"
               , position = position_dodge(0.9)
  ) +
  
  # geom_boxplot(position=position_dodge2(preserve = "single"),
  #              aes(group = month)) + # to preserve SantaMaria April missing
  # # geom_point(
  # #   position = position_jitterdodge(jitter.width = .05) # only way of dodging the points and jitter it
  # #   # position = position_dodge2(0.1, preserve = "total")
  # # ) +
  # geom_point(position = position_jitterdodge(jitter.width = .6),
  #            # size = 3, 
  #            aes(group = month)
  #            ) +
# ylab("SDD (in meters)") +
# ylim(0, 800) +
facet_wrap(~disp_day, nrow = 2) +
  # facet_wrap(vars(disp_day, source), nrow = 2) +
  
  # others
  theme(axis.text = element_text(size = 9)) +
  # geom_point(position = position_jitterdodge(jitter.width = 0.7)) 
  
  # facet_grid(cols = vars(disp_day), rows = vars(source)) +
  facet_grid(cols = vars(source), rows = vars(disp_day)) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  ylab("SDD (m)") +
  ggtitle("Seed dispersal distance by fragment and month")

# # Save plot
ggsave(paste0(path, "/", '02_simple_SDD_disp_day_grid-violin.png'), height = 5, width = 7)



#### Example run density By group and month ####
ex_seed <- db_sd_raw$seed %>%
  na.exclude() %>%
  sample(size = 1)

# Relevel fragment order
db_sd_raw$fragment %>% summary()
db_sd_raw <- db_sd_raw %>%
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
a <- db_sd_raw %>% dplyr::filter(fragment == "check")
a

# Reduce seed dispersal dataset to plot good ggridges and boxplots:
db_sd_example <- db_sd_raw %>% 
  # group_by(group, month, feedingbout_on., seed) %>% 
  group_by(group, month, feedingbout_on.) %>% 
  dplyr::filter(seed == ex_seed | source == "observed") %>% 
  # make SDD = SDD_seeds for observed values
  mutate(
    SDD_seeds = case_when(
      SDD_seeds == is.na(SDD_seeds) ~ SDD,
      TRUE ~ SDD_seeds
    )
  )

# Relevel fragment order
db_sd_example <- db_sd_example %>%
  mutate(
    fragment = forcats::fct_relevel(fragment, "Riparian", "Small", "Medium", "Continuous")
  ) %>% 
  mutate(
    source = forcats::fct_relevel(source, "observed")
  )


# check:
db_sd_example$seed %>% unique()
db_sd_example$source %>% unique()
db_sd_example$SDD_seeds %>% unique()
db_sd_example$fragment %>% unique()
db_sd_example$fragment


db_sd_example %>% colnames()


# density
theme_update(
  axis.text.x = element_text(size = 11)
)
db_sd_example %>% 
  # Santa Maria April is missing from observed data (not enough observations), so we drop the simulations
  dplyr::filter(
    group != "SantaMaria" | month != "Apr"
  ) %>%
  dplyr::filter(disp_day == "same day") %>% 
  ggplot(
    # aes(x = SDD, y = group, fill = month) #, height = ..density..)
    aes(x = SDD_seeds, y = fragment, fill = month) #, height = ..density..)
  ) +
  geom_density_ridges( 
    # bandwidth = 5, # specify global bandwidth. Problem: we have more simulation data
    alpha = 0.8,
    # adjust = 5,
    # position = "stack"
    # stat = "identity",
    # trim = TRUE,
    bandwidth = 20,
    scale = 1.2, # heigth
    jittered_points = TRUE,
    point_shape = "|", point_size = 2,
    position = position_points_jitter(width = 0.1, height = 0)
  ) +
  xlab("SDD (m)") +
  # xlim(0, 800) +
  # facet_grid(source ~ disp_day) +
  facet_grid(rows = vars(source)) +
  scale_fill_viridis_d() +
  ggtitle(paste0("Ex. run: Distance of seeds dispersed on the same day", "\nrseed: ", ex_seed)) +
  theme(plot.title = element_text(size = 14)) #+
# theme_ridges()

# # Save plot
ggsave(paste0(path, "/", '02_simple_SDD_same-day_density_ridges_example.png'), height = 5, width = 7)


db_sd_example %>% 
  # Santa Maria April is missing from observed data (not enough observations), so we drop the simulations
  dplyr::filter(
    group != "SantaMaria" | month != "Apr"
  ) %>%
  dplyr::filter(disp_day == "next day") %>% 
  ggplot(
    # aes(x = SDD, y = group, fill = month) #, height = ..density..)
    aes(x = SDD_seeds, y = fragment, fill = month) #, height = ..density..)
  ) +
  geom_density_ridges( 
    alpha = 0.8,
    # adjust = 5,
    # position = "stack"
    # stat = "identity",
    # trim = TRUE,
    bandwidth = 20,
    scale = 1.2, # heigth
    jittered_points = TRUE,
    point_shape = "|", point_size = 2,
    position = position_points_jitter(width = 0.1, height = 0)
  ) +
  xlab("SDD (m)") +
  # xlim(0, 800) +
  # facet_grid(source ~ disp_day) +
  facet_grid(rows = vars(source)) +
  scale_fill_viridis_d() +
  ggtitle(label = paste0("Ex. run: SDD of all events (same + next day). ", "rseed: ", ex_seed)) +
  theme(plot.title = element_text(size = 14))

# Save plot
ggsave(paste0(path, "/", '02_simple_SDD_next-day_density_ridges_example.png'), height = 5, width = 7)


db_sd_example %>% 
  # Santa Maria April is missing from observed data (not enough observations), so we drop the simulations
  dplyr::filter(
    group != "SantaMaria" | month != "Apr"
  ) %>%
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  ggplot(
    aes(x = SDD_seeds, y = month, fill = month) #, height = ..density..)
  ) +
  geom_density_ridges( 
    alpha = 0.8,
    # adjust = 5,
    # position = "stack"
    # stat = "identity",
    # trim = TRUE,
    scale = 1.5, # heigth
    jittered_points = TRUE,
    point_shape = "|", point_size = 2,
    position = position_points_jitter(width = 0.5, height = 0)
  ) +
  xlab("SDD (m)") +
  ggtitle(label = paste0("Ex. run: SDD of same and next day events. ", "rseed: ", ex_seed)) +
  # facet_grid(source ~ disp_day) +
  facet_grid(source ~ disp_day) +
  scale_fill_viridis_d()

# # Save plot
ggsave(paste0(path, "/", '02_simple_SDD_disp_day_density_ridges1_example.png'), height = 5, width = 10)


# ggrides with boxplot
db_sd_example %>% 
  # Santa Maria April is missing from observed data (not enough observations), so we drop the simulations
  dplyr::filter(
    group != "SantaMaria" | month != "Apr"
  ) %>%
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  ggplot(
    aes(x = SDD_seeds, y = month, fill = month) #, height = ..density..)
  ) +
  geom_density_ridges( #alpha = 0.4,
    # adjust = 5,
    # position = "stack"
    # stat = "identity",
    # trim = TRUE,
    bandwidth = 20,
    scale = 0.9, # heigth
    jittered_points = TRUE,
    point_shape = "|", point_size = 2,
    position = position_points_jitter(width = 0.5, height = 0)
  ) +
  geom_boxplot(
    width = .20, position = position_nudge(y = -.25) #, outlier.shape = NA
  ) +
  xlab("SDD (m)") +
  ggtitle(paste0("Ex. run: SDD of same and next day events. ", "rseed: ", ex_seed)) +
  # facet_grid(source ~ disp_day) +
  facet_grid(source ~ disp_day) +
  scale_fill_viridis_d()

# # Save plot
ggsave(paste0(path, "/", '02_simple_SDD_disp_day_density_ridges1_withboxplot_example.png'), height = 5, width = 10)




db_sd_example %>% 
  # Santa Maria April is missing from observed data (not enough observations), so we drop the simulations
  dplyr::filter(
    group != "SantaMaria" | month != "Apr"
  ) %>%
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  ggplot(
    aes(x = SDD_seeds, y = month, fill = month) #, height = ..density..)
  ) +
  geom_density_ridges( #alpha = 0.4,
    # adjust = 5,
    # position = "stack"
    # stat = "identity",
    # trim = TRUE,
    scale = 1.2, # heigth
    jittered_points = TRUE,
    point_shape = "|", point_size = 2,
    position = position_points_jitter(width = 0.5, height = 0)
  ) +
  xlab("SDD (m)") +
  xlim(0, 800) +
  # facet_grid(source ~ disp_day) +
  facet_grid(~source) +
  scale_fill_viridis_d() +
  ggtitle(paste0("Example run: SDD of all events. ","rseed: ", ex_seed)) +
  theme(plot.title = element_text(size = 16))

# # Save plot
ggsave(paste0(path, "/", '02_simple_SDD_disp_day_density_ridges2_example.png'), height = 5, width = 7)



## ggrides with boxplot
db_sd_example %>% 
  # Santa Maria April is missing from observed data (not enough observations), so we drop the simulations
  dplyr::filter(
    group != "SantaMaria" | month != "Apr"
  ) %>%
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  ggplot(
    aes(x = SDD_seeds, y = month, fill = month) #, height = ..density..)
  ) +
  geom_density_ridges( #alpha = 0.4,
    # adjust = 5,
    # position = "stack"
    # stat = "identity",
    # trim = TRUE,
    bandwidth = 20,
    scale = 0.9, # heigth
    jittered_points = TRUE,
    point_shape = "|", point_size = 2,
    position = position_points_jitter(width = 0.5, height = 0)
  ) +
  geom_boxplot(
    width = .15, position = position_nudge(y = -.15) #, outlier.shape = NA
  ) +
  xlab("SDD (m)") +
  # facet_grid(source ~ disp_day) +
  facet_grid(~source) +
  scale_fill_viridis_d() +
  ggtitle(paste0("Ex. run: SDD all events (same and next day). ", "rseed: ", ex_seed)) +
  theme(plot.title = element_text(size = 16))

# # Save plot
ggsave(paste0(path, "/", '02_simple_SDD_disp_day_density_ridges2_example.png'), height = 5, width = 7)



# boxplot
theme_update(
  axis.text.x = element_text(size = 8)
)
db_sd_example %>% 
  # Santa Maria April is missing from observed data (not enough observations), so we drop the simulations
  dplyr::filter(
    group != "SantaMaria" | month != "Apr"
  ) %>%
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  ggplot(
    # aes(x = group, y = SDD_seeds, color = month)
    aes(x = fragment, y = SDD_seeds, color = month)
  ) +
  geom_boxplot() +
  geom_point(
    size = 0.5,
    position = position_jitterdodge(jitter.width = .65) # only way of dodging the points and jitter it
  ) +
  
  # geom_boxplot(position=position_dodge2(preserve = "single"),
  #              aes(group = month)) + # to preserve SantaMaria April missing
  # # geom_point(
  # #   position = position_jitterdodge(jitter.width = .05) # only way of dodging the points and jitter it
  # #   # position = position_dodge2(0.1, preserve = "total")
  # # ) +
  # geom_point(position = position_jitterdodge(jitter.width = .6),
  #            # size = 3, 
  #            aes(group = month)
  #            ) +
# ylab("SDD (in meters)") +
# ylim(0, 800) +
facet_wrap(~disp_day, nrow = 2) +
  # scale_color_viridis_d() +
  # facet_wrap(vars(disp_day, source), nrow = 2) +
  
  # others
  theme(axis.text = element_text(size = 9)) +
  # geom_point(position = position_jitterdodge(jitter.width = 0.7)) 
  
  
  # Save plot
  # ggsave(paste0(path, "/", '02_simple_SDD_disp_day_boxplot.png'), height = 5, width = 7)
  
  
  # facet_grid(cols = vars(disp_day), rows = vars(source)) +
  facet_grid(cols = vars(source), rows = vars(disp_day)) +
  scale_color_viridis_d() +
  ylab("SDD (m)") +
  ggtitle(paste0("rseed = ", ex_seed))

# # Save plot
ggsave(paste0(path, "/", '02_simple_SDD_disp_day_grid-boxplot_example.png'), height = 5, width = 7)






































## Movement patterns -------------------------
theme_update(
  axis.title.x = element_blank()
)

# define boxplot width
# bpw <- 6/length(unique(paste(db1$group, db1$month)))
# bpw # 0.44444


# db1_mv <- readRDS(paste0(path, "/", "02_Simoutput-simple_monkeys_long.rds")) %>% 
db_mv <- read.csv(paste0(path, "/", "02_Simoutput-simple_monkeys.csv")) %>% 
  # mutate(group = recode(group, "Guarei" = "Guareí")) #%>%  # to match all other datasets
  ungroup() %>% 
  as.data.frame()

db_mv %>% str()

# Remove runs that presented issues: 
db_mv <- db_mv %>% dplyr::filter(DPL > 100) # most of them Guareí May and Suzano Sep 

# db1_mv  <-  db1_mv %>%
#   dplyr::select(-c(
#     # "DPL",
#     "SDD"))


# Wrangle data:

db_mv <- db_mv %>% 
  # dplyr::filter(breed == "monkeys") %>% 
  # rename_with(~ str_remove(.x, "g_"), starts_with("g_")
  # ) %>%
  rename(
    KDE95 = KDE_95,
    KDE50 = KDE_50,
    # DPL = g_DPL,
    # p_feeding = g_p_feeding,
    # p_feeding = g_p_feeding,
    # 
    # 
  ) #%>% 
# # only if HR is being output as m²
# mutate(
#   KDE95 = KDE95 / 10000,
#   KDE50 = KDE50 / 10000
# )


# load DPL empirical data
obs.dpl <- read.csv(here("Data", "Movement", "Curated", "Validation", 
                         "Siminputrow_DPL_by-day.csv")
                    , stringsAsFactors = TRUE
)  %>% 
  # mutate(group = recode(group, "Guarei" = "Guareí")) %>% # to match all other datasets
  mutate(source = as.factor("observed")) %>% 
  rename(month = id_month) #%>% 
# as_tibble()

# load HR empirical data
obs.hr <- read.csv(here("Data", "Movement", "Curated", "Validation", 
                        "Siminputrow_Home-range_by-month.csv")
                   , stringsAsFactors = TRUE
)  %>% 
  # mutate(group = recode(group, "Guarei" = "Guareí")) %>% # to match all other datasets
  mutate(source = as.factor("observed")) %>% 
  rename(month = id_month)

obs.hr <- obs.hr %>% 
  dplyr::select(group, month, source, KDE_value, area) %>% 
  tidyr::pivot_wider(
    names_from = KDE_value,
    values_from = area
  ) %>% 
  as.data.frame()


# Load Defendability Index (DI)
obs.DI <- read.csv(here("Data", "Movement", "Covariables_Territoriality_DI.csv"), sep = ";") %>% 
  mutate(
    source = "observed"
  )

# obs <- obs.dpl %>% dplyr::left_join(obs.hr, by = c("group"="group", "month"= "month",
#                                                      "source"="source"
#                                                      # "KDE95" = "KDE95",
#                                                      # "KDE50" = "KDE50"
# ))  %>% 
obs <- obs.dpl %>% dplyr::left_join(obs.hr, by = c("group", "month", "source"))
obs <- obs %>% dplyr::left_join(obs.DI, by = c("group", "month", "source"))

# Merge obserded data into db1_mv
db1_mv <- dplyr::bind_rows(db_mv, obs)

db1_mv$group %>% unique()
db1_mv$source %>% unique()
db1_mv %>% str()

db1_mv <- db1_mv %>% 
  mutate(group = recode(group,
                        "Santa Maria" = "SantaMaria"
  ))

db1_mv$group %>% as.factor() %>% levels()

db1_mv <- db1_mv %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "SantaMaria", "Taquara")) %>% 
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  mutate(source = forcats::fct_relevel(source, "observed", "simulated"))

# Relevel all fragment names to size categories
db1_mv <- db1_mv %>% 
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

#check:
db1_mv$fragment %>% unique()
db1_mv$source %>% unique()

db1_mv %>% summary()
db1_mv %>% group_by(source) %>% summarize(KDE95_mean = mean(KDE95))

# obs.hr$group
# db1_mv$group
# 
# obs.hr %>% str()
# db1_mv %>% str()
# obs.dpl %>% str()
# 
# db1_mv %>% str()

# db1_mv %>% str()
# obs.dpl %>% str()


### DPL -------------------------
theme_update(
  axis.text.x = element_text(size = 8)
)
# Check data:
# d <- db1_mv %>% dplyr::filter(source == "observed")
# d <- a %>%  dplyr::filter(random_seed == "-1996108983") # por algum motivo essa simullação tem 16 dias em vez de 8 

# # Option 1 (can't identify simulated from observed ones)
# db1_mv %>% 
#   ggplot(aes(x = group, y = DPL, group = paste(source, month), color = month)) +
#   # ggplot(aes(x = group, y = DPL, fill = source, color = month)) +
#   geom_boxplot() +
#   guides(scale="none") +
#   ylab("DPL (m)") +
#   ylim(c(0,5000)) +
#   scale_colour_viridis_d() +
#   scale_fill_discrete(guide = "none")


# # Option2  (oly way of identifyin simulated from observed ones)
db1_mv %>% 
  # ggplot(aes(x = group, y = DPL, 
  ggplot(aes(x = fragment, y = DPL, 
             color = month,
             # fill = month
  ), shape = source) +
  # geom_boxplot(position = position_dodge(preserve = "single")) +
  geom_boxplot() +
  guides(scale="none") +
  ylab("DPL (m)") +
  ylim(c(0,4000)) +
  ggtitle("Daily Path Length (m)") +
  scale_colour_viridis_d() +
  # scale_fill_viridis_d() +
  facet_grid(~source) +
  # others
  theme(axis.text = element_text(size = 9)) #+
# geom_point(position = position_jitterdodge(jitter.width = 0.7))
# db1_mv %>%
#   ggplot(aes(x = group, y = DPL, color = month)) +
#   geom_boxplot() +
#   guides(scale="none") +
#   ylim(c(0,5000)) +
#   scale_colour_viridis_d()

# # Save plot
ggsave(paste0(path,  "/", '02_simple_DPL_By-month_boxplot.png'), height = 3.5, width = 7)









### Home range -------------------------

# KDE 95 log
db1_mv %>% 
  # ggplot(aes(x = group, y = log10(KDE95), color = month)) +
  ggplot(aes(x = fragment, y = log10(KDE95), color = month)) +
  # geom_boxplot(position = position_dodge(preserve = "single")) +
  geom_boxplot() +
  guides(fill=FALSE) +
  ylim(0, 2.5) +
  ylab("log 10(Area in ha)") +
  ggtitle("KDE 95% (monthly area used)") +
  scale_colour_viridis_d() +
  facet_grid(~source) +
  
  # others
  theme(axis.text = element_text(size = 9))
# geom_point(position = position_jitterdodge(jitter.width = 0.7)) 

# geom_jitter(width = 0.15) +
# annotate("text", x=2, y=5000, label= paste0("Mean ± sd = ", round(avg_dist, 2), " ± ", round(sd_dist, 2)))

# Save plot
ggsave(paste0(path,  "/", '02_simple_HR_KDE95_boxplot_log10.png'), height = 3.5, width = 7)

db1_mv %>% 
  # ggplot(aes(x = group, y = KDE95, color = month)) +
  ggplot(aes(x = fragment, y = KDE95, color = month)) +
  # geom_boxplot(position = position_dodge(preserve = "single")) +
  geom_boxplot() +
  guides(fill=FALSE) +
  # ylim(0, 350) +
  ylab("Area (in ha)") +
  ggtitle("KDE 95% (monthly area used)") +
  scale_colour_viridis_d() +
  facet_grid(~source) +
  
  # others
  theme(axis.text = element_text(size = 9))
# geom_point(position = position_jitterdodge(jitter.width = 0.7)) 

# geom_jitter(width = 0.15) +
# annotate("text", x=2, y=5000, label= paste0("Mean ± sd = ", round(avg_dist, 2), " ± ", round(sd_dist, 2)))

# Save plot
ggsave(paste0(path,  "/", '02_simple_HR_KDE95_boxplot.png'), height = 3.5, width = 7)

# ggsave(filename = here("Model_analysis", "Workflow", "Run_travelspeedvar",
#                        "HomeRangeKDE95-speedval.png"),
#        dpi = 300, width = 30, height = 20, units = "cm")

# KDE 50 log
db1_mv %>% 
  # ggplot(aes(x = group, y = log10(KDE50), color = month)) +
  ggplot(aes(x = fragment, y = log10(KDE50), color = month)) +
  # geom_boxplot(position = position_dodge(preserve = "single")) +
  geom_boxplot() +
  ylim(0, 2.5) +
  guides(fill=FALSE) +
  ylab("log10(Area in ha)") +
  ggtitle("KDE 50% (Core area)") +
  scale_colour_viridis_d() +
  facet_grid(~source) +
  
  # others
  theme(axis.text = element_text(size = 9))# +
# geom_point(position = position_jitterdodge(jitter.width = 0.7)) 

# geom_jitter(width = 0.15)
# annotate("text", x=2, y=5000, label= paste0("Mean ± sd = ", round(avg_dist, 2), " ± ", round(sd_dist, 2)))

# Save plot
ggsave(paste0(path,  "/", '02_simple_HR_KDE50_boxplot_log.png'), height = 3.5, width = 7)


# KDE 50
db1_mv %>% 
  # ggplot(aes(x = group, y = KDE50, color = month)) +
  ggplot(aes(x = fragment, y = KDE50, color = month)) +
  # geom_boxplot(position = position_dodge(preserve = "single")) +
  geom_boxplot() +
  guides(fill=FALSE) +
  # ylim(0, 350) +
  ylim(0, 100) +
  ylab("Area (ha)") +
  ggtitle("KDE 50% (Core area)") +
  scale_colour_viridis_d() +
  facet_grid(~source) +
  
  # others
  theme(axis.text = element_text(size = 9))# +
# geom_point(position = position_jitterdodge(jitter.width = 0.7)) 

# geom_jitter(width = 0.15)
# annotate("text", x=2, y=5000, label= paste0("Mean ± sd = ", round(avg_dist, 2), " ± ", round(sd_dist, 2)))

# Save plot
ggsave(paste0(path,  "/", '02_simple_HR_KDE50_boxplot.png'), height = 3.5, width = 7)



#### secondary axis log ---------------------

# KDE 95
db1_mv %>% 
  # ggplot(aes(x = group, y = KDE50, color = month)) +
  ggplot(aes(x = fragment, y = log10(KDE95), color = month)) +
  # geom_boxplot(position = position_dodge(preserve = "single")) +
  geom_boxplot() +
  guides(fill=FALSE) +
  ylim(0, 350) +
  # ylim(0, 100) +
  # ylab("Area (ha)") +
  ggtitle("KDE 95% (monthly area used)") +
  scale_colour_viridis_d() +
  facet_grid(~source) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "log10(Area in ha)",
    # trans = scales::log10_trans(),
    # labels = scales::trans_format("log10", scales::math_format(10^.x)),
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ 10^(.), name="Area (ha)",
                        breaks = c(0, 2.5, 5, 10, 25, 50, 100, 200, 300),
                        # breaks = scales::pretty_breaks(n = 10)
    )
  ) + 
  
  # others
  theme(axis.text = element_text(size = 9))# +
# geom_point(position = position_jitterdodge(jitter.width = 0.7)) 

# geom_jitter(width = 0.15)
# annotate("text", x=2, y=5000, label= paste0("Mean ± sd = ", round(avg_dist, 2), " ± ", round(sd_dist, 2)))

# Save plot
ggsave(paste0(path,  "/", '02_simple_HR_KDE95_boxplot_secaxis.png'), height = 3.5, width = 7)



# KDE 50
db1_mv %>% 
  # ggplot(aes(x = group, y = KDE50, color = month)) +
  ggplot(aes(x = fragment, y = log10(KDE50), color = month)) +
  # geom_boxplot(position = position_dodge(preserve = "single")) +
  geom_boxplot() +
  guides(fill=FALSE) +
  # ylim(0, 350) +
  ylim(0, 100) +
  # ylab("Area (ha)") +
  ggtitle("KDE 50% (Core area)") +
  scale_colour_viridis_d() +
  facet_grid(~source) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "log10(Area in ha)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ 10^(.), name="Area (ha)",
                        breaks = c(0, 2.5, 5, 10, 15, 20, 30, 50, 100)
                        # breaks = scales::pretty_breaks()
    )
  ) + 
  
  # others
  theme(axis.text = element_text(size = 9))# +
# geom_point(position = position_jitterdodge(jitter.width = 0.7)) 

# geom_jitter(width = 0.15)
# annotate("text", x=2, y=5000, label= paste0("Mean ± sd = ", round(avg_dist, 2), " ± ", round(sd_dist, 2)))

# Save plot
ggsave(paste0(path,  "/", '02_simple_HR_KDE50_boxplot_secaxis.png'), height = 3.5, width = 7)







### Activity budget -------------------------

# load activity budget empirical data
obs.ab <- read.csv(here("Data", "Movement", "Curated", "Validation", 
                        "Siminputrow_Activity-budget_By-month.csv")
                   , stringsAsFactors = TRUE
)  %>% 
  # mutate(group = recode(group, "Guarei" = "Guareí")) %>% # to match all other datasets
  mutate(source = as.factor("observed")) %>% 
  rename(month = id_month) %>%
  as.data.frame() %>%
  mutate(group = recode(group,
                        "Santa Maria" = "SantaMaria"
  ))
# as_tibble()

obs.ab$group %>% levels()

obs.ab$perc_behavior_mean %>% str()
obs.ab %>% str()

foo <- function(x) ( x = x / 100 )
obs.ab <- obs.ab %>% 
  dplyr::select(-perc_behavior_sd) %>%  # we are interested in mean values (perc_behavior_mean)
  tidyr::pivot_wider(names_from = behavior,
                     values_from = perc_behavior_mean,
                     values_fill = 0 # only for Guareí in the month they didn't rest (0% resting)
  ) %>% 
  rename(
    p_feeding = Frugivory,
    p_foraging = Foraging,
    p_traveling = Travel,
    p_resting = Resting
  ) %>% 
  mutate_if(is.numeric, foo) %>% 
  as.data.frame()

db1_mv %>% str()
# a %>% str()
# a$p_traveling %>% hist()
# a$p_traveling %>% tail()
obs.ab %>% str()

# Merge obserded data into db1_mv
db1_mv <- db1_mv %>%  dplyr::left_join(obs.ab, by = c("group", "month", "source")) %>% #,  by = c("group"="group", "month"= "month",
  # "source"="source")) #, # PORQUE DIABOS N FUNCIOMA
  # "p_feeding" = "p_feeding",
  # "p_foraging" = "p_foraging",
  # "p_traveling" = "p_traveling",
  # "p_resting" = "p_resting"
  # ))
  tidyr::unite(p_feeding.x, p_feeding.y, col = "p_feeding", remove = TRUE, na.rm = TRUE) %>% 
  tidyr::unite(p_foraging.x, p_foraging.y, col = "p_foraging", remove = TRUE, na.rm = TRUE) %>% 
  tidyr::unite(p_traveling.x, p_traveling.y, col = "p_traveling", remove = TRUE, na.rm = TRUE) %>% 
  tidyr::unite(p_resting.x, p_resting.y, col = "p_resting", remove = TRUE, na.rm = TRUE) #%>%

# a$p_feeding %>% tail()
# a$p_foraging %>% tail()
# a$p_feeding %>% as.numeric() %>%  tail()


db1_mv <- db1_mv %>%  
  mutate(
    p_feeding = as.numeric(p_feeding),
    p_foraging = as.numeric(p_foraging),
    p_traveling = as.numeric(p_traveling),
    p_resting = as.numeric(p_resting),
  )
# mutate(across(starts_with("p_"), as.numeric(na.rm = TRUE)))

db1_mv %>% select(starts_with("p_")) %>% tail()


db1_mv <- db1_mv %>% 
  # mutate(group = recode(group,
  #                       "Santa Maria" = "SantaMaria"
  # )) %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "SantaMaria", "Taquara")) %>% 
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  mutate(source = forcats::fct_relevel(source, "observed", "simulated"))

db1_mv$group %>% levels()

# Relevel all fragment names to size categories
db1_mv <- db1_mv %>% 
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

#check:
db1_mv$fragment %>% unique()
db1_mv$source %>% unique()

db1_mv %>% summary()
# db1_mv %>% group_by(source) %>% summarize(KDE95_mean = mean(KDE95))


# Take other variables to another dataset
# db1_mv_others <- db1_mv %>% 
#   dplyr::select(c(p_visited_trees, energy_stored))


# Make it longer
db1_mv_longer <- db1_mv %>% 
  # dplyr::select(-c(p_disputed_trees, p_timesteps_to_rest)) %>%  # these ones also starts with "p_"
  tidyr::pivot_longer(cols = starts_with("p_"), names_to = "behavior", values_to = "percentage_mean") %>% 
  dplyr::filter(behavior != "p_visited_trees" & behavior!= "p_forage_param."
  ) 

# Remove other columns starting with "p_":
targetb <- c("p_disputed_trees", "p_timesteps_to_rest")
db1_mv_longer <- db1_mv_longer %>% 
  dplyr::filter(!(behavior %in% targetb))


db1_mv_longer <- db1_mv_longer %>% 
  mutate(percentage_mean = stringr::str_replace_all(percentage_mean, c("\\[" = "", "\\]" = "")),
         percentage_mean = as.numeric(percentage_mean))

db1_mv_longer %>% str()

db1_mv_longer$percentage_mean <- db1_mv_longer$percentage_mean %>% as.numeric()



# Option 1

db1_mv_longer %>% 
  # ggplot(aes(x = interaction(group, source), y = percentage_mean, fill = behavior)) +
  ggplot(aes(x = interaction(fragment, source), y = percentage_mean, fill = behavior)) +
  # geom_histogram() +
  # geom_col(position = position_dodge()) +
  # geom_bar(position = position_dodge()) +
  # geom_bar(position = "dodge", stat = "identity") +
  # geom_bar(position = "fill", stat = "identity") +
  geom_bar(method="mean", stat = "identity", position=position_dodge()) +
  
  ggtitle("Activity budget of 4 behaviors") +
  # ylab("Proportion (%)") +
  xlab("")  +
  theme(
    # legend.position = "none",
    axis.text = element_text(size = 9),
    axis.title.x = element_blank()
    # axis.text.y = element_blank()
  ) +
  
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_viridis_d() #+
# scale_y_continuous(labels = scales::percent)



# Option 2 (stat summary)
# ggplot(data=db1_mv_longer,aes(x=interaction(group, month),y=percentage_mean,fill=behavior)) +
# ggplot(data=db1_mv_longer,aes(x=group ,y=percentage_mean
ggplot(data=db1_mv_longer,aes(x=fragment ,y=percentage_mean
                              ,fill=month
)) +
  stat_summary(geom='bar', fun='mean', position='dodge',
               # fill = "white", color = "red"
  ) +
  stat_summary(geom='errorbar', fun.data='mean_cl_boot', position='dodge') +
  # ylim(0, 1) +
  ylab("Percentage (%)") +
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text = element_text(size = 9),
    axis.title.x = element_blank()#,
    # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1) 
  ) +
  scale_fill_viridis_d() +
  # facet_wrap(~source)
  facet_grid(behavior ~ source)

# Save plot
ggsave(paste0(path,  "/", '02_simple_ActivityBudget_barplot_statsummary.png'), height = 5, width = 7)


# Option 3
db1_mv_sum <- db1_mv_longer %>% 
  # group_by(group, month, source, behavior) %>% 
  group_by(fragment, month, source, behavior) %>% 
  summarise(
    mean = mean(percentage_mean),
    sd = sd(percentage_mean)
  )
# db1_mv_sum %>% str()
# db1_mv_longer %>%
# db1_mv_sum %>%
# ggplot(data = db1_mv_sum, aes(x = factor(group), y = mean, fill = month)) +
ggplot(data = db1_mv_sum, aes(x = factor(fragment), y = mean, fill = month)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  geom_errorbar(aes(ymin=mean, ymax=mean+sd), position = position_dodge(.9)) +
  # stat_summary(geom='errorbar', fun.data='mean_cl_boot', position='dodge') +
  ylab("Percentage (%)") +
  facet_grid(behavior ~ source) +
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text = element_text(size = 9),
    axis.title.x = element_blank()
  ) +
  scale_fill_viridis_d()

# Save plot
ggsave(paste0(path,  "/", '02_simple_ActivityBudget_barplot_errorbar.png'), height = 5, width = 7)



# Option 3
# ggplot(data = db1_mv_longer, aes(x = group, y = percentage_mean, fill = month))  +
ggplot(data = db1_mv_longer, aes(x = fragment, y = percentage_mean, fill = month))  +
  # geom_bar(stat="identity", position="dodge", colour="black") +  # to check the errors
  geom_bar(stat="summary", fun = "median", position="dodge", colour="black") + 
  ylab("Percentage (%)") +
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_blank()
  ) +
  scale_fill_viridis_d() +
  facet_grid(behavior ~ source)

# a <- db1_mv_longer %>% 
#   # dplyr::filter(p_resting > 0.1)
#   dplyr::filter(group == "Guareí") %>% 
#   dplyr::filter(behavior == "p_resting") #%>% 
#   dplyr::filter(group == "Guareí") 

# Save plot
ggsave(paste0(path,  "/", '02_simple_ActivityBudget_barplot_option2.png'), height = 5, width = 7)






### Movement rate (MR) and Path twisting (PT) -------------------------
# Load empirical data and calculate MR and PT 
obs.mv.metrics <- read.csv(here("Data", "Movement", "Curated", "Validation", 
                                "Siminputrow_MR-PT_by-day.csv")) %>% 
  dplyr::select(c(1:MR, PT)) %>% 
  rename(month = id_month) %>% 
  mutate(group = recode(group,
                        "Santa Maria" = "SantaMaria"
  ))

obs.mv.metrics %>% str()
obs.mv.metrics$group %>% as.factor() %>% levels()

obs.mv.metrics.summary <- obs.mv.metrics %>% 
  group_by(group, month) %>% 
  summarise(
    MR_mean = mean(MR, na.rm = TRUE),
    MR_sd = sd(MR, na.rm = TRUE),
    PT_mean = mean(PT, na.rm = TRUE),
    PT_sd = sd(PT, na.rm = TRUE)
  ) %>% 
  mutate(source = "observed")

db1_mv_summarry <- db1_mv %>% 
  group_by(source, group, month) %>% 
  dplyr::summarise(
    MR_mean = mean(MR, na.rm = TRUE),
    MR_sd = sd(MR, na.rm = TRUE),
    PT_mean = mean(PT, na.rm = TRUE), #/10000 ,  # to convert to m²
    PT_sd = sd(PT, na.rm = TRUE)  #/10000       # to convert to m²
  ) %>% 
  dplyr::filter(source == "simulated")

db1_mv_summarry <- db1_mv_summarry %>% 
  bind_rows(obs.mv.metrics.summary) 

db1_mv_summarry <- db1_mv_summarry %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "SantaMaria", "Taquara")) %>% 
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  mutate(source = forcats::fct_relevel(source, "observed", "simulated"))

# Relevel all fragment names to size categories
db1_mv_summarry <- db1_mv_summarry %>% 
  mutate(
    fragment = case_when(
      group == "Suzano" ~ "Riparian",
      group == "Guareí" ~ "Small",
      group == "SantaMaria" ~ "Medium",
      group == "Taquara" ~ "Continuous",
      TRUE ~ "check"
    )
  )

db1_mv_summarry <- db1_mv_summarry %>% 
  mutate(
    fragment = forcats::fct_relevel(fragment, "Riparian", "Small", "Medium", "Continuous")
  )

#check:
db1_mv_summarry$fragment %>% unique()
db1_mv_summarry$source %>% unique()

db1_mv_summarry %>% summary()
# db1_mv %>% group_by(source) %>% summarize(KDE95_mean = mean(KDE95))





# Movement rate
db1_mv_summarry %>% ggplot(
  # aes(x = group, y = MR_mean, 
  aes(x = fragment, y = MR_mean, 
      ymin = MR_mean - MR_sd,
      ymax = MR_mean + MR_sd,
      color = month,
      shape = source
  )
) +
  geom_pointrange(aes(shape = source), position = position_dodge2(width = .5), size = 0.9) +
  scale_color_viridis_d() +
  ylab("DPL / hours active") +
  facet_grid(~source) +
  ggtitle("Movement rate")  +
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text = element_text(size = 9),
    axis.title.x = element_blank()
  ) +
  guides(shape = FALSE) # drop legend of shape only

# # Save plot
ggsave(paste0(path,  "/", '02_simple_MR.png'), height = 3.5, width = 7)



# Path twisting
db1_mv_summarry %>% ggplot(
  # aes(x = group, y = PT_mean, 
  aes(x = fragment, y = PT_mean, 
      ymin = PT_mean - PT_sd,
      ymax = PT_mean + PT_sd,
      color = month,
      shape = source
  )
) +
  geom_pointrange(aes(shape = source), position = position_dodge2(width = .5), size = 0.9) +
  scale_color_viridis_d() +
  # ylab(expression(DPL^{'2-'}/home range)) +
  ylab(bquote(~DPL^2~'/home range size')) +
  ggtitle("Path twisting")  +
  # facet_grid(~factor(source, levels = c("simulated", "observed"))) +
  facet_grid(~source) +
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text = element_text(size = 9),
  )+
  guides(shape = FALSE) # drop legend of shape only

# Save plot
ggsave(paste0(path,  "/", '02_simple_PT.png'), height = 3.5, width = 7)





### R index (defecations) -------------------------
R.obs <- read.csv(here("Data", "Seed_dispersal", "Curated", "Validation", 
                       "Summary_by-month_R-aggregation.csv")) %>% 
  mutate(
    source = "observed"
  )

R.sim <- db_sd %>% 
  dplyr::select(group, month, random_seed, R_seeds, R_seeds_p) %>% 
  rename(R = R_seeds,
         p = R_seeds_p) %>% 
  mutate(
    source = "simulated"
  ) #%>% 
# group_by(group, month) %>% 
# summarise(
#   R = mean(R, na.rm = TRUE),
#   p = mean(p, na.rm = TRUE),
#   R_sd = mean(R, na.rm = TRUE),
#   p_sd = mean(p, na.rm = TRUE)
# )

R.all <- bind_rows(R.obs, R.sim) %>% 
  distinct() %>% 
  mutate(
    point_pattern = case_when(
      R > 1 & p <= 0.05 ~ "ordered",
      R < 1 & p <= 0.05 ~ "clumped",
      TRUE ~ "random"
    )
  ) %>% 
  mutate(group = recode(group,
                        "Santa Maria" = "SantaMaria"
  )) %>%
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "SantaMaria", "Taquara")) %>% 
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  mutate(source = forcats::fct_relevel(source, "observed", "simulated")) 

# Relevel all fragment names to size categories
R.all <- R.all %>% 
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

#check:
R.all$fragment %>% unique()
R.all$source %>% unique()

R.all %>% summary()
# db1_mv %>% group_by(source) %>% summarize(KDE95_mean = mean(KDE95))


# Check why some R values are 0 (filtering them out for now)
R.all <- R.all %>%
  dplyr::filter(R > 0.1)

R.all %>% 
  # Santa Maria April is missing from observed data (not enough observations), so we drop the simulations
  dplyr::filter(
    group != "SantaMaria" | month != "Apr"
  ) %>%
  ggplot(
    # aes(x = group, y = R, 
    aes(x = fragment, y = R, 
        color = month,
        shape = point_pattern
    )
  ) +
  # geom_boxplot() +
  geom_point(
    aes(size = 1.5),
    alpha = 0.8,
    # position = position_jitterdodge(jitter.width = 0.25)
    position = position_dodge2(width = .9)
  ) +
  scale_color_viridis_d() +
  scale_shape_manual(values=c(17,16)) +
  # scale_shape_manual(values=c(17, 15, 16)) +
  # scale_shape_manual(values=c(1, 0, 2)) +
  # ylab(expression(DPL^{'2-'}/home range)) +
  ylim(0, 1.5) +
  ylab("R index") +
  ggtitle("Seed aggregation")  +
  # facet_grid(~factor(source, levels = c("simulated", "observed"))) +
  facet_grid(~source) +
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text = element_text(size = 9),
    axis.title.x = element_blank()
  ) +
  guides(size = FALSE) # drop legend of size only

# Save plot
ggsave(paste0(path,  "/", '02_simple_R-aggregation.png'), height = 5, width = 8)




## DI and M index (defendability) -------------------------

db1_mv_summaryDI <- db1_mv %>% 
  group_by(group, month, source) %>% 
  dplyr::summarise(
    M_mean = mean(M_index, na.rm = TRUE),
    M_sd = sd(M_index, na.rm = TRUE),
    M_mean_log = mean(log10(M_index), na.rm = TRUE),
    M_sd_log = sd(log10(M_index), na.rm = TRUE),
    DI_mean = mean(DI_index, na.rm = TRUE),
    DI_sd = sd(DI_index, na.rm = TRUE)
  ) #%>% 
# dplyr::filter(source == "simulated")

# db1_mv_summaryDI <- db1_mv_summaryDI %>% 
#   bind_rows(obs.mv.metrics.summary) 

db1_mv_summaryDI <- db1_mv_summaryDI %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "SantaMaria", "Taquara")) %>% 
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  mutate(source = forcats::fct_relevel(source, "observed", "simulated"))

# Relevel all fragment names to size categories
db1_mv_summaryDI <- db1_mv_summaryDI %>% 
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

#check:
db1_mv_summaryDI$fragment %>% unique()
db1_mv_summaryDI$source %>% unique()

db1_mv_summaryDI %>% summary()
# db1_mv %>% group_by(source) %>% summarize(KDE95_mean = mean(KDE95))


# Define territorial based on both indices:
db1_mv_summaryDI <- db1_mv_summaryDI %>% 
  mutate(
    territorial = case_when(
      DI_mean > 0.98 & M_mean >= 0.08 ~ "territorial",
      TRUE ~ "non-territorial"
    )
  )


## DI index  -------------------------------

db1_mv_summaryDI %>% ggplot(
  # aes(x = group, y = MR_mean, 
  aes(x = fragment, y = DI_mean, 
      ymin = DI_mean - DI_sd,
      ymax = DI_mean + DI_sd,
      color = month,
      shape = source
  )
) +
  geom_pointrange(aes(shape = territorial), position = position_dodge2(width = .5), size = 0.9) +
  scale_color_viridis_d() +
  ylab("DI value") +
  facet_grid(~source) +
  ggtitle("Defendability Index (Mitani & Rodman 1979)")  +
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text = element_text(size = 9),
    axis.title.x = element_blank(),
    title = element_text(size = 13)
  ) +
  scale_shape_manual(values = c(1, 4)) +
  geom_hline(yintercept=0.98, linetype="dashed", color = "red", linewidth = 1.5) +
  # theme(legend.position="bottom") + # drop legend
  guides(shape = guide_legend(title = "territoriality", order = 1)) # = NULL to drop the title of territoriality (it is wrong)
# guides(shape = FALSE) # drop legend of shape only

# # Save plot
ggsave(paste0(path,  "/", '02_simple_DI_index.png'), height = 4, width = 7)



## M index  -------------------------------

db1_mv_summaryDI %>% ggplot(
  # aes(x = group, y = MR_mean, 
  aes(x = fragment, y = M_mean, 
      ymin = M_mean - M_sd,
      ymax = M_mean + M_sd,
      color = month,
      shape = source
  )
) +
  geom_pointrange(aes(shape = territorial), position = position_dodge2(width = .5), size = 0.9) +
  scale_color_viridis_d() +
  ylab("M value") +
  facet_grid(~source) +
  ggtitle("M index (Lowen & Dunbar 1994)")  +
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text = element_text(size = 9),
    axis.title.x = element_blank(),
    title = element_text(size = 13)
  ) +
  geom_hline(yintercept=0.08, linetype="dashed", color = "red", linewidth = 1.5) +
  scale_shape_manual(values = c(1, 4)) +
  guides(shape = guide_legend(title = "territoriality", order = 1)) # = NULL to drop the title of territoriality (it is wrong)
# guides(shape = FALSE) # drop legend of shape only

# # Save plot
ggsave(paste0(path,  "/", '02_simple_M_index.png'), height = 4, width = 7)


### M index log10 ------- 

db1_mv_summaryDI %>% ggplot(
  # aes(x = group, y = MR_mean, 
  aes(x = fragment, y = M_mean_log, 
      ymin = M_mean_log - M_sd_log,
      ymax = M_mean_log + M_sd_log,
      color = month,
      shape = source
  )
) +
  geom_pointrange(aes(shape = territorial), position = position_dodge2(width = .5), size = 0.9) +
  scale_color_viridis_d() +
  # ylab("log10 M value") +
  facet_grid(~source) +
  ggtitle("M index (Lowen & Dunbar 1994)")  +
  scale_y_continuous(
    
    # Features of the first axis
    name = "log10 M value",
    # trans = scales::log10_trans(),
    # labels = scales::trans_format("log10", scales::math_format(10^.x)),
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ 10^(.), name="M value",
                        breaks = c(0, 0.08, 0.25, 0.5, 1, 2, 5, 15, 30, 80)
                        # breaks = scales::log_breaks(10),
                        # labels = scales::label_number(scale_cut = scales::cut_short_scale())
                        # labels = scales::label_number(accuracy = 1.00)
                        # breaks = scales::pretty_breaks(n = 10)
    )
  ) +
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text = element_text(size = 9),
    axis.title.x = element_blank(),
    title = element_text(size = 13)
  ) +
  geom_hline(yintercept=log10(0.08), linetype="dashed", color = "red", linewidth = 1.5) +
  scale_shape_manual(values = c(1, 4)) +
  guides(shape = guide_legend(title = "territoriality", order = 1)) #+ # = NULL to drop the title of territoriality (it is wrong)
# theme(legend.position="none") # drop legend
# guides(shape = FALSE) # drop legend of shape only


# # Save plot
ggsave(paste0(path,  "/", '02_simple_M_index_secaxis.png'), height = 4, width = 7.5)





# Resource visitation -----

theme_update(
  axis.title.x = element_blank()
)

# Not ideal, but grab number of fruiting trees from param.table
visits <- read.csv(here("Data", "Parameter_table.csv"),
                   sep = ",", dec = ".", stringsAsFactors = TRUE,
                   encoding = "UTF-8") %>% 
  dplyr::mutate(group = recode(group, "Guarei" = "Guareí",
                               "Santa Maria" = "SantaMaria")) %>%  # only to match those of the NetLogo model
  rename(
    n_trees = n_trees_Frugivory
  ) %>% 
  mutate(
    p_visited_trees = 1,
    source = "observed",
    fragment = case_when(
      group == "Suzano" ~ "Riparian",
      group == "Guareí" ~ "Small",
      group == "SantaMaria" ~ "Medium",
      group == "Taquara" ~ "Continuous"
    )
  ) %>% 
  dplyr::select(group, month, source, fragment, n_trees, p_visited_trees)


db1_mv_vi <- db1_mv %>% 
  dplyr::filter(source == "simulated")

db1_mv_vi <- dplyr::full_join(db1_mv_vi, visits) #, by = join_by(group, month))

# db1_mv_vi <- db1_mv_vi %>%
#   mutate(
#     p_visited_trees = case_when(
#       p_visited_trees == "NA" ~ 1,
#       TRUE ~ as.numeric(p_visited_trees)
#     )
#   )

db1_mv_vi %>% str()

db1_mv_vi_summary <- db1_mv_vi %>% 
  group_by(group, month, source) %>% 
  dplyr::summarise(
    visit_mean = mean(p_visited_trees, na.rm = TRUE),
    visit_sd = sd(p_visited_trees, na.rm = TRUE)
  ) %>% 
  # dplyr::filter(source == "simulated") %>% 
  mutate(
    visit_mean = case_when(
      visit_mean == "NaN" ~ 1,
      TRUE ~ as.numeric(visit_mean)
    ),
    visit_sd = case_when(
      is.na(visit_sd) ~ 0,
      TRUE ~ as.numeric(visit_sd)
    )
  ) %>% 
  ungroup()

# db1_mv_summaryDI <- db1_mv_summaryDI %>% 
#   bind_rows(obs.mv.metrics.summary) 

db1_mv_vi_summary <- db1_mv_vi_summary %>% 
  mutate(
    # group = forcats::fct_relevel(group, "Suzano", "Guareí", "SantaMaria", "Taquara"),
    month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                 "Jun", "Jul", "Aug", "Sep", "Dec"), 
    source = forcats::fct_relevel(source, "observed", "simulated")
  ) %>% 
  # Relevel all fragment names to size categories
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

#check:
db1_mv_vi_summary$fragment %>% unique()
db1_mv_vi_summary$source %>% unique()

db1_mv_vi_summary %>% summary()



# Plot visits
# db1_mv_vi %>% 
#   ggplot(
#   aes(x = fragment, y = p_visited_trees, 
#       color = month,
#       shape = source,
#       size = source
#   )
db1_mv_vi_summary %>% 
  ggplot() +
  aes(x = fragment, y = visit_mean, 
      ymin = visit_mean - visit_sd,
      ymax = visit_mean + visit_sd,
      color = month
      # shape = source
  ) +
  geom_pointrange(aes(shape = source), position = position_dodge2(width = .5), size = 0.9) +
  scale_color_viridis_d() +
  scale_shape_manual(values = c(16, 17)) +
  ylab("% of visited trees") +
  ggtitle("Proportion of visited fruiting trees")  +
  # facet_grid(~source) +
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text = element_text(size = 9),
  ) #+
# guides(shape = FALSE) # drop legend of shape only

# Save plot
ggsave(paste0(path,  "/", '02_simple_visited-trees.png'), height = 5, width = 5)
