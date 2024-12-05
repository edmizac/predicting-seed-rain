# Script name: 03_Validation-patterns.R
# Script purpose: summarize, plot and analize Seed dispersal empirical data for VALIDATION.

# Date created: 2022-11-16d
# Author: Eduardo Zanette

## Notes --------------------------- 
#
#

## Packages -------------------------
library("here")
library("dplyr")
library("forcats")
library("readxl")
library("lubridate")
library("hms")
library("ggplot2")
library("ggalt")
library("ggridges")

## Options -------------------------
# (plotting, memory limit, decimal digits)
theme_set(theme_bw(base_size = 15))


# Load data
dat.all <- read.csv(here("Data", "Seed_dispersal", "Curated", "All-areas_SeedDispersal.csv"),
                    sep = ",", stringsAsFactors = TRUE)
# dat.all %>% str()
# is.na(dat.all$def_datetime)
# nrow(dat.all[is.na(dat.all$def_datetime), ])


# Wrangling data
dat.all <- dat.all %>%
  
  # make datetime POSIXct
  mutate(
    def_datetime = lubridate::as_datetime(def_datetime), #, tz = "America/Sao_Paulo"),
    feed_datetime = lubridate::as_datetime(feed_datetime) #, tz = "America/Sao_Paulo"),
  ) %>% 
  
  # gather yyyy/mm/dd as id
  mutate(
    day = lubridate::as_date(def_datetime),
  ) %>% 
  
  # Defining each dispersal event as in the same day or next day
  mutate(disp_day = ifelse(lubridate::day(def_datetime) == lubridate::day(feed_datetime), # use case_when for a dplyr-er solution: https://stackoverflow.com/questions/22337394/dplyr-mutate-with-conditional-values
                           "same day",
                           "next day"),
         disp_day = as.factor(disp_day)
  ) %>% 
  
  # Order group levels and drop NA
  dplyr::filter(!is.na(id_feces)) %>% 
  droplevels() %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "Santa Maria", "Taquara")) %>% 
  mutate(id_month = forcats::fct_relevel(id_month, "Jan", "Feb", "Mar", "Apr", "May", 
                                         "Jun", "Jul", "Aug", "Sep", "Nov",  "Dec", "Dec2017", "Dec2018"))

# dat.all$id_month %>% as.factor() %>% levels()
# str(dat.all)
# str(a)


# Check data
dat.all %>% 
  # dplyr::filter(day == "2017-12-03") %>% 
  summarise_all(funs(sum(is.na(.)))) #-> extra_NA

dat.all %>% 
  str()

dat.all[ , c("group", "id_month") ] %>% distinct() # This dataset is not filtered for the siminputrow matrix

# Inport siminputrowmatrix to filter Validation data only for the simulated months:
siminputmatrix <- read.csv(here("Data", "Movement", "Curated", "Param_Simulation-time",  "BLT_groups_data_summary_siminputrow.csv"),
                           sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) # to match all other datasets
siminputmatrix %>% str()

dat.all <- dplyr::left_join(siminputmatrix, dat.all)
# dat.all$id_month  %>% levels()


## Write csv
# dat.all %>%
#   write.csv(here("Data", "Seed_dispersal", "Curated", "Validation", "Siminputrow_SDD.csv"),
#             row.names = FALSE)


# General summary by month -> THESE ARE THE VALUES I WILL USE FORVALIDATION ON CHAPTER 1
round2 <- function(x) (round(x, digits = 2))

dat.all.summary.month <- dat.all %>% 
  group_by(group, id_month, disp_day) %>% 
  summarize(
    # nseeds_mean = mean((n_seeds)),
    # nseeds_sd = sd((n_seeds)),
    mean_SDD = mean(SDD),
    sd_SDD = sd(SDD),
    
    # Agreggation patterns
    
  ) %>% 
  mutate(across(where(is.numeric), round2))

## Write csv
# dat.all.summary.month %>%
#   write.csv(here("Data", "Seed_dispersal", "Curated", "Validation", "Summary_by-month.csv"),
#             row.names = FALSE)





#### SDD #####

## Density Option 1: by day of dispersal
# By group
dat.all %>% ggplot(
  aes(x = SDD, fill = group, group = group)
) +
  geom_density(alpha = 0.4) +
  xlab("SDD (in meters)") +
  facet_wrap(~disp_day, nrow = 2)

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", "Validation", 'SDD_disp_day_By-group_density.png'), height = 7, width = 6)


# By group and month
dat.all %>% ggplot(
  aes(x = SDD, color = id_month)
) +
  geom_density(alpha = 0.4,
               adjust = 5,
               position = "stack"
  ) +
  xlab("SDD (in meters)") +
  facet_wrap(~disp_day, nrow = 2) +
  scale_color_viridis_d()

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", "Validation", 'SDD_disp_day_By-month_density-stack.png'), height = 7, width = 6)


dat.all %>% ggplot(
  aes(x = SDD, y = group, fill = id_month)
) +
  geom_density_ridges( #alpha = 0.4,
               # adjust = 5,
               # position = "stack"
  ) +
  xlab("SDD (in meters)") +
  facet_wrap(~disp_day, nrow = 2) +
  scale_fill_viridis_d()

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", "Validation", 'SDD_disp_day_By-month_density-ridges.png'), height = 7, width = 6)


## Density Option 2: by plant species
# dat.all %>% ggplot(
#   aes(x = SDD, group = species)
# ) +
#   geom_density(fill = "black") +
#   facet_wrap(~species, ncol = 6) +
#   theme_minimal(base_size = 10) +
#   theme(legend.position="none") #+
#   # facet_wrap(~disp_day, nrow = 2)
#   # theme(axis.text.x = element_text(size=10))


## Boxplot
# By group
dat.all %>%
  ggplot() +
  aes(x = group, y = SDD, fill = group) +
  geom_violin() +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~disp_day, nrow = 2) +
  ylab("SDD (m)") +
  ylim(0, 1000)

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", "Validation", 'SDD_disp_day_By-group_violin.png'), height = 5, width = 7)


# By group and month
dat.all %>%
  ggplot() +
  aes(x = group, y = SDD, color = id_month) +
  geom_violin() +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~disp_day, nrow = 2) +
  ylab("SDD (m)") +
  ylim(0, 1000) +
  scale_color_viridis_d()

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", "Validation", 'SDD_disp_day_By-month_violin.png'), height = 5, width = 7)


dat.all %>% ggplot(
  aes(x = group, y = SDD, color = id_month)
) +
  geom_violin() +
  # geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
  geom_point(
    position = position_jitterdodge(jitter.width = .01) # only way of dodging the points and jitter it
  ) +
  ylab("SDD (in meters)") +
  facet_wrap(~disp_day, nrow = 2) +
  scale_color_viridis_d()

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", "Validation", 'SDD_disp_day_By-month_violin.png'), height = 5, width = 7)


dat.all %>% ggplot(
  aes(x = group, y = SDD, color = id_month)
) +
  geom_boxplot() +
  geom_point(
    position = position_jitterdodge(jitter.width = .05) # only way of dodging the points and jitter it
  ) +
  ylab("SDD (in meters)") +
  facet_wrap(~disp_day, nrow = 2) +
  scale_color_viridis_d()

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", "Validation", 'SDD_disp_day_By-month_boxplot.png'), height = 5, width = 7)



# By plant species
# dat.all %>%
#   ggplot() +
#   aes(x = group, y = SDD, fill = group) +
#   geom_boxplot() +
#   theme(axis.title.x = element_blank()) +
#   # theme(aspect.ratio = 0.5) +
#   theme(legend.text=element_text(size=10)) +
#   theme(strip.text = element_text(size = 10),
#         axis.text.x = element_text(size = 8,
#                                    angle = 45),
#         axis.text.y = element_text(size = 8)) +
#   # theme(legend.position="none") +
#   # facet_grid(~species, rows = 6)#, scales = "free", space = "free")
#   facet_wrap(~species, nrow = 6)

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", "Validation", 'Plot_species_SDD_byarea.png'), height = 15, width = 15)



#### R index of seeds #####
library(spatstat)
library(maptools)
library(sf)
library(adehabitatHR)
dat.all <- dat.all %>% 
  rename(month = id_month)

# Get owin as limits of feeding trees (= NetLogo v1.1 model)
trees <- read.csv(here("Data", "Movement", "Curated",  "BLT_groups_data.csv")) %>% 
  # dplyr::filter(behavior == "Frugivory") %>% 
  dplyr::select(-c(datetime, behavior, id_day_all)) %>% 
  distinct()

trees_gua <- trees %>% 
  dplyr::filter(group == "Guareí")
xy_gua <- SpatialPoints(trees_gua[ , 1:2])
proj4string(xy_gua) <- CRS('+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
limitsOwin_gua <- mcp(xy_gua, percent = 100) # define mcp as owin
# plot(limitsOwin_gua)
limitsOwin_gua <- as.owin(limitsOwin_gua)
                                
trees_sma <- trees %>% 
  dplyr::filter(group == "Santa Maria")
xy_sma <- SpatialPoints(trees_sma[ , 1:2])
proj4string(xy_sma) <- CRS('+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
limitsOwin_sma <- mcp(xy_sma, percent = 100) # define mcp as owin
# plot(limitsOwin_sma)
limitsOwin_sma <- as.owin(limitsOwin_sma)

trees_taq <- trees %>% 
  dplyr::filter(group == "Taquara")
xy_taq <- SpatialPoints(trees_taq[ , 1:2])
proj4string(xy_taq) <- CRS('+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
limitsOwin_taq <- mcp(xy_taq, percent = 100) # define mcp as owin
# plot(limitsOwin_taq)
limitsOwin_taq <- as.owin(limitsOwin_taq)

trees_suz <- trees %>% 
  dplyr::filter(group == "Suzano")
xy_suz <- SpatialPoints(trees_suz[ , 1:2])
proj4string(xy_suz) <- CRS('+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
limitsOwin_suz <- mcp(xy_suz, percent = 100) # define mcp as owin
# plot(limitsOwin_suz)
limitsOwin_suz <- as.owin(limitsOwin_suz)


# Guareí
seeds <- dat.all %>%  
  # dplyr::filter(group == "Guareí") %>% 
  dplyr::select(group, month, def_x, def_y) %>%
  distinct()

# lp <- levels(seeds$month)
rm(clark.values)
rm(clarktest)
rm(i)
rm(j)
rm(seeds.i)
rm(seeds.j)
clark.values <- data.frame(R = double(), p = double(), group = character(), month = character())
# clark.values %>% str()

for ( i in levels(seeds$group) ) {
  # i <- "Santa Maria"
  # i <- "Guareí"
  # i <- "Taquara"
  # i <- "Suzano"
  if (i == "Guareí") {
    seeds.i <- seeds %>%  dplyr::filter(group == i)
    limitsOwin <- limitsOwin_gua
  }
  if (i == "Santa Maria") {
    seeds.i <- seeds %>%  dplyr::filter(group == i)
    limitsOwin <- limitsOwin_sma
  }
  if (i == "Taquara") {
    seeds.i <- seeds %>%  dplyr::filter(group == i)
    limitsOwin <- limitsOwin_taq
  }
  if (i == "Suzano") {
    seeds.i <- seeds %>%  dplyr::filter(group == i)
    limitsOwin <- limitsOwin_suz
  }
  
  
  for ( j in levels(seeds.i$month)) {
    
    # j <- "Jul"
    seeds.j <- seeds.i %>% dplyr::filter(month == j)
    obs <- ppp(seeds.j[,3], seeds.j[,4], window=limitsOwin)
    clarktest <- clarkevans.test(obs,correction = 'cdf', alternative=c('two.sided'))
    clarktest <- data.frame(R = clarktest$statistic, p = clarktest$p.value, group = i, month = j)
    
    clark.values <- clark.values %>%  bind_rows(clarktest)
    
    print(clark.values)
    
  }
  
}



# cvbackup <- clark.values
siminputmatrix <- siminputmatrix %>% 
  rename(month = id_month)
a <- dplyr::left_join(siminputmatrix, clark.values)


# # Write csv
# a %>%
#   write.csv(here("Data", "Seed_dispersal", "Curated", "Validation", "Summary_by-month_R-aggregation.csv"),
#             row.names = FALSE)




