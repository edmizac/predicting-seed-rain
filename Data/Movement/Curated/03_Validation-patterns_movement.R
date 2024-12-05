# Script name: 03_Validation-patterns_movement.R
# Script purpose: summarize, plot and analize Movement empirical data for VALIDATION.
# Also generates variable values for Validation table (necessary for ga analysis)



# **** STIL NEEDS TO PLOT THE FIGURES ***8


#### ***********************************************************************************
#### ********** CHECK SENSITIVITY ANALYSIS FOLDER ( for plots and data filtering ) *****
#### ***********************************************************************************





# Date created: 2022-11-03d
# Author: Eduardo Zanette

## Notes --------------------------- 
# 
#

## Packages -------------------------
library("here")
library("dplyr")
library("ggplot2")
library("readxl")
library("lubridate")
library("hms")
library("amt")
library("sf")
library("stringr")

## Options -------------------------
# (plotting, memory limit, decimal digits)
# ggplot theme
theme_set(theme_bw(base_size = 15))



# # Siminputrow matrix  -------------------------
# 
# ## Load siminputrow matrix from simulation time data -------------------
# siminputmatrix <- read.csv(here("Data", "Movement", "Curated", "Param_Simulation-time", 
#                                 "BLT_groups_data_summary_siminputrow.csv"),
#                            sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
#   mutate(group = recode(group, "Guarei" = "Guareí")) # to match all other datasets


# Read siminputrow movement data and define levels
dat.all.mv <- read.csv(here("Data", "Movement", "Curated", "Param_siminputrow", 
                            "Siminputrow_movement-data.csv") #"BLT_groups_data_siminputrow.csv"),
            #row.names = FALSE
            ) %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) %>%  # to match all other datasets
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "SantaMaria", "Taquara")) %>% 
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                         "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  mutate(
    datetime = ymd_hms(datetime),
    date = lubridate::date(datetime)
    )



# Summarize activity budget -------------------------
dat.all.mv$behavior %>% unique()

# First make idle = resting (the model does not distinguish them)
dat.all.mv <- dat.all.mv %>% 
  mutate(
    behavior = case_when(
      behavior == "Inactive" ~ "Resting",
      TRUE ~ behavior
    )
  )
# Check:
dat.all.mv$behavior %>% unique()

# Select target behaviors:
target_behav <- c("Frugivory", "Travel", "Foraging", "Resting")

# By day
dat.ab.summary.day <- dat.all.mv %>% 
  group_by(group, month, date, behavior) %>%
  summarise(
    n = n()
  ) %>% 
  mutate(
    perc_behavior = 100 * (n / sum(n))
  )

# filter only behaviors of interest of the model
dat.ab.summary.day %>% 
  dplyr::filter(behavior %in% target_behav
  ) # %>% 
# # Write csv 
#   write.csv(here("Data", "Movement", "Curated", "Validation", "Siminputrow_Activity-budget_By-day.csv"),
#             row.names = FALSE)


# check gummivory (not included in the model)
dat.all.mv$behavior %>% unique()
gumivory <- c("Gummivory")
gum <- dat.all.mv %>% 
  group_by(group, month, behavior) %>%
  # group_by(group, month, date, behavior) %>%
  summarise(
    n = n()
  ) %>% 
  mutate(
    perc_behavior = 100 * (n / sum(n))
  ) %>%  
  dplyr::filter(behavior %in% gumivory
  )
  

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
#   write.csv(here("Data", "Movement", "Curated", "Validation", "Siminputrow_Activity-budget_By-month.csv"),
#             row.names = FALSE)

  


# Summarize DPL -------------------------
dat.dpl.summary <- dat.all.mv %>% group_by(group, month, date) %>% 
  summarise(
    DPL = sum(dist, na.rm = TRUE)
  )

# # Write csv
# dat.dpl.summary %>%
#   write.csv(here("Data", "Movement", "Curated", "Validation", "Siminputrow_DPL_by-day.csv"),
#             row.names = FALSE)

dat.dpl.summary.mo <- dat.dpl.summary %>% group_by(group, month) %>% 
  summarise(
    DPL_mean = mean(DPL, na.rm = TRUE)
  )
# # Write csv
# dat.dpl.summary.mo %>%
#   write.csv(here("Data", "Movement", "Curated", "Validation", "Siminputrow_DPL_by-month.csv"),
#             row.names = FALSE)




# Summarize Home range -------------------------

dat.all.mv.tr <- dat.all.mv %>%
  mutate(
    id = paste0(group, " - ", month)
  ) %>% 
  make_track(.x=x, .y=y, id = id, crs = 32722, all_cols = TRUE) %>% 
  nest(data = -c(id, group, month))

KDE <- dat.all.mv.tr %>%
# hrvalues <- dat.all.mv.tr %>%
  mutate( KDE95 = amt::map(data, ~ hr_kde(., level = 0.95)), 
          KDE50 = amt::map(data, ~ hr_kde(., level = 0.50)) 
  )

# Transform KDE to sf
# KDE_sf <- amt::hr_isopleths(hrvalues) %>% sf::st_as_sf(data, crs = 32722)

KDE_sf <- KDE %>% 
  mutate(
        isop_95 = amt::map(KDE95, ~hr_isopleths(.)) %>% 
          purrr::map(., ~sf::st_as_sf(., 
                                      coords = c("x_", "y_"),
                                      crs = 32722)), #%>% 
          # purrr::map(., ~sf::st_transform(., crs = 32722)),
        isop_50 = amt::map(KDE50, ~hr_isopleths(.)) %>% 
          purrr::map(., ~sf::st_as_sf(., 
                                      coords = c("x_", "y_"),
                                      crs = 32722)), # %>% 
          # purrr::map(., ~sf::st_transform(., crs = 32722))
  ) %>% 
dplyr::select(-data) #%>% 
  # amt::unnest()

KDE_sf$isop_95[[1]] %>% st_crs()

# Add shapefiles to crop
KDE_sf <- KDE_sf %>% 
  mutate(
    shp = case_when(
      group == "Suzano" ~ "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Suzano_polygon_unishp.shp",
      group == "Guareí" ~ "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Guarei_polyg_sept2022.shp",
      group == "SantaMaria" ~ "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/SantaMaria_only_rec.shp",
      group == "Taquara" ~ "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Taquara_only2.shp",
      TRUE ~ NA
      )
  ) %>% 
  # read shp as sf
  mutate(
    shp = purrr::map(shp, ~ sf::read_sf(.)) #%>% 
      # purrr::map(., ~ sf::st_set_crs(., 32722))
  ) #%>% 
  # KDE_sf$shp[[1]] %>% st_crs() #%>%   #checking crs
  # set crs
  # mutate(
  #   shp = purrr::map(shp, ~ sf::st_set_crs(., 32722))
  # )
# (no need to set crs because it is 32722 already)


# Intersecting only one line of the df:
library("tmap")
ex_shp <- KDE_sf$shp[[3]]
ex_shp %>% st_crs()
tm_shape(ex_shp) +
  tm_polygons()

ex_isop <- KDE_sf$isop_95[[3]]
ex_isop %>% st_crs()
tm_shape(ex_isop) +
  tm_polygons()

cropped_tm <- tm_shape(ex_shp) +
  tm_polygons() +
  tm_shape(ex_isop) +
  tm_polygons(alpha = 0.2)
cropped_tm

# # Save tmap as example
# tmap_save(
#   tm = cropped_tm,
#   filename = here("Data", "Movement", "Curated", "Validation", "Cropped_HR_example6.png"), dpi = 300
# )

cropped <- st_intersection(ex_isop, ex_shp)
tm_shape(cropped) +
  tm_polygons()
# It works. Why it does not work inside map()?

for (i in 1:nrow(KDE_sf)) {
  ex_shp <- KDE_sf$shp[i]
  ex_isop <- KDE_sf$isop_50[i]
  ex_isop95 <- KDE_sf$isop_95[i]
  
  print(st_crs(ex_shp) == st_crs(ex_isop))
  print(st_crs(ex_shp) == st_crs(ex_isop95))
  # sf::st_intersection(ex_isop, ex_shp) # the problem is within the st_intersection() function
  # sf::st_intersection(ex_isop95, ex_shp) # the problem is within the st_intersection() function
  
  print(i)
}

# For all lines/groups:
KDE_sf <- KDE_sf %>% 
  mutate(
    cropped_95 = purrr::map(isop_95, ~sf::st_intersection(., shp)),
    cropped_50 = purrr::map(isop_50, ~sf::st_intersection(., shp))
  )
# OH MY GOSH WHY THIS DOES NOT WORK

# I will just do it manually:
KDE_sf$cropped95[[1]] <- sf::st_intersection(KDE_sf$isop_95[[1]], KDE_sf$shp[[1]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[2]] <- sf::st_intersection(KDE_sf$isop_95[[2]], KDE_sf$shp[[2]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[3]] <- sf::st_intersection(KDE_sf$isop_95[[3]], KDE_sf$shp[[3]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[4]] <- sf::st_intersection(KDE_sf$isop_95[[4]], KDE_sf$shp[[4]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[5]] <- sf::st_intersection(KDE_sf$isop_95[[5]], KDE_sf$shp[[5]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[6]] <- sf::st_intersection(KDE_sf$isop_95[[6]], KDE_sf$shp[[6]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[7]] <- sf::st_intersection(KDE_sf$isop_95[[7]], KDE_sf$shp[[7]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[8]] <- sf::st_intersection(KDE_sf$isop_95[[8]], KDE_sf$shp[[8]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[9]] <- sf::st_intersection(KDE_sf$isop_95[[9]], KDE_sf$shp[[9]]) #%>% sf::st_area(.)

KDE_sf$cropped50[[1]] <- sf::st_intersection(KDE_sf$isop_50[[1]], KDE_sf$shp[[1]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[2]] <- sf::st_intersection(KDE_sf$isop_50[[2]], KDE_sf$shp[[2]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[3]] <- sf::st_intersection(KDE_sf$isop_50[[3]], KDE_sf$shp[[3]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[4]] <- sf::st_intersection(KDE_sf$isop_50[[4]], KDE_sf$shp[[4]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[5]] <- sf::st_intersection(KDE_sf$isop_50[[5]], KDE_sf$shp[[5]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[6]] <- sf::st_intersection(KDE_sf$isop_50[[6]], KDE_sf$shp[[6]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[7]] <- sf::st_intersection(KDE_sf$isop_50[[7]], KDE_sf$shp[[7]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[8]] <- sf::st_intersection(KDE_sf$isop_50[[8]], KDE_sf$shp[[8]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[9]] <- sf::st_intersection(KDE_sf$isop_50[[9]], KDE_sf$shp[[9]]) #%>% sf::st_area(.)

tm_shape(KDE_sf$cropped50[[9]]) +
  tm_polygons()

# Visualize cropped plots
i <- 1
for (feature in 1:nrow(KDE_sf)) {
  tm_obj <- KDE_sf$cropped95[[i]] %>% 
   tm_shape() +
    tm_polygons()
  print(tm_obj)
  
 i <- i+1 
}

# # KDE_sf2 <- bind_cols(KDE_sf$shp, KDE_sf$id, KDE_sf$group, KDE_sf$month) 
# KDE_sf2 <- KDE_sf$shp
# 
# KDE_sf2 <- KDE_sf2 %>% 
#   mutate(
#     id = KDE_sf$id,
#     group = KDE_sf$group,
#     month = KDE_sf$month
#   )
#   dplyr::left_join(KDE_sf[, 1:3])


KDE_sf <- KDE_sf %>%
  # group_by(id) %>% 
  mutate(
    id.x = id %>% str_remove_all(., " ") %>% str_replace(., "-", "_"),
    pathsave95 = paste0(here("Data", "Movement", "Curated", "Validation"), "/", id.x, "_cropped_KDE95.shp"),
    pathsave50 = paste0(here("Data", "Movement", "Curated", "Validation"), "/", id.x, "_cropped_KDE50.shp")
    )

# Save all shapes separetly
i <- 1
for (feature in 1:nrow(KDE_sf)) {
  
  shp_95 <- KDE_sf$cropped95[[i]] %>% st_as_sf()
  shp_50 <- KDE_sf$cropped50[[i]] %>% st_as_sf()
  
  sf::st_write(shp_95, dsn = KDE_sf$pathsave95[i])
  sf::st_write(shp_50, dsn = KDE_sf$pathsave50[i])
  
  i <- i + 1
}


# Wrangle and select valuable columns
KDE_sf %>% colnames()

KDE_sf <- KDE_sf %>%
  tidyr::pivot_longer(cropped95:cropped50, names_to = 'KDE_value', values_to = 'hr')
KDE_sf <- KDE_sf %>%
  dplyr::select(-c("KDE95", "KDE50", "isop_95", "isop_50", "shp"))


# Calculate home range area from clipped sf
KDE_sf <- KDE_sf %>%
  mutate(
    hr_area = purrr::map(hr, ~ sf::st_area(.))
    ) %>% 
  dplyr::select(-hr) #%>%
  # unnest(cols = hr_area)

# Transform to ha
KDE_sf <- KDE_sf %>%
  mutate(
    hr_area_ha = hr_area %>% as.numeric() / 10000
  ) %>% 
  dplyr::select(-hr_area)

# Match cropped with KDE names
KDE_sf <- KDE_sf %>%
  mutate(
    KDE_value = case_when(
      KDE_value == "cropped95" ~ "KDE95",
      KDE_value == "cropped50" ~ "KDE50",
      TRUE ~ KDE_value
    )
  )

KDE_sf %>% str()

# # Write csv
# KDE_sf %>%
#   write.csv(here("Data", "Movement", "Curated", "Validation", "Siminputrow_Home-range_by-month.csv"),
#             row.names = FALSE)



# # Old procedure (without cropping):
# hrvalues <- hrvalues %>%  
#   select(-data) %>% tidyr::pivot_longer(KDE95:KDE50, names_to = 'KDE_value', values_to = 'hr')
# 
# hrvalues <- hrvalues %>% 
#   mutate(hr_area = map(hr, hr_area)) %>%
#   unnest(cols = hr_area)
# 
# hrvalues <- hrvalues %>% dplyr::select(-c('what', 'hr'))
# 
# hrvalues <- hrvalues %>% mutate(hr_area_ha = area / 10000)
# # Write csv
# hrvalues %>%
#   write.csv(here("Data", "Movement", "Curated", "Validation", "Siminputrow_Home-range_by-month.csv"),
#             row.names = FALSE)



## Plot home range -------------------------

# Define factor order

# KDE 95
hrvalues %>% 
  dplyr::filter(KDE_value == "KDE95") %>%
  group_by(group, month) %>%
  # facet_wrap(~KDE_value) +
  ggplot(aes(x = group, y = hr_area_ha, color = month)) +
  geom_boxplot() +
  guides(fill=FALSE) +
  ylim(0, 350) +
  ylab("Area (ha)") +
  # xlab("Group - month") + 
  theme(axis.title.x=element_blank()) +
  ggtitle("KDE 95% (Home range)") +
  scale_colour_viridis_d()
# geom_jitter(width = 0.15) +
# annotate("text", x=2, y=5000, label= paste0("Mean ± sd = ", round(avg_dist, 2), " ± ", round(sd_dist, 2)))

# ggsave(filename = here("Data", "Movement", "Curated", "Validation",
#                        "Siminputrow_HomeRange_KDE95.png"),
#        dpi = 300, width = 15, height = 10 , units = "cm")

# KDE50
hrvalues %>% 
  dplyr::filter(KDE_value == "KDE50") %>%
  group_by(group, month) %>%
  # facet_wrap(~KDE_value) +
  ggplot(aes(x = group, y = hr_area_ha, color = month)) +
  geom_boxplot() +
  # geom_point() +
  guides(fill=FALSE) +
  ylim(0, 350) +
  ylab("Area (ha)") +
  # xlab("Group - month") +
  theme(axis.title.x=element_blank()) +
  ggtitle("KDE 50% (Core area)") +
  scale_colour_viridis_d()
# geom_jitter(width = 0.15) +
# annotate("text", x=2, y=5000, label= paste0("Mean ± sd = ", round(avg_dist, 2), " ± ", round(sd_dist, 2)))

ggsave(filename = here("Data", "Movement", "Curated", "Validation",
                       "Siminputrow_HomeRange_KDE50.png"),
       dpi = 300, width = 15, height = 10, units = "cm")




# Other parameters not initially assessed -------------------------
# (R2n = Square displacement)

# Movement Rate
mv.metrics <- dat.all.mv %>% 
  dplyr::select(-id) %>% 
  group_by(group, month, date) %>% 
  summarise(
    DPL = sum(dist, na.rm = TRUE),
    activitytime = sum(dt, na.rm = TRUE) / 60 / 60, # / minutes / hours
    MR = DPL/activitytime
  )

hrvalues <- hrvalues %>%
  # dplyr::select(-id) %>% 
  dplyr::filter(KDE_value == "KDE95")
  
mv.metrics <- mv.metrics %>% 
  left_join(hrvalues) %>% 
  dplyr::select(-level)

# Path Twisting (PT)
mv.metrics <- mv.metrics %>% 
  mutate(
    PT = DPL^2/area
  )


# # Write csv
# mv.metrics %>%
#   write.csv(here("Data", "Movement", "Curated", "Validation", "Siminputrow_MR-PT_by-day.csv"),
#             row.names = FALSE)

