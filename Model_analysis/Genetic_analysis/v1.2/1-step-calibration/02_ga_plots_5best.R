# Script name: 01_ga_plots.R
# Script purpose: summarize genetic algorithm results

# Date created: 2022-12-26d
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
library("nlrx")
library("tictoc")
library("magrittr")
library("ggplot2")
library("stringr")

# ggplot theme
theme_set(theme_bw(base_size = 18))

# path <- here("Model_analysis", "Genetic_analysis", "temp", "without_stored_energy")
path <- here("Model_analysis", "Genetic_analysis",  "v1.2", "1-step-calibration", "temp")
outpath <- here("Model_analysis", "Genetic_analysis", "v1.2", "1-step-calibration", "temp")

# suz <- filesga <- list.files(path, pattern = "Dec_nl.rds")
# suz <- paste0(path, "/", filesga)
# suz <- readRDS(suz)



# filesga <- list.files(path, pattern = "results_feedingbouton.rds")  # use the rgba file
filesga <- list.files(path, pattern = "results_")  # use the rgba file

filesga <- paste0(path, "/", filesga)

# dfga <- data.frame("expname" = character(), "parameter" = character(), "value" = double())
# dfga <- data.frame()
n <- 1
for (i in filesga) {
  # i <- filesga[9]
    # resultsrbga <- read.csv(i, encoding = "latin1")
  resultsrbga <- readRDS(i)
  
  # expname <- stringr::str_extract(i, pattern = "^[_results]")
  # expname <- stringr::str_extract(i, pattern = "[:alpha:]+(?=_results)")
  expname <- stringr::str_extract(i, pattern = "\\w+(?=_results)")
  expname <- stringr::str_remove(expname, pattern = "GA_")
  
  population <- resultsrbga[7] %>% purrr::map_df(., ~as.data.frame(.))
  best_results <- resultsrbga[11] %>% unlist() #%>% as_tibble()
  
  # best5 <- tail(resultsrbga$best, 5)
  
  # cat(summary(resultsrbga))
  
  bestSolution<-resultsrbga$population[which.max(resultsrbga$evaluations), ]
  # values[which(GA@solution[1,] == 1)]
  
  # best5_idx <- pmatch(best5, best_results)
  # bestSolutions<-resultsrbga$population[best5_idx, ]
  
  
  resultsrbga$evaluations
  # resultsrbga[10]
  resultsrbga$best
  # resultsrbga[11]
  
  # best5 <- resultsrbga$best %>% unlist() %>% as.vector() #sort(., decreasing = TRUE)
  # fitness <- resultsrbga$best %>% unlist() %>% as.vector() #sort(., decreasing = TRUE)
  fitness <- resultsrbga$evaluations %>% unlist() %>% as.vector() #sort(., decreasing = TRUE)
  idx <- 1:length(fitness)
  fitness <- cbind(fitness, idx)
  best5 <- fitness %>%
    as_tibble() %>%
    arrange(desc(fitness)) %>% 
    slice_head(n=5)
    # slice_max(fitness, n = 5, with_ties = FALSE) 

  best5
  
  # ### Option 1: filtering despite the number of chromosomes: ###
  # best5_idx <- resultsrbga$best[resultsrbga$best %in% best5] ; length(best5_idx)
  # # idx <- match(best5, resultsrbga$best)
  # optimized_param <- population[1:length(best5_idx), ]
  
  
  ### Option 2:  filtering only one chromossome per fitness value: ###
  # best5_idx <- pmatch(best5, best_results)
  # best5_idx <- pmatch(best5, resultsrbga$population)
  
  # best5_idx <- pmatch(best5$idx, row_number(population))
  best5_idx <- best5$idx
  
  optimized_param <- population[best5_idx, ]
  
  # get parameter names:
  params <- resultsrbga[2] %>% unlist()
  # names(params)
  
  colnames(optimized_param) <- names(params) %>% stringr::str_sub(., end=-5)
  colnames(optimized_param) <- colnames(optimized_param) %>% stringr::str_sub(., start=11)
  colnames(optimized_param)
  # a <- colnames(optimized_param)
  
  optimized_param <- cbind(optimized_param, best5)
  len <- colnames(optimized_param) %>% length()
  # colnames(optimized_param)[len] <- "fitness_log"
  
  optimized_param$expname <- basename(i) %>% str_match(., '(?:_[^_]+){3}') %>% as.character() %>% 
    str_remove(., '^_{1}') %>% str_remove(., "_results")
  # optimized_param <- optimized_param %>% t() %>% as.data.frame(row.names = TRUE)
  # optimized_param <- cbind(optimized_param, a)
  # optimized_param$simulation_scenario <- paste0(area_run, "_", month_run)
  # optimized_param <- optimized_param[, c(3, 2, 1)]
  
  
  if (n == 1) {
    dfga <- optimized_param
  }
    
  dfga <- dplyr::bind_rows(dfga, optimized_param)
  n <- n + 1
  
}

# Remove duplicates
dfga <- dfga %>% 
  distinct()

# extract min/max parameter info from experiments from last i
min_param <- resultsrbga[2] # parameters min
max_param <- resultsrbga[3] # parameters max

ga_input <- data.frame(
  min = min_param,
  max = max_param
)

rownames(ga_input) <- rownames(ga_input) %>% stringr::str_sub(., end=-5)
ga_input$parameter <- rownames(ga_input)
rownames(ga_input) <- NULL


# Transform fitness to log scale
dfga$fitness
dfga <- dfga %>% 
  rename(fitness_log = fitness) %>% 
  mutate(fitness_log = log10(fitness_log))
dfga$fitness_log

# create custom min/max for fitness and bind to ga_input
fitmax <- dfga %>% 
  # dplyr::filter(parameter == "fitness") %>% 
  dplyr::select(fitness_log) %>% 
  max(., na.rm = TRUE)
  # there are infinite values sometimes
  

fitinfo <- data.frame(stringMin = 0, 
                      stringMax = fitmax,
                      parameter = "fitness_log")

ga_input <- ga_input %>% rbind(fitinfo)


# pivot into longer for plotting
dfga <- dfga %>% 
  tidyr::pivot_longer(names_to = "parameter", values_to = "value",
                      cols = `energy-from-fruits`:fitness_log)

# join min and max values from ga_input
dfga <- dfga %>% dplyr::left_join(ga_input, by="parameter")


# dfga <- dfga %>%
#   dplyr::mutate(parameter =
#                 str_replace_all(parameter, "-", "_"))

dfga <- dfga %>% 
  dplyr::mutate(parameter = 
                  str_replace_all(parameter, c("-" = "_", " " = "")))


dfga <- dfga %>% 
  mutate(parameter = recode(parameter, "prop_trees_to_reset_memory" = "prop_reset_memory")) #%>% 
dfga <- dfga %>%
  mutate(group = forcats::fct_relevel(expname, 
                                      # "Suzano_Sep", "Suzano_Dec", 
                                      # "Guareí_May", 
                                      "Guareí_Jun", "Guareí_Jul", "Guareí_Aug", 
                                      # "SantaMaria_Mar","SantaMaria_Apr", 
                                      "Taquara_Jan")) %>% 
  mutate_if(is.character, as.factor)
dfga <- dfga %>% 
  rename("min" = "stringMin",
         "max" = "stringMax",
         )

dfga %>% str()
dfga$parameter %>% levels()

# take all punctuation out
dfga$expname %>% stringr::str_replace_all(.,"[^[:graph:]]", "") 


# Save csv
dfga %>% 
  writexl::write_xlsx(path = paste0(outpath, "/optimized_best5.xlsx"))

dfga %>% 
  group_by(group, month, parameter) %>% 
  dplyr::summarise(
    value_mean = mean(value, na.rm = TRUE),
    value_sd = sd(value, na.rm = TRUE),
    value_max = max(value, na.rm = TRUE),
    value_min = min(value, na.rm = TRUE),
    value_n = n()
  ) %>% 
  writexl::write_xlsx(path = paste0(outpath, "/optimized_best5_summary.xlsx"))


# dfga_en <- dfga %>% 
#   dplyr::filter(str_detect(parameter, "energy"))
# 
# dfga_otr <- dfga %>% 
#   dplyr::filter(!str_detect(parameter, "energy"))


# Rename expname to 'month' and relevel
dfga <- dfga %>% 
  rename(
    month = expname
  ) %>% 
  mutate(
    month = case_when(
      month == "Guareí_May" ~ "May",
      month == "Guareí_Jun" ~ "Jun",
      month == "Guareí_Jul" ~ "Jul",
      month == "Guareí_Aug" ~ "Aug",
      month == "SantaMaria_Mar" ~ "Mar",
      month == "SantaMaria_Apr" ~ "Apr",
      month == "Suzano_Sep" ~ "Sep",
      month == "Suzano_Dec" ~ "Dec",
      month == "Taquara_Jan" ~ "Jan"
    )
  ) %>%
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec")) 


target <- c("start_energy", "energy_level_1", "energy_level_2")
dfga_en <- dfga %>% 
    dplyr::filter(
      parameter %in% target
    )

target <- c("energy_from_fruits", "energy_from_prey", "energy_loss_foraging", "energy_loss_traveling", "energy_loss_resting")
dfga_en2 <- dfga %>% 
  dplyr::filter(parameter %in% target)

target <- c("duration", "species_time", "prop_reset_memory"
            # , "energy-loss-foraging", "energy-loss-traveling"
)
dfga_otr <- dfga %>% 
  dplyr::filter(parameter %in% target)


# Plot showing how the optimization differed for each energy parameter
set.seed(412)

dfga_en %>% 
  ggplot() +
  # geom_pointrange(aes(ymin = min, ymax = max, x = parameter)) +
  geom_errorbar(aes(ymin = min, ymax = max, x = parameter)
                , width = 0.2
                , linewidth = 0.2) +
  geom_point(aes(x = parameter, y = value, color = month),
             size = 3
             , alpha = 0.5
             , position =  position_jitter(w = 0.1)
             ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 10
    )
    , axis.title.x = element_blank()
  ) +
  scale_color_viridis_d() +
  # ggtitle("Optimization of energy variables")
  ggtitle("Optimization of energy levels")

# Save plot
# ggsave(paste0(path, '02_GA_optimized-params_energylevels_best5.png'), height = 5, width = 7)


# Plot showing how the optimization differed for each energy parameter
dfga_en2 %>% 
  ggplot() +
  # geom_pointrange(aes(ymin = min, ymax = max, x = parameter)) +
  geom_errorbar(aes(ymin = min, ymax = max, x = parameter)
                , width = 0.2
                , linewidth = 0.2) +
  geom_point(aes(x = parameter, y = value, color = month),
             size = 3
             , alpha = 0.5
             , position =  position_jitter(w = 0.1)
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 10
    )
    , axis.title.x = element_blank()
  ) +
  scale_color_viridis_d() +
  # ggtitle("Optimization of energy variables")
  ggtitle("Optimization of energy gain and loss")

# Save plot
# ggsave(paste0(path,  '/02_GA_optimized-params_energygainloss_best5.png'), height = 5, width = 7)


# Plot showing how the optimization differed for each parameter (others)
dfga_otr %>% 
  ggplot() +
  # geom_pointrange(aes(ymin = min, ymax = max, x = parameter)) +
  geom_errorbar(aes(ymin = min, ymax = max, x = parameter)
                , linewidth = 0.2
                , size = 0.2) +
  geom_point(aes(x = parameter, y = value, color = month),
             size = 3
             , alpha = 0.5
             , position =  position_jitter(w = 0.1)
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 10
    )
    , axis.title.x = element_blank()
  ) +
  scale_color_viridis_d() +
  # ggtitle("Optimization of other parameters")
  ggtitle("Optimization of non-energy parameters")

# Save plot
# ggsave(paste0(path, '/02_GA_optimized-params_nonenergy_best5.png'), height = 5, width = 7)
#### ggsave(paste0(path,  "/with_stored_energy/", '02_GA_optimized-params_nonenergy_best5.png'), height = 5, width = 7)


# By category
dfga <- dfga %>% 
  mutate(
    param_category = case_when(
      parameter == "fitness_log" ~ "fitness",
      
      parameter == "start_energy" ~ "energy levels",
      parameter == "energy_level_1" ~ "energy levels",
      parameter == "energy_level_2" ~ "energy levels",
      
      parameter == "duration" ~ "others",
      parameter == "species_time" ~ "others",
      parameter == "p_disputed_trees" ~ "others",
      parameter == "p_timesteps_to_rest" ~ "others",

      parameter == "prop_reset_memory" ~ "memory reset",
      parameter == "step_forget" ~ "memory revisit",
      
      parameter == "energy_from_fruits" ~ "energy gain/loss",
      parameter == "energy_from_prey" ~ "energy gain/loss",
      parameter == "energy_loss_foraging" ~ "energy gain/loss",
      parameter == "energy_loss_traveling" ~ "energy gain/loss",
      parameter == "energy_loss_resting" ~ "energy gain/loss"
      
      )
  )




# Plot grid
dfga %>% 
  ggplot() +
  # geom_pointrange(aes(ymin = min, ymax = max, x = parameter)) +
  geom_errorbar(aes(ymin = min, ymax = max, x = parameter)
                , width = 0.2
                , size = 0.2) +
  geom_point(aes(x = parameter, y = value, color = month),
             size = 2.5
             , alpha = 0.5
             # , position =  position_jitter(w = 0.2)
             , position =  position_dodge(w = 0.5)
  ) +
  theme(
    axis.text.x = element_text(
      # angle = 90,
      # hjust = 1,
      # size = 10
    )
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    , legend.position = "bottom"
  ) +
  scale_color_viridis_d() +
  # scale_color_manual(values = c("#C833EC", "#33A4EC")) +
  ggtitle("Optimization of model parameters (best 5)") +
  facet_wrap(~param_category, scales = "free", nrow = 6) +
  coord_flip()

# # Save plot
ggsave(paste0(path, '/02_GA_optimized-params-grid-best5_opt1.png'),
       height = 12, width = 8)


# Plot grid
dfga %>% 
  ggplot() +
  # geom_pointrange(aes(ymin = min, ymax = max, x = parameter)) +
  geom_errorbar(aes(ymin = min, ymax = max, x = parameter)
                , width = 0.2
                , size = 0.2) +
  geom_point(aes(x = parameter, y = value, color = month),
             size = 2.5
             , alpha = 0.5
             # , position =  position_jitter(w = 0.2)
             , position =  position_dodge(w = 0.5)
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 10
    )
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    , legend.position = "bottom"
  ) +
  scale_color_viridis_d() +
  # scale_color_manual(values = c("#C833EC", "#33A4EC")) +
  ggtitle("Optimization of model parameters (best 5)") +
  facet_wrap(~param_category, scales = "free", nrow = 1)
  # coord_flip()

# # Save plot
ggsave(paste0(path, '/02_GA_optimized-params-grid-best5_opt2.png'),
       height = 8, width = 12)









# Plot per category with association lines ----


# For plotting paired plots
dfga_paired <- dfga %>% 
  group_by(month, parameter) %>%
  # arrange(parameter=="fitness_log", .by_group = TRUE)
  mutate(paired = 1:n())

# dfga_paired <- dfga_paired %>% 
#   mutate(expname = forcats::fct_relevel(expname, 
#                                         # "Suzano_Sep", "Suzano_Dec", 
#                                         # "Guareí_May", 
#                                         "Guareí_Jun", "Guareí_Jul", "Guareí_Aug", 
#                                         # "SantaMaria_Mar","SantaMaria_Apr", 
#                                         "Taquara_Jan"))
dfga_paired %>% str()

target <- c("start_energy", "energy_level_1", "energy_level_2")
dfga_en <- dfga_paired %>% 
  dplyr::filter(
    parameter %in% target
  )

target <- c("energy_from_fruits", "energy_from_prey", "energy_loss_foraging", "energy_loss_traveling", "energy_loss_resting")
dfga_en2 <- dfga_paired %>% 
  dplyr::filter(parameter %in% target)

target <- c("duration", "species_time", "prop_reset_memory"
            # , "energy-loss-foraging", "energy-loss-traveling"
)
dfga_otr <- dfga_paired %>% 
  dplyr::filter(parameter %in% target)

target <- c("p_timesteps_to_rest", "p_disputed_trees")
dfga_otr2 <- dfga_paired %>% 
  dplyr::filter(parameter %in% target)



## Plot showing how the optimization differed for each energy parameter
set.seed(42)
dfga_en %>% 
  ggplot(aes(x = parameter, y = value, group = interaction(month, paired))) +
  geom_errorbar(aes(ymin = min, ymax = max, x = parameter)
                , width = 0.2
                , size = 0.5) +
  geom_point(aes(#x = parameter, y = value.
                 color = month)
                 # , color = "black"
             , size = 4
             # , shape = 21
             , alpha = 0.5
             , position =   position_dodge(width = 0.3)
             # , position =  position_jitter(width = 0.05, seed=42)
  ) +
  geom_line(aes(#x = parameter, y = value,
                color = month)
            , lwd = 0.8
            , alpha = 0.5
            , position =   position_dodge(width = 0.3)
            # , position =  position_jitter(width = 0.05, seed=42)
  ) +
  # stat_summary(fun=mean, geom="line", aes(group=expname, color = expname)
  #              # , position = position_jitterdodge()
  #              ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 10
    )
    # , axis.title.y = element_blank()
  ) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  # ggtitle("Optimization of energy variables")
  ggtitle("Optimization of energy levels")

# Save plot
# ggsave(paste0(path,  '/02_GA_optimized-params_energylevels_best5_connected.png')
#        , height = 5, width = 7)


## Plot showing how the optimization differed for each energy parameter
dfga_en2 %>% 
  ggplot(aes(x = parameter, y = value, group = interaction(month, paired))) +
  geom_errorbar(aes(ymin = min, ymax = max, x = parameter)
                , width = 0.2
                , size = 0.5) +
  geom_point(aes(x = parameter, y = value, color = month),
             size = 4
             , alpha = 0.5
             , position =   position_dodge(width = 0.3)
             # , position =  position_jitter(w = 0.05, seed=42)
  ) +
  geom_line(aes(color = month)
            , lwd = 0.8
            , alpha = 0.5
            , position =   position_dodge(width = 0.3)
            # , position =  position_jitter(w = 0.05, seed=42)
            ) +
  # stat_summary(fun=mean, geom="line", aes(group=month, color = month)
  #              # , position = position_jitterdodge()
  #              ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 10
    )
    # , axis.title.y = element_blank()
  ) +
  scale_color_viridis_d() +
  # ggtitle("Optimization of energy variables")
  ggtitle("Optimization of energy gain and loss")

# # Save plot
# ggsave(paste0(path,  '/02_GA_optimized-params_energygainloss_best5_connected.png')
#        , height = 5, width = 7)


## Plot showing how the optimization differed for each parameter (others)
dfga_otr %>% 
  ggplot(aes(x = parameter, y = value, group = interaction(month, paired))) +
  geom_errorbar(aes(ymin = min, ymax = max, x = parameter)
                , width = 0.2
                , size = 0.5) +
  geom_point(aes(x = parameter, y = value, color = month),
             size = 4
             , alpha = 0.4
             # , position =  position_jitter(w = 0.05, seed=42)
             , position =   position_dodge(width = 0.3)
  ) +
  geom_line(aes(color = month)
            , lwd = 0.8
            , alpha = 0.5
            , position =   position_dodge(width = 0.3)
            # , position =  position_jitter(w = 0.05, seed=42)
  ) +
  # stat_summary(fun=mean, geom="line", aes(group=month, color = month)
  #              # , position = position_jitterdodge()
  #              ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 10
    )
    , axis.title.x = element_blank()
  ) +
  scale_color_viridis_d() +
  ggtitle("Optimization of other parameters")


# # Save plot
# ggsave(paste0(path,  "/02_GA_optimized-params_nonenergy_best5_connected.png")
#        , height = 5, width = 7)


dfga_otr2 %>% 
  ggplot(aes(x = parameter, y = value, group = interaction(month, paired))) +
  geom_errorbar(aes(ymin = min, ymax = max, x = parameter)
                , width = 0.2
                , size = 0.5) +
  geom_point(aes(x = parameter, y = value, color = month),
             size = 4
             , alpha = 0.4
             # , position =  position_jitter(w = 0.05, seed=42)
             , position =   position_dodge(width = 0.3)
  ) +
  geom_line(aes(color = month)
            , lwd = 0.8
            , alpha = 0.5
            , position =   position_dodge(width = 0.3)
            # , position =  position_jitter(w = 0.05, seed=42)
  ) +
  # stat_summary(fun=mean, geom="line", aes(group=month, color = month)
  #              # , position = position_jitterdodge()
  #              ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 10
    )
    , axis.title.x = element_blank()
  ) +
  scale_color_viridis_d() +
  ggtitle("Optimization of other parameters")


# # Save plot
# ggsave(paste0(path,  "/02_GA_optimized-params_nonenergy2_best5_connected.png")
#        , height = 5, width = 7)


## Plot showing fitness
dfga_paired %>% 
  dplyr::filter("fitness_log" %in% parameter) %>% 
  ggplot(aes(x = parameter, y = value, group = interaction(month, paired))) +
  geom_errorbar(aes(ymin = min, ymax = max, x = parameter)
                , width = 0.2
                , size = 0.5) +
  geom_point(aes(x = parameter, y = value, color = month),
             size = 3
             , alpha = 0.4
             , position =   position_dodge(width = 0.3)
             # , position = "jitter"
  ) +
  # geom_line(aes(color = month)
  #           , size = 0.7
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 10
    )
    , axis.title.x = element_blank()
  ) +
  # ggtitle("Optimization of other parameters")
  scale_color_viridis_d() +
  ggtitle("Fitness values")

dfga_paired[dfga_paired$parameter == "fitness_log", ] %>% summary()

dfga_paired %>% 
  dplyr::filter(parameter == "fitness_log") %>% 
  dplyr::select("value")# %>% 
  # summary()

# ## Save plot
# ggsave(paste0(path,  "/02_GA_optimized-params_fitness_best5_connected.png")
#        , height = 5, width = 7)








# Plot per category with association lines (dashed + solid) ----


# # For plotting paired plots
# dfga_paired <- dfga_noninf %>% 
#   group_by(expname, parameter) %>%
#   # arrange(parameter=="fitness_log", .by_group = TRUE)
#   mutate(paired = 1:n())


# Define which is the best run based on fitness
dfga_paired_fitness <- dfga_paired %>% 
  dplyr::filter(parameter == "fitness_log") %>% 
  group_by(month) %>%
  arrange(desc(value), .by_group = TRUE) %>% 
  mutate(
    idx_max = first(idx)
  ) %>% 
  dplyr::select(group, idx, idx_max)

dfga_paired_fitness

# Put it into the dfga_paired and define linetype
dfga_paired <- dplyr::left_join(dfga_paired,dfga_paired_fitness)

dfga_paired <- dfga_paired %>% 
  mutate(
    linetype = case_when(
      idx_max == idx ~ "bestrun",
      TRUE ~ "others"
    )
  )

# Get max fitness value 
maxfitness <- dfga_paired %>% 
  ungroup() %>% 
  dplyr::filter(parameter == "fitness_log") %>% 
  # group_by(expname) %>%
  dplyr::filter(value != Inf) %>% 
  summarise(
    max = max(value, na.rm=TRUE)
  ) %>% pull()
maxfitness <- maxfitness + (0.05 * maxfitness)
maxfitness <- round(maxfitness, digits = -2)

# Relevel
# dfga_paired <- dfga_paired %>% 
#   mutate(expname = forcats::fct_relevel(expname, 
#                                         "Taquara_Jan",
#                                         # "SantaMaria_Mar","SantaMaria_Apr", 
#                                         # "Guareí_May", 
#                                         "Guareí_Jun", "Guareí_Jul", "Guareí_Aug", 
#                                         # "Suzano_Sep", "Suzano_Dec", 
#  ))
dfga_paired %>% str()

target <- c("start_energy", "energy_level_1", "energy_level_2")
dfga_en <- dfga_paired %>% 
  dplyr::filter(
    parameter %in% target
  )

target <- c("energy_from_fruits", "energy_from_prey", "energy_loss_foraging", "energy_loss_traveling", "energy_loss_resting")
dfga_en2 <- dfga_paired %>% 
  dplyr::filter(parameter %in% target)

target <- c(#"duration", "species_time", 
            "prop_reset_memory"
            # , "energy-loss-foraging", "energy-loss-traveling"
)
dfga_otr <- dfga_paired %>% 
  dplyr::filter(parameter %in% target)

target <- c("p_timesteps_to_rest", "p_disputed_trees")
dfga_otr2 <- dfga_paired %>% 
  dplyr::filter(parameter %in% target)

target <- c("step_forget")
dfga_otr3 <- dfga_paired %>% 
  dplyr::filter(parameter %in% target)



## Plot showing how the optimization differed for each energy parameter
dfga_en %>% 
  ggplot(aes(x = parameter, y = value, group = interaction(month, paired))) +
  geom_errorbar(aes(ymin = min, ymax = max, x = parameter)
                , width = 0.2
                , size = 0.5) +
  geom_point(aes(x = parameter, y = value
                 , color = month, alpha = linetype
                 , shape = linetype, size = linetype
                 )
             # size = 3
             , position =  position_dodge(width = 0.3)
  ) +
  geom_line(aes(color = month
                , linetype = linetype
                , lwd = linetype
                , alpha = linetype
  )
  , position =  position_dodge(width = 0.3)
  ) +
  scale_size_manual(values = c(3, 2)) +
  scale_linewidth_manual(values = c(1, 0.5)) +
  scale_alpha_manual(values = c(0.8, 0.5)) +
  scale_shape_manual(values = c(16, 1)) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 10
    )
    , axis.title.x = element_blank()
  ) +
  scale_colour_viridis_d() +
  ggtitle("Optimization of energy levels")

# # # Save plot
# ggsave(paste0(path, '/02_GA_optimized-params_energylevels_best5_connected_dashed.png')
#        , height = 5, width = 8)


## Plot showing how the optimization differed for each energy parameter
dfga_en2 %>% 
  ggplot(aes(x = parameter, y = value, group = interaction(month, paired))) +
  geom_errorbar(aes(ymin = min, ymax = max, x = parameter)
                , width = 0.2
                , size = 0.5) +
  geom_point(aes(x = parameter, y = value
                 , color = month, alpha = linetype
                 , shape = linetype, size = linetype
  )
  # size = 3
  , position =  position_dodge(width = 0.3)
  ) +
  geom_line(aes(color = month
                , linetype = linetype
                , lwd = linetype
                , alpha = linetype
  )
  , position =  position_dodge(width = 0.3)
  ) +
  scale_size_manual(values = c(3, 2)) +
  scale_linewidth_manual(values = c(1, 0.5)) +
  scale_alpha_manual(values = c(0.8, 0.5)) +
  scale_shape_manual(values = c(16, 1)) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 10
    )
    , axis.title.x = element_blank()
  ) +
  scale_colour_viridis_d() +
  ggtitle("Optimization of energy gain and loss")

# # Save plot
# ggsave(paste0(path, '/02_GA_optimized-params_energygainloss_best5_connected_dashed.png')
#        , height = 5, width = 8)


## Plot showing how the optimization differed for each parameter (others)
dfga_otr %>% 
  ggplot(aes(x = parameter, y = value, group = interaction(month, paired))) +
  geom_errorbar(aes(ymin = min, ymax = max, x = parameter)
                , width = 0.2
                , size = 0.5) +
  geom_point(aes(x = parameter, y = value, color = month
                 # , alpha = linetype
                 , shape = linetype
                 , size = linetype
                 )
                 , alpha = 0.8
                 # , size = 3
             , position =  position_dodge(width = 0.1)
  ) +
  # geom_line(aes(color = month
  #               , linetype = linetype
  #               , lwd = linetype
  #               , alpha = linetype
  # )
  # , position =  position_dodge(width = 0.3)
  # ) +
  scale_size_manual(values = c(3, 2)) +
  # scale_linewidth_manual(values = c(1, 0.5)) +
  # scale_alpha_manual(values = c(0.8, 0.2)) +
  scale_shape_manual(values = c(16, 1)) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 10
    )
    , axis.title.x = element_blank()
  ) +
  scale_colour_viridis_d() +
  ggtitle("Optimization of memory reset")

# # Save plot
# ggsave(paste0(path, '/02_GA_optimized-params_nonenergy_best5_connected_dashed.png')
#        , height = 5, width = 8)


## Plot showing how the optimization differed for each parameter (others2)
dfga_otr2 %>% 
  ggplot(aes(x = parameter, y = value, group = interaction(month, paired))) +
  geom_errorbar(aes(ymin = min, ymax = max, x = parameter)
                , width = 0.2
                , size = 0.5) +
  geom_point(aes(x = parameter, y = value
                 , color = month, alpha = linetype
                 , shape = linetype, size = linetype
  )
  # size = 3
  , position =  position_dodge(width = 0.3)
  ) +
  geom_line(aes(color = month
                , linetype = linetype
                , lwd = linetype
                , alpha = linetype
  )
  , position =  position_dodge(width = 0.3)
  ) +
  scale_size_manual(values = c(3, 2)) +
  scale_linewidth_manual(values = c(1, 0.5)) +
  scale_alpha_manual(values = c(0.8, 0.5)) +
  scale_shape_manual(values = c(16, 1)) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 10
    )
    , axis.title.x = element_blank()
  ) +
  scale_colour_viridis_d() +
  ggtitle("Optimization of other parameters")

# # Save plot
# ggsave(paste0(path, '/02_GA_optimized-params_nonenergy2_best5_connected_dashed.png')
#        , height = 5, width = 8)


## Plot showing how the optimization differed for each parameter (others3)
dfga_otr3 %>% 
  ggplot(aes(x = parameter, y = value, group = interaction(month, paired))) +
  geom_errorbar(aes(ymin = min, ymax = max, x = parameter)
                , width = 0.2
                , size = 0.5) +
  geom_point(aes(x = parameter, y = value, color = month
                 # , alpha = linetype
                 , shape = linetype
                 , size = linetype
  )
  , alpha = 0.8
  # , size = 3
  , position =  position_dodge(width = 0.1)
  ) +
  # geom_line(aes(color = month
  #               , linetype = linetype
  #               , lwd = linetype
  #               , alpha = linetype
  # )
  # , position =  position_dodge(width = 0.3)
  # ) +
  scale_size_manual(values = c(3, 2)) +
  # scale_linewidth_manual(values = c(1, 0.5)) +
  # scale_alpha_manual(values = c(0.8, 0.2)) +
  scale_shape_manual(values = c(16, 1)) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 10
    )
    , axis.title.x = element_blank()
  ) +
  scale_colour_viridis_d() +
  ggtitle("Optimization of revisitation period")

# # Save plot
# ggsave(paste0(path, '/02_GA_optimized-params_nonenergy3_best5_connected_dashed.png')
#        , height = 5, width = 8)


## Plot showing fitness
dfga_paired %>% 
  dplyr::filter("fitness_log" %in% parameter) %>% 
  ggplot(aes(x = parameter, y = value, group = interaction(month, paired))) +
  geom_errorbar(aes(ymin = min(value, na.rm=TRUE), ymax = max(value, na.rm = TRUE)
                    , x = parameter)
                , width = 0.2
                , size = 0.5) +
  geom_point(aes(x = parameter, y = value, color = month
                 # , alpha = linetype
                 , shape = linetype
                 , size = linetype
  )
  , alpha = 0.8
  # , size = 3
  , position =  position_dodge(width = 0.1)
  ) +
  # geom_line(aes(color = month
  #               , linetype = linetype
  #               , lwd = linetype
  #               , alpha = linetype
  # )
  # , position =  position_dodge(width = 0.3)
  # ) +
  scale_size_manual(values = c(3, 2)) +
  # scale_linewidth_manual(values = c(1, 0.5)) +
  # scale_alpha_manual(values = c(0.8, 0.2)) +
  scale_shape_manual(values = c(16, 1)) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 10
    )
    , axis.title.x = element_blank()
  ) +
  scale_colour_viridis_d() +
  # ggtitle("Optimization of other parameters")
  scale_shape_manual(values = c(16, 21)) +
  ggtitle("Fitness values")

dfga_paired[dfga_paired$parameter == "fitness_log", ] %>% summary()

dfga_paired %>% 
  dplyr::filter(parameter == "fitness_log") %>% 
  dplyr::select("value") %>% 
  print(n = 20)

# # Save plot
# ggsave(paste0(path, '/02_GA_optimized-params_fitness_best5_connected_dashed.png')
#        , height = 5, width = 7)






# By category
dfga_paired <- dfga_paired %>% 
  mutate(
    param_category = case_when(
      parameter == "fitness_log" ~ "fitness_log",
      
      parameter == "start_energy" ~ "energy levels",
      parameter == "energy_level_1" ~ "energy levels",
      parameter == "energy_level_2" ~ "energy levels",
      
      parameter == "duration" ~ "others",
      parameter == "species_time" ~ "others",
      parameter == "p_disputed_trees" ~ "others",
      parameter == "p_timesteps_to_rest" ~ "others",
      
      parameter == "prop_reset_memory" ~ "memory\nreset",
      parameter == "step_forget" ~ "revisitation\nperiod",
      
      parameter == "energy_from_fruits" ~ "energy gain",
      parameter == "energy_from_prey" ~ "energy gain",
      parameter == "energy_loss_foraging" ~ "energy loss",
      parameter == "energy_loss_traveling" ~ "energy loss",
      parameter == "energy_loss_resting" ~ "energy loss"
      
    )
  )

# Reorder levels
dfga_paired <- dfga_paired %>% 
  mutate(
    param_category = forcats::fct_relevel(param_category, 
                                          "energy levels", "energy gain", "energy loss"
                                          , "others", "revisitation\nperiod", "memory\nreset"
                                          , "fitness"
                                          )
    
  )

dfga_paired$param_category %>% levels()



# Plot grid

dfga_paired %>% 
  ggplot(aes(x = parameter, y = value, group = interaction(month, paired))) +
  geom_errorbar(aes(ymin = min, ymax = max, x = parameter)
                , width = 0.3
                , size = 0.3) +
  geom_point(aes(#x = parameter, y = value
    color = month, alpha = linetype
    , shape = linetype, size = linetype
  )
  # size = 3
  , position =  position_dodge(width = 0.3)
  ) +
  geom_line(aes(color = month
                , linetype = linetype
                , lwd = linetype
                , alpha = linetype
  )
  , position =  position_dodge(width = 0.3)
  ) +
  scale_size_manual(values = c(2, 1)) +
  scale_linewidth_manual(values = c(0.8, 0.4)) +
  scale_alpha_manual(values = c(0.8, 0.5)) +
  scale_shape_manual(values = c(16, 1)) +
  theme(
    axis.text.x = element_text(
      # angle = 90,
      # hjust = 1,
      # size = 10
    )
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    , legend.position = "right"
    # , legend.position = "bottom"
    # , legend.box="vertical", # legend.margin=margin()
  ) +
  # guides(col = guide_legend(nrow = 3)) +
  scale_color_viridis_d() +
  # scale_color_manual(values = c("#C833EC", "#33A4EC")) +
  ggtitle("Optimization of model parameters (best 5)") +
  facet_wrap(~param_category, scales = "free", nrow = 7) +
  coord_flip()

# # Save plot
ggsave(paste0(path, '/02_GA_optimized-params-grid-best5_opt1_dashed.png'),
       height = 15, width = 8)


dfga_paired %>% 
  ggplot(aes(x = parameter, y = value, group = interaction(month, paired))) +
  geom_errorbar(aes(ymin = min, ymax = max, x = parameter)
                , width = 0.3
                , size = 0.3) +
  geom_point(aes(#x = parameter, y = value
                 color = month, alpha = linetype
                 , shape = linetype, size = linetype
  )
  # size = 3
  , position =  position_dodge(width = 0.3)
  ) +
  geom_line(aes(color = month
                , linetype = linetype
                , lwd = linetype
                , alpha = linetype
  )
  , position =  position_dodge(width = 0.3)
  ) +
  scale_size_manual(values = c(2, 1)) +
  scale_linewidth_manual(values = c(0.8, 0.4)) +
  scale_alpha_manual(values = c(0.8, 0.5)) +
  scale_shape_manual(values = c(16, 1)) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 10
    )
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    , legend.position = "bottom"
    # , legend.position = "right"
    # , strip.text = element_text(size = 10)
  ) +
  scale_color_viridis_d() +
  # scale_color_manual(values = c("#C833EC", "#33A4EC")) +
  ggtitle("Optimization of model parameters (best 5)") +
  facet_wrap(~param_category, scales = "free", nrow = 1)# +
  # coord_flip()

# # Save plot
ggsave(paste0(path, '/02_GA_optimized-params-grid-best5_opt2_dashed.png'),
       height = 6, width = 12)

