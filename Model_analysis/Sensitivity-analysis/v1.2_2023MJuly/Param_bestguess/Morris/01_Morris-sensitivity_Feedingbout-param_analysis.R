# Script name: 01_Morris-sensitivity_Feedingbout-param_analysis.R
# Script purpose: Plot Morris analysis from 00_Morris-sensitivity

# Date created: 2023-06-22d
# Author: Eduardo Zanette

## Notes --------------------------- 
#

## Options -------------------------
# (plotting, memory limit, decimal digits)
# 

## Packages -------------------------
library("here")
library("dplyr")
library("nlrx")
# library("magrittr")
library("stringr")
library("ggplot2")
library("Hmisc")
library("ggh4x")

# ggplot theme
theme_set(theme_bw(base_size = 15))
theme_update(
  axis.text.x = element_text(size = 11, angle = 45, vjust = 1, hjust = 0.75) 
)


## Step 1: Create nl object ------------------------ 

if(Sys.info()[["nodename"]] == "DESKTOP-R12V3D6") {
  netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
  # modelpath <- here("Model_development", "BLT_model_v1.2.nlogo")
  modelpath <- here("Model_simulations", "BLT_model_v1.2.nlogo") # Last version with stored-energy
  outpath <- here("Model_analysis", "Sensitivity-analysis", "v1.2_2023MJuly", "Param_bestguess", "Morris", "temp")
  user_scp = "\"Eduardo\""
}
if(Sys.info()[["nodename"]] == "PC9") { # LEEC
  netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
  # modelpath <- here("Model_development", "BLT_model_v1.2.nlogo")
  modelpath <- here("Model_simulations", "BLT_model_v1.2.nlogo") # Last version with stored-energy
  # outpath <- here("Model_analysis", "Sensitivity-analysis", "v1.2_December2022", "temp")
  Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8.0_351")
  user_scp = "\"LEEC\""
}


# Morris analysis for all areas -----

path <- paste0(outpath, "/")



### Grep files -----
nls_to_df <- list.files(path, pattern = "[0-9].rds")

# # Specify one file only:
# nls_to_df <- nls_to_df[2]


n <- 1 #counter
for (f in nls_to_df) {
  
  nl_file <- readRDS(paste0(path, "/", f))
  
  # test loop:
  # print(f)
  # }
  # nl_file <- readRDS(paste0(path, "/", "v1.2_Morris_Taquara_Jan_Feedingbout_on_2023-06-21.rds"))
  # f <- nls_to_df[9]
  # db <- nl_file@simdesign@simoutput

 
  # ### Remove runs where tamarins died (DPL or KDE = 0)
  out <- nl_file@simdesign@simoutput %>%
    dplyr::filter(g_DPL == 0) %>% as_tibble()

  nl_file@simdesign@simoutput <- nl_file@simdesign@simoutput %>%
    dplyr::filter(g_DPL != 0) %>% as_tibble()

  ### Remove runs where tamarins died before finishing the run (energy?)

    
  ### For initializing the first dbl
  if (n == 1) {
    morris_db <- analyze_nl(nl_file, "simoutput") #; rm(nl_file)
    
    # add identifiers
    morris_db$area_run <- nl_file@experiment@constants$study_area %>% gsub(., pattern = ('\"'), replacement = '', fixed = T)
    morris_db$month_run <- nl_file@experiment@constants$`feeding-trees-scenario` %>% gsub(., pattern = ('\"'), replacement = '', fixed = T)
    
    print("--- Run details ---")
    print(morris_db$area_run[1])
    print(morris_db$month_run[1])
    print(paste("feeding bout = ", nl_file@experiment@constants$`feedingbout-on?`))
    
    if (nl_file@experiment@constants$`feedingbout-on?` == "true") {
      morris_db$feedingbout <- "feedingbout on"
    } else {
      morris_db$feedingbout <- "feedingbout off"
    }
    
    # Assign number of unviable runs:
    morris_db$unviable_runs <- nl_file %>% 
      eval_simoutput() %>% 
      nrow()
    morris_db$viable_runs <- nl_file@simdesign@simoutput %>% nrow()
    
    n <- n + 1
    
  } else {
    
    # f <- nls_to_df[2]
    # nl_file <- readRDS(paste0(path, "/", f))
    
    ### Attatch data from following dbl
    morris_n <- analyze_nl(nl_file, "simoutput")
    
    # add identifiers
    morris_n$area_run <- nl_file@experiment@constants$study_area %>% gsub(., pattern = ('\"'), replacement = '', fixed = T)
    morris_n$month_run <- nl_file@experiment@constants$`feeding-trees-scenario` %>% gsub(., pattern = ('\"'), replacement = '', fixed = T)
    
    print("--- Run details ---")
    print(morris_n$area_run[1])
    print(morris_n$month_run[1])
    print(paste("feeding bout = ", nl_file@experiment@constants$`feedingbout-on?`))
    
    if (nl_file@experiment@constants$`feedingbout-on?` == "true") {
      morris_n$feedingbout <- "feedingbout on"
    } else {
      morris_n$feedingbout <- "feedingbout off"
    }
    
    # Assign number of unviable runs:
    morris_n$unviable_runs <- nl_file %>% 
      eval_simoutput() %>% 
      nrow()
    morris_n$viable_runs <- nl_file@simdesign@simoutput %>% nrow()
    
    # Attach
    morris_db <- dplyr::bind_rows(morris_db, morris_n)
    
  }
  
  # morris_db %>% str()
  
  n <- n + 1
}

warnings() #-> sigma values are not estimated correctly

# Missing outputs (probably unviable runs where tamarins die):
# eval_simoutput(nl_file)

### Check missing rows (unviable runs) ----
# check <- eval_simoutput(nl_file)
# check
# 
# # Rerun missing combinations within check tibble:
# rerun <- purrr::map_dfr(seq(nrow(check)), function(x) {
#   res <- run_nl_one(nl, siminputrow=check$siminputrow[x], seed=check$seed[x])
#     return(res)
#     }) %>%
#       dplyr::bind_rows(., nl@@simdesign@@simoutput)

morris_db$unviable_runs %>% unique()
# morris_db %>% ggplot() + 
#   geom_density(aes(x = unviable_runs))



### Recode parameters and Rename columns ----
morris_db <- morris_db %>% 
  dplyr::mutate(parameter =
                str_replace_all(parameter, "-", "_"))

morris_db <- morris_db %>% 
  mutate(parameter = recode(parameter, 
                            "prop_trees_to_reset_memory" = "prop_reset_memory",
                            )
         )

morris_db <- morris_db %>%
  mutate(
    metric = str_replace(metric, "g_", "")
    ) #%>% 
  # mutate(parameter = recode(metric, 
  #                           "p_feedinmean" = "p_feeding", 
  #                           "p_foraging_mean"  = "p_foraging",
  #                           "p_travelinmean" = "p_traveling",
  #                           "p_restinmean" = "p_resting"
  #                           )
  # )
  

### Add categories ----
morris_db$metric %>% unique()

morris_db <- morris_db %>% 
  mutate(
    var_category = case_when(
      metric == "day_mean" ~ "simulation time",
      metric == "timestep_mean" ~ "simulation time",
      
      metric == "SDD_mean" ~ "seed dispersal",
      metric == "SDD_sd_mean" ~ "seed dispersal",
      metric == "NN_seeds_mean" ~ "seed dispersal",
      
      metric == "NN_feeding_trees_mean" ~ "resource aggregation",
      metric == "NN_sleeping_trees_mean" ~ "resource aggregation",
      
      metric == "energy_stored_mean" ~ "energy",
      
      metric == "KDE95" ~ "home range",
      metric == "KDE50" ~ "home range",
      
      metric == "DPL_mean" ~ "movement",
      metric == "DPL_sd_mean" ~ "movement",
      metric == "MR_mean" ~ "movement",
      metric == "MR_sd_mean" ~ "movement",
      metric == "PT_mean" ~ "movement",
      metric == "PT_sd_mean" ~ "movement",
      
      
      metric == "p_feeding_mean" ~ "activity budget",
      metric == "p_foraging_mean" ~ "activity budget",
      metric == "p_traveling_mean" ~ "activity budget",
      metric == "p_resting_mean" ~ "activity budget",

      metric == "step_length_mean_mean" ~ "movement",
      metric == "step_length_sd_mean" ~ "movement",
      metric == "turn_ang_mean_mean" ~ "movement",
      metric == "turn_ang_sd_mean" ~ "movement",
            
      metric == "p-visited-trees_mean" ~ "revisits"
      
     
    )
  )


## Classify parameters:
morris_db$parameter %>% unique()
morris_db <- morris_db %>%
  mutate(
    param_category = case_when(

      parameter == "start_energy" ~ "energy levels",
      parameter == "energy_level_1" ~ "energy levels",
      parameter == "energy_level_2" ~ "energy levels",

      
      parameter == "duration" ~ "resting",
      parameter == "p_timesteps_to_rest" ~ "resting",
      

      parameter == "step_forget" ~ "revisits",
      parameter == "prop_reset_memory" ~ "revisits",
      parameter == "p_disputed_trees" ~ "revisits",

      parameter == "energy_from_fruits" ~ "energy gain/loss",
      parameter == "energy_from_prey" ~ "energy gain/loss",
      parameter == "energy_loss_foraging" ~ "energy gain/loss",
      parameter == "energy_loss_traveling" ~ "energy gain/loss",
      parameter == "energy_loss_resting" ~ "energy gain/loss",
      parameter == "energy_stored_val" ~ "energy gain/loss",
      parameter == "species_time_val" ~ "energy gain/loss"

    )
  )
morris_db$param_category %>% unique()

# morris_db %>% dplyr::filter(is.na(param_category)) %>% View()




### Define area/month order ----
morris_db <- morris_db %>%
  mutate(simulation_scenario = paste0(area_run, "_", month_run)) %>% 
  mutate(
    simulation_scenario = forcats::fct_relevel(simulation_scenario, 
                                               "Suzano_Sep", "Suzano_Dec",
                                               "Guareí_May", "Guareí_Jun", "Guareí_Jul", "Guareí_Aug",
                                               "SantaMaria_Mar", "SantaMaria_Apr",
                                               "Taquara_Jan"
                                               )) 

### Transform output scale ------
morris_db %>% colnames()
morris_db$metric %>% unique()

# morris_db <- morris_db %>% 
#   mutate(metric = case_when(
#     metric == DPL_mean ~ DPL_mean / 1000,
#     metric == DPL_sd_mean ~ DPL_sd_mean / 1000,
#     TRUE ~ 
#   )
#   )


### Transform data ----
a <- morris_db %>% 
  dplyr::filter(metric == "KDE_50_mean")
  

# Now the model outputs the ha values correctly
# morris_db <- morris_db %>% 
#   mutate(
#     value = ifelse(metric == "KDE_50_mean", value/10000, value), # to hectares
#     value = ifelse(metric == "KDE_95_mean", value/10000, value),
#     value = ifelse(metric == "DPL_mean", value/1000, value),     # to km
#     value = ifelse(metric == "DPL_sd_mean", value/1000, value),     
#     value = ifelse(metric == "MR_mean", value/1000, value),     
#     value = ifelse(metric == "MR_sd_mean", value/1000, value)
#   )


### summary with mean + sd or ci
morris_db_summary <- morris_db %>% 
  # dplyr::filter(metric =="KDE_50_mean") %>%
  # tidyr::pivot_wider(names_from = index, values_from = value) %>% 
  group_by(simulation_scenario, feedingbout, metric, parameter, index) %>% 
  summarise(
    mean = mean(value, na.rm=TRUE),
    sd = sd(value, na.rm=TRUE),
    # n = n(),
    # sqrt = sqrt(5), # = morrisr in the nlrx morris design
    var = var(value, na.rm=TRUE),
    # SEM = var/sqrt(5)        # Standard error of the mean as in Morris (1991). 5 = morrisr
    # across(mustar:sigma,
    #        list(
    #          mean = mean,
    #          sd = sd#,
    #          # length = length
    #          # sqrt = sqrt(length)
    #       )
    max = max(value, na.rm=TRUE),
    min = min(value, na.rm=TRUE)
    #       
    
  ) 


### Now use the long format
morris_db_long <- morris_db %>%
  left_join(morris_db_summary) %>%
  tidyr::pivot_wider(names_from = index, values_from = c(value, mean, sd, var
                                                         # , SEM
                                                         ,max, min
                                                         ))


# a <- morris_db_summary %>% 
#   tidyr::pivot_wider(names_from = index, values_from = c(mean, sd, n, sqrt)) #%>% 
#   # left_join(morris_db)
# morris_db_long <- a

# I am not sure about this anymore:
# se <- morris_db_long %>%
#   group_by(simulation_scenario, metric, parameter, feedingbout) %>%
#   summarise(
#     se = na.omit(value_sigma)/sqrt(n())                 # standard error (as in https://www.sciencedirect.com/science/article/pii/S0304380020304592?via%3Dihub#fig0003)
#   )
# 
# morris_db_long <- morris_db_long %>%
#   left_join(se)



## Plots -----

# ### Option 1: boxplots + points
# morris_db %>%
#   dplyr::filter(metric=="KDE_95_mean") %>%
#   # ggplot(aes(x=value, y=parameter, fill = index)) +
#   ggplot(aes(x=log1p(value), y=parameter, fill = index)) + # use log1p instead of log10 # https://stackoverflow.com/questions/48520517/how-to-solve-error-of-log-produces-nans-in-r
#   geom_boxplot(lwd= 0.1) +
#   geom_vline(xintercept = 0) +
#   # facet_grid(~area_run) +
#   facet_nested(feedingbout ~ simulation_scenario) +
#   ggtitle("Morris effects on KDE95") +
#   xlab("log1p(Home range area in ha)") +
#   theme(legend.position="bottom")
# 
# # # Save plot
# # ggsave(paste0(outpath, "/",
# #               '01_Morris_KDE95_option1.png'), height = 7, width = 14, dpi = 600)


# Option 2: as in this publication: https://www.researchgate.net/publication/309185108_Sensitivity_analysis_methods_for_building_energy_models_Comparing_computational_costs_and_extractable_information/figures?lo=1
# strip <- ggh4x::strip_themed(background_x = elem_list_rect(fill = "viridis"))
# set.seed(42)


### Define number of succesful runs: ------------
n_runs <- morris_db_long %>%
  dplyr::select(feedingbout, simulation_scenario, 
                # metric, parameter,
                viable_runs, unviable_runs) %>% 
  distinct()

# aux <- morris_db_long %>%
#   group_by(feedingbout, simulation_scenario,
#            metric, #parameter,
#            viable_runs, unviable_runs) %>%
#   summarise(
#     max_mustar = max(max_mustar),
#     max_mu = max(max_mu),
#     max_sigma = max(max_sigma)
#     )
# 
# n_runs <- left_join(n_runs, aux)







##############################################################################
##############################################################################
##############################################################################

# Plots per area/month ## ----


### SDD ----

morris_db_long %>% 
  dplyr::filter(metric =="SDD_mean") %>%
  ggplot(aes(x=value_mustar, y=value_mu
             # , shape = parameter
             , color = parameter, shape = parameter)) + # position_jitterdodge() requires fill and color
  geom_abline(aes(slope = -1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0, intercept=0), linetype = 2) +
  # geom_abline(aes(slope = SEM_mu, intercept = 0, linetype = 4)) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_mu), size=2.5 #, shape = 20
             , alpha = 0.7
  ) +
  # facet_nested(feedingbout ~ metric+simulation_scenario
  facet_nested(feedingbout ~ simulation_scenario
  # facet_nested(~ feedingbout
               # , scales = "free_x"
               # , scales = "free_y"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  # scale_color_manual(values = rainbow(n = 14)) +
  # scale_color_viridis_d(option = "turbo") +
  # Errorbars:
  # geom_errorbar(aes(ymin=mean_sigma-2*se, ymax=mean_sigma+2*se)
  #               #, position = position_dodge(.9)
  #               , size = 0.5) +
  theme(legend.position="bottom") +
  # theme(strip.background = element_rect(fill=strip))
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris first-order effects on SDD") +
  # xlab("mustar - Home range area (ha)") +
  xlab(expression(paste(mu, "*", " - Seed dispersal distance (m)")))+
  ylab(expression(paste(mu,      " - Seed dispersal distance (m)"))) +
  # ylim(-15, 80) +
  # xlim(0, 80) +
  ggpp::geom_text_npc(data = n_runs, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
                      )
  
# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_SDD_option2_first-order.png'), height = 7, width = 14, dpi = 600)

# Interaction effects
morris_db_long %>% 
  dplyr::filter(metric =="SDD_mean") %>%
  ggplot(aes(x=value_mustar, y=value_sigma
             # , shape = parameter
             , color = parameter, shape = parameter)) + # position_jitterdodge() requires fill and color
  # geom_point(size = 4
  #            #            #, alpha = 0.8, 
  #            #            #, geom = "point",
  #            #            #, position = position_jitterdodge(dodge.width = 0.5, seed = 42
  #            #            #preserve = "single")
  # ) +
  # All observations:
  # geom_point(alpha = 0.8) + 
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.5, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.1, intercept=0), linetype = 2) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_sigma), size=2.5 #, shape = 20
             , alpha = 0.7
  ) +
  # facet_nested(feedingbout ~ metric+simulation_scenario
  facet_nested(feedingbout ~ simulation_scenario
               # , scales = "free_x"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # Errorbars:
  # geom_errorbarh(aes(xmin=mean_mustar-2*se, xmax=mean_mustar+2*se)
  #                #, position = position_dodge(.9)
  #                , size = 0.5) +
  theme(legend.position="bottom") +
  # theme(strip.background = element_rect(fill=strip))
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris interaction effects on SDD") +
  # xlab("mustar - Home range area (ha)") +
  xlab(expression(paste(mu, "*", " - Seed dispersal distance (m)")))+
  ylab(expression(paste(mu,      " - Seed dispersal distance (m)"))) +
  # ylim(0, 100) +
  # xlim(0, 100) +
  ggpp::geom_text_npc(data = n_runs, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
  )

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_SDD_option2_interaction.png'), height = 7, width = 14, dpi = 600)


### Seed dispersal aggregation ---------------

# First order
morris_db_long %>%
  # dplyr::filter(param_category =="energy levels") %>%
  # dplyr::filter(var_category =="resource aggregation") %>%
  dplyr::filter(metric == "NN_seeds_mean" ) %>%
  ggplot(aes(x=value_mustar, y=value_mu, group = parameter, shape = parameter, color = parameter)) +
  # All observations:
  # geom_point(alpha = 0.5, size = 2.5) +
  # Mean values (summarizing n=6):
  geom_point(aes(x=mean_mustar, y=mean_mu), size=2.5#, #shape = 20
             , alpha = 0.7
  ) +
  # add reference lines:
  geom_abline(aes(slope = -1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0, intercept=0), linetype = 2) +
  # facet_nested(feedingbout ~ metric+simulation_scenario) +
  facet_nested(feedingbout ~ simulation_scenario
               # , scales = "free_x"
               ) +
  theme(legend.position="bottom") +
  # add color+shape combination for every parameter:
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # define plot limits:
  # xlim(0, 8) +
  # ylim(-4, 4) +
  ggtitle("Morris first-order effects on Seed aggregation") +
  ylab(expression(paste(mu,      " - Nearest neighbor distance (meters)"))) +
  xlab(expression(paste(mu, "*", " - Nearest neighbor distance (meters)"))) +
  ggpp::geom_text_npc(data = n_runs, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
  )


# # Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_NNseeds_option2_first-order.png'), height = 7, width = 14, dpi = 600)


# Interaction
morris_db_long %>%
  # dplyr::filter(param_category =="energy levels") %>%
  # dplyr::filter(var_category =="resource aggregation") %>%
  dplyr::filter(metric == "NN_seeds_mean" ) %>%
  ggplot(aes(x=value_mustar, y=value_sigma, group = parameter, shape = parameter, color = parameter)) +
  # All observations:
  # geom_point(alpha = 0.5, size = 2.5) +
  # Mean values (summarizing n=6):
  geom_point(aes(x=mean_mustar, y=mean_sigma), size=2.5#, #shape = 20
             , alpha = 0.7
  ) +
  # add reference lines:
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.5, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.1, intercept=0), linetype = 2) +
  # facet_nested(feedingbout ~ metric+simulation_scenario) +
  facet_nested(feedingbout ~ simulation_scenario
               # , scales = "free_x"
               ) +
  theme(legend.position="bottom") +
  # add color+shape combination for every parameter:
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # define plot limits:
  # xlim(0, 8) +
  # ylim(0, 10) +
  ggtitle("Morris interaction effects on Seed aggregation") +
  ylab(expression(paste(sigma, " - Nearest neighbor distance (meters)"))) +
  xlab(expression(paste(mu, "*", " - Nearest neighbor distance (meters)"))) +
  ggpp::geom_text_npc(data = n_runs, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
  )


# # Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_NNseeds_option2_interaction.png'), height = 7, width = 14, dpi = 600)




### KDE50 ------------------------

# First order
morris_db_long %>% 
  dplyr::filter(metric =="KDE_50_mean") %>%
  # mutate(
    # mean_mustar = log10(mean_mustar)
    # mean_mu = log(mean_mu)
  # ) %>%
  ggplot(aes(x=value_mustar, y=value_mu
             , color = parameter, shape = parameter)) + # position_jitterdodge() requires fill and color
  geom_abline(aes(slope = -1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0, intercept=0), linetype = 2) +
  geom_point(aes(x=mean_mustar, y=mean_mu), size=2.5 #, shape = 20
             , alpha = 0.7
             ) +
  # facet_nested(feedingbout ~ metric+simulation_scenario
  facet_nested(feedingbout ~ simulation_scenario
               , scales = "free_x"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  theme(legend.position="bottom") +
  ggtitle("Morris first-order effects on KDE50") +
  xlab(expression(paste(mu, "*", " - Core UD area (ha)")))+
  ylab(expression(paste(mu,      " - Core UD area (ha)"))) +
  # xlim(0, 15) +
  # ylim(0, 15)
  ggpp::geom_text_npc(data = n_runs, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
  )

# # Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_KDE50_option2_first-order.png'), height = 7, width = 14, dpi = 600)


### ggsave('C:/Users/eduar/Desktop/Sensitivity_analysis_bkp/01_Morris_KDE50_option2_first-order.png',
###        height = 7, width = 14, dpi = 600)



# Interaction effects
morris_db_long %>% 
  dplyr::filter(metric =="KDE_50_mean") %>%
  ggplot(aes(x=value_mustar, y=value_sigma
             # , shape = parameter
             , color = parameter, shape = parameter)) + # position_jitterdodge() requires fill and color
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.5, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.1, intercept=0), linetype = 2) +
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_sigma), size=2.5 #, shape = 20
             , alpha = 0.7
  ) +
  # facet_nested(feedingbout ~ metric+simulation_scenario
  facet_nested(feedingbout ~ simulation_scenario
               , scales = "free_x"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  theme(legend.position="bottom") +
  ggtitle("Morris interaction effects on KDE50") +
  xlab(expression(paste(mu, "*", " - Core UD area (ha)"))) +
  ylab(expression(paste(sigma, " - Core UD area (ha)"))) +
  # xlim(0, 15) +
  # ylim(0, 15)
  ggpp::geom_text_npc(data = n_runs, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
  )

# # Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_KDE50_option2_interaction.png'), height = 7, width = 14, dpi = 600)


### KDE95 -----------------------

# First order
morris_db_long %>% 
  dplyr::filter(metric =="KDE_95_mean") %>%
  ggplot(aes(x=value_mustar, y=value_mu
             # , shape = parameter
             , color = parameter, shape = parameter)) + # position_jitterdodge() requires fill and color
  geom_abline(aes(slope = -1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0, intercept=0), linetype = 2) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_mu), size=2.5 #,shape = 20
             , alpha = 0.7
  ) +
  # facet_nested(feedingbout ~ metric+simulation_scenario
  facet_nested(feedingbout ~ simulation_scenario
               , scales = "free_x"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  theme(legend.position="bottom") +
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris first-order effects on KDE95") +
  # xlab("mustar - Home range area (ha)") +
  xlab(expression(paste(mu, "*", " - Total UD area (ha)"))) +
  ylab(expression(paste(mu,      " - Total UD area (ha)"))) +
  # xlim(0, 250) +
  # ylim(-250, 150)
  ggpp::geom_text_npc(data = n_runs, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
  )
  
# # Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_KDE95_option2_first-order.png'), height = 7, width = 14, dpi = 600)


# Interaction effects
morris_db_long %>% 
  dplyr::filter(metric =="KDE_95_mean") %>%
  ggplot(aes(x=value_mustar, y=value_sigma
             , color = parameter, shape = parameter)) + 
  # All observations:
  # geom_point(alpha = 0.8) + 
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.5, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.1, intercept=0), linetype = 2) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_sigma), size=2.5#shape = 20
             , alpha = 0.7
  ) +
  # facet_nested(feedingbout ~ metric+simulation_scenario
  facet_nested(feedingbout ~ simulation_scenario
               , scales = "free_x"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  theme(legend.position="bottom") +
  # theme(strip.background = element_rect(fill=strip))
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris interaction effects on KDE95") +
  xlab(expression(paste(mu, "*", " - Total UD area (ha)")))+
  ylab(expression(paste(sigma, " - Total UD area (ha)"))) +
  # xlim(0, 45) +
  # ylim(0, 45)
  ggpp::geom_text_npc(data = n_runs, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
  )

# # Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_KDE95_option2_interaction.png'), height = 7, width = 14, dpi = 600)




## Movement patterns ------------

### DPL -------------------------

# First order
morris_db_long %>%
  # dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(var_category =="movement") %>%
  dplyr::filter(metric == "DPL_mean") %>%
  # dplyr::filter(index =="mustar" | index =="mu") %>%
  ggplot(aes(x=value_mustar, y=value_mu, shape = parameter, color = parameter)) +
  # All observations:
  # geom_point(alpha = 0.5, size = 2.5) +
  # Mean values (summarizing n=6):
  geom_point(aes(x=mean_mustar, y=mean_mu), size=2.5#, #shape = 20
             , alpha = 0.7
  ) +
  # add reference lines:
  geom_abline(aes(slope = -1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0, intercept=0), linetype = 2) +
  facet_nested(feedingbout ~ simulation_scenario
               # , scales = "free_x"
               ) +
  ggtitle("Morris first-order effects on DPL") +
  xlab(expression(paste(mu, "*", " - DPL (m)"))) +
  ylab(expression(paste(mu,      " - DPL (m)"))) +
  # add color+shape combination for every parameter:
  theme(legend.position="bottom") +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter)))+ # add this to take shape + color into account (too many categories)
  scale_color_manual(values = rainbow(n = 14)) +
  # define plot limits:
  # xlim(0, 600) +
  # ylim(0, 150) +
  ggpp::geom_text_npc(data = n_runs, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
  )

# # Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_DPL_option2_first-order.png'), height = 7, width = 14, dpi = 600)

# Interaction
morris_db_long %>%
  # dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(var_category =="movement") %>%
  dplyr::filter(metric == "DPL_mean") %>%
  # dplyr::filter(index =="mustar" | index =="mu") %>%
  ggplot(aes(x=value_mustar, y=value_sigma, shape = parameter, color = parameter)) +
  # All observations:
  # geom_point(alpha = 0.5, size = 2.5) +
  # Mean values (summarizing n=6):
    geom_point(aes(x=mean_mustar, y=mean_sigma), size=2.5#, #shape = 20
               , alpha = 0.7
    ) +
  # add reference lines:
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.5, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.1, intercept=0), linetype = 2) +
  facet_nested(feedingbout ~ simulation_scenario) +
  ggtitle("Morris interaction effects on DPL") +
  xlab(expression(paste(mu, "*", " - DPL (m)"))) +
  ylab(expression(paste(sigma, " - DPL (m)"))) +
  theme(legend.position="bottom") +
  # add color+shape combination for every parameter:
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter)))+ #+ # add this to take shape + color into account (too many categories)
  scale_color_manual(values = rainbow(n = 14)) +
  # define plot limits:
  # xlim(0, 600) +
  # ylim(0, 800) +
  ggpp::geom_text_npc(data = n_runs, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
  )

# # Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_DPL_option2_interaction.png'), height = 7, width = 14, dpi = 600)


  
### MR ----------------------

# First order
morris_db_long %>%
  # dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(var_category =="movement") %>%
  dplyr::filter(metric == "MR_mean") %>%
  ggplot(aes(x=value_mustar, y=value_mu, group = parameter, shape = parameter, color = parameter)) +
  # All observations:
  # geom_point(alpha = 0.5, size = 2.5) +
  # Mean values (summarizing n=6):
  geom_point(aes(x=mean_mustar, y=mean_mu), size=2.5#, #shape = 20
             , alpha = 0.7
  ) +
  # add reference lines:
  geom_abline(aes(slope = -1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0, intercept=0), linetype = 2) +
  # facet_nested(feedingbout ~ metric+simulation_scenario) +
  facet_nested(feedingbout ~ simulation_scenario
               # , scales = "free_x"
               ) +
  theme(legend.position="bottom") +
  # add color+shape combination for every parameter:
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # define plot limits:
  # xlim(0, 60) +
  # ylim(0, 15) +
  xlab(expression(paste(mu, "*", " - MR (m/hours active)"))) +
  ylab(expression(paste(mu, " - MR (m/hours active)"))) +
  ggtitle("Morris first-order effects on Movement rate") +
  ggpp::geom_text_npc(data = n_runs, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
  )

# # Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_MR_option2_first-order.png'), height = 7, width = 14, dpi = 600)


# Interaction
morris_db_long %>%
  # dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(var_category =="movement") %>%
  dplyr::filter(metric == "MR_mean") %>%
  ggplot(aes(x=value_mustar, y=value_sigma, group = parameter, shape = parameter, color = parameter)) +
  # All observations:
  # geom_point(alpha = 0.5, size = 2.5) +
  # Mean values (summarizing n=6):
  geom_point(aes(x=mean_mustar, y=mean_sigma), size=2.5#, #shape = 20
             , alpha = 0.7
  ) +
  # add reference lines:
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.5, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.1, intercept=0), linetype = 2) +
  # facet_nested(feedingbout ~ metric+simulation_scenario) +
  facet_nested(feedingbout ~ simulation_scenario
               , scales = "free_x"
               ) +
  theme(legend.position="bottom") +
  # add color+shape combination for every parameter:
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # define plot limits:
  # xlim(0, 60) +
  # ylim(0, 70) +
  xlab(expression(paste(mu, "*", " - MR (m/hours active)"))) +
  ylab(expression(paste(sigma, " - MR (m/hours active)"))) +
  ggtitle("Morris interaction effects on Movement rate") +
  ggpp::geom_text_npc(data = n_runs, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
  )
  
# # Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_MR_option2_interaction.png'), height = 7, width = 14, dpi = 600)


### PT -----------------------

# First order
morris_db_long %>%
  # dplyr::filter(param_category =="energy levels") %>%
  # dplyr::filter(var_category =="movement") %>%
  dplyr::filter(metric == "PT_mean") %>%
  ggplot(aes(x=value_mustar, y=value_mu, group = parameter, shape = parameter, color = parameter)) +
  # All observations:
  # geom_point(alpha = 0.5, size = 2.5) +
  # Mean values (summarizing n=6):
  geom_point(aes(x=mean_mustar, y=mean_mu), size=2.5#, #shape = 20
             , alpha = 0.7
  ) +
  # add reference lines:
  geom_abline(aes(slope = -1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0, intercept=0), linetype = 2) +
  # facet_nested(feedingbout ~ metric+simulation_scenario
  facet_nested(feedingbout ~ simulation_scenario
               # , scales = "free_x"
               ) +
  theme(legend.position="bottom") +
  # add color+shape combination for every parameter:
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # define plot limits:
  # xlim(0, 0.01) +
  # ylim(0, 0.01) +
  ggtitle("Morris first-order effects on Path twisting") +
  xlab(expression(paste(mu, "*", " - PT (unitless)"))) +
  ylab(expression(paste(mu,      " - PT (unitless)"))) +
  ggpp::geom_text_npc(data = n_runs, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
  )

# # Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_PT_option2_first-order.png'), height = 7, width = 14, dpi = 600)

# Interaction
morris_db_long %>%
  # dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(var_category =="movement") %>%
  dplyr::filter(metric == "PT_mean") %>%
  ggplot(aes(x=value_mustar, y=value_sigma, group = parameter, shape = parameter, color = parameter)) +
  # All observations:
  # geom_point(alpha = 0.5, size = 2.5) +
  # Mean values (summarizing n=6):
  geom_point(aes(x=mean_mustar, y=mean_sigma), size=2.5#, #shape = 20
             , alpha = 0.7
  ) +
  # add reference lines:
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.5, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.1, intercept=0), linetype = 2) +
  # facet_nested(feedingbout ~ metric+simulation_scenario
  facet_nested(feedingbout ~ simulation_scenario
               , scales = "free_x"
               ) +
  theme(legend.position="bottom") +
  # add color+shape combination for every parameter:
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # define plot limits:
  # xlim(0, 0.01) +
  # ylim(0, 0.01) +
  ggtitle("Morris interaction effects on Path twisting") +
  xlab(expression(paste(mu, "*", " - PT (unitless)"))) +
  ylab(expression(paste(sigma, " - PT (unitless)"))) +
  ggpp::geom_text_npc(data = n_runs, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
  )

# # Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_PT_option2_interaction.png'), height = 7, width = 14, dpi = 600)






### Activity budget -------------

# First order
morris_db_long %>%
  # dplyr::filter(param_category =="energy levels") %>%
  # dplyr::filter(var_category =="activity budget") %>%
  dplyr::filter(metric == "p_feeding_mean" | metric == "p_foraging_mean" | metric == "p_resting_mean" | metric == "p_traveling_mean") %>%
  ggplot(aes(x=value_mustar, y=value_mu, shape = parameter, color = parameter)) +
  # All observations:
  # geom_point(alpha = 0.5, size = 2.5) +
  # Mean values (summarizing n=6):
  geom_point(aes(x=mean_mustar, y=mean_mu), size=2.5#, #shape = 20
             , alpha = 0.7
  ) +
  # add reference lines:
  geom_abline(aes(slope = -1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0, intercept=0), linetype = 2) +
  facet_nested(metric+feedingbout ~ simulation_scenario
               # , scales = "free_x"
               ) +
  # facet_nested(simulation_scenario ~ metric+feedingbout) +
  theme(legend.position="bottom") +
  # add color+shape combination for every parameter:
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  theme(
    axis.text.x = element_text(
      # angle = 90,
      # hjust = 1,
      size = 12
    )) +
  # define plot limits:
  # xlim(0, 0.45) +
  # ylim(0, 0.45) +
  ggtitle("Morris first-order effects on Activity budget") +
  xlab(expression(paste(mu, "*", " - Percentage of activity budget"))) +
  ylab(expression(paste(mu,      " - Percentage of activity budget"))) #+
  # ggpp::geom_text_npc(data = n_runs, aes(label=paste("n=", viable_runs)), 
  #                     npcx = "center", npcy = "top"
  # )
  # 

# # Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_Activity-budget_option2_first-order.png'), height = 12.5, width = 12.5, dpi = 600)



# Interaction
morris_db_long %>%
  # dplyr::filter(param_category =="energy levels") %>%
  # dplyr::filter(var_category =="activity budget") %>%
  dplyr::filter(metric == "p_feeding_mean" | metric == "p_foraging_mean" | metric == "p_resting_mean" | metric == "p_traveling_mean") %>%
  ggplot(aes(x=value_mustar, y=value_sigma, shape = parameter, color = parameter)) +
  # All observations:
  # geom_point(alpha = 0.5, size = 2.5) +
  # Mean values (summarizing n=6):
  geom_point(aes(x=mean_mustar, y=mean_sigma), size=2.5#, #shape = 20
             , alpha = 0.7
  ) +
  # add reference lines:
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.5, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.1, intercept=0), linetype = 2) +
  facet_nested(metric+feedingbout ~ simulation_scenario
               # , scales = "free_x"
               ) +
  # facet_nested(simulation_scenario ~ metric+feedingbout) +
  theme(legend.position="bottom") +
  # add color+shape combination for every parameter:
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # define plot limits:
  # xlim(0, 0.45) +
  # ylim(0, 0.45) +
  ggtitle("Morris interaction effects on Activity budget") +
  xlab(expression(paste(mu, "*", " - Percentage of activity budget"))) +
  ylab(expression(paste(mu,      " - Percentage of activity budget"))) #+
  # ggpp::geom_text_npc(data = n_runs, aes(label=paste("n=", viable_runs)), 
  #                     npcx = "center", npcy = "top"
  # )
  # 

# # Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_Activity-budget_option2_interaction.png'), height = 14, width = 14, dpi = 600)



## Survival --------------------

### Simulation time ------------


# First order
morris_db_long %>%
  # dplyr::filter(param_category =="energy levels") %>%
  # dplyr::filter(var_category =="simulation time") %>%
  dplyr::filter(metric == "timestep_mean") %>%
  ggplot(aes(x=value_mustar, y=value_mu, group = parameter, shape = parameter, color = parameter)) +
  # All observations:
  # geom_point(alpha = 0.5, size = 2.5) +
  # Mean values (summarizing n=6):
  geom_point(aes(x=mean_mustar, y=mean_mu), size=2.5#, #shape = 20
             , alpha = 0.7
  ) +
  # add reference lines:
  geom_abline(aes(slope = -1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0, intercept=0), linetype = 2) +
  
  # facet_nested(feedingbout ~ metric+simulation_scenario) +
  facet_nested(feedingbout ~ simulation_scenario
               # ,scales = "free_x"
  ) +
  theme(legend.position="bottom") +
  # add color+shape combination for every parameter:
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # define plot limits:
  # xlim(0, 0.8) +
  # ylim(0, 1) +
  ggtitle("Morris first-order effects on activity period") +
  xlab(expression(paste(mu, "*", " - hours active"))) +
  ylab(expression(paste(mu,      " - hours active"))) +
  ggpp::geom_text_npc(data = n_runs, aes(label=paste("n=", viable_runs)),
                      npcx = "center", npcy = "top"
  )


# # Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_Hours-active_option2_first-order.png'), height = 7, width = 14, dpi = 600)



# Interaction
morris_db_long %>%
  # dplyr::filter(param_category =="energy levels") %>%
  # dplyr::filter(var_category =="simulation time") %>%
  dplyr::filter(metric == "timestep_mean") %>%
  ggplot(aes(x=value_mustar, y=value_sigma, group = parameter, shape = parameter, color = parameter)) +
  # All observations:
  # geom_point(alpha = 0.5, size = 2.5) +
  # Mean values (summarizing n=6):
  geom_point(aes(x=mean_mustar, y=mean_sigma), size=2.5#, #shape = 20
             , alpha = 0.7
  ) +
  # add reference lines:
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.5, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.1, intercept=0), linetype = 2) +
  # facet_nested(feedingbout ~ metric+simulation_scenario) +
  facet_nested(feedingbout ~ simulation_scenario
               # ,scales = "free_x"
               ) +
  theme(legend.position="bottom") +
  # add color+shape combination for every parameter:
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # define plot limits:
  # xlim(0, 0.8) +
  # ylim(0, 1) +
  ggtitle("Morris interaction effects on activity period") +
  xlab(expression(paste(mu, "*", " - hours active"))) +
  ylab(expression(paste(sigma, " - hours active"))) +
  ggpp::geom_text_npc(data = n_runs, aes(label=paste("n=", viable_runs)),
                      npcx = "center", npcy = "top"
  )

# # Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_Hours-active_option2_interaction.png'), height = 7, width = 14, dpi = 600)




### Stored energy -------------

morris_db_long %>%
  # dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(var_category =="energy") %>%
  # dplyr::filter(metric == "p_feeding_mean" | metric == "p_foraging_mean" | metric == "p_resting_mean" | metric == "p_traveling_mean") %>%
  ggplot(aes(x=value_mustar, y=value_mu, group = parameter, shape = parameter, color = parameter)) +
  # All observations:
  # geom_point(alpha = 0.5, size = 2.5) +
  # Mean values (summarizing n=6):
  geom_point(aes(x=mean_mustar, y=mean_mu), size=2.5#, #shape = 20
             , alpha = 0.7
  ) +
  
  # add reference lines:
  geom_abline(aes(slope = -1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0, intercept=0), linetype = 2) +
  # facet_nested(feedingbout ~ metric+simulation_scenario
  facet_nested(feedingbout ~ simulation_scenario
               # , scales = "free_x"
               ) +
  theme(legend.position="bottom") +
  # add color+shape combination for every parameter:
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  theme(
    axis.text.x = element_text(
      # angle = 90,
      # hjust = 1,
      size = 12
    )) +
  # define plot limits:
  # xlim(0, 0.75) +
  # ylim(0, 0.75) +
  ggtitle("Morris first-order effects on stored energy") +
  xlab(expression(paste(mu, "*", " - energy (unitless)"))) +
  ylab(expression(paste(mu,      " - energy (unitless)"))) +
  ggpp::geom_text_npc(data = n_runs, aes(label=paste("n=", viable_runs)),
                      npcx = "center", npcy = "top"
  )


# # Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_Stored-energy_option2_first-order.png'), height = 7, width = 14, dpi = 600)


# Interaction
morris_db_long %>%
  # dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(var_category =="energy") %>%
  # dplyr::filter(metric == "p_feeding_mean" | metric == "p_foraging_mean" | metric == "p_resting_mean" | metric == "p_traveling_mean") %>%
  ggplot(aes(x=value_mustar, y=value_sigma, group = parameter, shape = parameter, color = parameter)) +
  # All observations:
  # geom_point(alpha = 0.5, size = 2.5) +
  # Mean values (summarizing n=6):
  geom_point(aes(x=mean_mustar, y=mean_sigma), size=2.5#, #shape = 20
             , alpha = 0.7
  ) +
  
  # add reference lines:
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.5, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.1, intercept=0), linetype = 2) +
  # facet_nested(feedingbout ~ metric+simulation_scenario
  facet_nested(feedingbout ~ simulation_scenario
               # , scales = "free_x"
               ) +
  theme(legend.position="bottom") +
  # add color+shape combination for every parameter:
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  theme(
    axis.text.x = element_text(
      # angle = 90,
      # hjust = 1,
      size = 12
    )) +
  # define plot limits:
  # xlim(0, 0.75) +
  # ylim(0, 0.75) +
  ggtitle("Morris interaction effects on stored energy") +
  xlab(expression(paste(mu, "*", " - energy (unitless)"))) +
  ylab(expression(paste(sigma, " - energy (unitless)"))) +
  ggpp::geom_text_npc(data = n_runs, aes(label=paste("n=", viable_runs)),
                      npcx = "center", npcy = "top"
  )


# # Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_Stored-energy_option2_interaction.png'), height = 7, width = 14, dpi = 600)



### Revisits ---------------------

# First order
morris_db_long %>%
  # dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(var_category =="revisits") %>%
  # dplyr::filter(metric == "p_feeding_mean" | metric == "p_foraging_mean" | metric == "p_resting_mean" | metric == "p_traveling_mean") %>%
  ggplot(aes(x=value_mustar, y=value_mu, group = parameter, shape = parameter, color = parameter)) +
  # All observations:
  # geom_point(alpha = 0.5, size = 2.5) +
  # Mean values (summarizing n=6):
  geom_point(aes(x=mean_mustar, y=mean_mu), size=2.5#, #shape = 20
             , alpha = 0.7
  ) +
  # add reference lines:
  geom_abline(aes(slope = -1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0, intercept=0), linetype = 2) +
  facet_nested(feedingbout ~ simulation_scenario
               # , scales = "free_x"
  ) +
  theme(legend.position="bottom") +
  theme(
    axis.text.x = element_text(
      # angle = 90,
      # hjust = 1,
      size = 12
    )) +
  # add color+shape combination for every parameter:
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # define plot limits:
  # xlim(0, 0.5) +
  # ylim(0, 0.5) +
  ggtitle("Morris first-order effects on Proportion of visited trees") +
  xlab(expression(paste(mu, "*",      " - proportion of visited trees (%)"))) +
  ylab(expression(paste(mu, " - proportion of visited trees (%)"))) +
  ggpp::geom_text_npc(data = n_runs, aes(label=paste("n=", viable_runs)),
                      npcx = "center", npcy = "top"
  )


# # Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_P-visited-trees_option2_first-order.png'), height = 7, width = 14, dpi = 600)




# Interaction
morris_db_long %>%
  # dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(var_category =="revisits") %>%
  # dplyr::filter(metric == "p_feeding_mean" | metric == "p_foraging_mean" | metric == "p_resting_mean" | metric == "p_traveling_mean") %>%
  ggplot(aes(x=value_mustar, y=value_sigma, group = parameter, shape = parameter, color = parameter)) +
  # All observations:
  # geom_point(alpha = 0.5, size = 2.5) +
  # Mean values (summarizing n=6):
  geom_point(aes(x=mean_mustar, y=mean_sigma), size=2.5#, #shape = 20
             , alpha = 0.7
  ) +
  # add reference lines:
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.5, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.1, intercept=0), linetype = 2) +
  facet_nested(feedingbout ~ simulation_scenario
               # , scales = "free_x"
               ) +
  theme(legend.position="bottom") +
  theme(
    axis.text.x = element_text(
      # angle = 90,
      # hjust = 1,
      size = 12
    )) +
  # add color+shape combination for every parameter:
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # define plot limits:
  # xlim(0, 0.5) +
  # ylim(0, 0.5) +
  ggtitle("Morris interaction effects on Proportion of visited trees") +
  xlab(expression(paste(mu, "*", " - proportion of visited trees (%)"))) +
  ylab(expression(paste(sigma, " - proportion of visited trees (%)"))) +
  ggpp::geom_text_npc(data = n_runs, aes(label=paste("n=", viable_runs)),
                      npcx = "center", npcy = "top"
  )


# # Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_P-visited-trees_option2_interaction.png'), height = 7, width = 14, dpi = 600)






##############################################################################
##############################################################################
##############################################################################

# Plots per parameter category ## ----

## Use mean effects of all areas/month:

# bkp <- morris_db_long
# morris_db_long <- bkp

morris_db_long_avg <- morris_db_long %>% 
  group_by(metric, feedingbout, param_category, parameter) %>% 
  summarise(
    mean_mu = mean(value_mu),
    sd_mu = sd(value_mu),
    mean_mustar = mean(value_mustar),
    sd_mustar = sd(value_mustar),
    mean_sigma = mean(value_sigma),
    sd_sigma = sd(value_sigma),
    
    # standard error (SEM)
    n = n(),
    se_mu = sd_mu/sqrt(n),                
    se_mustar = sd_mustar/sqrt(n),        
    se_sigma = sd_sigma/sqrt(n)
  )

# Define number of succesful runs:
n_runs_avg <- n_runs %>% 
  group_by(feedingbout) %>% 
  summarise(
    viable_runs = sum(viable_runs),
    unviable_runs = sum(unviable_runs)
  )

## Use mean effects per areas/month:
morris_db_long_areas <- morris_db_long %>% 
  group_by(simulation_scenario, feedingbout, metric, param_category, parameter) %>% 
  summarise(
    mean_mu = mean(value_mu),
    sd_mu = sd(value_mu),
    mean_mustar = mean(value_mustar),
    sd_mustar = sd(value_mustar),
    mean_sigma = mean(value_sigma),
    sd_sigma = sd(value_sigma),
    
    # standard error (SEM)
    n = n(),
    se_mu = sd_mu/sqrt(n),                
    se_mustar = sd_mustar/sqrt(n),        
    se_sigma = sd_sigma/sqrt(n)
  )

### Define number of succesful runs: ------------
n_runs_areas <- n_runs %>% 
  group_by(feedingbout) %>% 
  summarise(
    viable_runs = sum(viable_runs),
    unviable_runs = sum(unviable_runs)
  )


### SDD ----

morris_db_long_areas %>% 
  dplyr::filter(metric =="SDD_mean") %>%
  ggplot(aes(x=value_mustar, y=value_mu
  # ggplot(aes(x=mean_mustar, y=mean_mu
             , color = parameter
             , shape = parameter
             )) + # position_jitterdodge() requires fill and color
  geom_abline(aes(slope = -1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0, intercept=0), linetype = 2) +
  # geom_abline(aes(slope = SEM_mu, intercept = 0, linetype = 4)) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_mu), size=2.5 #, shape = 20
  # geom_point(aes(x=value_mustar, y=value_mu), size=2.5 #, shape = 20
             , alpha = 0.7
  ) +
  # facet_nested(feedingbout ~ metric+simulation_scenario
  facet_nested(feedingbout ~ param_category
  # facet_nested(~feedingbout
               # facet_nested(~ feedingbout
               # , scales = "free_x"
               # , scales = "free_y"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # scale_color_viridis_d(option = "turbo") +
  theme(legend.position="bottom") +
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris first-order effects on SDD") +
  # xlab("mustar - Home range area (ha)") +
  xlab(expression(paste(mu, "*", " - Seed dispersal distance (m)"))) +
  ylab(expression(paste(mu,      " - Seed dispersal distance (m)"))) +
  ggpp::geom_text_npc(data = n_runs_areas, aes(label=paste("n=", viable_runs)),
                      npcx = "center", npcy = "top"
  )

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_SDD_option2_first-order_paramcategory.png'), height = 7, width = 10, dpi = 600)



# Interaction effects
morris_db_long_areas %>% 
  dplyr::filter(metric =="SDD_mean") %>%
  ggplot(aes(x=mean_mustar, y=mean_sigma
             , color = parameter
             , shape = parameter
             )) + # position_jitterdodge() requires fill and color
  # All observations:
  # geom_point(alpha = 0.8) + 
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.5, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.1, intercept=0), linetype = 2) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_sigma), size=2.5 #, shape = 20
             , alpha = 0.7
  ) +
  facet_nested(feedingbout ~ param_category
               # , scales = "free_x"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  theme(legend.position="bottom") +
  # theme(strip.background = element_rect(fill=strip))
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris interaction effects on SDD") +
  # xlab("mustar - Home range area (ha)") +
  xlab(expression(paste(mu, "*", " - Seed dispersal distance (m)")))+
  ylab(expression(paste(sigma,      " - Seed dispersal distance (m)"))) +
  # ylim(0, 100) +
  # xlim(0, 100) +
  ggpp::geom_text_npc(data = n_runs_areas, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
  ) #+
  # xlim(0, 100) +
  # ylim(0, 100)

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_SDD_option2_interaction_paramcategory.png'), height = 7, width = 10, dpi = 600)


# First order effects (average)
morris_db_long_avg %>% 
  dplyr::filter(metric =="SDD_mean") %>%
  # ggplot(aes(x=value_mustar, y=value_mu
  ggplot(aes(x=mean_mustar, y=mean_mu
             , color = parameter
             , shape = parameter
  )) + # position_jitterdodge() requires fill and color
  geom_abline(aes(slope = -1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0, intercept=0), linetype = 2) +
  # geom_abline(aes(slope = SEM_mu, intercept = 0, linetype = 4)) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_mu), size=2.5 #, shape = 20
             # geom_point(aes(x=value_mustar, y=value_mu), size=2.5 #, shape = 20
             # , alpha = 0.7
  ) +
  # facet_nested(feedingbout ~ metric+simulation_scenario
  facet_nested(feedingbout ~ param_category
               # facet_nested(~feedingbout
               # facet_nested(~ feedingbout
               # , scales = "free_x"
               # , scales = "free_y"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # scale_color_viridis_d(option = "turbo") +
  # Errorbars:
  # geom_errorbar(aes(ymin=mean_mu-sd_mu, ymax=mean_mu+sd_mu)
  geom_errorbar(aes(ymin=mean_mu-se_mu, ymax=mean_mu+se_mu)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  # geom_errorbar(aes(xmin=mean_mustar-sd_mustar, xmax=mean_mustar+sd_mustar)
  geom_errorbar(aes(xmin=mean_mustar-se_mustar, xmax=mean_mustar+se_mustar)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  theme(legend.position="bottom") +
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris first-order effects on SDD") +
  # xlab("mustar - Home range area (ha)") +
  xlab(expression(paste(mu, "*", " - Seed dispersal distance (m)"))) +
  ylab(expression(paste(mu,      " - Seed dispersal distance (m)"))) +
  ggpp::geom_text_npc(data = n_runs_avg, aes(label=paste("n=", viable_runs)),
                      npcx = "center", npcy = "top"
  )

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_SDD_option2_first-order_paramcategory_average.png'), height = 7, width = 12, dpi = 600)



# Interaction effects (average)
morris_db_long_avg %>% 
  dplyr::filter(metric =="SDD_mean") %>%
  ggplot(aes(x=mean_mustar, y=mean_sigma
             , color = parameter
             , shape = parameter
  )) + # position_jitterdodge() requires fill and color
  # All observations:
  # geom_point(alpha = 0.8) + 
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.5, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.1, intercept=0), linetype = 2) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_sigma), size=2.5 #, shape = 20
             # , alpha = 0.7
  ) +
  facet_nested(feedingbout ~ param_category
               # , scales = "free_x"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # Errorbars:
  geom_errorbar(aes(ymin=mean_sigma-se_sigma, ymax=mean_sigma+se_sigma)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  # geom_errorbar(aes(xmin=mean_mustar-sd_mustar, xmax=mean_mustar+sd_mustar)
  geom_errorbar(aes(xmin=mean_mustar-se_mustar, xmax=mean_mustar+se_mustar)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  theme(legend.position="bottom") +
  # theme(strip.background = element_rect(fill=strip))
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris interaction effects on SDD") +
  # xlab("mustar - Home range area (ha)") +
  xlab(expression(paste(mu, "*", " - Seed dispersal distance (m)")))+
  ylab(expression(paste(sigma,      " - Seed dispersal distance (m)"))) +
  ylim(50, 100) +
  xlim(50, 100) +
  ggpp::geom_text_npc(data = n_runs_avg, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
  ) #+
# xlim(0, 100) +
# ylim(0, 100)

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_SDD_option2_interaction_paramcategory_average.png'), height = 7, width = 12, dpi = 600)



### Seed aggregation ----


# First order effects (average)
morris_db_long_avg %>% 
  dplyr::filter(metric =="NN_seeds_mean") %>%
  # ggplot(aes(x=value_mustar, y=value_mu
  ggplot(aes(x=mean_mustar, y=mean_mu
             , color = parameter
             , shape = parameter
  )) + # position_jitterdodge() requires fill and color
  geom_abline(aes(slope = -1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0, intercept=0), linetype = 2) +
  # geom_abline(aes(slope = SEM_mu, intercept = 0, linetype = 4)) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_mu), size=2.5 #, shape = 20
             # geom_point(aes(x=value_mustar, y=value_mu), size=2.5 #, shape = 20
             # , alpha = 0.7
  ) +
  # facet_nested(feedingbout ~ metric+simulation_scenario
  facet_nested(feedingbout ~ param_category
               # facet_nested(~feedingbout
               # facet_nested(~ feedingbout
               # , scales = "free_x"
               # , scales = "free_y"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # scale_color_viridis_d(option = "turbo") +
  # Errorbars:
  # geom_errorbar(aes(ymin=mean_mu-sd_mu, ymax=mean_mu+sd_mu)
  geom_errorbar(aes(ymin=mean_mu-se_mu, ymax=mean_mu+se_mu)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  # geom_errorbar(aes(xmin=mean_mustar-sd_mustar, xmax=mean_mustar+sd_mustar)
  geom_errorbar(aes(xmin=mean_mustar-se_mustar, xmax=mean_mustar+se_mustar)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  theme(legend.position="bottom") +
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris first-order effects on Seed aggregation") +
  ylab(expression(paste(mu,      " - Nearest neighbor distance (meters)"))) +
  xlab(expression(paste(mu, "*", " - Nearest neighbor distance (meters)"))) +
  ggpp::geom_text_npc(data = n_runs_avg, aes(label=paste("n=", viable_runs)),
                      npcx = "center", npcy = "top"
  )

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_NNseeds_option2_first-order_paramcategory_average.png'), height = 7, width = 12, dpi = 600)



# Interaction effects (average)
morris_db_long_avg %>% 
  dplyr::filter(metric =="NN_seeds_mean") %>%
  ggplot(aes(x=mean_mustar, y=mean_sigma
             , color = parameter
             , shape = parameter
  )) + # position_jitterdodge() requires fill and color
  # All observations:
  # geom_point(alpha = 0.8) + 
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.5, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.1, intercept=0), linetype = 2) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_sigma), size=2.5 #, shape = 20
             # , alpha = 0.7
  ) +
  facet_nested(feedingbout ~ param_category
               # , scales = "free_x"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # Errorbars:
  geom_errorbar(aes(ymin=mean_sigma-se_sigma, ymax=mean_sigma+se_sigma)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  # geom_errorbar(aes(xmin=mean_mustar-sd_mustar, xmax=mean_mustar+sd_mustar)
  geom_errorbar(aes(xmin=mean_mustar-se_mustar, xmax=mean_mustar+se_mustar)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  theme(legend.position="bottom") +
  # theme(strip.background = element_rect(fill=strip))
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris interaction effects on Seed aggregation") +
  ylab(expression(paste(sigma, " - Nearest neighbor distance (meters)"))) +
  xlab(expression(paste(mu, "*", " - Nearest neighbor distance (meters)"))) +
  ggpp::geom_text_npc(data = n_runs_avg, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
  ) +
  xlim(5, 20) +
  ylim(5, 20)

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_NNseeds_option2_interaction_paramcategory_average.png'), height = 7, width = 12, dpi = 600)






### KDE50 ----


# First order effects (average)
morris_db_long_avg %>% 
  dplyr::filter(metric =="KDE_50_mean") %>%
  # ggplot(aes(x=value_mustar, y=value_mu
  ggplot(aes(x=mean_mustar, y=mean_mu
             , color = parameter
             , shape = parameter
  )) + # position_jitterdodge() requires fill and color
  geom_abline(aes(slope = -1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0, intercept=0), linetype = 2) +
  # geom_abline(aes(slope = SEM_mu, intercept = 0, linetype = 4)) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_mu), size=2.5 #, shape = 20
             # geom_point(aes(x=value_mustar, y=value_mu), size=2.5 #, shape = 20
             # , alpha = 0.7
  ) +
  # facet_nested(feedingbout ~ metric+simulation_scenario
  facet_nested(feedingbout ~ param_category
               # facet_nested(~feedingbout
               # facet_nested(~ feedingbout
               # , scales = "free_x"
               # , scales = "free_y"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # scale_color_viridis_d(option = "turbo") +
  # Errorbars:
  # geom_errorbar(aes(ymin=mean_mu-sd_mu, ymax=mean_mu+sd_mu)
  geom_errorbar(aes(ymin=mean_mu-se_mu, ymax=mean_mu+se_mu)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  # geom_errorbar(aes(xmin=mean_mustar-sd_mustar, xmax=mean_mustar+sd_mustar)
  geom_errorbar(aes(xmin=mean_mustar-se_mustar, xmax=mean_mustar+se_mustar)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  theme(legend.position="bottom") +
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris first-order effects on KDE50") +
  xlab(expression(paste(mu, "*", " - Core UD area (ha)")))+
  ylab(expression(paste(mu,      " - Core UD area (ha)"))) +
  ggpp::geom_text_npc(data = n_runs_avg, aes(label=paste("n=", viable_runs)),
                      npcx = "center", npcy = "top"
  )

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_KDE50_option2_first-order_paramcategory_average.png'), height = 7, width = 12, dpi = 600)



# Interaction effects (average)
morris_db_long_avg %>% 
  dplyr::filter(metric =="KDE_50_mean") %>%
  ggplot(aes(x=mean_mustar, y=mean_sigma
             , color = parameter
             , shape = parameter
  )) + # position_jitterdodge() requires fill and color
  # All observations:
  # geom_point(alpha = 0.8) + 
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.5, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.1, intercept=0), linetype = 2) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_sigma), size=2.5 #, shape = 20
             # , alpha = 0.7
  ) +
  facet_nested(feedingbout ~ param_category
               # , scales = "free_x"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # Errorbars:
  geom_errorbar(aes(ymin=mean_sigma-se_sigma, ymax=mean_sigma+se_sigma)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  # geom_errorbar(aes(xmin=mean_mustar-sd_mustar, xmax=mean_mustar+sd_mustar)
  geom_errorbar(aes(xmin=mean_mustar-se_mustar, xmax=mean_mustar+se_mustar)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  theme(legend.position="bottom") +
  # theme(strip.background = element_rect(fill=strip))
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris interaction effects on KDE50") +
  xlab(expression(paste(mu, "*", " - Core UD area (ha)"))) +
  ylab(expression(paste(sigma, " - Core UD area (ha)"))) +
  ggpp::geom_text_npc(data = n_runs_avg, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
  ) #+
  # xlim(5, 20) +
  # ylim(5, 20)

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_KDE50_option2_interaction_paramcategory_average.png'), height = 7, width = 12, dpi = 600)





### KDE95 ----


# First order effects (average)
morris_db_long_avg %>% 
  dplyr::filter(metric =="KDE_95_mean") %>%
  # ggplot(aes(x=value_mustar, y=value_mu
  ggplot(aes(x=mean_mustar, y=mean_mu
             , color = parameter
             , shape = parameter
  )) + # position_jitterdodge() requires fill and color
  geom_abline(aes(slope = -1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0, intercept=0), linetype = 2) +
  # geom_abline(aes(slope = SEM_mu, intercept = 0, linetype = 4)) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_mu), size=2.5 #, shape = 20
             # geom_point(aes(x=value_mustar, y=value_mu), size=2.5 #, shape = 20
             # , alpha = 0.7
  ) +
  # facet_nested(feedingbout ~ metric+simulation_scenario
  facet_nested(feedingbout ~ param_category
               # facet_nested(~feedingbout
               # facet_nested(~ feedingbout
               # , scales = "free_x"
               # , scales = "free_y"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # scale_color_viridis_d(option = "turbo") +
  # Errorbars:
  # geom_errorbar(aes(ymin=mean_mu-sd_mu, ymax=mean_mu+sd_mu)
  geom_errorbar(aes(ymin=mean_mu-se_mu, ymax=mean_mu+se_mu)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  # geom_errorbar(aes(xmin=mean_mustar-sd_mustar, xmax=mean_mustar+sd_mustar)
  geom_errorbar(aes(xmin=mean_mustar-se_mustar, xmax=mean_mustar+se_mustar)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  theme(legend.position="bottom") +
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris first-order effects on KDE95") +
  xlab(expression(paste(mu, "*", " - Total UD area (ha)")))+
  ylab(expression(paste(mu,      " - Total UD area (ha)"))) +
  ggpp::geom_text_npc(data = n_runs_avg, aes(label=paste("n=", viable_runs)),
                      npcx = "center", npcy = "top"
  )

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_KDE95_option2_first-order_paramcategory_average.png'), height = 7, width = 12, dpi = 600)



# Interaction effects (average)
morris_db_long_avg %>% 
  dplyr::filter(metric =="KDE_95_mean") %>%
  ggplot(aes(x=mean_mustar, y=mean_sigma
             , color = parameter
             , shape = parameter
  )) + # position_jitterdodge() requires fill and color
  # All observations:
  # geom_point(alpha = 0.8) + 
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.5, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.1, intercept=0), linetype = 2) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_sigma), size=2.5 #, shape = 20
             # , alpha = 0.7
  ) +
  facet_nested(feedingbout ~ param_category
               # , scales = "free_x"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # Errorbars:
  geom_errorbar(aes(ymin=mean_sigma-se_sigma, ymax=mean_sigma+se_sigma)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  # geom_errorbar(aes(xmin=mean_mustar-sd_mustar, xmax=mean_mustar+sd_mustar)
  geom_errorbar(aes(xmin=mean_mustar-se_mustar, xmax=mean_mustar+se_mustar)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  theme(legend.position="bottom") +
  # theme(strip.background = element_rect(fill=strip))
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris interaction effects on KDE95") +
  xlab(expression(paste(mu, "*", " - Total UD area (ha)"))) +
  ylab(expression(paste(sigma, " - Total UD area (ha)"))) +
  ggpp::geom_text_npc(data = n_runs_avg, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
  ) #+
# xlim(5, 20) +
# ylim(5, 20)

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_KDE95_option2_interaction_paramcategory_average.png'), height = 7, width = 12, dpi = 600)




## Movement patterns ------------


### DPL ----


# First order effects (average)
morris_db_long_avg %>% 
  dplyr::filter(metric =="DPL_mean") %>%
  # ggplot(aes(x=value_mustar, y=value_mu
  ggplot(aes(x=mean_mustar, y=mean_mu
             , color = parameter
             , shape = parameter
  )) + # position_jitterdodge() requires fill and color
  geom_abline(aes(slope = -1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0, intercept=0), linetype = 2) +
  # geom_abline(aes(slope = SEM_mu, intercept = 0, linetype = 4)) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_mu), size=2.5 #, shape = 20
             # geom_point(aes(x=value_mustar, y=value_mu), size=2.5 #, shape = 20
             # , alpha = 0.7
  ) +
  # facet_nested(feedingbout ~ metric+simulation_scenario
  facet_nested(feedingbout ~ param_category
               # facet_nested(~feedingbout
               # facet_nested(~ feedingbout
               # , scales = "free_x"
               # , scales = "free_y"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # scale_color_viridis_d(option = "turbo") +
  # Errorbars:
  # geom_errorbar(aes(ymin=mean_mu-sd_mu, ymax=mean_mu+sd_mu)
  geom_errorbar(aes(ymin=mean_mu-se_mu, ymax=mean_mu+se_mu)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  # geom_errorbar(aes(xmin=mean_mustar-sd_mustar, xmax=mean_mustar+sd_mustar)
  geom_errorbar(aes(xmin=mean_mustar-se_mustar, xmax=mean_mustar+se_mustar)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  theme(legend.position="bottom") +
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris first-order effects on DPL") +
  xlab(expression(paste(mu, "*", " - DPL (m)"))) +
  ylab(expression(paste(mu,      " - DPL (m)"))) +
  ggpp::geom_text_npc(data = n_runs_avg, aes(label=paste("n=", viable_runs)),
                      npcx = "center", npcy = "top"
  )

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_DPL_option2_first-order_paramcategory_average.png'), height = 7, width = 12, dpi = 600)



# Interaction effects (average)
morris_db_long_avg %>% 
  dplyr::filter(metric =="DPL_mean") %>%
  ggplot(aes(x=mean_mustar, y=mean_sigma
             , color = parameter
             , shape = parameter
  )) + # position_jitterdodge() requires fill and color
  # All observations:
  # geom_point(alpha = 0.8) + 
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.5, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.1, intercept=0), linetype = 2) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_sigma), size=2.5 #, shape = 20
             # , alpha = 0.7
  ) +
  facet_nested(feedingbout ~ param_category
               # , scales = "free_x"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # Errorbars:
  geom_errorbar(aes(ymin=mean_sigma-se_sigma, ymax=mean_sigma+se_sigma)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  # geom_errorbar(aes(xmin=mean_mustar-sd_mustar, xmax=mean_mustar+sd_mustar)
  geom_errorbar(aes(xmin=mean_mustar-se_mustar, xmax=mean_mustar+se_mustar)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  theme(legend.position="bottom") +
  # theme(strip.background = element_rect(fill=strip))
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris interaction effects on DPL") +
  xlab(expression(paste(mu, "*", " - DPL (m)"))) +
  ylab(expression(paste(sigma, " - DPL (m)"))) +
  ggpp::geom_text_npc(data = n_runs_avg, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
  ) #+
# xlim(5, 20) +
# ylim(5, 20)

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_DPL_option2_interaction_paramcategory_average.png'), height = 7, width = 12, dpi = 600)


### MR ----


# First order effects (average)
morris_db_long_avg %>% 
  dplyr::filter(metric =="MR_mean") %>%
  # ggplot(aes(x=value_mustar, y=value_mu
  ggplot(aes(x=mean_mustar, y=mean_mu
             , color = parameter
             , shape = parameter
  )) + # position_jitterdodge() requires fill and color
  geom_abline(aes(slope = -1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0, intercept=0), linetype = 2) +
  # geom_abline(aes(slope = SEM_mu, intercept = 0, linetype = 4)) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_mu), size=2.5 #, shape = 20
             # geom_point(aes(x=value_mustar, y=value_mu), size=2.5 #, shape = 20
             # , alpha = 0.7
  ) +
  # facet_nested(feedingbout ~ metric+simulation_scenario
  facet_nested(feedingbout ~ param_category
               # facet_nested(~feedingbout
               # facet_nested(~ feedingbout
               # , scales = "free_x"
               # , scales = "free_y"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # scale_color_viridis_d(option = "turbo") +
  # Errorbars:
  # geom_errorbar(aes(ymin=mean_mu-sd_mu, ymax=mean_mu+sd_mu)
  geom_errorbar(aes(ymin=mean_mu-se_mu, ymax=mean_mu+se_mu)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  # geom_errorbar(aes(xmin=mean_mustar-sd_mustar, xmax=mean_mustar+sd_mustar)
  geom_errorbar(aes(xmin=mean_mustar-se_mustar, xmax=mean_mustar+se_mustar)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  theme(legend.position="bottom") +
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  xlab(expression(paste(mu, "*", " - MR (m/hours active)"))) +
  ylab(expression(paste(mu, " - MR (m/hours active)"))) +
  ggtitle("Morris first-order effects on Movement rate") +
  ggpp::geom_text_npc(data = n_runs_avg, aes(label=paste("n=", viable_runs)),
                      npcx = "center", npcy = "top"
  )

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_MR_option2_first-order_paramcategory_average.png'), height = 7, width = 12, dpi = 600)



# Interaction effects (average)
morris_db_long_avg %>% 
  dplyr::filter(metric =="MR_mean") %>%
  ggplot(aes(x=mean_mustar, y=mean_sigma
             , color = parameter
             , shape = parameter
  )) + # position_jitterdodge() requires fill and color
  # All observations:
  # geom_point(alpha = 0.8) + 
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.5, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.1, intercept=0), linetype = 2) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_sigma), size=2.5 #, shape = 20
             # , alpha = 0.7
  ) +
  facet_nested(feedingbout ~ param_category
               # , scales = "free_x"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # Errorbars:
  geom_errorbar(aes(ymin=mean_sigma-se_sigma, ymax=mean_sigma+se_sigma)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  # geom_errorbar(aes(xmin=mean_mustar-sd_mustar, xmax=mean_mustar+sd_mustar)
  geom_errorbar(aes(xmin=mean_mustar-se_mustar, xmax=mean_mustar+se_mustar)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  theme(legend.position="bottom") +
  # theme(strip.background = element_rect(fill=strip))
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  xlab(expression(paste(mu, "*", " - MR (m/hours active)"))) +
  ylab(expression(paste(sigma, " - MR (m/hours active)"))) +
  ggtitle("Morris interaction effects on Movement rate") +
  ggpp::geom_text_npc(data = n_runs_avg, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
  ) #+
# xlim(5, 20) +
# ylim(5, 20)

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_MR_option2_interaction_paramcategory_average.png'), height = 7, width = 12, dpi = 600)





### PT ----


# First order effects (average)
morris_db_long_avg %>% 
  dplyr::filter(metric =="PT_mean") %>%
  # ggplot(aes(x=value_mustar, y=value_mu
  ggplot(aes(x=mean_mustar, y=mean_mu
             , color = parameter
             , shape = parameter
  )) + # position_jitterdodge() requires fill and color
  geom_abline(aes(slope = -1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0, intercept=0), linetype = 2) +
  # geom_abline(aes(slope = SEM_mu, intercept = 0, linetype = 4)) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_mu), size=2.5 #, shape = 20
             # geom_point(aes(x=value_mustar, y=value_mu), size=2.5 #, shape = 20
             # , alpha = 0.7
  ) +
  # facet_nested(feedingbout ~ metric+simulation_scenario
  facet_nested(feedingbout ~ param_category
               # facet_nested(~feedingbout
               # facet_nested(~ feedingbout
               # , scales = "free_x"
               # , scales = "free_y"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # scale_color_viridis_d(option = "turbo") +
  # Errorbars:
  # geom_errorbar(aes(ymin=mean_mu-sd_mu, ymax=mean_mu+sd_mu)
  geom_errorbar(aes(ymin=mean_mu-se_mu, ymax=mean_mu+se_mu)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  # geom_errorbar(aes(xmin=mean_mustar-sd_mustar, xmax=mean_mustar+sd_mustar)
  geom_errorbar(aes(xmin=mean_mustar-se_mustar, xmax=mean_mustar+se_mustar)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  theme(legend.position="bottom") +
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris first-order effects on Path twisting") +
  xlab(expression(paste(mu, "*", " - PT (unitless)"))) +
  ylab(expression(paste(mu,      " - PT (unitless)"))) +
  ggpp::geom_text_npc(data = n_runs_avg, aes(label=paste("n=", viable_runs)),
                      npcx = "center", npcy = "top"
  )

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_PT_option2_first-order_paramcategory_average.png'), height = 7, width = 12, dpi = 600)



# Interaction effects (average)
morris_db_long_avg %>% 
  dplyr::filter(metric =="PT_mean") %>%
  ggplot(aes(x=mean_mustar, y=mean_sigma
             , color = parameter
             , shape = parameter
  )) + # position_jitterdodge() requires fill and color
  # All observations:
  # geom_point(alpha = 0.8) + 
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.5, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.1, intercept=0), linetype = 2) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_sigma), size=2.5 #, shape = 20
             # , alpha = 0.7
  ) +
  facet_nested(feedingbout ~ param_category
               # , scales = "free_x"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # Errorbars:
  geom_errorbar(aes(ymin=mean_sigma-se_sigma, ymax=mean_sigma+se_sigma)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  # geom_errorbar(aes(xmin=mean_mustar-sd_mustar, xmax=mean_mustar+sd_mustar)
  geom_errorbar(aes(xmin=mean_mustar-se_mustar, xmax=mean_mustar+se_mustar)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  theme(legend.position="bottom") +
  # theme(strip.background = element_rect(fill=strip))
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris interaction effects on Path twisting") +
  xlab(expression(paste(mu, "*", " - PT (unitless)"))) +
  ylab(expression(paste(sigma, " - PT (unitless)"))) +
  ggpp::geom_text_npc(data = n_runs_avg, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
  ) #+
# xlim(5, 20) +
# ylim(5, 20)

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_PT_option2_interaction_paramcategory_average.png'), height = 7, width = 12, dpi = 600)





### Activity Budget ----


# First order effects (average)
morris_db_long_avg %>% 
  dplyr::filter(metric == "p_feeding_mean" | metric == "p_foraging_mean" | metric == "p_resting_mean" | metric == "p_traveling_mean") %>%
  # ggplot(aes(x=value_mustar, y=value_mu
  ggplot(aes(x=mean_mustar, y=mean_mu
             , color = parameter
             , shape = parameter
  )) + # position_jitterdodge() requires fill and color
  geom_abline(aes(slope = -1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0, intercept=0), linetype = 2) +
  # geom_abline(aes(slope = SEM_mu, intercept = 0, linetype = 4)) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_mu), size=2.5 #, shape = 20
             # geom_point(aes(x=value_mustar, y=value_mu), size=2.5 #, shape = 20
             # , alpha = 0.7
  ) +
  # facet_nested(feedingbout ~ metric+simulation_scenario
  facet_nested(metric+feedingbout ~ param_category
               # facet_nested(~feedingbout
               # facet_nested(~ feedingbout
               # , scales = "free_x"
               # , scales = "free_y"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # scale_color_viridis_d(option = "turbo") +
  # Errorbars:
  # geom_errorbar(aes(ymin=mean_mu-sd_mu, ymax=mean_mu+sd_mu)
  geom_errorbar(aes(ymin=mean_mu-se_mu, ymax=mean_mu+se_mu)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  # geom_errorbar(aes(xmin=mean_mustar-sd_mustar, xmax=mean_mustar+sd_mustar)
  geom_errorbar(aes(xmin=mean_mustar-se_mustar, xmax=mean_mustar+se_mustar)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  theme(legend.position="bottom") +
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris first-order effects on Acitivyt Budget") +
  xlab(expression(paste(mu, "*", "- Percentage of activity budget"))) +
  ylab(expression(paste(mu,      "- Percentage of activity budget"))) #+
  # ggpp::geom_text_npc(data = n_runs_avg, aes(label=paste("n=", viable_runs)),
  #                     npcx = "center", npcy = "top"
  # ) #+
  # xlim(0)

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_Activity-budget_option2_first-order_paramcategory_average.png')
       , height = 12.5, width = 10, dpi = 600)



# Interaction effects (average)
morris_db_long_avg %>% 
  dplyr::filter(metric == "p_feeding_mean" | metric == "p_foraging_mean" | metric == "p_resting_mean" | metric == "p_traveling_mean") %>%
  ggplot(aes(x=mean_mustar, y=mean_sigma
             , color = parameter
             , shape = parameter
  )) + # position_jitterdodge() requires fill and color
  # All observations:
  # geom_point(alpha = 0.8) + 
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.5, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.1, intercept=0), linetype = 2) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_sigma), size=2.5 #, shape = 20
             # , alpha = 0.7
  ) +
  facet_nested(metric+feedingbout ~ param_category
               # , scales = "free_x"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # Errorbars:
  geom_errorbar(aes(ymin=mean_sigma-se_sigma, ymax=mean_sigma+se_sigma)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  # geom_errorbar(aes(xmin=mean_mustar-sd_mustar, xmax=mean_mustar+sd_mustar)
  geom_errorbar(aes(xmin=mean_mustar-se_mustar, xmax=mean_mustar+se_mustar)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  theme(legend.position="bottom") +
  # theme(strip.background = element_rect(fill=strip))
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris interaction effects on Acitivyt Budget") +
  xlab(expression(paste(mu, "*", "- Percentage of activity budget"))) +
  ylab(expression(paste(sigma, "- Percentage of activity budget"))) #+
  # ggpp::geom_text_npc(data = n_runs_avg, aes(label=paste("n=", viable_runs)), 
  #                     npcx = "center", npcy = "top"
  # ) #+
# xlim(5, 20) +
# ylim(5, 20)

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_Activity-budget_option2_interaction_paramcategory_average.png')
       , height = 12.5, width = 10, dpi = 600)




## Survival -----

### Simulation time -----


# First order effects (average)
morris_db_long_avg %>% 
  dplyr::filter(metric =="timestep_mean") %>%
  # ggplot(aes(x=value_mustar, y=value_mu
  ggplot(aes(x=mean_mustar, y=mean_mu
             , color = parameter
             , shape = parameter
  )) + # position_jitterdodge() requires fill and color
  geom_abline(aes(slope = -1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0, intercept=0), linetype = 2) +
  # geom_abline(aes(slope = SEM_mu, intercept = 0, linetype = 4)) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_mu), size=2.5 #, shape = 20
             # geom_point(aes(x=value_mustar, y=value_mu), size=2.5 #, shape = 20
             # , alpha = 0.7
  ) +
  # facet_nested(feedingbout ~ metric+simulation_scenario
  facet_nested(feedingbout ~ param_category
               # facet_nested(~feedingbout
               # facet_nested(~ feedingbout
               # , scales = "free_x"
               # , scales = "free_y"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # scale_color_viridis_d(option = "turbo") +
  # Errorbars:
  # geom_errorbar(aes(ymin=mean_mu-sd_mu, ymax=mean_mu+sd_mu)
  geom_errorbar(aes(ymin=mean_mu-se_mu, ymax=mean_mu+se_mu)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  # geom_errorbar(aes(xmin=mean_mustar-sd_mustar, xmax=mean_mustar+sd_mustar)
  geom_errorbar(aes(xmin=mean_mustar-se_mustar, xmax=mean_mustar+se_mustar)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  theme(legend.position="bottom") +
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris first-order effects on activity period") +
  xlab(expression(paste(mu, "*", " - hours active"))) +
  ylab(expression(paste(mu,      " - hours active"))) +
  ggpp::geom_text_npc(data = n_runs_avg, aes(label=paste("n=", viable_runs)),
                      npcx = "center", npcy = "top"
  )

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_timestep_option2_first-order_paramcategory_average.png'), height = 7, width = 12, dpi = 600)



# Interaction effects (average)
morris_db_long_avg %>% 
  dplyr::filter(metric =="timestep_mean") %>%
  ggplot(aes(x=mean_mustar, y=mean_sigma
             , color = parameter
             , shape = parameter
  )) + # position_jitterdodge() requires fill and color
  # All observations:
  # geom_point(alpha = 0.8) + 
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.5, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.1, intercept=0), linetype = 2) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_sigma), size=2.5 #, shape = 20
             # , alpha = 0.7
  ) +
  facet_nested(feedingbout ~ param_category
               # , scales = "free_x"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # Errorbars:
  geom_errorbar(aes(ymin=mean_sigma-se_sigma, ymax=mean_sigma+se_sigma)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  # geom_errorbar(aes(xmin=mean_mustar-sd_mustar, xmax=mean_mustar+sd_mustar)
  geom_errorbar(aes(xmin=mean_mustar-se_mustar, xmax=mean_mustar+se_mustar)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  theme(legend.position="bottom") +
  # theme(strip.background = element_rect(fill=strip))
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris interaction effects on activity period") +
  xlab(expression(paste(mu, "*", " - hours active"))) +
  ylab(expression(paste(sigma, " - hours active"))) +
  ggpp::geom_text_npc(data = n_runs_avg, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
  ) #+
# xlim(5, 20) +
# ylim(5, 20)

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_timestep_option2_interaction_paramcategory_average.png'), height = 7, width = 12, dpi = 600)





### Stored energy -----


# First order effects (average)
morris_db_long_avg %>% 
  dplyr::filter(metric =="energy_stored_mean") %>%
  # ggplot(aes(x=value_mustar, y=value_mu
  ggplot(aes(x=mean_mustar, y=mean_mu
             , color = parameter
             , shape = parameter
  )) + # position_jitterdodge() requires fill and color
  geom_abline(aes(slope = -1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0, intercept=0), linetype = 2) +
  # geom_abline(aes(slope = SEM_mu, intercept = 0, linetype = 4)) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_mu), size=2.5 #, shape = 20
             # geom_point(aes(x=value_mustar, y=value_mu), size=2.5 #, shape = 20
             # , alpha = 0.7
  ) +
  # facet_nested(feedingbout ~ metric+simulation_scenario
  facet_nested(feedingbout ~ param_category
               # facet_nested(~feedingbout
               # facet_nested(~ feedingbout
               # , scales = "free_x"
               # , scales = "free_y"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # scale_color_viridis_d(option = "turbo") +
  # Errorbars:
  # geom_errorbar(aes(ymin=mean_mu-sd_mu, ymax=mean_mu+sd_mu)
  geom_errorbar(aes(ymin=mean_mu-se_mu, ymax=mean_mu+se_mu)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  # geom_errorbar(aes(xmin=mean_mustar-sd_mustar, xmax=mean_mustar+sd_mustar)
  geom_errorbar(aes(xmin=mean_mustar-se_mustar, xmax=mean_mustar+se_mustar)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  theme(legend.position="bottom") +
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris first-order effects on stored energy") +
  xlab(expression(paste(mu, "*", " - energy (unitless)"))) +
  ylab(expression(paste(mu,      " - energy (unitless)"))) +
  ggpp::geom_text_npc(data = n_runs_avg, aes(label=paste("n=", viable_runs)),
                      npcx = "center", npcy = "top"
  )

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_Stored-energy_option2_first-order_paramcategory_average.png'), height = 7, width = 12, dpi = 600)



# Interaction effects (average)
morris_db_long_avg %>% 
  dplyr::filter(metric =="energy_stored_mean") %>%
  ggplot(aes(x=mean_mustar, y=mean_sigma
             , color = parameter
             , shape = parameter
  )) + # position_jitterdodge() requires fill and color
  # All observations:
  # geom_point(alpha = 0.8) + 
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.5, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.1, intercept=0), linetype = 2) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_sigma), size=2.5 #, shape = 20
             # , alpha = 0.7
  ) +
  facet_nested(feedingbout ~ param_category
               # , scales = "free_x"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # Errorbars:
  geom_errorbar(aes(ymin=mean_sigma-se_sigma, ymax=mean_sigma+se_sigma)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  # geom_errorbar(aes(xmin=mean_mustar-sd_mustar, xmax=mean_mustar+sd_mustar)
  geom_errorbar(aes(xmin=mean_mustar-se_mustar, xmax=mean_mustar+se_mustar)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  theme(legend.position="bottom") +
  # theme(strip.background = element_rect(fill=strip))
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris interaction effects on stored energy") +
  xlab(expression(paste(mu, "*", " - energy (unitless)"))) +
  ylab(expression(paste(sigma, " - energy (unitless)"))) +
  ggpp::geom_text_npc(data = n_runs_avg, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
  ) #+
# xlim(5, 20) +
# ylim(5, 20)

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_Stored-energy_option2_interaction_paramcategory_average.png'), height = 7, width = 12, dpi = 600)





### Revisits -----


# First order effects (average)
morris_db_long_avg %>% 
  dplyr::filter(metric =="p-visited-trees_mean") %>%
  # ggplot(aes(x=value_mustar, y=value_mu
  ggplot(aes(x=mean_mustar, y=mean_mu
             , color = parameter
             , shape = parameter
  )) + # position_jitterdodge() requires fill and color
  geom_abline(aes(slope = -1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0, intercept=0), linetype = 2) +
  # geom_abline(aes(slope = SEM_mu, intercept = 0, linetype = 4)) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_mu), size=2.5 #, shape = 20
             # geom_point(aes(x=value_mustar, y=value_mu), size=2.5 #, shape = 20
             # , alpha = 0.7
  ) +
  # facet_nested(feedingbout ~ metric+simulation_scenario
  facet_nested(feedingbout ~ param_category
               # facet_nested(~feedingbout
               # facet_nested(~ feedingbout
               # , scales = "free_x"
               # , scales = "free_y"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # scale_color_viridis_d(option = "turbo") +
  # Errorbars:
  # geom_errorbar(aes(ymin=mean_mu-sd_mu, ymax=mean_mu+sd_mu)
  geom_errorbar(aes(ymin=mean_mu-se_mu, ymax=mean_mu+se_mu)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  # geom_errorbar(aes(xmin=mean_mustar-sd_mustar, xmax=mean_mustar+sd_mustar)
  geom_errorbar(aes(xmin=mean_mustar-se_mustar, xmax=mean_mustar+se_mustar)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  theme(legend.position="bottom") +
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris first-order effects on Proportion of visited trees") +
  xlab(expression(paste(mu, "*", " - proportion of visited trees (%)"))) +
  ylab(expression(paste(mu, " - proportion of visited trees (%)"))) +
  ggpp::geom_text_npc(data = n_runs_avg, aes(label=paste("n=", viable_runs)),
                      npcx = "center", npcy = "top"
  )

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_P-visited-trees_option2_first-order_paramcategory_average.png'), height = 7, width = 12, dpi = 600)



# Interaction effects (average)
morris_db_long_avg %>% 
  dplyr::filter(metric =="p-visited-trees_mean") %>%
  ggplot(aes(x=mean_mustar, y=mean_sigma
             , color = parameter
             , shape = parameter
  )) + # position_jitterdodge() requires fill and color
  # All observations:
  # geom_point(alpha = 0.8) + 
  geom_abline(aes(slope = 1, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.5, intercept=0), linetype = 2) +
  geom_abline(aes(slope = 0.1, intercept=0), linetype = 2) +
  # Mean values (summarizing n=6)
  # geom_point(aes(x=log10(mean_mustar), y=log10(mean_sigma)), shape=17, size=4
  geom_point(aes(x=mean_mustar, y=mean_sigma), size=2.5 #, shape = 20
             # , alpha = 0.7
  ) +
  facet_nested(feedingbout ~ param_category
               # , scales = "free_x"
               # , scales = "free"
               # , independent = "all"
               # ,rows = 
  ) +
  scale_shape_manual(values=1:nlevels(as.factor(morris_db_long$parameter))) +
  scale_color_manual(values = rainbow(n = 14)) +
  # Errorbars:
  geom_errorbar(aes(ymin=mean_sigma-se_sigma, ymax=mean_sigma+se_sigma)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  # geom_errorbar(aes(xmin=mean_mustar-sd_mustar, xmax=mean_mustar+sd_mustar)
  geom_errorbar(aes(xmin=mean_mustar-se_mustar, xmax=mean_mustar+se_mustar)
                #, position = position_dodge(.9)
                , lwd = 0.2
  ) +
  theme(legend.position="bottom") +
  # theme(strip.background = element_rect(fill=strip))
  # scale_color_viridis_d()
  # facet_grid(metric ~ feedingbout) +
  ggtitle("Morris interaction effects on Proportion of visited trees") +
  xlab(expression(paste(mu, "*", " - proportion of visited trees (%)"))) +
  ylab(expression(paste(sigma, " - proportion of visited trees (%)"))) +
  ggpp::geom_text_npc(data = n_runs_avg, aes(label=paste("n=", viable_runs)), 
                      npcx = "center", npcy = "top"
  ) #+
# xlim(5, 20) +
# ylim(5, 20)

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_P-visited-trees_option2_interaction_paramcategory_average.png'), height = 7, width = 12, dpi = 600)

