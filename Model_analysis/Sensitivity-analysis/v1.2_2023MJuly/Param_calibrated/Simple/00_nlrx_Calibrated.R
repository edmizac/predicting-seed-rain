## Header --------------------------
# Script name: 00_nlrx
# Script purpose: Do simple runs for every siminputrow using calibrated parameters
# from the genetic algorithm
# Date created: 2023-1--31d
# Author: Eduardo Zanette

## Notes --------------------------- 


## Packages -------------------------
library("nlrx")
library("here")
library("dplyr")
library("stringr")
library("progressr")
library("future")
library("tictoc")
library("ggplot2")
# library("tidyverse")

## ---------------------------

# Packages from github:
# install.packages('devtools')
# devtools::install_github('thomasp85/gganimate')
# remotes::install_github("ropensci/nlrx")

## ---------------------------

# Spatial plots
theme_set(theme_bw())

## ---------------------------


# Options (plotting, memory limit, decimal digits)

## Config cores
ncores <- parallel::detectCores()

## Java memory:
if(Sys.getenv("JAVA_HOME") == "") {
  if(Sys.info()[["sysname"]] == "Linux") {
    Sys.setenv(JAVA_HOME = "/usr/lib/jvm/java-11-openjdk-amd64")
    unixtools::set.tempdir(".")
  } else {
    Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk1.8.0_321")
  }
}

## ---------------------------


# Step 1: Create nl object
if(Sys.info()[["nodename"]] == "DESKTOP-R12V3D6") {
  netlogopath <- file.path("C:/Program Files/NetLogo 6.3.0")
  # modelpath <- here("Model_development", "BLT_model_v1.1.nlogo")
  modelpath <- here("Model_simulations", "BLT_model_v1.2.nlogo")
  outpath <- here("Model_analysis", "Sensitivity-analysis", "v1.2_2023MJuly", 
                  "Param_calibrated", "Simple", "temp")
  user_scp = "\"Eduardo\""
}


nl <- nl(nlversion = "6.3.0",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

nlogo_model_param <- report_model_parameters(nl)
nlogo_model_param
# saveRDS(nlogo_model_param, file = paste0(outpath, "/nlogo_model_param.rds"))



## Parameterizations and calibrations -----

### Empirical data for parameterisation ----
param.table <- read.csv(here("Data", "Movement", "Curated", "BLT_groups_data_summary_siminputrow.csv"),
                        sep = ",", dec = ".", stringsAsFactors = TRUE,
                        encoding = "UTF-8") %>% 
  dplyr::mutate(group = recode(group, "Guarei" = "Guareí",
                               "Santa Maria" = "SantaMaria")) # only to match those of the NetLogo model


### Calibrated values for unknown parameters ----
# If you want to use the calibrated parameters and not the model values defined by sliders
calibrated_on <- "true"

if (calibrated_on == "true") {
  
  path <- here("Model_analysis", "Genetic_analysis", "v1.2",  "temp")
  
  filesga <- list.files(path, pattern = "23.csv")
  filesga <- paste0(path, "/", filesga)
  
  dfga <- data.frame("expname" = character(), "parameter" = character(), "value" = double())
  for (i in filesga) {
    # i <- filesga[2]
    # filei <- read.csv(i, encoding = "latin1")
    filei <- read.csv(i, encoding = "UTF-8")
    dfga <- dplyr::bind_rows(dfga, filei)
  }
  
  dfga %>% str()
  # dfga  <- dfga %>% 
  #   mutate(
  #     expname = recode(
  #       "Guare\xed_Jul" = "Guareí"
  #     )
  #   )
  
  # dfga <- dfga %>% 
  #   dplyr::mutate(parameter, str_replace_all(., c("-" = "_", 
  #                                                 # "\\." = "_",
  #                                                 " " = "")))# %>% 
  
  a <- dfga$expname %>%  
    str_split(., pattern = "_", simplify = TRUE)
  
  dfga <- dfga %>% 
    mutate(
      group = as.vector(a[, 1]),
      month_run = as.vector(a[, 2])
    ) %>% 
    as.data.frame()
  
  # dfga %>% str()
  # dfga %>% class()
  
  dfga <- dfga %>%
    dplyr::mutate(group = recode(group, "Guarei" = "Guareí")) # only to match those of the NetLogo model
  
  # dfga <- dfga %>% 
  #   mutate_if(is.character, as.factor)
  
  # dfga %>% str()
  dfga$parameter %>% levels()
  
  
  
  dfga %>% str()
  dfga %>% class()
  
}





## Simple design experiment ( = one go button, no varibles) ####

## Step 2: Attach an experiment
exptype <- "simple"

i <- 9
for (i in i:nrow(param.table)) {
  
  
  ### choose which parameterizations should be on
  feedingbout <- "true"
  step_model_param <- "true" # velocity parameters are setted inside the model. Change this when velocity is summarized and inclued in the parameter table
  gtt_param <- "true" # gtt parameters are setted inside the model. Change this when velocity is summarized and inclued in the parameter table
  p_forage_param <- "true" # p_foraging parameter is setted inside the model. Change this when velocity is summarized and inclued in the parameter table 
  
  
  
  ### Define run and parameterisation: (all strings must use escaped quotes)
  
  area_run <- param.table$group[i] %>% as.character()
  month_run <- param.table$id_month[i] %>% as.character()
  
  area_run_scp <- paste0('\"', area_run, '\"')
  month_run_scp <- paste0('\"', month_run, '\"')
  
  # area_run <- '"Guareí"'
  # month_run <- '"Jun"'
  expname <-  paste0("v1.2_", 
                     area_run,
                     "_", 
                     month_run,
                     "_", exptype)
  expname <- expname %>% 
    str_replace_all(., c("-" = "_", "\\s+" = "_")) # Remove - and space characters.
  # print(expname)
  # i <- i + 1
  #} # loop testing
  
  no_days_run <- param.table %>% 
    dplyr::filter(group == area_run,
                  id_month == month_run) %>% 
    dplyr::select(ndays) %>% 
    pull() #+ 1 # one day more for "initializing" the model (take this first day out when analyzing data?)
  
  simultime_run <- param.table %>% 
    dplyr::filter(group == area_run,
                  id_month == month_run) %>% 
    dplyr::select(mean_timesteps) %>% 
    pull()
  # simultime_run <- round(simultime_run * 0.9) # that's the timestep when tamarins should start looking for the sleeping site
  
  
  
  
  ### Define calbirated parameters:
  # I only calibrated for one month of each group, so we are using the set of parameters of each area for all months in the same area
  # memory
  energy_from_fruits <- dfga %>% 
    dplyr::filter(group == area_run) %>% 
    dplyr::filter(month_run == .env$month_run) %>% 
    dplyr::filter(parameter == "energy-from-fruits") %>% 
    dplyr::select(value) %>% 
    pull()
  
  energy_from_prey <- dfga %>% 
    dplyr::filter(group == area_run) %>% 
    dplyr::filter(month_run == .env$month_run) %>% 
    dplyr::filter(parameter == "energy-from-prey") %>% 
    dplyr::select(value) %>% 
    pull()
  
  energy_loss_traveling <- dfga %>% 
    dplyr::filter(group == area_run) %>% 
    dplyr::filter(month_run == .env$month_run) %>% 
    dplyr::filter(parameter == "energy-loss-traveling") %>% 
    dplyr::select(value) %>% 
    pull()
  
  energy_loss_foraging <- dfga %>% 
    dplyr::filter(group == area_run) %>% 
    dplyr::filter(month_run == .env$month_run) %>% 
    dplyr::filter(parameter == "energy-loss-foraging") %>% 
    dplyr::select(value) %>% 
    pull()
  
  energy_loss_resting <- dfga %>% 
    dplyr::filter(group == area_run) %>% 
    dplyr::filter(month_run == .env$month_run) %>% 
    dplyr::filter(parameter == "energy-loss-resting") %>% 
    dplyr::select(value) %>% 
    pull()
  
  # start_energy <- dfga %>%
  #   dplyr::filter(group == area_run) %>% 
  #   dplyr::filter(month_run == .env$month_run) %>% 
  #   dplyr::filter(parameter == "start-energy") %>% 
  #   dplyr::select(value) %>% 
  #   pull()
  
  energy_level_1 <- dfga %>% 
    dplyr::filter(group == area_run) %>% 
    dplyr::filter(month_run == .env$month_run) %>% 
    dplyr::filter(parameter == "energy_level_1") %>% 
    dplyr::select(value) %>% 
    pull()
  
  energy_level_2 <- dfga %>% 
    dplyr::filter(group == area_run) %>% 
    dplyr::filter(month_run == .env$month_run) %>% 
    dplyr::filter(parameter == "energy_level_2") %>% 
    dplyr::select(value) %>% 
    pull()
  
  step_forget <- dfga %>% 
    dplyr::filter(group == area_run) %>% 
    dplyr::filter(month_run == .env$month_run) %>% 
    dplyr::filter(parameter == "step_forget") %>% 
    dplyr::select(value) %>% 
    pull()
  
  p_memory <- dfga %>% 
    dplyr::filter(group == area_run) %>% 
    dplyr::filter(month_run == .env$month_run) %>% 
    dplyr::filter(parameter == "prop_trees_to_reset_memory") %>% 
    dplyr::select(value) %>% 
    pull()
  
  # This one is parameterized
  duration <- param.table %>% 
    dplyr::filter(group == area_run) %>% 
    dplyr::filter(id_month == .env$month_run) %>% 
    dplyr::select(duration) %>% 
    pull()
  
  
  
  # # Attach to nl experiment
  nl@experiment <- experiment(expname = expname,
                              outpath = outpath,
                              repetition = 1, # number of repetitions with the same seed (use repetition = 1)
                              tickmetrics = "false", # "true" for every tick, "false" for metrics only in the end of the simulation
                              idsetup = "setup",
                              idgo = "go",
                              runtime = 2000, #(if = 0 or NA_integer_, define stopcond_)
                              stopcond= "day > no_days", # reporter that returns TRUE
                              evalticks = NA_integer_, # NA_integer_ = measures each tick. Only applied if tickmetrics = TRUE
                              # idfinal = "r:stop", # for making r NetLogo extension to work: https://cran.r-project.org/web/packages/nlrx/vignettes/furthernotes.html
                              
                              # reporters:
                              metrics = c( # e.g. "count sheep" or "count patches with [pcolor = green]"
                                # "timestep",
                                "day",
                                "timestep",
                                "survived?",
                                
                                "g_SDD",
                                "g_SDD_sd",
                                "n",
                                "p-visited-trees",
                                "R_seeds",        
                                "R_seeds_p",      
                                "NN_seeds",       
                                "NN_feeding_trees", # this is calculated again by the end of the run although it was calculated priorly within the build_forest process
                                "NN_sleeping_trees", # might be a more important factor affecting SDD than the NN of feeding trees 
                                
                                # turtle variables as globals:
                                "g_energy_stored", # energy is reset to energy-start everyday, thus we take
                                # "g_energy",      # final energy                # the best set of parameters should make tamarins viable in energetic terms
                                # "g_enlvl1",         # value of energy_level_1 used at the start of the simulation
                                # "g_enlvl2",         # value of energy_level_2 used at the start of the simulation 
                                # "g_enstart",          # start-energy value at the start of the similuation
                                "g_DPL",         # mean DPL (in case we don't want all the values -> good for bar and pointrange plots)
                                # "g_DPL_d",       # DPL is set to 0 everyday    # the best set of parameters should reproduce the observed DPL
                                "g_DPL_sd",      # sd DPL  (in case we don't want all the values -> good for bar and pointrange plots)
                                "g_KDE_95",      # final value                 # the best set of parameters should reproduce the observed home range
                                "g_KDE_50",      # final value                 # the best set of parameters should reproduce the observed core area
                                "g_p_feeding",   # final value                 # the best set of parameters should optimize the activity budget
                                "g_p_foraging",  # final value                 # the best set of parameters should optimize the activity budget
                                "g_p_traveling", # final value                 # the best set of parameters should optimize the activity budget
                                "g_p_resting",    # final value                 # the best set of parameters should optimize the activity budget
                                # "g_step_length_mean",    # besides the parameterization, agents interactions make the observed step length and turning angles change
                                # "g_step_length_sd",      # besides the parameterization, agents interactions make the observed step length and turning angles change
                                # "g_turn_ang_mean",     # this one is quite consistent, so I don't think this one is necessary
                                # "g_turn_ang_sd",          # this one might be interesting though (but I haven't estimated the empirical ones yet)
                                
                                # additional movement variables
                                "g_MR",               # movement rate (MR) is used to predict SDD by primates: http://doi.wiley.com/10.1002/ajp.22659
                                "g_MR_sd",
                                # "g_MSD",              # other modelling studies have used this one (https://doi.org/10.3390/ani12182412.), but I believe it is very similar to MR
                                # "g_intensity_use",    # bether than MSD in my oppinion: read about it in: https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en
                                "g_PT",               # Highly recommended as it is the only unbiased estimator of tortuosity. Almost the same thing as intensity of use (IU). path twisting is used by Fuzessy et al 2017 to predict SDD among primates: http://doi.wiley.com/10.1002/ajp.22659
                                "g_PT_sd",
                                # "g_straightness",   # straightness is being wrongly estimated. DON'T USE IT NOW (first make it to be calculated in daily basis). straightness and sinuosity are slightlty different in terms of properties (https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en) and they were not tested as predictors of SDD, so i'm not using them
                                # "g_sinuosity"       # sinuosity can't be compared across scales. DON'T USE IT straightness and sinuosity are slightlty different in terms of properties (https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en) and they were not tested as predictors of SDD, so i'm not using them
                                
                                "g_n_visited_trees",
                                "g_n_unvisited_trees",
                                
                                "g_DI_index",      # Defendability index (Mitani & Rodman 1979, Lowen & Dunbar 1994)
                                "g_M_index"        # Extended Defendability index (Lowen & Dunbar 1994)
                                
                                
                                
                              ),
                              metrics.turtles = list(
                                "monkeys" = c(
                                  # "enstart",
                                  # "enlvl1",
                                  # "enlvl2",
                                  
                                  # "DPL_d",       # DPL is set to 0 everyday    # the best set of parameters should reproduce the observed DPL
                                  "step_length_mean",    # besides the parameterization, agents interactions make the observed step length and turning angles change
                                  "step_length_sd",      # besides the parameterization, agents interactions make the observed step length and turning angles change
                                  # "turn_ang_mean",     # this one is quite consistent (~0), so I don't think this one is necessary
                                  "turn_ang_sd"          # this one might be interesting though
                                  
                                  # additional movement variables
                                  # "MR",               # movement rate (MR) is used to predict SDD by primates: http://doi.wiley.com/10.1002/ajp.22659
                                  # "MR_sd",
                                  # "MSD",              # other modelling studies have used this one (https://doi.org/10.3390/ani12182412.), but I believe it is very similar to MR
                                  # "intensity_use",    # bether than MSD in my oppinion: read about it in: https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en
                                  # "PT",               # path twisting is used by Fuzessy et al 2017 to predict SDD among primates: http://doi.wiley.com/10.1002/ajp.22659
                                  # "PT_sd"
                                  # "straightness",   # straightness and sinuosity are slightlty different in terms of properties (https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en) and they were not tested as predictors of SDD, so i'm not using them
                                  # "sinuosity"       # straightness and sinuosity are slightlty different in terms of properties (https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en) and they were not tested as predictors of SDD, so i'm not using them
                                )
                                # 
                                # 
                                # "feeding-trees" = c(
                                #   "x_UTM", "y_UTM",
                                #   "species",
                                #   "id-tree",
                                #   "visitations"
                                # ),
                                # 
                                # "sleeping-trees" = c(
                                #   "x_UTM", "y_UTM",
                                #   "species",
                                #   "id-tree",
                                #   "visitations"
                                # ),
                                # 
                                ,
                                "seeds" = c(
                                  "SDD",
                                  "x_UTM", "y_UTM",
                                  "id-seed",
                                  "species",
                                  "mother-tree",
                                  "disp-day"
                                )
                                
                                # , "steps-moved"
                                
                              ), # "who" "color"
                              variables = list(
                                
                                # energy
                                # "energy-from-seeds" = list(min=1, max = 10, step = 2),
                                # 'energy-from-prey' = list(min=1, max=16, step = 3),
                                # "energy-loss-traveling" = list(min=-10, max = 0, step = 2),
                                # "energy-loss-foraging" = list(min=-10, max = 0, step = 2),
                                # "energy-loss-resting" = list(min=-10, max = 0, step = 2),
                                
                                # memory
                                # "step_forget" = list(min=0, max = 150, step = 10, qfun="qunif")
                                # "visual" = list(min=0, max = 3, step = 1)
                                
                                # movement
                                # "travel_speed_val" = list(values=seq(0.3, 1, by = 0.1))
                                # "foraging_speed_val" = list(min= 0, max = 1, step = 2)
                                # "duration" = list(min=0, max = 10, step = 2),
                                
                                # others
                                # 'species_time_val' = list(min = 1, max = 6, step = 2)
                              ),
                              
                              # (
                              #   'start-energy' = list(min=10, max=170, step = 40, qfun="qunif")
                              #                ),
                              constants = list(
                                
                                ### "true" for output related stuff
                                # "output-files?" = "false", #THIS IS VERY IMPORTANT (csv files)
                                # "output-print?" = "false", #true to output in the console
                                "USER" = user_scp,
                                'feedingbout-on?' = feedingbout,
                                "step-model-param?" = step_model_param,
                                "gtt-param?"= gtt_param,
                                "p-forage-param?" = p_forage_param,
                                
                                # "print-step?" = "false",
                                # 'export-png'= "false",
                                # "show-energy?" = "false",
                                # "show-path?" = "false",
                                # "all-slp-trees?" = "false",
                                # "path-color-by-day?" = "false",
                                
                                ### resource scenario
                                "study_area" = area_run_scp, #"\"Guareí\"",
                                'feeding-trees-scenario' = month_run_scp, #"\"May\"",
                                'no_days' = no_days_run, # DON'T TRY no_days = 1
                                'simulation-time' = simultime_run,
                                # 'feeding-trees?' = "true",
                                # 'sleeping-trees?' = "true",
                                'patch-type' = "\"empirical\"",
                                # 'empirical-trees-choice' = "\"closest\"",
                                
                                ### memory
                                'duration' = duration,
                                # 'visual' = 0,                # does not matter
                                "step_forget" = step_forget,
                                
                                ### energy
                                # 'start-energy' = start_energy,
                                "energy_level_1" = energy_level_1,
                                "energy_level_2" = energy_level_2,
                                "energy-from-fruits" = energy_from_fruits,
                                "energy-from-prey" = energy_from_prey,
                                "energy-loss-traveling" = energy_loss_traveling,
                                "energy-loss-foraging" = energy_loss_foraging,
                                "energy-loss-resting" = energy_loss_resting
                                
                                
                                # Seed dispersal
                                # "gut_transit_time_val" = 15   # not needed when gtt-param = true
                                # "n_seeds_hatched" = 1,
                                
                                ### movement
                                # travel_speed = 1
                                
                              )
  )
  
  
  
  
  report_model_parameters(nl)
  
  
  
  nseeds <- 30 # repetitions (ideally n = 30)
  
  # Step 3: Attach a simulation design.
  # nl@simdesign <- simdesign_distinct(nl, nseeds = 17)
  nl@simdesign <- simdesign_simple(nl, nseeds = nseeds)
  
  # Step 4: Run simulations
  # Evaluate nl object:
  # eval_variables_constants(nl)
  
  # print(nl)
  
  # nl@simdesign
  
  
  # Run all simulations (loop over all siminputrows and simseeds)
  
  
  
  k <- 1
  for (seed in unique(nl@simdesign@simseeds)) {
    
    # seed <- getsim(nl, "simseeds")[k]
    # print(seed)
    # k <- k + 1
    # print(k)
    # }
    
    seed <- getsim(nl, "simseeds")[k] 
    paste0("running seed ", seed[k])
    ## With run_nl_one (with only the first seed)
    tictoc::tic()
    progressr::handlers("progress")
    results <- progressr::with_progress(run_nl_one(nl,
                                                   seed = seed, #[1], # only first seed (simple run)
                                                   siminputrow = 1))
    tictoc::toc()
    
    
    # ## With run_nl_all (all 17 seeds)
    # # Check number of simimputrows:
    # siminput_nrow <- nrow(getsim(nl, "siminput"))
    # # siminput_nrow %%
    # 
    # tictoc::tic()
    # plan(multisession)
    # progressr::handlers("progress")
    # results <- progressr::with_progress(
    #   run_nl_all(nl,
    #              split = 1 # with simdesign = simple it is only possible to run one core?
    #              )
    # )
    # tictoc::toc()
    
    
    ## Step 5:
    #' Attach results to nl and run analysis In order to run the
    #' analyze_nl function, the simulation output has to be attached to the
    #' nl object first. The simdesign class within the nl object provides a
    #' slot for attaching output results (simoutput). An output results
    #' tibble can be attached to this slot by using the simdesign setter
    #' function setsim(nl, "simoutput"). After attaching the simulation
    #' results, these can also be written to the defined outpath of the
    #' experiment object.  Attach results to nl object:
    setsim(nl, "simoutput") <- results
    
    # rm(results)
    
    # nl@experiment@metrics.turtles
    # nl@experiment@metrics.patches
    # nl@experiment@variables
    
    #' Save RDS to avoid losing it by R abortion:
    filename <-
      paste0(outpath, "/", expname, seed, "_tempRDS.Rdata")
    saveRDS(nl, file = filename)
    # rm(nl)
    
    # nl <- readRDS(filename)
    
    gc()
    
    print(paste0("seed finished: ", seed))
    k <- k + 1
    
  }
  
  print(paste0("finishing ", expname))
  i <- i + 1
}




#' Go to file 00_Validation_patterns_v1 or ##### Screening data #####


##### Screening data #####
results_unnest <- unnest_simoutput(nl)
results_unnest <- results_unnest %>% 
  rename(x = x_UTM, y = y_UTM)

# Select random run by selecting random seed (run of x days = seed)
aux_run <- results_unnest$`random-seed` %>%
  na.exclude() %>%
  sample(size = 1)

results_unnest_turtles <- results_unnest %>%
  dplyr::filter(agent=="turtles") %>%               # by agent
  dplyr::filter(`random-seed` == aux_run)       # by random run (~= number of the agent = who)

# Check if is only one run (seed):
results_unnest_turtles$`random-seed` %>% unique()

# Plot routes
# ggplot() +
#   geom_path(data = results_unnest_turtles,
#             aes(x = x, y = y),
#             size = 0.15) +
#   geom_point(data = results_unnest_turtles,
#              aes(x = x, y = y
#                  , group = behavior,
#                  color = behavior,
#                  shape = behavior
#              ),
#              size = 1.4) +
#   scale_color_manual(values = simulated_colors) +
#   scale_shape_manual(values = simulated_shapes) +
#   ggtitle(paste0("Simulated data"), expname)





#### Factorial design experiment ( = various go buttons, with input matrix for varying parameters) ####

## Step 2: Attach an experiment
# exptype <- "fullfactorial"
