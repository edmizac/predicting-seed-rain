# Script name: 00_Morris-sensitivity_Feedingbout-param.R
# Script purpose: Define if parameterizing the feedingbouts per species (species_time) 
# is important for the patterns emerging in the model
# ** Important changes since last analysis (v1.2_2023May):
#     * Parameterized resting time: simulation-time when resting is possible + max 
#       number of steps spent resting (duration)
#     * Enhanced morris density (morrisr)

# Date created: 2023-07-03d
# Author: Eduardo Zanette

## Notes --------------------------- 
# *** IMPORTANT NOTE: I STARTED USING THE MODEL VERSION IN MODEL_SIMULATIONS FOLDER FROM NOW ON
# *** THIS MEANS MY PREVISOU CALIBRATIONS WERE DONE WITH THE MODEL VERSION THAT DIDN'T HAVE THE STORED ENERGY THING IMPLEMENTED
#

## Options -------------------------
# (plotting, memory limit, decimal digits)
# 

## Packages -------------------------
library("here")
library("dplyr")
library("nlrx")
library("progressr")
library("future")
library("tictoc")
library("magrittr")
library("ggplot2")

# ggplot theme
theme_set(theme_bw(base_size = 15))
                   # theme(axis.text.x = element_text(size = 11))))

## Config cores
ncores <- parallel::detectCores()

## Java memory:
if(Sys.getenv("JAVA_HOME") == "") {
  if(Sys.info()[["sysname"]] == "Linux") {
    Sys.setenv(JAVA_HOME = "/usr/lib/jvm/java-11-openjdk-amd64")
    unixtools::set.tempdir(".")
  } else {
    Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk-11")
  }
}

## ---------------------------


## Step 1: Create nl object ------------------------ 

if(Sys.info()[["nodename"]] == "DESKTOP-R12V3D6") {
  netlogopath <- file.path("C:/Program Files/NetLogo 6.3.0")
  # modelpath <- here("Model_development", "BLT_model_v1.2.nlogo")
  modelpath <- here("Model_simulations", "BLT_model_v1.2.nlogo") # Last version with stored-energy
  outpath <- here("Model_analysis", "Sensitivity-analysis", "v1.2_2023May", "Param_bestguess", "Morris", "temp")
  user_scp = "\"Eduardo\""
}
if(Sys.info()[["nodename"]] == "PC9") { # LEEC
  netlogopath <- file.path("C:/Program Files/NetLogo 6.3.0")
  # modelpath <- here("Model_development", "BLT_model_v1.2.nlogo")
  modelpath <- here("Model_simulations", "BLT_model_v1.2.nlogo") # Last version with stored-energy
  # outpath <- here("Model_analysis", "Sensitivity-analysis", "v1.2_December2022", "temp")
  Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8.0_351")
  user_scp = "\"LEEC\""
}
if(Sys.info()[["nodename"]] == "DESKTOP-71SL58S") { # PC 02 LEEC
  netlogopath <- file.path("C:/Program Files/NetLogo 6.3.0")
  modelpath <- here("Model_simulations", "BLT_model_v1.2.nlogo") # Last version with stored-energy
  outpath <- here("Model_analysis", "Sensitivity-analysis", "v1.2_2023MJuly",
                  "Param_bestguess", "Morris", "temp")
  # Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8/bin")
  Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk1.8.0_341")
  user_scp = "\"PC02\""
}
if(Sys.info()[["nodename"]] == "DESKTOP-1SKTQUA") { # AORUS-2 (LaP)
  netlogopath <- file.path("C:/Program Files/NetLogo 6.3.0")
  modelpath <- here("Model_simulations", "BLT_model_v1.2.nlogo") # Last version with stored-energy
  outpath <- here("Model_analysis", "Sensitivity-analysis", "v1.2_2023MJuly",
                  "Param_bestguess", "Morris", "temp")
  # Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8/bin")
  # Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk1.8.0_341")
  user_scp = "\"AORUS-2\""
}



nl <- nl(nlversion = "6.3.0",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

nlogo_model_param <- report_model_parameters(nl)
nlogo_model_param


### Empirical data for parameterisation ----
param.table <- read.csv(here("Data", "Parameter_table.csv"),
                        sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  dplyr::mutate(group = recode(group, "Guarei" = "Guareí",
                               "Santa Maria" = "SantaMaria")) # only to match those of the NetLogo model

# Substet runs if needed
param.table <- param.table[4:6, ]

## Step 2: Attach an experiment and run ------------------------

i <- 1
for (i in i:nrow(param.table)) {
  
  set.seed(21) # for having the same generated seeds everytime

  
  ### Loop (unblock for{} above) or decide which area/month to run the sensitivity on: ---
    # All (loop):
    area_run_scp <- paste0('\"', param.table$group[i], '\"')
    month_run_scp <- paste0('\"', param.table$month[i], '\"')
  
  # Specify area:
  # area_run_scp <- '"Guareí"' #scaped
  # month_run_scp <- '"May"'   #scaped
  
  # area_run_scp <- '"Suzano"' #scaped
  # month_run_scp <- '"Sep"'   #scaped

    
  ### Define run and parameterisation -----
    # (all strings must use escaped quotes) 
    # choose which parameterizations should be on or off
  
  # Decide feedinbout parameterization to run the sensitivity on:
  feedingbout <- "true"
  
  if (feedingbout == "true") { 
    print("RUNNING WITH FEEDING BOUT PARAMETERIZATION ON, COMMENT OUT (LOCK) 'species_time' FROM THE EXPERIMENT SLOT")
  } else {
    print("RUNNING WITH FEEDING BOUT PARAMETERIZATION OFF, COMMENT IN (UNLOCK) 'species_time' FROM THE EXPERIMENT SLOT")
  }
    
  # Decide if long distance targets are random or focused on the trees close to borders
    # ld_target <- "true" # ld targets are random trees
    ld_target <- "false" # ld targets are disputed trees
  
  
  step_model_param <- "true" # velocity parameters are setted inside the model. Change this when velocity is summarized and inclued in the parameter table
  gtt_param <- "true" # gtt parameters are setted inside the model. Change this when velocity is summarized and inclued in the parameter table
  p_forage_param <- "true" # p_foraging parameter is setted inside the model. Change this when velocity is summarized and inclued in the parameter table 
  
  
  
  ### Define expname ----
  expname <-  paste0("v1.2_Morris_", 
                     gsub(area_run_scp, pattern = ('\"'), replacement = '', fixed = T), 
                     "_", 
                     gsub(month_run_scp, pattern = ('\"'), replacement = '', fixed = T)
                     #, 
                     # "_", 
                     # "Feedingbout_", feedingbout, Sys.Date()
  )
  
  
  #   # test loop:
  #   print(expname)
  # 
  #   i <- i+1
  # }
  
  # expname = paste0("v1.2_Morris_", area_run, month_run)
  
  # expname <- expname %>% 
  # stringr::str_replace_all(., c("-" = "_", "\\s+" = "_")) # Remove - and space characters.
  # print(expname)
  # i <- i + 1
  #} # loop testing
  
  no_days_run <- param.table %>% 
    dplyr::filter(group == gsub(area_run_scp, pattern = ('\"'), replacement = '', fixed = T),
                  month == gsub(month_run_scp, pattern = ('\"'), replacement = '', fixed = T)) %>% 
    dplyr::select(ndays) %>% 
    pull() #+ 1 # one day more for "initializing" the model (take this first day out when analyzing data?)
  
  simultime_run <- param.table %>% 
    dplyr::filter(group == gsub(area_run_scp, pattern = ('\"'), replacement = '', fixed = T),
                  month == gsub(month_run_scp, pattern = ('\"'), replacement = '', fixed = T)) %>% 
    dplyr::select(mean_timesteps) %>% 
    pull()
  simultime_run <- round(simultime_run * 0.9) # that's the timestep when tamarins should start looking for the sleeping site
  
  
  
  ### Define parameters:
  # Here we are using the same values used for running the first version of the manuscript (same version from Master thesis)
  # available in "Table2_Parameter_table.xlsx"
  
  # energy_from_fruits <- 73
  # energy_from_prey <- 30								
  # energy_loss_traveling <- -10								
  # energy_loss_foraging <- -10
  # energy_loss_resting <- -6
  # start_energy <- 900
  # energy_level_1 <- 999
  # energy_level_2 <- 1430
  # step_forget <- 87
  # p_memory <- 2
  # duration <- 4
  
  # alternatively, one could use the calibrated parameters:
  # ********** ONLY AVAILABLE FOR WHEN FEEDINBOUT = ON **********
  # Input data from calibration (Calibrated values for unknown parameters)
  # If you want to use the calibrated parameters and not the model values defined by sliders
  calibrated_on <- "false"
  
  if (calibrated_on == "true") {
    
    path <- here("Model_analysis", "Genetic_analysis", "temp")
    
    filesga <- list.files(path, pattern = "feedingbouton.csv")
    filesga <- paste0(path, "/", filesga)
    
    dfga <- data.frame("expname" = character(), "parameter" = character(), "value" = double())
    for (i in filesga) {
      # i <- filesga[2]
      filei <- read.csv(i, encoding = "UTF-8")
      dfga <- dplyr::bind_rows(dfga, filei)
    }
    
    # dfga <- dfga %>% 
    #   dplyr::mutate(parameter, str_replace_all(., c("-" = "_", 
    #                                                 # "\\." = "_",
    #                                                 " " = "")))# %>% 
    
    a <- dfga$expname %>%  
      stringr::str_split(., pattern = "_", simplify = TRUE)
    
    dfga <- dfga %>% 
      mutate(
        group = as.vector(a[, 1]),
        month_run = as.vector(a[, 2])
      ) %>% 
      as.data.frame()
    
    # dfga %>% str()
    # dfga %>% class()
    
    dfga[ 1:11, "group"] <- "Guareí" # correct wrong character
    
    # dfga <- dfga %>%
    #   dplyr::mutate(group = recode(group, strerror = "Guareí")) # only to match those of the NetLogo model
    
    dfga <- dfga %>%
      mutate_if(is.character, as.factor)
    
    # dfga %>% str()
    dfga$parameter %>% levels()
    
    
    
    energy_from_fruits <- dfga %>%
      dplyr::filter(group == gsub(area_run, pattern = ('\"'), replacement = '', fixed = T)) %>%
      dplyr::filter(parameter == "energy-from-fruits") %>%
      dplyr::select(value) %>%
      pull()
    
    energy_from_prey <- dfga %>%
      dplyr::filter(group == gsub(area_run, pattern = ('\"'), replacement = '', fixed = T)) %>%
      dplyr::filter(parameter == "energy-from-prey") %>%
      dplyr::select(value) %>%
      pull()
    
    energy_loss_traveling <- dfga %>%
      dplyr::filter(group == gsub(area_run, pattern = ('\"'), replacement = '', fixed = T)) %>%
      dplyr::filter(parameter == "energy-loss-traveling") %>%
      dplyr::select(value) %>%
      pull()
    
    energy_loss_foraging <- dfga %>%
      dplyr::filter(group == gsub(area_run, pattern = ('\"'), replacement = '', fixed = T)) %>%
      dplyr::filter(parameter == "energy-loss-foraging") %>%
      dplyr::select(value) %>%
      pull()
    
    energy_loss_resting <- dfga %>%
      dplyr::filter(group == gsub(area_run, pattern = ('\"'), replacement = '', fixed = T)) %>%
      dplyr::filter(parameter == "energy-loss-resting") %>%
      dplyr::select(value) %>%
      pull()
    
    start_energy <- dfga %>%
      dplyr::filter(group == gsub(area_run, pattern = ('\"'), replacement = '', fixed = T)) %>%
      dplyr::filter(parameter == "start-energy") %>%
      dplyr::select(value) %>%
      pull()
    
    energy_level_1 <- dfga %>%
      dplyr::filter(group == gsub(area_run, pattern = ('\"'), replacement = '', fixed = T)) %>%
      dplyr::filter(parameter == "energy_level_1") %>%
      dplyr::select(value) %>%
      pull()
    
    energy_level_2 <- dfga %>%
      dplyr::filter(group == gsub(area_run, pattern = ('\"'), replacement = '', fixed = T)) %>%
      dplyr::filter(parameter == "energy_level_2") %>%
      dplyr::select(value) %>%
      pull()
    
    step_forget <- dfga %>%
      dplyr::filter(group == gsub(area_run, pattern = ('\"'), replacement = '', fixed = T)) %>%
      dplyr::filter(parameter == "step_forget") %>%
      dplyr::select(value) %>%
      pull()
    
    p_memory <- dfga %>%
      dplyr::filter(group == gsub(area_run, pattern = ('\"'), replacement = '', fixed = T)) %>%
      dplyr::filter(parameter == "prop_trees_to_reset_memory") %>%
      dplyr::select(value) %>%
      pull()
    
    duration <- dfga %>%
      dplyr::filter(group == gsub(area_run, pattern = ('\"'), replacement = '', fixed = T)) %>%
      dplyr::filter(parameter == "duration") %>%
      dplyr::select(value) %>%
      pull()
  }
  

  
  ### Attach to nl object ----
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
                              metrics = c(
                                # "timestep",
                                "day",
                                "timestep",
                                
                                "g_SDD",
                                "g_SDD_sd",
                                "n",
                                "p-visited-trees",
                                # "g_n_visited_trees",
                                # "g_n_unvisited_trees"
                                
                                "R_seeds",        
                                "R_seeds_p",
                                "NN_seeds",       
                                "NN_feeding_trees", # this is calculated again by the end of the run although it was calculated priorly within the build_forest process
                                "NN_sleeping_trees", # might be a more important factor affecting SDD than the NN of feeding trees 
                                
                                # turtle variables as globals:
                                "g_energy_stored", # energy is reset to energy-start in many situations (see flowchart)
                                # "g_energy",      # does not exist anymore. It was the final energy value        # the best set of parameters should make tamarins viable in energetic terms
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
                                "g_step_length_mean",    # besides the parameterization, agents interactions make the observed step length and turning angles change
                                "g_step_length_sd",      # besides the parameterization, agents interactions make the observed step length and turning angles change
                                "g_turn_ang_mean",     # this one is quite consistent, so I don't think this one is necessary
                                "g_turn_ang_sd",          # this one might be interesting though (but I haven't estimated the empirical ones yet)
                                
                                # additional movement variables
                                "g_MR",               # movement rate (MR) is used to predict SDD by primates: http://doi.wiley.com/10.1002/ajp.22659
                                "g_MR_sd",
                                # "g_MSD",              # other modelling studies have used this one (https://doi.org/10.3390/ani12182412.), but I believe it is very similar to MR
                                # "g_intensity_use",    # bether than MSD in my oppinion: read about it in: https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en
                                "g_PT",               # Highly recommended as it is the only unbiased estimator of tortuosity. Almost the same thing as intensity of use (IU). path twisting is used by Fuzessy et al 2017 to predict SDD among primates: http://doi.wiley.com/10.1002/ajp.22659
                                "g_PT_sd",
                                # "g_straightness",   # straightness is being wrongly estimated. DON'T USE IT NOW (first make it to be calculated in daily basis). straightness and sinuosity are slightlty different in terms of properties (https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en) and they were not tested as predictors of SDD, so i'm not using them
                                # "g_sinuosity"       # sinuosity can't be compared across scales. DON'T USE IT straightness and sinuosity are slightlty different in terms of properties (https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en) and they were not tested as predictors of SDD, so i'm not using them
                                "g_DI_index",
                                "g_M_index"
                                
                                
                                
                              ),
                              metrics.turtles = list(
                                
                                # For some reason the Morris simdesign only assess global metrics
                                
                                # "monkeys" = c(
                                #   # "energy",      # final energy                # the best set of parameters should make tamarins viable in energetic terms
                                #   # "enlvl1",         # value of energy_level_1 used at the start of the simulation
                                #   # "enlvl2",         # value of energy_level_2 used at the start of the simulation 
                                #   # "enstart",          # start-energy value at the start of the similuation
                                #   "DPL",         # mean DPL (in case we don't want all the values -> good for bar and pointrange plots)
                                #   # "DPL_d",       # DPL is set to 0 everyday    # the best set of parameters should reproduce the observed DPL
                                #   "DPL_sd",      # sd DPL  (in case we don't want all the values -> good for bar and pointrange plots)
                                #   "KDE_95",      # final value                 # the best set of parameters should reproduce the observed home range
                                #   "KDE_50",      # final value                 # the best set of parameters should reproduce the observed core area
                                #   "p_feeding",   # final value                 # the best set of parameters should optimize the activity budget
                                #   "p_foraging",  # final value                 # the best set of parameters should optimize the activity budget
                                #   "p_traveling", # final value                 # the best set of parameters should optimize the activity budget
                                #   "p_resting",    # final value                 # the best set of parameters should optimize the activity budget
                                #   # "step_length_mean",    # besides the parameterization, agents interactions make the observed step length and turning angles change
                                #   # "step_length_sd",      # besides the parameterization, agents interactions make the observed step length and turning angles change
                                #   # "turn_ang_mean",     # this one is quite consistent, so I don't think this one is necessary
                                #   # "turn_ang_sd",          # this one might be interesting though (but I haven't estimated the empirical ones yet)
                                #   
                                #   # additional movement variables
                                #   # "MR",               # movement rate (MR) is used to predict SDD by primates: http://doi.wiley.com/10.1002/ajp.22659
                                #   # "MR_sd",
                                #   # "MSD",              # other modelling studies have used this one (https://doi.org/10.3390/ani12182412.), but I believe it is very similar to MR
                                #   # "intensity_use",    # bether than MSD in my oppinion: read about it in: https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en
                                #   # "PT",               # path twisting is used by Fuzessy et al 2017 to predict SDD among primates: http://doi.wiley.com/10.1002/ajp.22659
                                #   # "PT_sd",
                                #   # "straightness",   # straightness and sinuosity are slightlty different in terms of properties (https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en) and they were not tested as predictors of SDD, so i'm not using them
                                #   # "sinuosity"       # straightness and sinuosity are slightlty different in terms of properties (https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en) and they were not tested as predictors of SDD, so i'm not using them
                                #   
                                #   "n_visited_trees",
                                #   "n_unvisited_trees"
                                
                                
                              ), # "who" "color"
                              variables = list(
                                
                                # energy
                                
                                "energy_stored_val" = list(min = 100, max = 2000, qfun="qunif"), # THIS IS THE MAIN DIFFERENCE FROM MODEL V1.1 TO V1.2
                                
                                "energy-from-fruits" = list(min=1, max = 300, qfun="qunif"),
                                'energy-from-prey' = list(min=1, max=300, qfun="qunif"),
                                "energy-loss-traveling" = list(min=-100, max = -1, qfun="qunif"), #, step = 2
                                "energy-loss-foraging" = list(min=-100, max = -1, qfun="qunif"),
                                "energy-loss-resting" = list(min=-100, max = -1, qfun="qunif"),

                                # "start-energy" = list(min=1000, max=2000, qfun="qunif"),
                                # "energy_level_1" = list(min=100, max=2000, qfun="qunif"),
                                # "energy_level_2" = list(min=100, max=2000, qfun="qunif"),
                                
                                # trying to make energy_level_1 always lower than energy_level_2
                                "energy_level_1" = list(min=100, max=1000, qfun="qunif"),
                                "energy_level_2" = list(min=1001, max=2000, qfun="qunif"),
                                
                                # 4. Movement
                                # "step_len_travel" = list(min= 0, max = 100)         # only when step-model-param? = 'false'
                                # "step_len_forage" = list(min= 0, max = 100)         # only when step-model-param? = 'false'
                                # "max_rel_ang_travel_75q" = list(min= 0, max= 180)   # only when step-model-param? = 'false'
                                # "max_rel_ang_forage_75q" = list(min= 0, max= 180)   # only when step-model-param? = 'false'
                                
                                # "p_foraging_while_traveling" = list(min= 0, max= 1) # only when p-forage-param? = 'false'
                                
                                # memory
                                "step_forget" = list(min=3, max = 400, qfun="qunif"),
                                # "visual" = list(min=0, max = 3),
                                'prop_trees_to_reset_memory' = list(min=2, max=8, qfun="qunif"),   # Initially I didn't think this one is needed (mainly because of the first sensitivity analysis in Guareí), but this might help (as step_forget) making some regions of the home range to not be targeted
                                
                                # 5. Feeding bout (only when "feedingbout-on?" = 'false')
                                # 'species_time_val' = list(min = 1, max = 10, qfun="qunif"),
                                
                                'duration' = list(min = 1, max = 25, qfun="qunif"),      #
                                'p-timesteps-to-rest' = list(min = 0.05, max = 0.9, qfun="qunif"),
                                'p_disputed_trees' = list(min = 0.1, max = 1, qfun="qunif")
                                
                                # 6. Seed dispersal
                                # "gut_transit_time_val" = 15,    # this won't be optimized as it is an emerging pattern
                                # "n_seeds_hatched" = 1           # this won't be optimized as it is an emerging pattern
                              ),
                              
                              constants = list(
                                
                                "USER" = user_scp, # "\"Eduardo\"",
                                'feedingbout-on?' = feedingbout,        # uses empirical values of time spent feeding on each tree species or specified one (defined in the experiment)
                                "step-model-param?" = step_model_param, # uses observed mean step length and 75q turning angles
                                "gtt-param?"= gtt_param,                # uses mean + sd of GTT for seed dispersal
                                "p-forage-param?" = p_forage_param,     # uses empirical probabilities of foraging while traveling (p_foraging = p_foraging + p_traveling)
                                'ld-target-random?' = ld_target,          # uses p_disputed_trees to target trees close to borders
                                
                                # "print-step?" = "false",
                                # 'export-png'= "false",
                                # "show-energy?" = "false",
                                # "show-path?" = "false",
                                # "all-slp-trees?" = "false",
                                # "path-color-by-day?" = "false",
                                
                                ### resource scenario
                                "study_area" = area_run_scp,               # "\"Taquara\"",   # we are optimizing with Taquara as it is the most natural condition
                                "feeding-trees-scenario" = month_run_scp,  #"\"Jan\"",        # we are optimizing with Taquara as it is the most natural condition
                                'no_days' = no_days_run, # DON'T TRY no_days = 1
                                'simulation-time' = simultime_run
                                # 'feeding-trees?' = "true",
                                # 'sleeping-trees?' = "true",
                                # 'sleeping-trees-scenario' = "\"empirical\"",
                                # 'empirical-trees-choice' = "\"closest\"",
                                
                                ### memory
                                # 'duration' = duration,
                                # 'visual' = 2,
                                # "step_forget" = step_forget,
                                # 'prop_trees_to_reset_memory' = p_memory,
                                
                                ### energy
                                # 'start-energy' = , start_energy,
                                # "energy_level_1" = energy_level_1,
                                # "energy_level_2" = energy_level_2,
                                # "energy-from-seeds" = energy_from_prey,# ?
                                # "energy-from-prey" = energy_from_fruits,
                                # "energy-loss-traveling" = energy_loss_traveling,
                                # "energy-loss-foraging" = energy_loss_foraging,
                                # "energy-loss-resting" = energy_loss_resting,
                                # "gut_transit_time_val" = 15,
                                # "n_seeds_hatched" = 1,
                                
                                ### movement
                                # travel_speed = 1
                                
                              )
  )
  
  # Step 3: Attach a simulation design.
  # install.packages("morris")
  # library("morris")
  
  nseeds <- 10  #4  # (= num repetitions -> match with n runs and ncores)
  # Specify seeds manually (not needed if set.seed() is used at the start of the loop)
  # nseeds = c(
  #   184309146, -1836669376,  -143944772,  -687600015,   815694198,    60805450,    64093864,   195482662,
  #   -225669049, -1787201002,  1847410139, -2076839226,  -368970335, -1172628750, -1719530584,   -73341398,
  #   644797734,  1809597619,  -590120745,  1525293168
  # )
  
  nl@simdesign <- simdesign_morris(nl = nl,
                                   morristype = "oat",
                                   morrislevels = 8, # sets the number of different values for each parameter (sampling density)
                                   morrisr = 20, # 10, # sets the number of repeated samplings (sampling size)
                                   morrisgridjump = 4, # sets the number of levels that are increased/decreased for computing the elementary effects. . Morris recommendation is to set this value to levels / 2.
                                   nseeds = nseeds)

  # Check seeds:  
  # print( nl@simdesign@simseeds)  ;  i <- i + 1  }

  # More information on the Morris specific parameters can be found in the description of the morris function in the sensitivity package (?morris).
  # ?morris
  report_model_parameters(nl)
  
  
  # Step 4: Run simulations
  # Evaluate nl object:
  eval_variables_constants(nl)
  
  print(nl)
  
  # Check all parameter combinations
  par <- nl@simdesign@siminput
  
  
  # # Test each parameter combination
  # tictoc::tic()
  # progressr::handlers("progress")
  # p <- 1
  # for (p in 1:nrow(nl@simdesign@siminput)) {
  # results %<-% progressr::with_progress(
  #   run_nl_one(nl = nl
  #              , seed = getsim(nl, "simseeds")[1]
  #              , siminputrow = p)
  # )
  # tictoc::toc()
  # print("================ One siminputrow Finished! =========================")
  # parp <- par[p ,]
  # p <- p + 1
  # }
  
  # Run all simulations (loop over all siminputrows and simseeds)
  
  library(future)
  library(tictoc)
  
  # plan(multisession)
  ## plan(list(sequential, multiseprocess))
  ## plan(list(sequential, multisession))
  plan(list(sequential, multicore))
  # split_param <- min(nrow(nl@simdesign@siminput), ((ncores - 2)/nseeds))
  tictoc::tic()
  progressr::handlers("progress")
  results %<-% progressr::with_progress(
    run_nl_all(nl = nl
               , split = 10)
  )
  tictoc::toc()
  print("================ Finished! =========================")
  results$USER <- NULL
  # results$'feeding-trees-scenario' <- NULL
  # results$'start-energy' <- NULL
  # results$energy_level_1  <- NULL
  # results$energy_level_2 <- NULL
  # results$'energy-from-fruits' <- NULL
  # results$'energy-from-prey' <- NULL
  # results$'energy-loss-traveling' <- NULL
  # results$'energy-loss-foraging' <- NULL
  # results$'energy-loss-resting' <- NULL
  # results$gut_transit_time_val <- NULL
  # results$n_seeds_hatched <- NULL
  
  ### movement
  # results$travel_speed_val <- NULL
  
  ### others
  # results$'simulation-time' <- NULL 
  
  # Step 5: Attach results to nl and run analysis In order to run the
  # analyze_nl function, the simulation output has to be attached to the
  # nl object first. The simdesign class within the nl object provides a
  # slot for attaching output results (simoutput). An output results
  # tibble can be attached to this slot by using the simdesign setter
  # function setsim(nl, "simoutput"). After attaching the simulation
  # results, these can also be written to the defined outpath of the
  # experiment object.  Attach results to nl object:
  setsim(nl, "simoutput") <- results
  
  print(nl)
  
  print("================== save nl! ==========================")
  # save(nl, file = paste0(nl@experiment@outpath, "/"
  #      , "morris_2022-07-20_feedingbout-off.RData"))
  # load(file = paste0(nl@experiment@outpath, "/"
  #                         , "morris_2022-07-23d_feedingbout-off.RData"))
  saveRDS(nl, file.path(paste0(nl@experiment@outpath
                               , "/"
                               # , "morris_2022-12-23d_feedingbout-"
                               , expname, 
                               "_Feedingbout_"
                               , if(feedingbout) {"on"} else {"off"}
                               , "_"
                               , Sys.Date()
                               , ".rds")))
  # readRDS(file.path(paste0(nl@experiment@outpath, "/"
  #                          , "morris_2022-12-23d_feedingbout-"
  #                          , if(feedingbout) {"on"} else {"off"}
  #                          , ".rds")))
  
  # print("================ save unnest =========================")
  # results_unnest <- unnest_simoutput(nl)
  # save(results_unnest,file = paste0(nl@experiment@outpath, "/"
  #                                , "results_unnest_2022-12-23_feedingbout"
  #                                , if(feedingbout) {"-on"} else {"-off"}
  #                                , ".RData"))
  
  
  i <- i + 1
  
}


