# Script name: 00_prep-ga.R
# Script purpose: identify the best set of energetic parameters that better predict 
# tamarins movement patterns/range behavior of Taquara group, the group which inhabits
# the most 'natural' condition

# Date created: 2022-11-25d
# Last update: 2023-11-03d
# Author: Eduardo Zanette


## Notes --------------------------- 


#################################################################################################################

## ###### ***** DO NOT FORGET TO COMMENT OUT CALC-SEED-AGGREGATION PROCEDURE IN THE NETLOGO MODEL ***** ###### ##

#################################################################################################################


## Options -------------------------
# (plotting, memory limit, decimal digits)
# 

## Packages -------------------------
library("here")
library("tidyverse")
library("nlrx")
library("progressr")
library("future")
library("tictoc")
library("magrittr")
# library("GenSA")
library("genalg")

## Config cores
# ncores <- parallel::detectCores() # ga is not paralelized

## Java memory:
if(Sys.getenv("JAVA_HOME") == "") {
  if(Sys.info()[["sysname"]] == "Linux") {
    Sys.setenv(JAVA_HOME = "/usr/lib/jvm/java-11-openjdk-amd64")
    # unixtools::set.tempdir(".")
  } else {
    Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk-11")
  }
}

## ---------------------------


# Step 1: Create nl object

if(Sys.info()[["nodename"]] == "DESKTOP-R12V3D6") {
  netlogopath <- file.path("C:/Program Files/NetLogo 6.3.0")
  modelpath <- here("Model_simulations", "BLT_model_v1.2.nlogo")
  outpath <- here("Model_analysis", "Genetic_analysis", "v1.2", "temp")
  user_scp = "\"Eduardo\""
}
if(Sys.info()[["nodename"]] == "PC9") { # LEEC
  netlogopath <- file.path("C:/Program Files/NetLogo 6.3.0")
  modelpath <- here("Model_simulations", "BLT_model_v1.2.nlogo") # Last version with stored-energy
  Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8.0_351")
  user_scp = "\"LEEC\""
}
if(Sys.info()[["nodename"]] == "DESKTOP-1SKTQUA") { # AORUS-2 (LaP)
  netlogopath <- file.path("C:/Program Files/NetLogo 6.3.0")
  modelpath <- here("Model_simulations", "BLT_model_v1.2.nlogo") # Last version with stored-energy
  outpath <- here("Model_analysis", "Genetic_analysis", "v1.2", "temp")
  user_scp = "\"AORUS-2\""
}
if(Sys.info()[["nodename"]] == "PC146") { # Ronald (NW-FVA)
  netlogopath <- file.path("/opt/netlogo_622")
  modelpath <- here("Model_simulations", "BLT_model_v1.2.nlogo") # Last version with stored-energy
  outpath <- here("Model_analysis", "Genetic_analysis", "v1.2", "temp")
  user_scp = "\"Ronald\""
}



nl <- nl(nlversion = "6.3.0",  ## "6.3.0"
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

nlogo_model_param <- report_model_parameters(nl)
nlogo_model_param




# Empirical data for parameterisation:
param.table <- read.csv(here("Data", "Parameter_table.csv"),
                        sep = ",", dec = ".", stringsAsFactors = TRUE,
                        encoding = "UTF-8") %>% 
  dplyr::mutate(group = dplyr::recode(group, "Guarei" = "Guareí",
                                      "Santa Maria" = "SantaMaria"))  # only to match those of the NetLogo model

## Step 2: Attach an experiment and run ------------------------

i <- 1
# for (i in i:nrow(param.table)) {
    for (i in i:nrow(param.table)) {   # subset runs here, not before (otherwise min and max values change)
  
      # print(nrow(param.table)) 
      # print(param.table$group) ; print(param.table$month)
      # } # test loop

  set.seed(21) # for having the same generated seeds everytime
  # set.seed(21)
  
  
  # ## Decide which area/month to run the optimization on: ----
  # area_run <- "Taquara"
  # month_run <- "Jan"
  
  area_run_scp <- paste0('\"', param.table$group[i], '\"')
  month_run_scp <- paste0('\"', param.table$month[i], '\"')
  
  area_run <- gsub(area_run_scp, pattern = ('\"'), replacement = '', fixed = T)
  month_run <- gsub(month_run_scp, pattern = ('\"'), replacement = '', fixed = T)
  
  ## Decide parameterisations: ----
  
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
  
  
  
  # Empirical data for criteval function:
  values_ga <- read.csv(here("Data", "Validation-table.csv"),
                        sep = ",", dec = ".", stringsAsFactors = TRUE,
                        encoding = "UTF-8") %>% 
    dplyr::mutate(group = dplyr::recode(group, "Guarei" = "Guareí",
                                        "Santa Maria" = "SantaMaria")) %>% # only to match those of the NetLogo model
    rename(
      "month" = "id_month"
    ) %>% 
    # replace NA observations that we know are = 0
    mutate(
      across(everything(), ~replace_na(.x, 0))
    )
  
  
  # Movement variables:
  values_ga_mv <-  read.csv(here("Data", "Movement", "Curated", "Validation", "Siminputrow_MR-PT_by-day.csv"),
                            sep = ",", dec = ".", stringsAsFactors = TRUE,
                            encoding = "UTF-8") %>% 
    dplyr::mutate(group = dplyr::recode(group, "Guarei" = "Guareí",
                                        "Santa Maria" = "SantaMaria")) %>% # only to match those of the NetLogo model
    rename(
      "month" = "id_month"
    )
  
  
  
  ## Create evaluation criteria function -------------------------
  
  # 1) Get min and max values for each parameter -----------------
  
  # Same as in the Morris sensitivity (MJuly)
  energy_min <- 0
  energy_max <- 2200 # a little bit more considering the store_energy mnight not be called every timestep
  
  # energy_stored_min <- 0
  # energy_stored_max <- 100000
  
  # KDE95_max <- values_ga %>% dplyr::select(KDE95) %>% unlist() %>% max() %>% as.vector()
  KDE95_max <- 400 + 100 # (400 is the year-round home range of Taquara group)
  KDE50_max <- values_ga %>% dplyr::select(KDE50) %>% unlist() %>% max() %>% as.vector()
  KDE50_max <- KDE50_max + 50
  
  KDE95_min <- 0
  # KDE95_min <- values_ga %>% dplyr::select(KDE95) %>% unlist() %>% min() %>% as.vector()
  KDE50_min <- 0
  # KDE50_min <- values_ga %>% dplyr::select(KDE50) %>% unlist() %>% min() %>% as.vector()
  
  # p_feeding_max <- values_ga %>% dplyr::select("Frugivory_perc_behavior_mean") %>% unlist() %>% max() %>% as.vector() %>% '/' (100)
  p_feeding_max <- 1
  # p_feeding_min <- values_ga %>% dplyr::select("Frugivory_perc_behavior_mean") %>% unlist() %>% min() %>% as.vector() %>% '/' (100)
  p_feeding_min <- 0
  # p_foraging_max <- values_ga %>%  dplyr::select("Foraging_perc_behavior_mean") %>% unlist() %>% max() %>% as.vector() %>% '/' (100)
  p_foraging_max <- 1
  # p_foraging_min <- values_ga %>%  dplyr::select("Foraging_perc_behavior_mean") %>% unlist() %>% min() %>% as.vector() %>% '/' (100)
  p_foraging_min <- 0
  # p_traveling_max <- values_ga %>% dplyr::select("Travel_perc_behavior_mean") %>% unlist() %>% max() %>% as.vector() %>% '/' (100)
  p_traveling_max <- 1
  p_traveling_min <- values_ga %>% dplyr::select("Travel_perc_behavior_mean") %>% unlist() %>% min() %>% as.vector() %>% '/' (100)
  # p_traveling_min <- 0.1
  # p_resting_max <- values_ga %>% dplyr::select("Resting_perc_behavior_mean") %>% 
  #   dplyr::filter(!is.na(.)) %>%  unlist() %>% max() %>% as.vector() %>% '/' (100)
  p_resting_max <- 1
  # p_resting_min <- values_ga %>% dplyr::select("Resting_perc_behavior_mean") %>% 
  #   dplyr::filter(!is.na(.)) %>%  unlist() %>% min() %>% as.vector() %>% '/' (100)
  p_resting_min <- 0 # (Guareí NA = 0, i.e., no resting)
  # obs: p_feeding_obs + p_foraging_obs + p_traveling_obs + p_resting_obs don't sum up to 1 (these are values filtered for the four behaviors in the model)
  
  
  # Step lenghts and turning angles were not included in v1.1:
  step_length_mean_min <- 0
  step_length_mean_max <- 50
  step_length_sd_min <- 0
  step_length_sd_max <- 100
  ta_sd_min <- 0
  ta_sd_max <- 100
  # step_length_mean_min <- param.table %>%
  #   dplyr::select("step_len_mean") %>% min() %>% unlist() %>% as.vector()
  # step_length_mean_max <- param.table %>%
  #   dplyr::select("step_len_mean") %>% max() %>% unlist() %>% as.vector()
  # step_length_sd_min <- param.table %>%
  #   dplyr::select("step_len_sd") %>% min() %>% unlist() %>% as.vector()
  # step_length_sd_max <- param.table %>%
  #   dplyr::select("step_len_sd") %>% max() %>% unlist() %>% as.vector()
  # ta_sd_min <- param.table %>%
  #   dplyr::select("rel_angle_sd") %>% min() %>% unlist() %>% as.vector()
  # ta_sd_max <- param.table %>%
  #   dplyr::select("rel_angle_sd") %>% max() %>% unlist() %>% as.vector()
  
  # param.table %>% colnames()
  # step_length_sd
  # turn_ang_sd
  # energy
  
  max_angle_75_travel_max <- param.table %>% 
    dplyr::select(max_random_angle_75q_Travel) %>% max() %>% unlist() %>% as.vector()
  max_angle_75_travel_min <- param.table %>% 
    dplyr::select(max_random_angle_75q_Travel) %>% min() %>% unlist() %>% as.vector()
  
  max_angle_75_forage_max <- param.table %>% 
    dplyr::select(max_random_angle_75q_Foraging) %>% max() %>% unlist() %>% as.vector()
  max_angle_75_forage_min <- param.table %>% 
    dplyr::select(max_random_angle_75q_Foraging) %>% min() %>% unlist() %>% as.vector()
  
  
  values_ga_mv_summary <- values_ga_mv %>% 
    dplyr::group_by(group, month) %>% 
    dplyr::summarise(
      DPL_mean_obs = mean(DPL),
      DPL_sd_obs = sd(DPL),
      MR_mean_obs = mean(MR), 
      MR_sd_obs = sd(MR),
      PT_mean_obs = mean(PT),
      PT_sd_obs = sd(PT)
    ) %>% 
    dplyr::ungroup()
  #dplyr::filter(group == area_run & month == month_run)
  
  dpl_mean_min <- values_ga_mv_summary %>%
    dplyr::select(DPL_mean_obs) %>% min() %>%  unlist() %>% as.vector()
  dpl_mean_max <- values_ga_mv_summary %>%
    dplyr::select(DPL_mean_obs) %>% max() %>%  unlist() %>% as.vector()
  dpl_sd_min <- values_ga_mv_summary %>%
    dplyr::select(DPL_sd_obs) %>% min() %>%  unlist() %>% as.vector()
  dpl_sd_max <- values_ga_mv_summary %>%
    dplyr::select(DPL_sd_obs) %>% max() %>%  unlist() %>% as.vector()
  
  
  mr_min <- 0
  # mr_min <- values_ga_mv_summary %>%
  #   dplyr::select(MR_mean_obs) %>% min() %>%  unlist() %>% as.vector()
  mr_max <- 500 
  # mr_max <- values_ga_mv_summary %>%
  #   dplyr::select(MR_mean_obs) %>% max() %>% unlist() %>% as.vector()
  mr_sd_min <- 0
  # mr_sd_min <- values_ga_mv_summary %>%
  #   dplyr::select(MR_sd_obs) %>% min() %>%  unlist() %>% as.vector()
  mr_sd_max <- 500
  # mr_sd_max <- values_ga_mv_summary %>%
  #   dplyr::select(MR_sd_obs) %>%max() %>% unlist() %>% as.vector()
  
  pt_min <- 0
  # pt_min <- values_ga_mv_summary %>%
  #   dplyr::select(PT_mean_obs) %>% min() %>% unlist() %>% as.vector()
  pt_max <- 50 
  # pt_max <- values_ga_mv_summary %>%
  #   dplyr::select(PT_mean_obs) %>% max() %>% unlist() %>% as.vector()
  # pt_sd_min <- values_ga_mv_summary %>%
  #   dplyr::select(PT_sd_obs) %>% min() %>% unlist() %>% as.vector()
  pt_sd_min <- 0 # check which value is the minimum theoritically possible
  pt_sd_max <- 100
  # pt_sd_max <- values_ga_mv_summary %>%
  #   dplyr::select(PT_sd_obs) %>% max() %>% unlist() %>% as.vector()
  
  
  # tamarins visit all the observed trees, but they don't usually visit all of them in the model (might be related to memory)
  visits_max <- param.table %>% 
    dplyr::select(n_trees_Frugivory) %>% max() %>% unlist() %>% as.vector()
  visits_min <- param.table %>% 
    dplyr::select(n_trees_Frugivory) %>% min() %>% unlist() %>% as.vector()
  
  
  
  # 2) Normalizing (min-max; 0-1) observed parameters --------------
  
  # normalized = (x-min(x))/(max(x)-min(x))
  # normalize <- function(x) {
  #   x.norm <- (x-min(x))/(max(x)-min(x))
  #   return(x.norm)
  # }
  # values_ga_mv_summary.norm <- values_ga_mv_summary %>% 
  #   mutate_if(is.numeric, normalize)
  # 
  # values_ga_mv.norm <- values_ga_mv %>% 
  #   mutate_if(is.numeric, normalize)
  
  normalize <- function(x, min, max) {
    x.norm <- ((x-min)/(max-min))
    return(x.norm)
  }
  # normalize(20, min=2.5, max=100)
  
  KDE95_obs <- values_ga %>% dplyr::filter(group == area_run & month == month_run) %>%
    dplyr::select(KDE95) %>% unlist() %>% as.vector() %>% normalize(min = KDE95_min, max = KDE95_max)
  KDE50_obs <- values_ga %>% dplyr::filter(group == area_run & month == month_run) %>%
    dplyr::select(KDE50) %>% unlist() %>% as.vector() %>% normalize(min = KDE50_min, max = KDE50_max)
  
  MR_obs <- values_ga_mv_summary %>% dplyr::filter(group == area_run & month == month_run) %>%
    dplyr::select("MR_mean_obs") %>% unlist() %>% as.vector() %>% normalize(min = mr_min, max = mr_max)
  PT_obs <- values_ga_mv_summary %>% dplyr::filter(group == area_run & month == month_run) %>%
    dplyr::select("PT_mean_obs") %>% unlist() %>% as.vector() %>% normalize(min = pt_min, max = pt_max)
  
  p_feeding_obs <- values_ga %>%  dplyr::filter(group == area_run & month == month_run) %>%
    dplyr::select("Frugivory_perc_behavior_mean") %>% unlist() %>% as.vector() %>%  '/' (100) %>% 
    normalize(min = p_feeding_min, max = p_feeding_max)
  p_foraging_obs <- values_ga %>%  dplyr::filter(group == area_run & month == month_run) %>%
    dplyr::select("Foraging_perc_behavior_mean") %>% unlist() %>% as.vector() %>%  '/' (100) %>% 
    normalize(min = p_foraging_min, max = p_foraging_max)
  p_traveling_obs <- values_ga %>%  dplyr::filter(group == area_run & month == month_run) %>%
    dplyr::select("Travel_perc_behavior_mean") %>% unlist() %>% as.vector() %>%  '/' (100) %>% 
    normalize(min = p_traveling_min, max = p_traveling_max)
  p_resting_obs <- values_ga %>%  dplyr::filter(group == area_run & month == month_run) %>%
    dplyr::select("Resting_perc_behavior_mean") %>% unlist() %>% as.vector() %>% '/' (100) %>% 
    normalize(min = p_resting_min, max = p_resting_max)
  
  
  visits_obs <- param.table %>% 
    dplyr::filter(group == area_run) %>% 
    dplyr::filter(month == month_run) %>%
    dplyr::select(n_trees_Frugivory) %>% unlist() %>% as.vector() %>% 
    normalize(min = visits_min, max = visits_max)
  
  
  DPL_mean_obs <- values_ga_mv_summary %>% 
    dplyr::filter(group == area_run & month == month_run) %>%
    dplyr::select(DPL_mean_obs)  %>% unlist() %>% as.vector() %>% 
    normalize(min = dpl_mean_min, max = dpl_mean_max)
  DPL_sd_obs <- values_ga_mv_summary %>% 
    dplyr::filter(group == area_run & month == month_run) %>%
    dplyr::select(DPL_sd_obs)  %>% unlist() %>% as.vector() %>% 
    normalize(min = dpl_sd_min, max = dpl_sd_max)
  
  MR_mean_obs <- values_ga_mv_summary %>% 
    dplyr::filter(group == area_run & month == month_run) %>%
    dplyr::select(MR_mean_obs)  %>% unlist() %>% as.vector() %>% 
    normalize(min = mr_min, max = mr_max)
  MR_sd_obs <- values_ga_mv_summary %>% 
    dplyr::filter(group == area_run & month == month_run) %>%
    dplyr::select(MR_sd_obs)  %>% unlist() %>% as.vector() %>% 
    normalize(min = mr_sd_min, max = mr_sd_max)
  
  PT_mean_obs <- values_ga_mv_summary %>%
    dplyr::filter(group == area_run & month == month_run) %>%
    dplyr::select(PT_mean_obs)  %>% unlist() %>% as.vector() %>% 
    normalize(min = pt_min, max = pt_max)
  PT_sd_obs <- values_ga_mv_summary %>% 
    dplyr::filter(group == area_run & month == month_run) %>%
    dplyr::select(PT_sd_obs)  %>% unlist() %>% as.vector() %>% 
    normalize(min = pt_sd_min, max = pt_sd_max)
  
  sl_min <- 0
  sl_max <- 250
  sl_sd_min <- 0
  sl_sd_max <- 250
  t_angle_min <- 0
  t_angle_max <- 180 
  
  sl_mean_obs <- param.table %>% dplyr::filter(group == area_run & month == month_run) %>%
    dplyr::select("step_len_mean")  %>% unlist() %>% as.vector() %>% 
    normalize(min = sl_min, max = sl_max)
  
  sl_sd_obs <- param.table %>% dplyr::filter(group == area_run & month == month_run) %>%
    dplyr::select("step_len_sd")  %>% unlist() %>% as.vector() %>% 
    normalize(min = sl_sd_min, max = sl_sd_max)
  
  t_angle_sd_obs <- param.table %>% dplyr::filter(group == area_run & month == month_run) %>%
    dplyr::select("rel_angle_sd")  %>% unlist() %>% as.vector() %>% 
    normalize(min = t_angle_min, max = t_angle_max)
  
  
  
  ## Step 2: Attach an experiment   -------------------------
  expname <- paste0("GA_", area_run, "_", month_run)
  
  # define how much each run should take based on empirical activity periods
  no_days_run <- param.table %>% 
    dplyr::filter(group == area_run,
                  month == month_run) %>% 
    dplyr::select(ndays) %>% 
    dplyr::pull() #+ 1 # one day more for "initializing" the model (take this first day out when analyzing data?)
  
  simultime_run <- param.table %>% 
    dplyr::filter(group == area_run,
                  month == month_run) %>% 
    dplyr::select(mean_timesteps) %>% 
    dplyr::pull()
  simultime_run <- round(simultime_run * 0.95) # that's the timestep when tamarins should start looking for the sleeping site
  
  # choose which parameterizations should be on:
  step_model_param <- "true" # velocity parameters are setted inside the model. Change this when velocity is summarized and inclued in the parameter table
  gtt_param <- "true" # gtt parameters are setted inside the model. Change this when velocity is summarized and inclued in the parameter table
  p_forage_param <- "true" # p_foraging parameter is setted inside the model. Change this when velocity is summarized and inclued in the parameter table 
  feedingbout <- "true" # previous sensitivity analysis showed that this parameterization diminishes overall stochasticity of the model (Morris)
  
  # # escape strings to nlrx experiment
  # area_run_scp <- paste0('"', area_run, '"')   # scaped
  # month_run_scp <- paste0('"', month_run, '"') # scaped

  
  # Other params
  duration <-  param.table %>% 
    dplyr::filter(group == area_run,
                  month == month_run) %>% 
    dplyr::select(duration) %>% 
    dplyr::pull()
  
  # Attach to nl object
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
                                
                                "survived?",     # first of all check if monkeys survived, otherwise they won't have numeric variables
                                
                                "g_energy_stored", # energy is reset to energy-start everyday, thus we take
                                "g_energy",      # final energy                # the best set of parameters should make tamarins viable in energetic terms
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
                                
                                
                                
                                # additional movement variables
                                "g_MR",               # movement rate (MR) is used to predict SDD by primates: http://doi.wiley.com/10.1002/ajp.22659
                                "g_MR_sd",
                                # "g_MSD",              # other modelling studies have used this one (https://doi.org/10.3390/ani12182412.), but I believe it is very similar to MR
                                # "g_intensity_use",    # bether than MSD in my oppinion: read about it in: https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en
                                "g_PT",               # Highly recommended as it is the only unbiased estimator of tortuosity. Almost the same thing as intensity of use (IU). path twisting is used by Fuzessy et al 2017 to predict SDD among primates: http://doi.wiley.com/10.1002/ajp.22659
                                "g_PT_sd",
                                # "g_straightness",   # straightness is being wrongly estimated. DON'T USE IT NOW (first make it to be calculated in daily basis). straightness and sinuosity are slightlty different in terms of properties (https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en) and they were not tested as predictors of SDD, so i'm not using them
                                # "g_sinuosity"       # sinuosity can't be compared across scales. DON'T USE IT straightness and sinuosity are slightlty different in terms of properties (https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en) and they were not tested as predictors of SDD, so i'm not using them
                                
                                "p-visited-trees",
                                "g_n_visited_trees",
                                # "n_visited_trees",
                                # "n_unvisited_trees"
                                
                                # Included in 2023-09-04d:
                                "g_step_length_mean",    # besides the parameterization, agents interactions make the observed step length and turning angles change
                                "g_step_length_sd",      # besides the parameterization, agents interactions make the observed step length and turning angles change
                                # "g_turn_ang_mean",     # this one is quite consistent (mean = 0), so I don't think this one is necessary
                                "g_turn_ang_sd"          # this one might be interesting though (but I haven't estimated the empirical ones yet)
                                
                              ),
                              metrics.turtles = list(
                                # 
                                #                               "monkeys" = c(
                                #                                 # "energy",      # final energy                # the best set of parameters should make tamarins viable in energetic terms
                                #                                 "enlvl1",         # value of energy_level_1 used at the start of the simulation
                                #                                 "enlvl2"         # value of energy_level_2 used at the start of the simulation
                                #                                 # "enstart",          # start-energy value at the start of the similuation -> does not exist anymore (=enlvl1)
                                #                               )
                                #
                              ), # "who" "color"
                              variables = list(
                                
                                ### DO NOT SPECIFY STEPS FOR GA:
                                
                                # energy
                                "energy-from-fruits" = list(min=1, max = 300),
                                'energy-from-prey' = list(min=1, max=300),
                                "energy-loss-traveling" = list(min=-100, max = -5), # at least 5 times more than the minimum resting
                                "energy-loss-foraging" = list(min=-100, max = -1),
                                "energy-loss-resting" = list(min=-100, max = -1),
                                
                                # trying to make energy_level_1 always lower than energy_level_2
                                "energy_level_1" = list(min=100, max=1000, qfun="qunif"),
                                "energy_level_2" = list(min=1001, max=2000, qfun="qunif"),
                                # "start-energy" = list(min=100, max=2000), -> does not exist anymore (=enlvl1)
                                
                                
                                # 4. Movement
                                # "step_len_travel" = list(min= 0, max = 100)         # only when step-model-param? = 'false'
                                # "step_len_forage" = list(min= 0, max = 100)         # only when step-model-param? = 'false'
                                # "max_rel_ang_travel_75q" = list(min= 0, max= 180)   # only when step-model-param? = 'false'
                                # "max_rel_ang_forage_75q" = list(min= 0, max= 180)   # only when step-model-param? = 'false'
                                
                                # "p_foraging_while_traveling" = list(min= 0, max= 1) # only when p-forage-param? = 'false'
                                
                                # # memory
                                "step_forget" = list(min=3, max = 400, qfun="qunif"),
                                # # "visual" = list(min=0, max = 3),
                                'prop_trees_to_reset_memory' = list(min=2, max=8, qfun="qunif"),   # Initially I didn't think this one is needed (mainly because of the first sensitivity analysis in Guareí), but this might help (as step_forget) making some regions of the home range to not be targeted
                                # 
                                # # 5. Feeding bout (only when "feedingbout-on?" = 'false')
                                # # 'species_time' = list(min = 1, max = 10), #
                                # 'duration' = list(min = 1, max = 25, qfun="qunif"),      #
                                'p-timesteps-to-rest' = list(min = 0.05, max = 0.9, qfun="qunif"),
                                'p_disputed_trees' = list(min = 0.1, max = 1, qfun="qunif")
                                
                                # 6. Seed dispersal -> not aimed at the calibration
                                # "gut_transit_time_val" = 15,    # this won't be optimized as it is an emerging pattern
                                # "n_seeds_hatched" = 1           # this won't be optimized as it is an emerging pattern
                              ),
                              
                              constants = list(
                                
                                
                                "USER" = user_scp, # "\"Eduardo\"",
                                
                                'feedingbout-on?' = "true",             # uses empirical values of time spent feeding on each tree species or specified one (defined in the experiment)
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
                                'simulation-time' = simultime_run,
                                # 'feeding-trees?' = "true",
                                # 'sleeping-trees?' = "true",
                                # 'sleeping-trees-scenario' = "\"empirical\"",
                                # 'empirical-trees-choice' = "\"closest\"",
                                
                                
                                # Guessing values to optimize energy parameters
                                # "step_forget" = 250,
                                # 'prop_trees_to_reset_memory' = 4,
                                'duration' = duration
                                # 'p-timesteps-to-rest' = 0.25,
                                # 'p_disputed_trees' = 0.5
                                
                                
                                ### memory
                                # 'duration' = 3,
                                # 'visual' = 2,
                                # "step_forget" = 130,
                                
                                ### energy
                                # 'start-energy' = 980,
                                # "energy_level_1" = 80,
                                # "energy_level_2" = 150,
                                # "energy-from-seeds" = 4,# ?
                                # "energy-from-prey" = 4,
                                # "energy-loss-traveling" = -1.6,
                                # "energy-loss-foraging" = -2,
                                # "energy-loss-resting" = -1.9,
                                # "gut_transit_time_val" = 15,
                                # "n_seeds_hatched" = 1,
                                
                                ### movement
                                # travel_speed = 1
                                
                              )
  )
  
  
  # eval_simoutput(nl) # not run for optimization designs
  
  
  
  ## Step 3: Attach a simulation design   -------------------------
  
  # nl<- readRDS(here("Model_analysis", "Sensitivity-analysis", "v1.1_November2022", "temp",
  #                   "v1.1_Taquara_Jan_simple-74525742_tempRDS.Rdata"))
  
  
  ### Define critfun (fitness function) --------------
  # deaths <- 0 # to count number of runs where tamarins died (does not work with nlrx)
  # runs <- 0 (does not work with nlrx)
  critfun <- function(nl) {
    
    # extract values from run from example nl object:
    # # db2 <- nlrx::getexp(nl.x, "variables") # or nl@experiment@variables
    # nl.x<- readRDS(here("Model_analysis", "Sensitivity-analysis", "v1.2_2023MJuly", "Param_bestguess", "Simple", "temp",
    # "v1.2_Taquara_Jan_simple674276753_tempRDS.Rdata"))
    # db <- unnest_simoutput(nl.x)
    
    # db <- unnest_simoutput(nl)
    db <- nl@simdesign@simoutput
    # colnames(db)
    
    
    # db <- nl@simdesign@simoutput
    # db <- nlrx::getexp(nl, "metrics.turtles")
    # db3 <- report_model_parameters(nl)
    
    # nl@experiment
    
    
    # nl.x@simdesign@siminput %>% as.data.frame
    # nl@experiment
    # nl@experiment@variables
    # nl@experiment@metrics.turtles
    # str(db)
    # class(db3)
    
    # print(paste("number of dead runs = ", deaths))
    # print(paste("number of runs = ", runs))
    
    if ("g_KDE_95" %in% colnames(db)) { # if monkey variables are present (i.e. they didn't die)
      # if (db$`survived?`[1] == "yes") {
      
      # 1) for criteria:
      # db[[1]][1]
      
      # en1 <- db %>% magrittr::extract("en1") %>%  unlist() %>% na.omit() %>% as.vector() %>%
      en1 <- db %>% dplyr::select("energy_level_1") %>%  unlist() %>% na.omit() %>% as.vector() %>% first() %>%
        # en1 <- db %>% dplyr::select("enlvl1") %>%  unlist() %>% na.omit() %>% as.vector() %>%
        normalize(min = energy_min, max = energy_max)
      # en2 <- db %>% magrittr::extract("enlvl2") %>%  unlist() %>% na.omit() %>% as.vector() %>%
      en2 <- db %>% dplyr::select("energy_level_2") %>%  unlist() %>% na.omit() %>% as.vector() %>% first() %>%
        # en2 <- db %>% dplyr::select("enlvl2") %>%  unlist() %>% na.omit() %>% as.vector() %>%
        normalize(min = energy_min, max = energy_max)
      # # energy_level2 <- db %>% magrittr::extract2("energy_level_2") %>% magrittr::use_series(value) %>%  unlist() %>% as.vector()
      # energy_obs <- 980
      # energy_obs <- db %>% dplyr::select("enstart") %>%  unlist() %>% na.omit() %>% as.vector()  %>%
      #   normalize(min = energy_min, max = energy_max) # = start-energy. does it make sense to use the start value as 'observed' (we don't have this number estimated)
      
      # 2) for fitness:
      # energy_obs <- db %>%  magrittr::extract("ens") %>% unlist() %>% as.vector() %>%
      #   normalize(min = energy_min, max = energy_max)
      # energy <- db %>%  magrittr::extract("energy") %>% unlist() %>% na.omit() %>% as.vector() %>% first() %>%
      # energy <- db %>%  dplyr::select("energy") %>% unlist() %>% na.omit() %>% as.vector() %>% first() %>%
      #   normalize(min = energy_min, max = energy_max)
      
      # KDE95 <- db %>%  magrittr::extract("KDE_95") %>% unlist() %>%  na.omit() %>% as.vector() %>% first() %>%
      KDE95 <- db %>%  dplyr::select("g_KDE_95") %>% unlist() %>%  na.omit() %>% as.vector() %>% first() %>%
        # `/` (10000) %>%  #convert to hectares
        normalize(min = KDE95_min, max = KDE95_max)
      # KDE50 <- db %>%  magrittr::extract("KDE_50") %>% unlist() %>%  na.omit() %>% as.vector() %>% first() %>% 
      KDE50 <- db %>%  dplyr::select("g_KDE_50") %>% unlist() %>%  na.omit() %>% as.vector() %>% first() %>%
        # `/` (10000) %>%  #convert to hectares
        normalize(min = KDE50_min, max = KDE50_max)
      
      # visits <- db %>% magrittr::extract("n_visited_trees") %>% unlist() %>% unique() %>% as.vector() %>% first() %>%
      visits <- db %>% dplyr::select("g_n_visited_trees") %>% unlist() %>% unique() %>% as.vector() %>% first() %>% 
        normalize(min = visits_min, max = visits_max)
      
      # p_feeding <- db %>%  magrittr::extract("p_feeding") %>% unlist()  %>% na.omit() %>% as.vector() %>% first() %>% 
      p_feeding <- db %>%  dplyr::select("g_p_feeding") %>% unlist()  %>% na.omit() %>% as.vector() %>% first() %>% 
        normalize(min = p_feeding_min, max = p_feeding_max)
      # p_foraging <- db %>%  magrittr::extract("p_foraging") %>% unlist() %>%  na.omit() %>% as.vector() %>% first() %>% 
      p_foraging <- db %>%  dplyr::select("g_p_foraging") %>% unlist() %>%  na.omit() %>% as.vector() %>% first() %>% 
        normalize(min = p_foraging_min, max = p_foraging_max)
      # p_traveling <- db %>%  magrittr::extract("p_traveling") %>% unlist() %>%  na.omit() %>% as.vector() %>% first() %>% 
      p_traveling <- db %>%  dplyr::select("g_p_traveling") %>% unlist() %>%  na.omit() %>% as.vector() %>% first() %>% 
        normalize(min = p_traveling_min, max = p_traveling_max)
      # p_resting <- db %>%  magrittr::extract("p_resting") %>% unlist() %>%  na.omit() %>% as.vector() %>% first() %>% 
      p_resting <- db %>%  dplyr::select("g_p_resting") %>% unlist() %>%  na.omit() %>% as.vector() %>% first() %>% 
        normalize(min = p_resting_min, max = p_resting_max)
      
      # sl_mean <- db %>%  dplyr::select("g_step_length_mean") %>% unlist() %>%  na.omit() %>% as.vector() %>% first() %>%
      #   normalize(min = step_length_mean_min, max = step_length_mean_max)
      # sl_sd <- db %>%  dplyr::select("g_step_length_sd") %>% unlist() %>%  na.omit() %>% as.vector() %>% first() %>%
      #   normalize(min = step_length_sd_min, max = step_length_sd_max)
      # t_angle_sd <- db %>%  dplyr::select("g_turn_ang_sd") %>% unlist() %>%  na.omit() %>% as.vector() %>% first() %>%
      #   normalize(min = ta_sd_min, max = ta_sd_max)
      
      
      # DPL_sd <- db$DPL_sd # check it
      # For runs with samples day by day (and not average DPL per run):
      # DPL_mean <- db$DPL_d
      # DPL_mean <- DPL_mean %>%
      #   str_replace_all(., c("\\[" = "", "\\]" = "")) %>%
      #   str_split(pattern = "_") %>%  #, simplify = TRUE) %>%
      #   purrr::map(as.numeric, na.rm = TRUE) %>% 
      #   # purrr::map(round, 2) %>%
      #   map_dbl(mean, na.rm=TRUE)
      # db$DPL_mean <- DPL_mean
      
      DPL_mean <- db$g_DPL %>%  na.omit() %>% as.vector() %>% first() %>% normalize(min = dpl_mean_min, max = dpl_mean_max)
      DPL_sd <- db$g_DPL_sd %>% na.omit() %>% as.vector() %>% first() %>% normalize(min = dpl_sd_min, max = dpl_sd_max)
      MR_mean <- db$g_MR %>% na.omit() %>% as.vector() %>% first() %>% normalize(min = mr_min, max = mr_max)
      MR_sd <- db$g_MR_sd  %>% na.omit() %>% as.vector() %>% first() %>% normalize(min = mr_sd_min, max = mr_sd_max)
      PT_mean <- db$g_PT  %>% na.omit() %>% as.vector() %>% first() %>% normalize(min = pt_min, max = pt_max)
      PT_sd <- db$g_PT_sd  %>% na.omit() %>% as.vector() %>% first() %>% normalize(min = pt_sd_min, max = pt_sd_max)
      
      
      ## Don't use it yet
      # energy_stored_obs <-  5000 %>% 
      #   normalize(min = energy_stored_min, max = energy_stored_max) # as we don't have observed empirical values, I'm using 5000 (kJ)
      
      
      
      # calc differences (or put all into a dataframe and use custom_fitness() function)
      # en <- abs(energy - energy_obs) -> took it out as 2023-09-03d. It does not make sense to optmize a parameter that we don't have reference for
      # ens <- abs(energy) # energy stored (don't use it yet)
      vi <- abs(visits - visits_obs)
      hr95 <- abs(KDE95 - KDE95_obs)
      hr50 <- abs(KDE50 - KDE50_obs)
      dp <- abs(DPL_mean - DPL_mean_obs)
      dpsd <- abs(DPL_sd - DPL_sd_obs)
      mr <- abs(MR_mean - MR_obs)
      mrsd <- abs(MR_sd - MR_sd_obs)
      pt <- abs(PT_mean - PT_obs)
      ptsd <- abs(PT_sd - PT_sd_obs)
      pfee <- abs(p_feeding - p_feeding_obs)
      pfor <- abs(p_foraging - p_foraging_obs)
      ptra <- abs(p_traveling - p_traveling_obs)
      pres <- abs(p_resting - p_resting_obs)
      
      # sl <- abs(sl_mean - sl_mean_obs)
      # slsd <- abs(sl_sd - sl_sd_obs)
      # tasd <- abs(t_angle_sd - t_angle_sd_obs)
      # sl_mean <- 1
      # slsd <- 1
      # tasd <- 1
      
      
      # calc the fitness function
      # 'strong patterns' sensu Chudzinska et al. Table S11:
      w <- 1 # weight 
      crit <- 
        # 1/sum(vi) * w + 
        1/sum(hr95) * w + 1/sum(hr50) * w + 
        1/sum(dp) * w + 1/sum(mr) * w + 1/sum(pt) * w
        
      w <- 0.5 # weight 
      crit <- crit +
        1/sum(pfee) * w + 1/sum(pfor) * w + 1/sum(ptra) * w + 1/sum(pres) * w #+ 
        # 1/sum(sl) * w
      
      # 'weak patterns' sensu Chudzinska et al. Table S11:
      # w <- 0.1
      # crit <- crit +
      #   # 1/sum(en) * w + # energy varies too much (min = 0, max = 3000) and we can't parameterize it (yet)
      #   1/sum(dpsd) * w + 1/sum(mrsd) * w + 1/sum(ptsd) * w +
      #   1/sum(slsd) * w + 1/sum(tasd) * w
      
      ## Don't use it yet:
      # w <- 0.1
      # crit <- crit +
      #   1/sum(ens) * w + # energy stored varied too much (min = 0, max = ? I used 10^4) and we can't parameterize it (yet)
      
      # print(en1)
      # print(en2)
      
      print(paste("Fitness value of run: ", crit))
      # print(paste("energy_lvl_1", en1))
      # print(paste("energy_lvl_2", en2))
      
      if ( en1 > en2 ) {
        crit <- 0
        # runs <- runs + 1
        print("energy_lvl_1 is bigger than energy_lvl 2, dropping simulation")
        return(crit)
      } else {
        # runs <- runs + 1
        return(crit)
      }
      
    } else {
      crit <- 0 # if monkeys died, the crit value is low and the run is dropped
      print("tamarins died, dropping simulation")
      # deaths <- deaths + 1
      return(crit)
    }
    
    # runs <- runs + 1
    gc()
  }
  
  # Test the eval function:
  # # extract values from run from example objects for testing:
  # nl.x<- readRDS(here("Model_analysis", "Sensitivity-analysis", "v1.2_2023MJuly", "Param_bestguess", "Simple", "temp",
  #                      "v1.2_Taquara_Jan_simple674276753_tempRDS.Rdata"))
  # # # # params_run <- nl.x %>% nlrx::report_model_parameters()
  # critfun(nl.x)
  # db <- nlrx::getexp(nl.x, "simoutput")
  # db1 <- nlrx::getexp(nl.x, "metrics.turtles")
  # db2 <- nlrx::getexp(nl.x, "variables")
  # db3 <- report_model_parameters(nl.x)
  # db4 <- nl.x@experiment@variables
  # db5 <- nl.x@experiment@metrics
  
  # Monitor
  # monitor <- function(obj) {
  #   # plot the population
  #   xlim = c(obj$stringMin[1], obj$stringMax[1]);
  #   ylim = c(obj$stringMin[2], obj$stringMax[2]);
  #   plot(obj$population, xlim=xlim, ylim=ylim, 
  #        xlab="pi", ylab="sqrt(50)");
  # }
  
  # For differences between genetic algorithm (simdesign_GenAlg) and genetic anealing (simdesign_GenSA):
  # https://stackoverflow.com/questions/4092774/what-are-the-differences-between-simulated-annealing-and-genetic-algorithms
  nl@simdesign <- simdesign_GenAlg(nl, 
                                   evalcrit = critfun, # 1, # "e.g. 1 would use the first defined metric of the experiment to evaluate each iteration)"
                                   
                                   popSize = 121, # or chromosomes. suggestion: population_size > num_genes^2 (https://stackoverflow.com/questions/7559274/prevent-inbreeding-and-monoculture-in-genetic-algorithm-newbie-question/7609715#7609715)
                                   iters = 100,
                                   elitism = NA, # default= 20%; from stackoverflow link above: "New members of the population are created in essentially one of three ways. The first is usually referred to as 'elitism' and in practice usually refers to just taking the highest ranked candidate solutions and passing them straight through--unmodified--to the next generation. The other two ways that new members of the population are usually referred to as 'mutation' and 'crossover'."
                                   mutationChance = 0.1,
                                   nseeds = 1
  )
  
  ## Step 4: run -------------------------
  # set.seed(1234)
  
  tictoc::tic()
  progressr::handlers("progress")
  
  results <- with_progress(
    run_nl_dyn(
      nl, 
      seed = nl@simdesign@simseeds
    )
  )
  tictoc::toc()
  
  
  ## Step 5: Investiga output
  # a <- noquote(area_run) # or gsub(area_run_scp, pattern = ('\"'), replacement = '', fixed = T)
  # m <- noquote(month_run) # or gsub(month_run_scp, pattern = ('\"'), replacement = '', fixed = T)
  
  # 00_rep-ga_resuls results
  cat(summary(results))
  results
  results$expname <- expname
  
  setsim(nl, "simoutput") <- results
  
  saveRDS(nl, file.path(outpath, paste0(expname, "_nl_feedingbouton_", Sys.Date(), ".rds")))
  saveRDS(results, file.path(outpath, paste0(expname, "_results_feedingbouton_", Sys.Date(), ".rds")))
  # save.image(file=paste0(outpath, '/GA_Taquara_Jan_Environment.RData'))
  
  # setsim(nl, "simoutput") <- tibble::enframe(results)
  # saveRDS(nl, file.path(nl@experiment@outpath, paste0(expname, ".rds")))
  
  resultsrbga <- results
  
  
  
  
  
  
  ## ------------------------------------------------------------------------------------ ##
  # # Load specific runs:
  # 
  # # ?genalg::rbga
  # 
  # # nl <- readRDS(paste0(outpath, "/GA_Taquara_Jan_nl_feedingbouton_2023-10-20.rds"))
  # # resultsrbga <- readRDS(paste0(outpath, "/GA_Taquara_Jan_results_feedingbouton_2023-10-20.rds"))
  # 
  # filesga <- list.files(path, pattern = "results_")  # use the rgba file
  # filesga <- paste0(path, "/", filesga)
  # filesganl <- list.files(path, pattern = "nl_feedingbouton")  # use the rgba file
  # filesganl <- paste0(path, "/", filesganl)
  # 
  # for (gaindex in 1:length(filesga)) { 
  #   
  #   # print(gaindex)
  #   # gaindex <- 9
  #   
  #   nl <- readRDS(filesganl[gaindex])
  #   resultsrbga <- readRDS(filesga[gaindex])
  # 
  #   expname <- nl@experiment@expname
  #   area_run <- str_split_1(expname, pattern = "_")[2]
  #   month_run <- str_split_1(expname, pattern = "_")[3]
  # 
  # 
  # 
  ## ------------------------------------------------------------------------------------ ##



  # resultsrbga <- resultsrbga@simdesign@simoutput
  cat(summary(resultsrbga))
  # resultsrbga %>% class()
  # resultsrbga %>% str()
  # resultsrbga@experiment

  nl@experiment@constants$study_area
  nl@experiment@constants$`feeding-trees-scenario`
  min_param <- resultsrbga[2] # parameters min
  max_param <- resultsrbga[3] # parameters max
  resultsrbga[4] # popSize
  resultsrbga[5] # iter
  # population <- resultsrbga[7] %>% unlist(recursive = FALSE) # population







  # As in 00_prep-ga.R (original)
  population <- resultsrbga[7] %>% purrr::map_df(., ~as.data.frame(.))
  colnames(population) <- names(min_param)
  fitness <- resultsrbga[10] %>% unlist() # evaluations
  # best_results <- resultsrbga[11] %>% unlist() # best
  best_result <- fitness %>% max() # best
  # idx <- resultsrbga$population[resultsrbga$evaluations == max(resultsrbga$best),][1,] #https://wittline.github.io/Data-Analytics-with-R/Genetic%20algorithms/Genetic_algorithms_with_R.html
  idx <- fitness == best_result

  idx <- match(best_result, fitness)

  min_param <- min_param %>% unlist()
  # min_param %>% class()
  max_param <- max_param %>% unlist()

  ga_input <- data.frame(
    min = min_param,
    max = max_param
  )

  # best <- best_results %>% unlist() %>% max()
  # best %>% class()
  # best_results %>% class()



  # best <- resultsrbga$population[resultsrbga$evaluations == max(resultsrbga$best),][1,] #https://wittline.github.io/Data-Analytics-with-R/Genetic%20algorithms/Genetic_algorithms_with_R.html


  # idx <- match(best, best_results)
  # best %in% best_results
  # best_results %in% best

  optimized_param <- population[idx, ]











  # ## As in 02_ga_plots_5best.R
  # population <- resultsrbga[7] %>% purrr::map_df(., ~as.data.frame(.))
  # best_results <- resultsrbga[11] %>% unlist() #%>% as_tibble()
  #
  # # best5 <- tail(resultsrbga$best, 5)
  #
  # # cat(summary(resultsrbga))
  #
  # bestSolution<-resultsrbga$population[which.max(resultsrbga$evaluations), ]
  # # values[which(GA@solution[1,] == 1)]
  #
  # # best5_idx <- pmatch(best5, best_results)
  # # bestSolutions<-resultsrbga$population[best5_idx, ]
  #
  #
  # resultsrbga$evaluations
  # # resultsrbga[10]
  # resultsrbga$best
  # # resultsrbga[11]
  #
  # # best5 <- resultsrbga$best %>% unlist() %>% as.vector() #sort(., decreasing = TRUE)
  # # fitness <- resultsrbga$best %>% unlist() %>% as.vector() #sort(., decreasing = TRUE)
  # fitness <- resultsrbga$evaluations %>% unlist() %>% as.vector() #sort(., decreasing = TRUE)
  # idx <- 1:length(fitness)
  # fitness <- cbind(fitness, idx)
  # best5 <- fitness %>%
  #   as_tibble() %>%
  #   arrange(desc(fitness)) %>%
  #   slice_head(n=5)
  # # slice_max(fitness, n = 5, with_ties = FALSE)
  #
  # best5
  #
  # # ### Option 1: filtering despite the number of chromosomes: ###
  # # best5_idx <- resultsrbga$best[resultsrbga$best %in% best5] ; length(best5_idx)
  # # # idx <- match(best5, resultsrbga$best)
  # # optimized_param <- population[1:length(best5_idx), ]
  #
  #
  # ### Option 2:  filtering only one chromossome per fitness value: ###
  # # best5_idx <- pmatch(best5, best_results)
  # # best5_idx <- pmatch(best5, resultsrbga$population)
  #
  # # best5_idx <- pmatch(best5$idx, row_number(population))
  # best5_idx <- best5$idx
  #
  # optimized_param <- population[best5_idx, ]
  #
  # # get parameter names:
  # params <- resultsrbga[2] %>% unlist()
  # # names(params)
  #
  # colnames(optimized_param) <- names(params) %>% stringr::str_sub(., end=-5)
  # colnames(optimized_param) <- colnames(optimized_param) %>% stringr::str_sub(., start=11)
  # colnames(optimized_param)
  # # a <- colnames(optimized_param)
  #
  # optimized_param <- cbind(optimized_param, best5)
  # len <- colnames(optimized_param) %>% length()
  # # colnames(optimized_param)[len] <- "fitness"
  #
  # # optimized_param$expname <- basename(i) %>% str_match(., '(?:_[^_]+){3}') %>% as.character() %>%
  #   # str_remove(., '^_{1}') %>% str_remove(., "_results")
  # optimized_param$expname <- resultsrbga$expname %>% str_remove(., "GA_")
  # # optimized_param <- optimized_param %>% t() %>% as.data.frame(row.names = TRUE)
  # # optimized_param <- cbind(optimized_param, a)
  # # optimized_param$simulation_scenario <- paste0(area_run, "_", month_run)
  # # optimized_param <- optimized_param[, c(3, 2, 1)]
  #




  # Save optimized
  library("stringr")
  # colnames(optimized_param) <- colnames(optimized_param) #%>% stringr::str_replace_all(., c("stringMin.", ".min"), "" )
  colnames(optimized_param) <- rownames(ga_input) %>% stringr::str_sub(., end=-5)
  colnames(optimized_param) <- colnames(optimized_param) %>% stringr::str_sub(., start=11)
  colnames(optimized_param)
  a <- colnames(optimized_param)

  optimized_param <- optimized_param %>% t() %>% as.data.frame(row.names = TRUE)
  optimized_param <- cbind(optimized_param, a)
  optimized_param$simulation_scenario <- paste0(area_run, "_", month_run)
  optimized_param <- optimized_param[, c(3, 2, 1)]

  optimized_param <- cbind(optimized_param, ga_input)
  row.names(optimized_param) <- NULL

  colnames(optimized_param) <- c("expname", "parameter", "value", "min", "max")

  optimized_param %>% write.csv(paste0(outpath, "/", expname, "_optimized_feedingbouton"
                                       , Sys.Date(), ".csv"), row.names = FALSE)


  par(mar = c(2, 2, 2, 2))
  # par(oma=c(5,7,1,1))

  # png(file=paste0(outpath, "/", expname, "_optimized_feedingbouton_points"
  #                 , Sys.Date(), ".png")
  #     , width = 480, height = 900
  #     )
  # plot(resultsrbga, type = "vars")
  # dev.off()
  #
  # png(file=paste0(outpath, "/", expname, "_optimized_feedingbouton_hist", Sys.Date(), ".png")
  #     , width = 480, height = 900
  #     )
  # plot(resultsrbga, type = "hist")
  # dev.off()


  eval <- resultsrbga$evaluations
  popSize <- 1:resultsrbga$popSize
  best <- resultsrbga$best %>% unique()
  bestn <- length(best)

  dfga <- data.frame(evaluations = eval,
                     popSize = popSize
                     # ,best = best
  )

  dfga %>% str()

  # dfga <- dfga %>%
  #   dplyr::filter(evaluations < 9000)

  library(ggplot2)
  dfga %>%
    ggplot() +
    geom_density(
      # aes(x = evaluations)
      aes(x = evaluations)
    ) +
    geom_vline(xintercept = mean(dfga$evaluations, na.rm=T), col = "blue") +
    geom_vline(xintercept = max(dfga$evaluations, na.rm=T), col = "red") +
    annotate("text",
             # Inf, Inf, col = "red" #, size = 10
             x = Inf, y = 1.0 * max(density(dfga$evaluations)$y, na.rm = TRUE), col = "red" #, size = 10
             # max(dfga$evaluations, na.rm=T), max(dfga$evaluations, na.rm=T), col = "red" #, size = 10
             , label = paste0("best = ", max(dfga$evaluations, na.rm=T)
                              , hjust = 0, vjust = 1
                              )
             ) +
    annotate("text",
             x = Inf,  #1.25 * mean(dfga$evaluations, na.rm=T), 
             y = 0.9 * max(density(dfga$evaluations)$y, na.rm = TRUE), col = "blue" #, size = 10
             # max(dfga$evaluations, na.rm=T), max(dfga$evaluations, na.rm=T), col = "red" #, size = 10
             , label = paste0("mean = ", mean(dfga$evaluations, na.rm=T)
                              , hjust = 0, vjust = 0
             )
    ) +
    # xlim(0, 500) +
    xlab("criteval value") +
    ggtitle(paste("Fitness values", "of", area_run, " - ", month_run))

  ggsave(filename = paste0(outpath, "/", expname, "_criteval_values", Sys.Date(), ".png"),
         dpi = 300, width = 7, height = 7)



  # } # finish loop for reploting fitness values






  # # Animation plot (didn't work). Based on https://www.r-bloggers.com/2012/08/genetic-algorithms-a-simple-r-example/
  # animate_plot <- function(x) {
  #   for (i in seq(1, bestn)) {
  #     temp <- data.frame(Generation = c(seq(1, i), seq(1, i)),
  #                        Variable = c(rep("mean", i), rep("best", i)),
  #                        Survivalpoints = c(-resultsrbga$mean[1:i], -resultsrbga$best[1:i])
  #     )
  #   }
  #
  #   pl <- ggplot(dat= temp,
  #                aes(x = Generation, y = Survivalpoints, group = Variable,
  #                    colour = Variable)
  #   ) +
  #     geom_line() +
  #     scale_x_continuous(limits = c(0, bestn)) +
  #     scale_y_continuous(limits = c(0, 110)) #+
  #     # geom_hline(y = max(temp$Survivalpoints),
  #     #            opts(title = "Evolution Knapsack optimization model")
  #     #            print(pl)
  #
  # }
  # in order to save the animation
  # library(animation)
  # saveGIF(animate_plot(), interval = 0.1, outdir = outpath)
}
