## Header --------------------------
# Script name: 01_nlrx
# Script purpose: Do simple runs for every siminputrow using BEST GUESS parameters
# Date created: 2023-07-03d
# Author: Eduardo Zanette

## Notes --------------------------- 
# Last additions: 
# ********* p-timesteps-to-rest -> new midday values parameterized
# ********* ld_target_random + proportion of denfeded trees 



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
# Options (plotting, memory limit, decimal digits)
# ggplot theme
theme_set(theme_bw(base_size = 15))
theme_update(
  axis.text.x = element_text(size = 11)
)

## ---------------------------


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


## Step 1: Create nl object ------------------------ 

if(Sys.info()[["nodename"]] == "DESKTOP-R12V3D6") {
  netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
  # modelpath <- here("Model_development", "BLT_model_v1.2.nlogo")
  modelpath <- here("Model_simulations", "BLT_model_v1.2.nlogo") # Last version with stored-energy
  outpath <- here("Model_analysis", "Sensitivity-analysis", "v1.2_2023MJuly", 
                  "Param_bestguess", "Simple", "temp")
  user_scp = "\"Eduardo\""
}
if(Sys.info()[["nodename"]] == "DESKTOP-71SL58S") { # PC 02 LEEC
  netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
  modelpath <- here("Model_simulations", "BLT_model_v1.2.nlogo") # Last version with stored-energy
  outpath <- here("Model_analysis", "Sensitivity-analysis", "v1.2_2023MJuly",
                  "Param_bestguess", "Simple", "temp")
  # Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8/bin")
  Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk1.8.0_341")
  user_scp = "\"PC02\""
}

nl <- nl(nlversion = "6.2.2",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

nlogo_model_param <- report_model_parameters(nl)
nlogo_model_param



## Parameterizations and calibrations -----

### Empirical data for parameterisation ----
param.table <- read.csv(here("Data", "Movement", "Curated", "BLT_groups_data_summary_siminputrow.csv"),
                        sep = ",", dec = ".", stringsAsFactors = TRUE,
                        encoding = "UTF-8") %>% 
  dplyr::mutate(group = recode(group, "Guarei" = "Guareí",
                                      "Santa Maria" = "SantaMaria")) # only to match those of the NetLogo model


## Simple design experiment ( = one go button, no varibles) ####

## Step 2: Attach an experiment and run ------------------------
exptype <- "simple"

### Define parameterisation -----
step_model_param <- "true" # velocity parameters are setted inside the model. Change this when velocity is summarized and inclued in the parameter table
gtt_param <- "true" # gtt parameters are setted inside the model. Change this when velocity is summarized and inclued in the parameter table
p_forage_param <- "true" # p_foraging parameter is setted inside the model. Change this when velocity is summarized and inclued in the parameter table 

feedingbout <- "true"

if (feedingbout == "false") { 
  species_time <- 2 # number of timesteps tamarins feed on each tree (add to experiment)
  print("RUNNING WITH FEEDING BOUT PARAMETERIZATION OFF, COMMENT IN 'species_time' FROM CONSTANTS")
} else {
  print("RUNNING WITH FEEDING BOUT PARAMETERIZATION ON, COMMENT OUT 'species_time' FROM CONSTANTS")
}

ld_target_random <- "false"   # no long distance random target. Instead, uses the ones further from the home range core
p_disputed_trees <- 0.25    # percentage of trees on the simulation that are defended at territory borders

if (ld_target_random == "false") { 
  print(paste0("RUNS WITH RESOURCE DEFENSE and "
               , p_disputed_trees, "% defended trees!"))
               }
p_timesteps_to_rest <- 0.15 # resting not allowed on the 15% of the start and end of simulation-time 




i <- 1
# for (i in i:nrow(param.table)) {
for (i in 1:nrow(param.table)) {
  
  set.seed(42) # generate always the same set of seeds
  
  ### Define area and month  -----
  # (all strings must use escaped quotes) 
  # choose which parameterizations should be on or off
  ### Define run and parameterisation: (all strings must use escaped quotes)
  area_run <- paste0('\"', param.table$group[i], '\"')
  month_run <- paste0('\"', param.table$id_month[i], '\"')
  
  # area_run <- '"Taquara"'
  # month_run <- '"Jan"'
  expname <-  paste0("v1.2_",
                     gsub(area_run, pattern = ('\"'), replacement = '', fixed = T), 
                     "_", 
                     gsub(month_run, pattern = ('\"'), replacement = '', fixed = T), 
                     "_", exptype)
  expname <- expname %>% 
    str_replace_all(., c("-" = "_", "\\s+" = "_")) # Remove - and space characters.

  #   # test loop:
  #   print(expname)
  # 
  #   i <- i+1
  # }
  
  no_days_run <- param.table %>% 
    dplyr::filter(group == gsub(area_run, pattern = ('\"'), replacement = '', fixed = T),
                  id_month == gsub(month_run, pattern = ('\"'), replacement = '', fixed = T)) %>% 
    dplyr::select(ndays) %>% 
    pull() #+ 1 # one day more for "initializing" the model (take this first day out when analyzing data?)
  
  simultime_run <- param.table %>% 
    dplyr::filter(group == gsub(area_run, pattern = ('\"'), replacement = '', fixed = T),
                  id_month == gsub(month_run, pattern = ('\"'), replacement = '', fixed = T)) %>% 
    dplyr::select(mean_timesteps) %>% 
    pull()
  # simultime_run <- round(simultime_run * 0.9) # that's the timestep when tamarins should start looking for the sleeping site
  
  duration <- param.table %>% 
    dplyr::filter(group == gsub(area_run, pattern = ('\"'), replacement = '', fixed = T),
                  id_month == gsub(month_run, pattern = ('\"'), replacement = '', fixed = T)) %>% 
    dplyr::select(duration) %>% 
    pull()
  
  
  ### Define parameters:
  # Here we are using the same values used for running the first version of the manuscript (same version from Master thesis)
  # available in "Table2_Parameter_table.xlsx". Files available in: Sensitivity-analysis/v1.1_November2022/Simple/
  
  energy_from_fruits <- 73
  energy_from_prey <- 30 #60								
  energy_loss_traveling <- -15								
  energy_loss_foraging <- -10
  energy_loss_resting <- -5
  # start_energy <- 900
  # energy_level_1 <- 999
  energy_level_1 <- 900
  energy_level_2 <- 1150
  energy_stored_val <- 1000
  step_forget <- 400
  p_memory <- 3
  # visual <- 0
  
  # alternatively, one could use the calibrated parameters. For this, go to:
  # v1.2_2023May/Param_calibrated/


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
                              idfinal = "r:stop", # for making r NetLogo extension to work: https://cran.r-project.org/web/packages/nlrx/vignettes/furthernotes.html
                              
                              # reporters:
                              metrics = c( # e.g. "count sheep" or "count patches with [pcolor = green]"
                                # "timestep",
                                "day",
                                "timestep",
                                "survived?",
                                
                                "g_SDD",
                                "g_SDD_sameday",
                                "g_SDD_nextday",
                                "g_SDD_sd_sameday",
                                "g_SDD_sd_nextday",
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
                                "g_straightness",   # straightness and sinuosity are slightlty different in terms of properties (https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en) and they were not tested as predictors of SDD, so i'm not using them
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
                                # 'species_time' = species_time,
                                
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
                                
                                'ld-target-random?' = ld_target_random,
                                'p_disputed_trees'= p_disputed_trees,
                                'p-timesteps-to-rest' = p_timesteps_to_rest,
                                
                                ### memory
                                'duration' = duration,
                                # 'visual' = visual,                # does not matter
                                "step_forget" = step_forget,
                                
                                ### energy
                                # 'start-energy' = start_energy,
                                "energy_level_1" = energy_level_1,
                                "energy_level_2" = energy_level_2,
                                "energy-from-fruits" = energy_from_fruits,
                                "energy-from-prey" = energy_from_prey,
                                "energy-loss-traveling" = energy_loss_traveling,
                                "energy-loss-foraging" = energy_loss_foraging,
                                "energy-loss-resting" = energy_loss_resting,
                                "energy_stored_val" = energy_stored_val
                                
                                # Seed dispersal
                                # "gut_transit_time_val" = 15   # not needed when gtt-param = true
                                # "n_seeds_hatched" = 1,
                                
                                ### movement
                                # travel_speed = 1
                                
                              )
  )
  

  report_model_parameters(nl)


# Step 3: Attach a simulation design.
  nseeds <- 20 # repetitions (ideally n = 30)
  
  # Step 3: Attach a simulation design.
  # nl@simdesign <- simdesign_distinct(nl, nseeds = 17)
  nl@simdesign <- simdesign_simple(nl, nseeds = nseeds)
  
  nl@simdesign@simseeds
  
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
  # saveRDS(nl, file.path(paste0(nl@experiment@outpath
  #                              , "/"
  #                              # , "morris_2022-12-23d_feedingbout-"
  #                              , expname 
  #                              , seed
  #                              , "_Feedingbout_"
  #                              , if(feedingbout) {"on"} else {"off"}
  #                              , "_"
  #                              , Sys.Date()
  #                              , ".rds")))
  filename <-
    paste0(outpath, "/", expname, seed, "_tempRDS.Rdata")
  saveRDS(nl, file = filename)
  
  
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
