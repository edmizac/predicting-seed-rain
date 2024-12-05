## Header --------------------------
# Script name: 03_spatial-plots
# Script purpose: Do simple runs for plotting spatial plots using BEST GUESS parameters
# Date created: 2023-10-29d
# Author: Eduardo Zanette

## Notes --------------------------- 
# Done for submitting Chapter 2


## Packages -------------------------
library("nlrx")
library("here")
library("tidyverse")
library("progressr")
library("future")
library("tictoc")
library("stringr")
library("cowplot")

## ---------------------------
# Options (plotting, memory limit, decimal digits)
# ggplot theme
# theme_set(theme_bw(base_size = 24))

custom_theme <- list(
  
  theme_bw(base_size = 14) +
    
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  ),
  guides(
    col = guide_legend(ncol = 1),
    shape = guide_legend(ncol = 1),
    size = guide_legend(ncol = 1)
  )
)

## ---------------------------


# Load spatial objects -----

# sf objects
load(here("Data", "06_sf-plots.RData"))


# set new lims to sf objects
our_crs <- "+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# gua_xlim_all = c(780000, 782350)
# gua_ylim_all = c(7407080, 7408250)
gua_xlim_all = c(781600, 782350)
gua_ylim_all = c(7407200, 7408250)
gua.x.min = 781587
gua.x.max = 782361.5
gua_xlim_set = c(781050, 782350)   # c(781200, 782350) # closer
gua_ylim_set = c(7407050, 7408250) # c(7407250, 7408250)  # closer
sma_xlim_set = c(364400, 366000)
sma_ylim_set = c(7540500, 7541700)
taq_xlim_set = c(370500, 373200)
taq_ylim_set = c(7498750, 7500550)
suz_xlim_set = c(705300, 706200)
suz_ylim_set = c(7480800, 7481600) # 7480990,


# gua.sf <- gua.sf +
#   coord_sf(
#     xlim = gua_xlim_all,
#     ylim = gua_ylim_all
#   )
  

# Plot aesthetics
behav_simulated_shapes <- c("foraging" = 1,
                            "frugivory" = 19,
                            "resting" = 1,
                            "sleeping" = 17,
                            "travel" = 1,
                            "others" = 4)

behav_simulated_colors <- c("foraging" = "grey",
                            "frugivory" = "#1E8449",
                            "resting" = "grey",
                            "sleeping" = "#E74C3C",
                            "travel" = "grey",
                            "others" = "grey25")



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
  netlogopath <- file.path("C:/Program Files/NetLogo 6.3.0")
  # modelpath <- here("Model_development", "BLT_model_v1.2.nlogo")
  modelpath <- here("Model_simulations", "BLT_model_v1.2.nlogo")
  outpath <- here("Model_analysis", "Sensitivity-analysis", "v1.2_2023MJuly", 
                  "Param_bestguess", "Simple", "temp", "spatial")
  user_scp = "\"Eduardo\""
}

nl <- nl(nlversion = "6.3.0",
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
exptype <- "spatial"


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
for (i in i:nrow(param.table)) {
# for (i in 1:nrow(param.table[2:9, ])) {
  
  set.seed(41) # generate always the same set of seeds
  
  ### Define area and month  -----
  # (all strings must use escaped quotes) 
  # choose which parameterizations should be on or off
  ### Define run and parameterisation: (all strings must use escaped quotes)
  area_run <- param.table$group[i]
  month_run <- param.table$id_month[i]
  
  area_run_scp <- paste0('\"', param.table$group[i], '\"')
  month_run_scp <- paste0('\"', param.table$id_month[i], '\"')
  
  
  # area_run <- '"Taquara"'
  # month_run <- '"Jan"'
  expname <-  paste0("v1.2_",
                    area_run,
                     "_", 
                     month_run,
                     "_", exptype)
  expname <- expname %>% 
    str_replace_all(., c("-" = "_", "\\s+" = "_")) # Remove - and space characters.
  
  #   # test loop:
  #   print(expname)
  # 
  #   i <- i+1
  # }
  
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
  
  duration <- param.table %>% 
    dplyr::filter(group == area_run,
                  id_month == month_run) %>% 
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
                              tickmetrics = "true", # "true" for every tick, "false" for metrics only in the end of the simulation
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
                                "survived?"
                                
                                # "g_SDD",
                                # "g_SDD_sameday",
                                # "g_SDD_nextday",
                                # "g_SDD_sd_sameday",
                                # "g_SDD_sd_nextday",
                                # "n",
                                # "p-visited-trees",
                                # "R_seeds",        
                                # "R_seeds_p",      
                                # "NN_seeds",       
                                # "NN_feeding_trees", # this is calculated again by the end of the run although it was calculated priorly within the build_forest process
                                # "NN_sleeping_trees", # might be a more important factor affecting SDD than the NN of feeding trees 
                                
                                # turtle variables as globals:
                                # "g_energy_stored", # energy is reset to energy-start everyday, thus we take
                                # "g_energy",      # final energy                # the best set of parameters should make tamarins viable in energetic terms
                                # "g_enlvl1",         # value of energy_level_1 used at the start of the simulation
                                # "g_enlvl2",         # value of energy_level_2 used at the start of the simulation 
                                # "g_enstart",          # start-energy value at the start of the similuation
                                # "g_DPL",         # mean DPL (in case we don't want all the values -> good for bar and pointrange plots)
                                ## "g_DPL_d",       # DPL is set to 0 everyday    # the best set of parameters should reproduce the observed DPL
                                # "g_DPL_sd",      # sd DPL  (in case we don't want all the values -> good for bar and pointrange plots)
                                # "g_KDE_95",      # final value                 # the best set of parameters should reproduce the observed home range
                                # "g_KDE_50",      # final value                 # the best set of parameters should reproduce the observed core area
                                # "g_p_feeding",   # final value                 # the best set of parameters should optimize the activity budget
                                # "g_p_foraging",  # final value                 # the best set of parameters should optimize the activity budget
                                # "g_p_traveling", # final value                 # the best set of parameters should optimize the activity budget
                                # "g_p_resting",    # final value                 # the best set of parameters should optimize the activity budget
                                # "g_step_length_mean",    # besides the parameterization, agents interactions make the observed step length and turning angles change
                                # "g_step_length_sd",      # besides the parameterization, agents interactions make the observed step length and turning angles change
                                # "g_turn_ang_mean",     # this one is quite consistent, so I don't think this one is necessary
                                # "g_turn_ang_sd",          # this one might be interesting though (but I haven't estimated the empirical ones yet)
                                
                                # additional movement variables
                                # "g_MR",               # movement rate (MR) is used to predict SDD by primates: http://doi.wiley.com/10.1002/ajp.22659
                                # "g_MR_sd",
                                # "g_MSD",              # other modelling studies have used this one (https://doi.org/10.3390/ani12182412.), but I believe it is very similar to MR
                                # "g_intensity_use",    # bether than MSD in my oppinion: read about it in: https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en
                                # "g_PT",               # Highly recommended as it is the only unbiased estimator of tortuosity. Almost the same thing as intensity of use (IU). path twisting is used by Fuzessy et al 2017 to predict SDD among primates: http://doi.wiley.com/10.1002/ajp.22659
                                # "g_PT_sd",
                                # "g_straightness",   # straightness and sinuosity are slightlty different in terms of properties (https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en) and they were not tested as predictors of SDD, so i'm not using them
                                # "g_sinuosity"       # sinuosity can't be compared across scales. DON'T USE IT straightness and sinuosity are slightlty different in terms of properties (https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en) and they were not tested as predictors of SDD, so i'm not using them
                                
                                # "g_n_visited_trees",
                                # "g_n_unvisited_trees",
                                
                                # "g_DI_index",      # Defendability index (Mitani & Rodman 1979, Lowen & Dunbar 1994)
                                # "g_M_index"        # Extended Defendability index (Lowen & Dunbar 1994)
                                
                              ),
                              metrics.turtles = list(
                                "monkeys" = c(
                                  # "enstart",
                                  # "enlvl1",
                                  # "enlvl2",
                                  
                                  "x_UTM", "y_UTM",
                                  # "X_coords", "Y_coords", # to plot the routes without running another nlrx experiment
                                  "behavior"
                                  
                                  # "DPL_d",       # DPL is set to 0 everyday    # the best set of parameters should reproduce the observed DPL
                                  # "step_length_mean",    # besides the parameterization, agents interactions make the observed step length and turning angles change
                                  # "step_length_sd",      # besides the parameterization, agents interactions make the observed step length and turning angles change
                                  # "turn_ang_mean",     # this one is quite consistent (~0), so I don't think this one is necessary
                                  # "turn_ang_sd"          # this one might be interesting though
                                  
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
  nseeds <- 1 # repetitions (ideally n = 30)
  
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
    
    
    ## Correct 'day' agent (wtf?)
    ## Separate between monkeys and seeds (seed output is weird)
    
    results_seeds <- results %>% 
      tail(n = 1) %>% 
      dplyr::select(-metrics.monkeys) %>%
      mutate(
        metrics.seeds = purrr::map(metrics.seeds, ~ if ("breed" %in% names(.x)) dplyr::filter(., breed == "seeds")),
        metrics.seeds = purrr::map(metrics.seeds, ~ mutate_if(., is.numeric, as.character))
      ) %>% 
      unnest_legacy() %>% 
      mutate_at(c("x_UTM", "y_UTM", "SDD"), as.numeric) %>% 
      # filter out species that are not dispersed:
      dplyr::filter(species != "[Syagrus romanzoffiana]") %>% 
      mutate(
        species = stringr::str_replace_all(species, c("\\[" = "", "\\]" = ""))
        )
      
    results_seeds %>% str()
      
    results_monkeys <- results %>% 
      dplyr::select(-metrics.seeds) %>% 
      unnest_legacy() %>% 
      mutate_at(c("x_UTM", "y_UTM"), as.numeric)
    
    
    results_monkeys <- results_monkeys %>% 
      mutate(
        behavior = case_when(
          behavior == "" ~ "sleeping",
          TRUE ~ behavior
        )
      )
    
    results_monkeys %>% str()
    results_monkeys$behavior %>% unique()
    
    results <- full_join(results_monkeys, results_seeds)
    # rm(results_monkeys) ; rm(results_seeds)
    
    # clean other stuff
    results <- results %>% 
      # mutate_all(~stringr::str_replace_all(., c("\\[" = "", "\\]" = ""))) %>% 
      rename_with(~stringr::str_replace_all(., c("-" = "_", "\\[" = "", "\\]" = "")))
    
    # change behavior values
    results <- results %>% 
      mutate(
        behavior = case_when(
          behavior == "forage" ~ "foraging",
          TRUE ~ behavior
        )
      )
    
    results$behavior %>% unique()
    
    # View(results[[31]][[20]])
    # str(results[[31]][[20]])
    # str(results[[31]][[200]])
    # # str(results)
    # results$metrics.seeds %>% dplyr::filter(breed = "seeds")
    # res <- results %>%
    #   mutate(
    #     metrics.seeds = purrr::map(metrics.seeds, ~ if ("breed" %in% names(.x)) dplyr::filter(., breed == "seeds")),
    #     metrics.seeds = purrr::map(metrics.seeds, ~ mutate_if(., is.numeric, as.character))
    #     # metrics.seeds = map(metrics.seeds, mutate_at(c("SDD", "x_UTM", "y_UTM"), ~ as.numeric))
    #     # a = map(metrics.seeds, names)
    #   )
    # View(res$metrics.seeds[[200]])
    # str(res$metrics.seeds[[200]])
    # a <- results$metrics.seeds %>%
    #   purrr::map(~ dplyr::filter(., breed != "day"))
    
    
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
    # setsim(nl, "simoutput") <- res
    
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
      paste0(outpath, "/", expname, seed, ".Rdata")
    saveRDS(nl, file = filename)
    
    
    nl <- readRDS(filename)
    
    gc()
    
    print(paste0("seed finished: ", seed))
    k <- k + 1
    
  }
  
  print(paste0("finishing ", expname))
  i <- i + 1
  
# } # strop 1



##### Screening data #####
  
  
# # Load test run:
# nl <- readRDS(here("Model_analysis", "Sensitivity-analysis",
#                           "v1.2_2023MJuly", "Param_bestguess", "Simple", "temp",
#                           "spatial","v1.2_Guareí_Jul_spatial1781578391.Rdata"))
  
  
# results_unnest <- unnest_simoutput(nl)
results_unnest <- nl@simdesign@simoutput
# results_unnest <- results_unnest %>% 
#   rename(x = x_UTM, y = y_UTM)


# Select random run by selecting random seed (run of x days = seed)
aux_run <- results_unnest$random_seed %>%
  na.exclude() %>%
  sample(size = 1)

# # Separate row with all monkey coordinates (nlrx not outputting it correctly)
# results_unnest <- results_unnest %>%
#   dplyr::filter(`random-seed` == aux_run) %>%        # by random run (~= number of the agent = who)
#   # dplyr::filter(agent=="turtles") %>%             # by agent
#   dplyr::filter(breed=="monkeys") %>%              # by breed
#   mutate(X_coords = str_replace_all(X_coords, c("\\[" = "", "\\]" = ""))) %>% # Remove - and space characters.
#   mutate(Y_coords = str_replace_all(Y_coords, c("\\[" = "", "\\]" = ""))) %>% 
#   separate_rows(X_coords, sep = " ") %>% 
#   separate_rows(Y_coords, sep = " ") %>% 
#   mutate_at(c("X_coords", "Y_coords"), as.numeric) %>% 
#   dplyr::select(-c(SDD, x_UTM, y_UTM, `id-seed`, species, `mother-tree`)) %>% 
#   rename("x_UTM" = "X_coords",
#          "y_UTM" = "Y_coords")

# results_unnest %>% str()  
# 
# # Check if is only one run (seed):
# results_unnest$random_seed %>% unique()

# # Plot routes
# ggplot(data = results_unnest) +
#   geom_path(aes(x = x_UTM, y = y_UTM),
#             lwd = 0.15) +
#   geom_point(aes(x = x_UTM, y = y_UTM
#                  , group = behavior,
#                  color = behavior,
#                  shape = behavior
#              ),
#              size = 1.4) +
#   scale_color_manual(values = behav_simulated_colors) +
#   scale_shape_manual(values = behav_simulated_shapes) +
#   ggtitle(paste0("Simulated data"), expname)




db1 <- results_unnest

# db1$month %>% unique()
# db1$group %>% unique()
# db_monkeys$month %>% unique()
# db_monkeys$group %>% unique()

# db1 <- db1 %>%
#   mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "Santa Maria", "Taquara")) %>%
#   mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May",
#                                          "Jun", "Jul", "Aug", "Sep", "Dec"))
# #
# db_monkeys <- db_monkeys %>%
#   mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "Santa Maria", "Taquara")) %>%
#   mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May",
#                                       "Jun", "Jul", "Aug", "Sep", "Dec"))


# db1 <- db1 %>%
#   dplyr::filter(breed == "monkeys") #%>%
#   dplyr::select(-c("x":disp_day)) %>%
  # mutate_all(~stringr::str_replace_all(., c("\\[" = "", "\\]" = ""))) %>% 
  # rename_with(~stringr::str_replace_all(., c("-" = "_", "\\[" = "", "\\]" = "")))
  


# Plants (feeding, sleeping-trees and seeds)
# db1 <- db1 %>%
#   dplyr::filter(breed != "monkeys") %>%
#   dplyr::select(-c(energy:PT)) %>%
#   dplyr::select(-species.y) #%>%
  # dplyr::filter(if (breed != "seeds") species != "Syagrus romanzoffiana" else TRUE) %>% 
  # mutate(species = stringr::str_replace_all(species, c("\\[" = "", "\\]" = "")))

db1$species %>% as.factor() %>% levels()
db1 %>% str()

a <- db1 %>% dplyr::filter(feeding_trees_scenario == as.character(month_run)) 
a$species %>% unique()
# define boxplot width
# bpw <- 4/length(unique(paste(db1$group, db1$month)))
# bpw # 0.44444





## Plot example runs -------------------------

# rseed <- db1 %>%
#   # dplyr::filter(group == "Guareí") %>%
#   dplyr::select(random_seed) %>%
#   pull(1) %>%
#   sample(size = 1)
# breed_ <- "seeds"


# db1_filt <- db1 %>%
#   dplyr::filter(
#     # group == "Guareí" &
#       breed == breed_ &
#       random_seed == rseed
#   )


### Load empirical data -----

# (code from Data/Movement/Curated/02_params-siminputrow_movement.R)
mov <- read.csv(here("Data", "Movement", "Curated", "BLT_groups_data_siminputrow.csv")
                    #, stringsAsFactors = TRUE
)  %>% 
  mutate(group = recode(group, 
                        "Guarei" = "Guareí",
                        "Santa Maria" = "SantaMaria"
  )) %>%  # to match all other datasets
  rename(
    month = id_month
  ) %>% 
  mutate(datetime = lubridate::ymd_hms(datetime)) %>% 
  mutate(
    behavior = case_when(
      behavior == "Sleeping site" ~ "sleeping", 
      behavior == "Travel" ~ "travel", 
      behavior == "Frugivory" ~ "frugivory", 
      behavior == "Foraging" ~ "foraging", 
      behavior == "Resting" ~ "resting", 
      TRUE ~ "others"
    )
  )

mov %>% str()
mov$behavior %>% unique()
mov$species %>% unique()
mov$group %>% unique()


# (dataset created in Data/Seed_dispersal/03_Validation-patterns.R)
sdd <- read.csv(here("Data", "Seed_dispersal", "Curated", "All-areas_SeedDispersal.csv")) %>%
  mutate(group = recode(group, 
                        "Guarei" = "Guareí",
                        "Santa Maria" = "SantaMaria"
  )) %>%  # to match all other datasets
  rename(
    month = id_month,
    x_UTM = def_x,
    y_UTM = def_y
  ) %>% 
  mutate(def_datetime = lubridate::ymd_hms(def_datetime)) %>% 
  mutate(
    species = case_when(
      species == "inch_pass" ~ "Unindentified",
      TRUE ~ species
    )
  )

sdd %>% str()
sdd$species %>% unique()
sdd$group %>% unique()


trees.id <- read.csv(here("Data", "Movement", "Resource-trees", "Siminputrow_trees.csv")) %>% 
  mutate(group = recode(group, 
                        "Guarei" = "Guareí",
                        "Santa Maria" = "SantaMaria"
  )) %>%  # to match all other datasets
  rename(
    month = id_month,
    x_UTM = x,
    y_UTM = y
  ) %>% 
  mutate(datetime = lubridate::ymd_hms(datetime)) %>% 
  mutate(
    species = case_when(
      species == "inch_pass" ~ "Unindentified",
      species == "Unknown" ~ "Unindentified",
      TRUE ~ species
    )
  )

trees.id %>% str()
trees.id$species %>% unique()
trees.id$group %>% unique()



### Define empirical movement data
mov.db <- mov %>% 
  dplyr::filter(
    group == area_run & month == month_run 
  ) %>% 
  rename(
    x_UTM = x,
    y_UTM = y
  ) %>% 
  mutate(
    timestep = seq(1:nrow(.))
  )


### Define empirical seed dispersal data
sdd.db <- sdd %>% 
  dplyr::filter(
    group == area_run & month == month_run 
  )


### Define empirical tree species data
trees.db <- trees.id %>% 
  dplyr::filter(
    group == area_run & month == month_run 
  )
trees.db$species %>% unique()


area_run_vec <- as.character(area_run)
str(area_run_vec)
str(a)


  
  
  ### Define base sf plot
  if(area_run_vec == "Guareí") { 
    base.sf <- gua.sf
    base.name <- "Small fragment"
    # sdd.shapes <- sdd.db %>% 
    #   dplyr::select(species) %>% 
    #   unique()
  } else if ( area_run_vec == "SantaMaria" ) { 
      base.sf <- sma.sf
      base.name <- "Medium fragment"
  } else if( area_run_vec == "Taquara" ) { 
      base.sf <- taq.sf
      base.name <- "Continuous forest"
  } else if ( area_run_vec == "Suzano" ) { 
      base.sf <- suz.sf
      base.name <- "Riparian forest"
  }

# base.sf
base.name


### Spatial plots -----

# Subset simulated agents:
mov_sim <- subset(db1, breed == "monkeys")
seed_sim <- subset(db1, breed == "seeds")


# Aesthetic levels for plotting:

n_sp_names <- c(mov.db$species, trees.db$species)
n_sp_names <- unique(n_sp_names)
n_sp_names <- c(n_sp_names, 'NA')

# n_sp <- length(sdd$species %>% unique()) + 1 # add one more to NA values
n_sp <- length(n_sp_names)

set.seed(43)

species_colors <- viridis::turbo(n = n_sp)
names(species_colors) <- n_sp_names

species_shapes <- rep(1, n_sp)
names(species_shapes) <- n_sp_names
# species_shapes <- sample(x = c(1:25), n_sp, replace = TRUE)

unique(seed_sim$species) %in% names(species_shapes)
seed_sim$species %in% names(species_shapes)
species_shapes[names(species_shapes) %in% unique(seed_sim$species)]

sp_sim <- n_sp_names[n_sp_names %in% seed_sim$species]

# check if there are repeated shape values:
# sp_sim <- seed_sim$species %>% unique()


if ( length(sp_sim) >= length(unique(mov.db$species))) {
  
  # species_shapes <- species_shapes[names(species_shapes) %in% sp_sim]
  b <- sample(x = c(2:10, 15:25), length(sp_sim), replace = FALSE)
  species_shapes <- b
  names(species_shapes) <- sp_sim
  
} else {
  species_shapes <- sample(x = c(1:10, 12, 14:25)
                           , size = length(unique(mov.db$species)) + 1 # one more for NAs
                           , replace = FALSE)
  # names(species_shapes) <- 
}


#### sim1
sim1 <-
  base.sf +
  geom_point(data = mov_sim,
             aes(x = x_UTM, y = y_UTM
                 # , group = behavior
                 , color = behavior
                 , shape = behavior
                 , size = behavior
             )
             # size = 3
  ) +
  scale_color_manual(values = behav_simulated_colors) +
  scale_shape_manual(values = behav_simulated_shapes) +
  # scale_shape_manual(values = c(1, 16, 1, 16, 1)) +
  scale_size_manual(values = c(1, 3, 1, 3, 1)) +
  labs(color  = "behavior", shape = "behavior", size = "behavior") +
  
  ggnewscale::new_scale_color() +
  geom_path(data = mov_sim,
            aes(x = x_UTM, y = y_UTM, color = day) #timestep
            , lwd = 0.15
            # , linetype = "dashed"
            # , color = "grey25") +
            ) +
  # scale_color_viridis_b() +
  
  ggtitle(paste0(#base.name, #"Simulated path and seed dispersal events",
    # " (rseed = ", aux_run, ")")
    "Simulated (seed = ", aux_run, ")")
  ) +
  custom_theme  +
  theme(
    axis.text.x = element_text(hjust = 1)
  ) +
  xlab("") +
  ylab("") +
  guides(shape = "none",
         color = "none"
         # shape = guide_legend(order = 1)
  )

sim1


#### sim2
sim2 <- 
  base.sf +
  geom_point(data = seed_sim,
             aes(x = x_UTM, y = y_UTM
                 # , size = SDD
                 , color = SDD
                 , fill = SDD
                 # shape = disp_day
                 , shape = species
             ),
             alpha = 0.7
             # , shape = 21
             , stroke = 0.5
             # , size = 4
             , position = position_jitter(width = 10, height = 10)
  ) +
  scale_shape_manual(values = species_shapes) +
  scale_color_gradient(low = "blue", high = "#F51616", limits = c(0, 1000)) +
  scale_fill_gradient(low = "blue", high = "#F51616", limits = c(0, 1000)) +
  # scale_color_gradient2(
  #   midpoint = 500
  #   , low = "red"
  #   , mid = "darkgreen"
  #   , high = "blue"#,
  # ) +
  ggtitle(" ") +
  ylab("") +
  custom_theme +
  theme(
    axis.text.x = element_text(hjust = 1)
    # axis.title.x = element_blank()
  ) +
  guides(fill = "none", 
         color = guide_colorbar(direction = "horizontal"
                                , order = 1)
  )

sim2

# shapes <- subset(db1, breed == "seeds") %>% 
#   dplyr::select(species) %>% 
#   unique() %>% 
#   nrow()
# 
# nshapes <- rep(1, nshapes)
#   
# sim2 <- 
#   base.sf +
#   geom_point(data = subset(db1, breed == "seeds"),
#              aes(x = x_UTM, y = y_UTM
#                  , size = SDD
#                  , color = species
#                  , fill = species
#                  # shape = disp_day
#                  # , shape = species
#              ),
#              alpha = 0.4
#              , shape = 21
#              , stroke = 1
#              # , size = 4
#              # , position = position_jitter(width = 3)
#              ) +
#   scale_color_viridis_d(option = "inferno") +
#   scale_fill_viridis_d(option = "inferno") +
#   scale_size_continuous(limits = c(0, 1000)) +
#   # scale_size_identity(trans="log10",guide="legend") +
#   # scale_color_viridis_d(option = "viridis") +
#   # scale_shape_manual(values = nshapes) +
#   # scale_fill_gradient(low = "#7F7E7C", high = "#B50404") +
#   ggtitle(" ") +
#   custom_theme +
#   theme(
#     axis.text.x = element_text(hjust = 1)
#   ) #+
#   # guides 
# 
# sim2








#### obs1

##### create sequence of days

mov.db <- mov.db %>% 
  mutate(
    dd = lubridate::day(datetime)
  ) %>% 
  group_by(dd) %>% 
  mutate(
    day = cur_group_id()
  )

obs1 <-
  base.sf +
  geom_point(data = mov.db,
             aes(x = x_UTM, y = y_UTM
                 # , group = behavior
                 , color = behavior
                 , shape = behavior
                 , size = behavior
             ),
             # size = 3
  ) +
  scale_color_manual(values = behav_simulated_colors) +
  scale_shape_manual(values = c(behav_simulated_shapes)) +
  # scale_shape_manual(values = c(1, 16, 4, 1, 16, 1)) +
  scale_size_manual(values = c(1, 3, 3, 1, 3, 1)) +
  labs(color  = "behavior", shape = "behavior", size = "behavior") +
  
  ggnewscale::new_scale_color() +
  geom_path(data = mov.db,
            aes(x = x_UTM, y = y_UTM, color = day) #timestep
            , lwd = 0.15
            # , linetype = "dashed"
            # , color = "grey25") +
  ) +
  # scale_color_viridis_b() +
  # scale_color_continuous(type = "viridis") +
  # scale_color_continuous(type = "gradient") +
  
  # ggtitle(paste0("Observed, ", base.name, " (", month_run, ")")
  ggtitle(paste0("Observed")
  ) +
  xlab("") +
  ylab("y_UTM") +
  custom_theme +
  theme(
    axis.text.x = element_text(hjust = 1)
  ) +
  guides( color = "none"
  # #   #shape = "none"
  # # #        # shape = guide_legend(order = 1)
  )

obs1


#### obs2
obs2 <- 
  base.sf +
  geom_point(data = sdd.db,
             aes(x = x_UTM, y = y_UTM
                 # , size = SDD
                 , color = SDD
                 , fill = SDD
                 # shape = disp_day
                 , shape = species
             ),
             alpha = 0.7
             # , shape = 21
             , stroke = 0.5
             # , size = 4
             , position = position_jitter(width = 10, height = 10)
  ) +
  scale_shape_manual(values = species_shapes) +
  # scale_color_gradient(low = "grey70", high = "#F51616", limits = c(0, 1000)) +
  # scale_fill_gradient(low = "grey70", high = "#F51616", limits = c(0, 1000)) +
  scale_color_gradient(low = "blue", high = "#F51616", limits = c(0, 1000)) +
  scale_fill_gradient(low = "blue", high = "#F51616", limits = c(0, 1000)) +
  ggtitle(" ") +
  # ylab("") +
  custom_theme +
  theme(
    axis.text.x = element_text(hjust = 1)
  ) +
  guides(fill = "none", 
         color = guide_colorbar(direction = "horizontal"
                                , order = 1, hjust = 0.5)
  )

obs2




# Fruiting trees:

# # target <- c("feeding-trees", "sleeping-trees")
# trees.gua <- db1 %>%
#   dplyr::filter(
#     # group == group &
#       breed != "seeds" &
#       # breed == "feeding-trees" | breed == "sleeping-trees" &
#       # breed %in% target &
#       random_seed == rseed
#   )
# trees.gua$breed %>% as.factor %>%  levels()
# trees.gua$species %>% as.factor %>%  levels()


# ## SDD + trees
# gua.p1 <- gua.sf +
#   geom_point(data = db1_filt,
#              aes(x = x_UTM, y = y_UTM
#                  , size = SDD
#                  , color = species
#                  # shape = disp_day
#              ),
#              alpha = 0.4) +
#   scale_color_viridis_d(option = "magma") +
#   # ggtitle(paste0("Simulated seed dispersal coordinates",
#   #                " (", unique(db1_filt$month), ") - one run")
#   # ) +
# 
#   geom_point(data = trees.gua, # they don't disperse Syagrus (this has to be corrected in the model)
#              aes(x = x, y = y, #group = breed,
#                  # size = breed,
#                  # color = species,
#                  shape = breed
#              ),
#              size = 2
#              # alpha = 0.4
#              ) +
#   scale_shape_manual(values = c(16, 17)) +
#   # scale_color_viridis_d(option = "inferno") +
# 
#   # change SDD legend point size:
#   scale_size_continuous(
#     limits=c(1,1000),
#     breaks=c(100,200, 300, 500, 750, 1000),
#     range=c(1,10)
#   ) +
#   # change legend symbol sizes:
#   guides(
#     color = guide_legend(override.aes = list(size = 3, alpha = 1, shape = 15) ),  # , alpha = 0.6
#     shape = guide_legend(override.aes = list(size = 3, alpha = 1) )
#   )

#### Save plot

# Simulated only:
# cow <- cowplot::plot_grid(sim1, sim2, labels = c('A', 'B'))
# cow


# Extract legend of observed behaviors
behav_legend <- get_legend(obs1)
# day_legend <- get_plot_component(obs1, 'guide-box', return_all = TRUE)
# day_legend <- get_legend(obs1)
if(area_run_vec == "SantaMaria") { 
  seed_legend <- get_legend(sim2)
} else {
  seed_legend <- get_legend(obs2)
}

legend <- plot_grid(behav_legend, seed_legend
                    , ncol = 1
                    , rel_widths = c(1, 1)
                    )

# Simulated + observed:

cow <- cowplot::plot_grid(
  obs1 + theme(legend.position = "none"),
  sim1 + theme(legend.position = "none"),
  obs2 + theme(legend.position = "none"),
  sim2 + theme(legend.position = "none")
  # behav_legend
  , labels = c('A', 'B', 'C', 'D')
  , label_size = 24
)

cow <- plot_grid(cow, legend, ncol = 2,
                 rel_widths = c(0.8, 0.2))

cow
# cow %>%
#   save_plot(filename = paste0(
#     outpath, "/", "03_", expname, ".png")
#     , base_height = 8, base_width = 14
#     , bg = "white"
#     )



# Create title
title1 <- ggdraw() +
  draw_label(label = paste0(base.name, " (", month_run, ")"),
             size = 24
             # theme = theme_georgia(),# element = "plot.title",
             , x = 0.5, hjust = 0.5, vjust = 0)


cow_combined <- plot_grid(title1
                          , cow
                          , ncol = 1,
                          rel_heights = c(0.08, 1),
                          rel_widths = c(1, 1),
                          label_size = 24)


# cow_combined

cow_combined %>%
  save_plot(filename = paste0(
    outpath, "/", "03_", expname, "grid.png")
    , base_height = 9, base_width = 14
    , bg = "white"
  )



} # stop 2






# #### Grid plot
# 
# cow1 <- cowplot::plot_grid(obs1 +
#                              theme(#plot.title = element_blank(),
#                                    #plot.subtitle = element_blank(),
#                                    legend.position = "none",
#                                    plot.margin = unit(c(0,0,0,0), "cm")
#                                    ),
#                            sim1 +
#                              theme(legend.position = "none",
#                                    plot.margin = unit(c(0,0,0,0), "cm")
#                                    ),
#                            align = "h"
#                            # , rel_widths = c(1.5, 1)
#                            , labels = c('A', 'B')
# )
# cow1
# 
# legend1 <- get_legend(obs1 +
#                         theme(legend.position = "right"))
# 
# cow1_grid <- plot_grid(cow1, legend1, ncol = 2,
#                        rel_heights = c(0.5, 0.9),
#                        rel_widths = c(0.2, 0.05))
# # cow1_grid <- plot_grid(title1, legend1, cow1, ncol = 1,
# #                   rel_heights = c(0.1, 0.1, 1),
# #                   rel_widths = c(0.1, 0.5, 0.1))
# cow1_grid
# 
# # cow1_grid %>%
# #   save_plot(filename = paste0(
# #     outpath, "/", "03_", expname, "grid_upper.png")
# #     , base_height = 4, base_width = 12
# #     , bg = "white"
# #   )
# 
# 
# cow2 <- cowplot::plot_grid(obs2 +
#                              theme(#plot.title = element_blank(),
#                                #plot.subtitle = element_blank(),
#                                plot.margin = unit(c(0,0,0,0), "cm"),
#                                legend.position = "none"),
#                            sim2 +
#                              theme(
#                                legend.position = "none",
#                                plot.margin = unit(c(0,0,0,0), "cm")
#                                ),
#                            align = "h"
#                            # , rel_widths = c(1.5, 1)
#                            , labels = c('C', 'D')
# )
# cow2
# 
# legend2 <- get_legend(sim2 +
#                         theme(legend.position = "right"))
# 
# cow2_grid <- plot_grid(cow2, legend2, ncol = 2,
#                        rel_heights = c(0.8, 0.9),
#                        rel_widths = c(0.2, 0.05)
#                        # , label_size = 18
#                        )
# cow2_grid
# 
# # cow2_grid %>%
# #   save_plot(filename = paste0(
# #     outpath, "/", "03_", expname, "grid_lower.png")
# #     , base_height = 4, base_width = 12
# #     , bg = "white"
# #   )
# 
# 
# cow_combined <- plot_grid(title1
#                           , cow1_grid #+ theme(plot.margin = unit(c(0,0,0,0), "cm"))
#                           , cow2_grid #+ theme(plot.margin = unit(c(0,0,0,0), "cm"))
#                           , ncol = 1,
#                           rel_heights = c(0.10, 0.80, 0.80),
#                           rel_widths = c(0.9, 0.9, 0.1),
#                           label_size = 24)
# 
# 
# # cow_combined
# 
# cow_combined %>%
#   save_plot(filename = paste0(
#     outpath, "/", "03_", expname, "grid.png")
#     , base_height = 10, base_width = 12
#     , bg = "white"
#     )
# 

