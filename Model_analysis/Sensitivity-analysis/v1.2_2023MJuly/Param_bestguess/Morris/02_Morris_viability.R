library("nlrx")
library("tidyverse")


# path <-  "D:/Yourpath"
path <- "D:/Data/Documentos/github/BLT_IBM-Model/Model_analysis/Sensitivity-analysis/v1.2_2023MJuly/Param_bestguess/Morris/temp/"

# Correctly estimates sigma:
nl_file <- readRDS(paste0(path, "/", "v1.2_Morris_SantaMaria_Apr_Feedingbout_on_2023-08-21.rds"))

# Incorrectly estimates sigma:
nl_file <- readRDS(paste0(path, "/", "v1.2_Morris_Taquara_Jan_Feedingbout_off_2023-08-22.rds"))




### Testing code from github 
# https://github.com/ropensci/nlrx/blob/master/R/analyze_nl.R

mo <- getsim(nl_file, "simobject")[[1]]
nl_file@simdesign@simobject
nl_file@simdesign@simoutput

mo$X

mo$ee # only available after sensitivity::te
a <- mo$ee %>% as_tibble()

nl <- nl_file

myfuns <- list(mean=mean, sd=sd, min=min, max=max)
funs = myfuns

sensindex <- NULL
so <- getsim(nl, "simobject")[[1]]

na.discovered <- FALSE

# Calculate sensitivity indices separately for each random seed:
# for (i in getsim(nl, "simseeds")) {
  
  i <- getsim(nl, "simseeds")[1] # TEST EACH SEED
  
  # Select seed runs, aggregate across steps and select only output columns:
  simoutput.i <- getsim(nl, "simoutput") %>%
    dplyr::filter(`random-seed` == i) %>%
    dplyr::group_by(siminputrow) %>%
    dplyr::summarise_at(getexp(nl, "metrics"), funs) %>%
    dplyr::select(-siminputrow) %>%
    dplyr::select_if(~ !all(is.na(.)))
  
  metrics <- colnames(simoutput.i)
  simoutput.i <- t(as.matrix(simoutput.i))
  
  
  # Loop over metric columns and calculate sensitivity indices:
  # for (j in seq_len(nrow(simoutput.i))) {
    
    j <- 1# test loop
    
    sensitivity::tell(mo, simoutput.i[j, ])
    
    ####################################################
    ### Why am I getting 4 lines with all NAs? ---------
    mo$ee
    ####################################################
    
    if (anyNA(mo$ee)) {
      na.discovered <- TRUE
    }
    
    mustar <- tibble::tibble(
      metric = metrics[j],
      parameter = colnames(mo$ee),
      index = "mustar",
      value = apply(mo$ee, 2, function(x) mean(abs(x), na.rm=TRUE)),
      seed = i
    )
    mu <- tibble::tibble(
      metric = metrics[j],
      parameter = colnames(mo$ee),
      index = "mu",
      value = apply(mo$ee, 2, function(x) mean(x, na.rm=TRUE)),
      seed = i
    )
    sigma <- tibble::tibble(
      metric = metrics[j],
      parameter = colnames(mo$ee),
      index = "sigma",
      value = apply(mo$ee, 2, function(x) stats::sd(x, na.rm=TRUE)),
      seed = i
    )
    
    sensindex <- rbind(sensindex, mustar, mu, sigma)
  # }
  
    
    # Print warning if NAs were discovered:
    if (isTRUE(na.discovered))
    {
      warning("NAs were discovered in the simulation output data during morris index calculation!")
    }
    
    # Remove rownames
    rownames(sensindex) <- NULL
    sensindex <- tibble::as_tibble(sensindex)
    
    return(sensindex)
    # }
    
    
    
    

    
 ## Compare parameter space results ------------
    
    ### Remove runs where tamarins died (DPL or KDE = 0)
    out <- nl_file@simdesign@simoutput %>%
      dplyr::filter(g_DPL == 0) %>% as_tibble()
    
    nl_file@simdesign@simoutput <- nl_file@simdesign@simoutput %>%
      dplyr::filter(g_DPL != 0) %>% as_tibble()
    
    db <- nl_file@simdesign@simoutput
    
    nl_file@simdesign@simmethod
    nl_file@simdesign@siminput
    nl_file@simdesign@simseeds # 20 seeds, ok
    nl_file@simdesign@simoutput
    
    
    
    ### Morris
    morris_db <- analyze_nl(nl_file, "simoutput")
    
    ### Assign number of unviable runs:
    morris_db$unviable_runs <- nl_file %>% 
      eval_simoutput() %>% 
      nrow()
    
    ### Check sigma values
    a <- morris_db %>% dplyr:: filter(index == "sigma")
    
    
    ### Checking parameter values
    
    #### 1) of viable runs
    db <- db %>% 
      tidyr::pivot_longer(
        cols = energy_stored_val:duration,
        names_to = 'parameter',
        values_to = "value"
      ) %>% 
      mutate(viable = "yes")
    
    db %>% ggplot() +
      geom_density(aes(x = value)) +
      facet_wrap(~parameter, scales = "free")
    
    
    
    #### 2) of unviable runs
    out <- out %>% 
      tidyr::pivot_longer(
        cols = energy_stored_val:duration,
        names_to = 'parameter',
        values_to = "value"
      ) %>% 
      mutate(viable = "not")
    
    out %>% ggplot() +
      geom_density(aes(x = value)) +
      facet_wrap(~parameter, scales = "free")
    
    
    #### 3) All together
    db_all <- dplyr::full_join(db, out)
    db_all %>% ggplot() +
      geom_density(aes(x = value, fill = viable), alpha = 0.5) +
      facet_wrap(~parameter, scales = "free")
    
    # Save plot
    # ggsave(paste0(path, "/",
    #               '02_Morris_viable_parameters_Guarei-Aug-on.png'), height = 7, width = 14, dpi = 600)
    # '02_Morris_viable_parameters_Taquara-Jan-on.png'), height = 7, width = 14, dpi = 600)
    
    
    a <- db_all %>% dplyr::filter(is.na(parameter))
    
    




