directory_name <- "data"
fig_path_tis_analysis <- "output/isat"
bnf_names <-c("atorvastatin","inclisiran","tirzepatide")
date_atorvastatin_ng <-as.Date("2023-05-01")
date_inclisiran_ng <-as.Date("2021-10-06")
date_tirzepatide_ng <-as.Date("2024-12-23")
date_atorvastatin_diab <-as.Date(NA)
date_inclisiran_diab <-as.Date(NA)
date_tirzepatide_diab <-as.Date("2023-10-25")
plot_show <- FALSE 

n_splits <- 3
verbose <- T
sample <- F
measure <- F
custom_measure <- F
direction <- "both"
use_cache <- T
overwrite <- F
draw_figures <- 'yes'
bq_folder <- "measures"
saveplots_analysis <- T ###save plots of output of analysis

p_alpha <- 0.0001  ## level of significance for the detection of breaks (main calibration choice)
parallel <- NULL  ### set as integer (=number of cores-1) if selection should run in parallel (may increase speed for longer time series)
# recommended to set to "NULL" unless datasets are very large.

###### Timing Measures
known.t <- 20 ### Time of known intervention in the sample, e.g. medication became available as generic at observation t=18
break.t.lim <- .8 ### Proportion offset after negative break 

###### Slope Measures
slope.lim <- 0.5   ### Proportion of slope drop for construction of slope measure

