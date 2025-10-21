library(readr)
library(magrittr)
library(dplyr)
library(glue)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(ggrepel)
library(purrr)

source("lib/ISPO_functions.R")
# load("lib/dat/baseline-data.Rdat")
# load("lib/dat/modification-profiles.Rdat")
# load("lib/dat/modified-data.Rdat")

###################################################################
### STEP 4: RUN INDICATOR SATURATION ANALYSIS                   ###
###################################################################

parameter_space = expand.grid(
  p = c( 0.01, 0.001, 0.0001, 0.00001 ),
  #p = c( 0.01, 0.001 ),
  r = seq( 0.20, 0.40, by=0.10 )
  #r = c( 0.30, 0.40 )
)
n_practices = 500

source("lib/change_detection/indicator_saturation_functions.R")

outdir = "output"

# list_of_modified_datasets = all_modified_datasets %>% pull( modification_profile ) %>% unique()
# 
# ### To control what is run during testing:
# n_modified_datasets = length(list_of_modified_datasets)
# n_parameter_combinations = nrow(parameter_space)
# 
# # Started 11th Feb 18:40
# 
# list_of_modified_datasets = list_of_modified_datasets[1:n_modified_datasets]
# parameter_space = parameter_space[1:n_parameter_combinations,]
# 
# m_i = 0
# test_mode = NA
# 
# 
# files_to_remove = c(
#   "coefficients_data.csv",
#   "plot_data.csv",
#   "r_input_*.csv",
#   "r_intermediate_*.RData"
# )
# 
# 
# for ( this_dataset_name in list_of_modified_datasets ) {
#   m_i = m_i + 1
#   
#   model_summary_data = tibble()
#   
#   this_dataset = all_modified_datasets %>%
#     filter( modification_profile == this_dataset_name ) %>% 
#     mutate( numerator = value_updated )  %>% 
#     mutate( denominator = 1 )  %>% 
#     mutate( value = value_updated ) %>% 
#     select( id, numerator, denominator, value, date )
#   
#   this_indir = outdir
#   this_outdir = outdir #glue("{outdir}/{this_dataset_name}")
#   this_dataset_file = glue("{this_dataset_name}.csv")
# #   
#   if ( !dir.exists(this_outdir) ) {
#     cat( glue("creating output directory: {this_outdir}\n\n") )
#     dir.create(this_outdir)
#   }
#   
#   write.csv(
#     this_dataset,
#     file=glue("{this_indir}/{this_dataset_file}" ),
#     row.names = FALSE
#   )
#   
  # for ( i in 1:nrow(parameter_space) ) {
  #   
  #   this_p = parameter_space[i,] %>% pull(p)
  #   this_r = parameter_space[i,] %>% pull(r)
  #   
    for (this_direction in c("up", "down")) {
      
      measure_indicator = ChangeDetection(
        name = "inclisiran",
        code_variable = "regional_team",
        numerator_variable = "items",
        denominator_variable = "total_list_size",
        date_variable = "month",
        numcores = 1,
        indir = "data",
        outdir = "output",
        direction = this_direction,
        overwrite = TRUE,
        draw_figures = TRUE,
        verbose = FALSE,
        change_detection_location = glue("{getwd()}/lib/change_detection"),
        results_extract_location = glue("{getwd()}/lib/change_detection"),
        # test_number = n_practices,
        csv_name = "tirzepatide_list.csv",
        p_threshold = 0.0001,
        ratio_threshold = 0.2
      )
      
      test_mode = measure_indicator@test_number
      
      this_output_file = glue("{this_outdir}/{this_dataset_name}/summary_output_TEST-{test_mode}_{this_direction}_{this_p}_{this_r}.csv")
      
      if (file.exists(this_output_file)) {
        cat(glue("* {this_dataset_name} (TEST={test_mode}) ({this_direction}) with parameters p={this_p} & r={this_r} has already been run\n\n"))
      } else {
        cat(glue("* {this_dataset_name} (TEST={test_mode}) ({this_direction}) [{m_i} of {n_modified_datasets}] -- p={this_p} & r={this_r} [{i} of {nrow(parameter_space)}]\n\n"))
        
        run(measure_indicator)
        
        if (file.exists(glue("{this_outdir}/{this_dataset_name}/summary_output.csv"))) {
          mv_string = glue("mv {this_outdir}/{this_dataset_name}/summary_output.csv {this_output_file}")
          cat(glue("{mv_string}\n\n"))
          system(mv_string)
          
          glue("rm {this_outdir}/{this_dataset_name}/{files_to_remove}") %>%
            paste(collapse = "; ") %>%
            system()
        }
      }
      ### If there is still no output file, then the algorithm didn't run
      if (file.exists(this_output_file)) {
        
        this_summary_data = read.csv(this_output_file) %>%
          mutate(
            p_threshold = this_p,
            r_threshold = this_r,
            dataset = this_dataset_name,
            test_number = test_mode
          )
        
        if (class(this_summary_data %>% pull(breaks.loc.pos)) == "integer") {
          this_summary_data = this_summary_data %>%
            mutate(breaks.loc.pos = as.character(breaks.loc.pos)) %>%
            mutate(breaks.coef.pos = as.character(breaks.coef.pos))
        }
        
        if (class(this_summary_data %>% pull(breaks.loc.neg)) == "integer") {
          this_summary_data = this_summary_data %>%
            mutate(breaks.loc.neg = as.character(breaks.loc.neg)) %>%
            mutate(breaks.coef.neg = as.character(breaks.coef.neg))
        }
        
        model_summary_data = model_summary_data %>%
          bind_rows(this_summary_data)
        
      }
    }
  }
  
  save(
    model_summary_data,
    file = glue("lib/dat/{this_dataset_name}_model-outputs.Rdat")
  )
  
# }

save(parameter_space,
     n_practices,
     test_mode,
     file = "lib/dat/indicator-saturation-variables.Rdat")
