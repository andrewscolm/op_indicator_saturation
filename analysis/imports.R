library(readr)
library(here)
library(glue)
library(tidyr)
library(dplyr)

source("analysis/design.R")

###  
df_ccgs<- read_csv(here::here(directory_name,"prac_ccgs.csv")) 

### list size (restricted to type 4 practices)
df_stats_prac <- read_csv(here::here(directory_name,"practice_info.csv.gz")) 

## list size (not restricted to type 4 practices)
df_stats_prac_full <- read_csv(here::here(directory_name,"practice_info_full.csv.gz")) 

## atorvastatin at regional level 
df_atorvastatin <- read_csv(here::here(directory_name,"atorvastatin_list.csv"))

## atorvastatin at practice level 
df_atorvastatin_prac <- read_csv(here::here(directory_name,"atorvastatin_practice.csv.gz"))

df_stp <- read_csv(here::here(directory_name,"prac_stps.csv")) 

### regions from region team
df_regions <- read_csv(here::here("data","NHS_England_Names_and_Codes_in_England.csv")) %>%
  rename(regional_team = NHSER24CDH ,region = NHSER24NM) %>%
  dplyr::select(regional_team,region)
