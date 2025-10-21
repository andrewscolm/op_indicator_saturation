library(dplyr)
library(readr)
library(here)
library(glue)
library(tidyr)

source("analysis/design.R")

###  
df_ccgs<- read_csv(here::here(directory_name,"prac_ccgs.csv")) %>%
  select("code","name","stp_id", "regional_team_id","org_type") 

### list size (restricted to type 4 practices)
df_stats_prac <- read_csv(here::here(directory_name,"practice_info.csv")) 

## list size (not restricted to type 4 practices)
df_stats_prac_full <- read_csv(here::here(directory_name,"practice_info_full.csv")) 

## atorvastatin at regional level 
df_atorvastatin <- read_csv(here::here(directory_name,"atorvastatin_list.csv"))

## atorvastatin at practice level 
df_atorvastatin_prac <- read_csv(here::here(directory_name,"atorvastatin_practice.csv"))

df_stp <- read_csv(here::here(directory_name,"prac_stps.csv")) 

### regions from region team
df_regions <- read_csv(here::here("data","NHS_England_Names_and_Codes_in_England.csv")) %>%
  rename(regional_team = NHSER24CDH ,region = NHSER24NM) %>%
  select(regional_team,region)
