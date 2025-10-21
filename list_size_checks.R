library(marginaleffects)
library(patchwork)
library(lme4)
library(tinyplot)
library(lubridate)
library(purrr)
library(dplyr)
library(readr)
library(here)
library(glue)
library(tidyr)
library(stringr)
library(fpp2)
library(scales)

source("analysis/design.R")

source("imports.R")

### check difference in practices between full and type 4 list sizes
### restricted to type 4
 nrow(df_stats_prac) # 432764
 
 ### full
 nrow(df_stats_prac_full) # 433629
 
nrow(df_stats_prac_full) -  nrow(df_stats_prac) ## 865

# do we lose practices when combining with CCG
 ### restricted to type 4
 df_stats_prac_region <- df_stats_prac %>%
   left_join(df_ccgs_prac,by=join_by(pct_id ==code)) 
 
 df_stats_prac_region %>%
   filter(is.na(regional_team_id))
 ### 1 practice (Foundations) missing region
 ### there are items for the missing region but the practice is filtered out from the region dataset

 ### full
df_stats_prac_full_region <- df_stats_prac_full %>%
  left_join(df_ccgs_prac,by=join_by(pct_id ==code)) 

df_stats_prac_full_region %>%
  filter(is.na(regional_team_id))
  ### 2 practices (Foundations & COMPASS MEDICAL PRACTICE) missing region

## how many practices missing list size
  ### restricted to type 4
df_stats_prac_region %>% 
  filter(is.na(total_list_size)) %>%
  select(practice) %>%
  unique() ### none

 ## full
df_stats_prac_full_region %>% 
  filter(is.na(total_list_size)) %>%
  select(practice) %>%
  unique() ### none

### compare to regional atorvastatin
#needs df_atorvastatin
df_atorvastatin_region <- df_atorvastatin %>%
  filter(str_detect(bnf_name, '20')) %>%
  group_by(month,regional_team) %>%
  summarise(items=sum(items),
            total_list_size=mean(total_list_size),
            rate = items / total_list_size *1000)  %>%
  ungroup()

### restricted
df_stats_prac_region_month <- df_stats_prac_region %>%  
  group_by(month,regional_team_id) %>%
  summarise(total_list_size_stats=sum(total_list_size, na.rm = TRUE)) %>%
  filter(!is.na(regional_team_id))

compare_restricted <- df_stats_prac_region_month %>%
  left_join(df_atorvastatin_region,by=join_by(regional_team_id==regional_team,month)) %>%
  ungroup() %>%
  summarise(month,
            regional_team_id,
            total_list_size_stats,
            total_list_size,
            diff = total_list_size_stats - total_list_size)
  ### difference all negative 

### full
df_stats_prac_full_region_month <- df_stats_prac_full_region %>%  
  group_by(month,regional_team_id) %>%
  summarise(total_list_size_stats=sum(total_list_size, na.rm = TRUE)) %>%
  filter(!is.na(regional_team_id))

compare_restricted_full <- df_stats_prac_full_region_month %>%
  left_join(df_atorvastatin_region,by=join_by(regional_team_id==regional_team,month)) %>%
  ungroup() %>%
  summarise(month,
            regional_team_id,
            total_list_size_stats,
            total_list_size,
            diff = total_list_size_stats - total_list_size)
  ### no difference in region

### how many sub-icb locations (historically CCG) in df_ccgs_prac
sicbls<-unique(df_ccgs_prac %>% filter(org_type=="CCG") %>% select(code))
### 248 

# how many sub-icb locations in info?
df_stats_sicbls <- df_stats_prac %>% 
  group_by(pct_id) %>%
  summarise(pct_id = last(pct_id))
### 219

sicbl_lost_list_id <- sicbls %>% 
  anti_join(df_stats_sicbls,by=join_by(code ==pct_id)) 
## 30 missing sicbl

sicbl_lost_list <- sicbl_lost_list_id %>%
  left_join(df_ccgs_prac)
## From spot-checks I suspect these have merged or changed name pre-2020.

### check already performed above, confirm it is same practice (Foundation)
stats_sicbl_lost_list_id<-df_stats_sicbls %>% 
  anti_join(sicbls,by=join_by(pct_id ==code))
# 1 sicbl in df_stats_sicbls not in sicbls

stats_sicbl_lost_list <- stats_sicbl_lost_list_id %>%
  left_join(df_stats_prac)
### same practice without region above (Foundation) 
