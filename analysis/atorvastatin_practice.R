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
library(Amelia)

source("analysis/design.R")

source("analysis/imports.R")
# df_atorvastatin is region level
# df_atorvastatin_prac is practice level
# df_stats_prac has the practice sizes

df_ccgs_prac <- read_csv(here::here(directory_name,"prac_ccgs.csv")) %>%
  select("code","name","stp_id", "regional_team_id","org_type") 

# filter to 20
df_atorvastatin_prac_20 <- df_atorvastatin_prac %>%
  filter(str_detect(bnf_name, '20')) 

# do we lose practices in the filter?
df_atorvastatin_prac_20 %>%
  group_by(practice) %>%
  summarise(practice=last(practice)) %>%
  nrow() ## 6845

df_atorvastatin_prac %>%
  group_by(practice) %>%
  summarise(practice=last(practice)) %>%
  nrow()  ## 6858
#### 13 lost practices

### identify lost practices
ator_prac_list_20 <- df_atorvastatin_prac_20 %>%
  group_by(practice) %>%
  summarise(practice=last(practice))

ator_prac_list <- df_atorvastatin_prac %>%
  group_by(practice) %>%
  summarise(practice=last(practice))  

ator_lost_list <- ator_prac_list %>% 
  anti_join(ator_prac_list_20)

df_ator_lost_list <- ator_lost_list %>%
  left_join(df_stats_prac_full) %>%
  group_by(practice) %>%
  summarise(prac_name = last(name),
            sicbl=last(sicbl)) %>%
  left_join(df_ccgs_prac,by=join_by(sicbl ==code)) %>%
  left_join(df_regions,by=join_by(regional_team_id ==regional_team )) %>%
  select(practice,prac_name,sicbl,name,regional_team_id,region)
### look at list size of practices later on

### how many practices are missing sicbl code  
miss_ccg <- df_atorvastatin_prac_20  %>% 
  left_join(df_stats_prac) %>% 
  filter(is.na(sicbl)) # 1,624 lines with missing sicbl

miss_ccg_id <- miss_ccg %>%
  group_by(practice) %>%
  summarise(practice=last(practice)) #344 practices missing ccg (means missing list size)

## double check this is the same as missing list size
miss_list <- df_atorvastatin_prac_20  %>% 
  left_join(df_stats_prac) %>% 
  filter(is.na(total_list_size)) ## 1,624 lines with list size (same as sicbl)


#### pratice per month (combines all items per practice per month)
df_atorvastatin_prac_month <- df_atorvastatin_prac %>%
  left_join(df_stats_prac) %>%
  group_by(month,practice) %>%
  summarise(
            items=sum(items),
            total_list_size=mean(total_list_size, na.rm = TRUE),
            regional_team = last(regional_team ),
            sicbl = last(sicbl),
            rate = items / total_list_size *1000)  %>%
  ungroup() %>%
  mutate(month = as.Date(month))


### compare practice to region dfs
### 
df_atorvastatin_chem <- df_atorvastatin %>%
  # filter(str_detect(bnf_name, '20')) %>%
  group_by(month,regional_team) %>%
  summarise(items=sum(items),
            total_list_size=mean(total_list_size),
            rate = items / total_list_size *1000)  %>%
  ungroup() 



df_atorvastatin_prac_compare <- df_atorvastatin_prac_month %>%
  ungroup() %>%
  group_by(month,regional_team) %>%
  summarise(items=sum(items),
            total_list_size=sum(total_list_size, na.rm = TRUE),
            rate = items / total_list_size *1000) %>%
  select(month,regional_team,items,total_list_size,rate) %>%
  ungroup() %>%
  full_join(df_atorvastatin_chem,by=join_by(month,regional_team)) %>%
  mutate(items= items.x-items.y,
         total_list_size = total_list_size.x-total_list_size.y)
       ##### difference in list size not in items ### list sometimes more / sometimes less


df_atorvastatin_prac_month %>%filter(regional_team=="Y60",month=="2022-05-01") %>%
  select(practice) #1300

df_atorvastatin_prac_month_id <- df_atorvastatin_prac_month %>%filter(regional_team=="Y60",month=="2022-05-01") %>%
  select(practice) #1300

### practice region from CCG (pct_id not sicbl)
df_stats_prac_region <- df_stats_prac %>%
  left_join(df_ccgs_prac,by=join_by(sicbl ==code)) 


df_stats_prac_region_id <- df_stats_prac_region %>% filter(regional_team_id=="Y60",month==as.Date("2022-05-01")) %>%
  select(practice) %>% unique()  #1289

df_atorvastatin_prac_month_id %>%
  anti_join(df_stats_prac_region_id)  %>%
  left_join(df_atorvastatin_prac_month) %>% View()

df_stats_prac_region_id %>%
  anti_join(df_atorvastatin_prac_month_id) %>%
  left_join(df_stats_prac_region) %>% View()


df_stats_prac_region %>% 
  group_by(sicbl,regional_team_id) %>%
  summarise(sicbl = last(sicbl),
            regional_team_id = last(regional_team_id)) %>% 
  count(sicbl) %>% View()

### sicbl which change region
df_stats_prac_region %>% 
  group_by(stp_id,regional_team_id) %>%
  summarise(stp_id = last(stp_id),
            regional_team_id = last(regional_team_id)) %>% 
  count(stp_id) %>%
  filter(n>1) %>%
  left_join(df_stats_prac_region) 

### practices which change region
df_stats_prac_region %>% 
  group_by(practice ,regional_team_id) %>%
  summarise(practice  = last(practice ),
            regional_team_id = last(regional_team_id)) %>% 
  count(practice ) %>%
  filter(n>1) %>% ## 6 practices, 5 from 15M (Y62 North West -> Y60 Midlands) and 1 from 78H (Y61 East of England -> Y60 Midlands)
  left_join(df_stats_prac_region)

### compare practice to region dfs
df_atorvastatin_prac_compare <- df_atorvastatin_prac %>%
  left_join(df_stats_prac) %>%
  group_by(month,practice) %>%
  summarise(
    items=sum(items),
    total_list_size=mean(total_list_size, na.rm = TRUE),
    regional_team = last(regional_team ),
    pct_id = last(pct_id),
    rate = items / total_list_size *1000)  %>%
  ungroup() %>%
  mutate(month = as.Date(month)) %>%
  ungroup() %>%
  group_by(month,regional_team) %>%
  summarise(items=sum(items),
            total_list_size=sum(total_list_size, na.rm = TRUE),
            rate = items / total_list_size *1000) %>%
  select(month,regional_team,items,total_list_size,rate) %>%
  ungroup() %>%
  full_join(df_atorvastatin,by=join_by(month,regional_team)) %>%
  mutate(items= items.x-items.y,
         total_list_size = total_list_size.x-total_list_size.y)

##### find practices with multiple ccgs
df_atorvastatin_prac_multi <- df_atorvastatin_prac %>% 
  left_join(df_stats_prac) %>%
  group_by(practice ,pct_id) %>%
  summarise(practice  = last(practice ),
            pct_id = last(pct_id)) %>% 
  count(practice )


df_atorvastatin_prac_multi %>% filter(n>1) %>% nrow() ## 3566
df_atorvastatin_prac_multi %>% filter(n==2) %>% nrow() ## 3399
df_atorvastatin_prac_multi %>% filter(n==3) %>% nrow() ## 167


## find practices with multiple sicbl (more relevant)
df_atorvastatin_prac_multi_sicbl <- df_atorvastatin_prac %>% 
  left_join(df_stats_prac) %>%
  group_by(practice ,sicbl) %>%
  summarise(practice  = last(practice ),
            sicbl = last(sicbl)) %>% 
  count(practice )

df_atorvastatin_prac_multi_sicbl %>% filter(n>1) %>% nrow() ## 340
df_atorvastatin_prac_multi_sicbl %>% filter(n==2) %>% nrow() ## 340
df_atorvastatin_prac_multi_sicbl %>% filter(n==3) %>% nrow() ## 0


df_atorvastatin_prac_region <- df_atorvastatin_prac %>% 
  left_join(df_stats_prac) %>%
  mutate(month =as.Date(month))

multi_pract <- df_atorvastatin_prac %>% 
  left_join(df_stats_prac) %>%
  full_join(df_atorvastatin_prac_multi_sicbl) %>%
  group_by(practice,sicbl) %>%
  summarise(change_month=max(month)) %>% 
  arrange(change_month) %>%
  filter(row_number() != n())

############## accounting for changes and change back

# df_atorvastatin_prac %>% mutate(pct_id = replace_na(pct_id,"unknown")) %>% 
#   arrange(practice,desc(month)) %>% 
#   filter(pct_id!=lead(pct_id)) %>%
#   group_by(practice,pct_id) %>%
#   summarise(month=first(month),month_after=month %m+% months(1)) %>%
#   ungroup() %>%
#   count(practice) %>%
#   View()

df_atorvastatin_prac_multi_sicbl_id <- df_atorvastatin_prac_multi_sicbl %>%
  filter(n>1) %>%
  select(practice) %>%
  unlist()

for(prac in df_atorvastatin_prac_multi_sicbl_id){
      multi_ccgs<- df_atorvastatin_prac_region %>% 
           filter(practice==prac) %>%
           ggplot(aes(x = month, y = total_list_size)) +
           geom_line() +
           scale_y_continuous(labels = label_comma()) +
           scale_x_date(date_breaks = "4 months") +
           theme(axis.text.x = element_text(angle =90)) +
           geom_vline(xintercept= df_atorvastatin_prac %>%  
                        filter(practice==prac) %>% 
                        left_join(multi_pract) %>% 
                        select(change_month) %>% 
                        unique() %>%
                        unlist())
           
  
  ggsave(
    filename = here::here(
      "output",
      "isat",
      "atorvastatin",
      "multi_prac",
      glue("multi_ccgs_{prac}.png")),
    multi_ccgs,
    dpi = 600,
    width = 40,
    height = 15,
    units = "cm",
    create.dir = T
  )
}

big_changes<-tibble()
for(prac in df_atorvastatin_prac_multi_sicbl_id){
  big_change <- df_atorvastatin_prac_region %>%  
    filter(practice==prac) %>% 
    left_join(multi_pract) %>%
    filter(month <=  change_month %m+% months(2),month >=  change_month %m-% months(2)) %>% 
    summarise(practice=last(practice), diff=100-min(total_list_size ) / max(total_list_size) *100)  %>% 
    filter(abs(diff) > 10 | is.na(diff))
  
  big_changes<-big_changes %>%
    bind_rows(big_change)
}

for (prac in unlist(big_changes%>%select(practice))){
  multi_ccgs<- df_atorvastatin_prac_region %>% 
    filter(practice==prac) %>%
    ggplot(aes(x = month, y = total_list_size)) +
    geom_line() +
    scale_y_continuous(labels = label_comma()) +
    scale_x_date(date_breaks = "1 months") +
    theme(axis.text.x = element_text(angle =90)) +
    geom_vline(xintercept= df_atorvastatin_prac %>%  
                 filter(practice==prac) %>% 
                 left_join(multi_pract) %>% 
                 select(change_month) %>% 
                 unique() %>%
                 unlist(), 
               linetype="dashed")
  
  
  ggsave(
    filename = here::here(
      "output",
      "isat",
      "atorvastatin",
      "big_change",
      glue("big_change_{prac}.png")),
    multi_ccgs,
    dpi = 600,
    width = 40,
    height = 15,
    units = "cm",
    create.dir = T
  )
  
}


### TODO
df_atorvastatin_prac_sicbl <- df_atorvastatin_prac %>%
  left_join(df_ccgs_prac, by = join_by(pct_id == code)) %>%
  mutate(name = replace_na(name,"XXUnknown"))



practices<-unlist(unique(df_atorvastatin_prac_sicbl %>% arrange(name) %>% select(practice)))



exploratory_plots <-function(df,bnf_name,practice_code){
  sicbl <- unique(df %>%
                 filter(practice==practice_code) %>%
                 select(name))
  print(practice_code)
  print(sicbl[1,1])
  df_plot<-df %>%
    filter(practice==practice_code) %>%
    ggplot(aes(x = month, y = rate)) +
    geom_line() +
    scale_y_continuous(labels = label_comma()) +
    scale_x_date(date_breaks = "4 months") +
    theme(axis.text.x = element_text(angle =90)) +
    geom_vline(xintercept=get(glue("date_{bnf_name}_ng")), linetype="dashed") +
    # geom_vline(xintercept=get(glue("date_{bnf_name}_diab")), linetype="dotted") +
    ylab("Rate")
  ggsave(
    filename = here::here(
      "output",
      "isat",
      bnf_name,
      sicbl[1,1],
      glue("{bnf_name}_{practice_code}_{sicbl[1,1]}_region_plot.png")),
    df_plot,
    dpi = 600,
    width = 40,
    height = 15,
    units = "cm",
    create.dir = T
  )
  
}


for (practice_code in practices){
  exploratory_plots(df_atorvastatin_prac_sicbl,"atorvastatin",practice_code)
}


#### practices with no list size
df_atorvastatin_nolist <- df_atorvastatin_prac_region %>%
  filter(is.na(total_list_size)) %>%
  mutate(total = sum(items))  # 25,482 items prescribed where there is no list size


df_atorvastatin_prac_region %>%
  filter(!is.na(total_list_size)) %>%
  mutate(total = sum(items)) ## 338,154,101 items prescribed where there is a list size


df_atorvastatin_nolist %>% 
  select(regional_team) %>%
  unique() #  all regions have missing list sizes


df_atorvastatin_nolist_id <- df_atorvastatin_nolist %>% 
  select(practice) %>%
  unique() # 399 practices with missing list size

df_atorvastatin_prac_region %>%
  right_join(df_atorvastatin_nolist_id) %>% View() ### some missing at start, some at end, some all the way

### Nice graph of practices dropping in and out of list size

regions <- unique(df_atorvastatin_prac_region %>% arrange(regional_team) %>% select(regional_team)) %>% unlist()

missing_list_size_prac <- df_atorvastatin_prac_region %>%
  group_by(month,practice) %>%
  summarise(
    items=sum(items),
    total_list_size=mean(total_list_size, na.rm = TRUE),
    regional_team= last(regional_team)) %>%
  right_join(df_atorvastatin_nolist_id) %>% 
  group_by(month,practice,) %>%
  summarise(list_size= factor(is.na(total_list_size)*1,levels =c(0,1),labels =c("items\n&\nlist size","items\nwith\nno list size")),
            regional_team= last(regional_team))


for (regional in regions){
  assign(glue("plot_missing_list_size_prac_{regional}"), missing_list_size_prac %>%
  filter(regional_team==regional) %>%
  ggplot(aes(x=month, y=practice, fill=list_size)) +
  geom_tile() +
    ggtitle(df_regions %>% filter(regional_team==regional)%>%select(region)))
}

plot_missing_list_size_prac_NA<-missing_list_size_prac %>%
  filter(is.na(regional_team)) %>%
  ggplot(aes(x=month, y=practice, fill=list_size)) +
  geom_tile() +
  ggtitle("NA")

A<-ggplot() +theme_minimal()

plot_missing_list_size_prac <- patchwork::wrap_plots(
                      plot_missing_list_size_prac_Y56,
                      plot_missing_list_size_prac_Y58,
                      plot_missing_list_size_prac_Y59,
                      plot_missing_list_size_prac_Y60,
                      plot_missing_list_size_prac_Y61,
                      plot_missing_list_size_prac_Y62,
                      plot_missing_list_size_prac_Y63,
                      plot_missing_list_size_prac_NA,
                      ncol = 7,
                      guides = "collect",
                      heights = c(1,0.1))

ggsave(
  filename = here::here(
    "output",
    "isat",
    "atorvastatin",
    glue("list_size_missingness.png")),
  plot_missing_list_size_prac,
  dpi = 800,
  width = 60,
  height = 30,
  units = "cm"
)

### season plot list sizes
# 
df_atorvastatin_prac_region_chem <- df_atorvastatin_prac %>%
  left_join(df_stats_prac) %>%
  left_join(df_ccgs_prac,by=join_by(pct_id ==code)) %>%
  group_by(month,practice) %>%
  summarise(items=sum(items),
            total_list_size=mean(total_list_size, na.rm = TRUE),
            rate = items / total_list_size *1000,
            regional_team  = last(regional_team  ))  %>%
  group_by(month,regional_team ) %>%
  summarise(items=sum(items),
            total_list_size=sum(total_list_size, na.rm = TRUE),
            rate = items / total_list_size *1000)  %>%
  left_join(df_regions) %>%
  ungroup()%>%
  arrange(month)


df_atorvastatin_prac_region_chem_pct <- df_atorvastatin_prac %>%
  left_join(df_stats_prac) %>%
  left_join(df_ccgs_prac,by=join_by(pct_id ==code)) %>%
  group_by(month,practice) %>%
  summarise(items=sum(items),
            total_list_size=mean(total_list_size, na.rm = TRUE),
            rate = items / total_list_size *1000,
            regional_team_id  = last(regional_team_id  ))  %>%
  group_by(month,regional_team_id ) %>%
  summarise(items=sum(items),
            total_list_size=sum(total_list_size, na.rm = TRUE),
            rate = items / total_list_size *1000)  %>%
  left_join(df_regions,by=join_by(regional_team_id ==regional_team )) %>%
  ungroup()%>%
  arrange(month)
  

df_atorvastatin_prac_region_chem_sicbl <- df_atorvastatin_prac %>%
  left_join(df_stats_prac) %>%
  left_join(df_ccgs_prac,by=join_by(sicbl ==code)) %>%
  group_by(month,practice) %>%
  summarise(items=sum(items),
            total_list_size=mean(total_list_size, na.rm = TRUE),
            rate = items / total_list_size *1000,
            regional_team_id  = last(regional_team_id  ))  %>%
  group_by(month,regional_team_id) %>%
  summarise(items=sum(items),
            total_list_size=sum(total_list_size, na.rm = TRUE),
            rate = items / total_list_size *1000)  %>%
  left_join(df_regions,by=join_by(regional_team_id ==regional_team )) %>%
  ungroup() %>%
  arrange(month)
  
regions <- regions[1:7] 
  
  
for(i in regions){
  
  
  assign(glue("season_plot_total_list_size_{df_regions %>% filter(regional_team == i) %>% select(region)}"),   assign(glue("season_plot_{i}"), ggseasonplot(ts(df_atorvastatin_prac_region_chem_sicbl %>% filter(regional_team_id == i) %>% select(total_list_size), start = c(2020,01), frequency = 12),main = glue("{df_regions %>% filter(regional_team == i) %>% select(region)} list size seasonal plot"))))
  
}
  
seasons_combined_list_size <- patchwork::wrap_plots(`season_plot_total_list_size_East of England`,`season_plot_total_list_size_London`,`season_plot_total_list_size_Midlands`, `season_plot_total_list_size_North East and Yorkshire`,
                                          `season_plot_total_list_size_North West`,`season_plot_total_list_size_South East`,`season_plot_total_list_size_South West`)



ggsave(
  filename = here::here(
    "output",
    "isat",
    "atorvastatin",
    glue("list_size_seasons_combined_sicbl.png")),
  seasons_combined_list_size,
  dpi = 600,
  width = 40,
  height = 15,
  units = "cm"
)


for(i in regions){
  
  
  assign(glue("season_plot_total_list_size_{df_regions %>% filter(regional_team == i) %>% select(region)}"),   assign(glue("season_plot_{i}"), ggseasonplot(ts(df_atorvastatin_prac_region_chem_pct %>% filter(regional_team_id == i) %>% select(total_list_size), start = c(2020,01), frequency = 12),main = glue("{df_regions %>% filter(regional_team == i) %>% select(region)} list size seasonal plot"))))
  
}

seasons_combined_list_size <- patchwork::wrap_plots(`season_plot_total_list_size_East of England`,`season_plot_total_list_size_London`,`season_plot_total_list_size_Midlands`, `season_plot_total_list_size_North East and Yorkshire`,
                                                    `season_plot_total_list_size_North West`,`season_plot_total_list_size_South East`,`season_plot_total_list_size_South West`)



ggsave(
  filename = here::here(
    "output",
    "isat",
    "atorvastatin",
    glue("list_size_seasons_combined_pct.png")),
  seasons_combined_list_size,
  dpi = 600,
  width = 40,
  height = 15,
  units = "cm"
)


for(i in regions){
  
  
  assign(glue("season_plot_total_list_size_{df_regions %>% filter(regional_team == i) %>% select(region)}"),   assign(glue("season_plot_{i}"), ggseasonplot(ts(df_atorvastatin_prac_region_chem %>% filter(regional_team == i) %>% select(total_list_size), start = c(2020,01), frequency = 12),main = glue("{df_regions %>% filter(regional_team == i) %>% select(region)} list size seasonal plot"))))
  
}

seasons_combined_list_size <- patchwork::wrap_plots(`season_plot_total_list_size_East of England`,`season_plot_total_list_size_London`,`season_plot_total_list_size_Midlands`, `season_plot_total_list_size_North East and Yorkshire`,
                                                    `season_plot_total_list_size_North West`,`season_plot_total_list_size_South East`,`season_plot_total_list_size_South West`)



ggsave(
  filename = here::here(
    "output",
    "isat",
    "atorvastatin",
    glue("list_size_seasons_combined.png")),
  seasons_combined_list_size,
  dpi = 600,
  width = 40,
  height = 15,
  units = "cm"
)
#


season_sicbl<-df_stats_prac %>%
  left_join(df_ccgs_prac,by=join_by(sicbl ==code)) %>% 
  group_by(month,regional_team_id) %>%
  summarise(
            total_list_size=sum(total_list_size, na.rm = TRUE))  %>%
  left_join(df_regions,by=join_by(regional_team_id ==regional_team )) %>%
  ungroup() %>%
  arrange(month)

season_pct<-df_stats_prac %>%
  left_join(df_ccgs_prac,by=join_by(pct_id ==code)) %>% 
  group_by(month,regional_team_id) %>%
  summarise(
    total_list_size=sum(total_list_size, na.rm = TRUE))  %>%
  left_join(df_regions,by=join_by(regional_team_id ==regional_team )) %>%
  ungroup() %>%
  arrange(month)




for(i in regions){
  
  
  assign(glue("season_plot_total_list_size_{df_regions %>% filter(regional_team == i) %>% select(region)}"),   assign(glue("season_plot_{i}"), ggseasonplot(ts(season_sicbl %>% filter(regional_team_id == i) %>% select(total_list_size), start = c(2020,01), frequency = 12),main = glue("{df_regions %>% filter(regional_team == i) %>% select(region)} list size seasonal plot"))))
  
}

seasons_combined_list_size <- patchwork::wrap_plots(`season_plot_total_list_size_East of England`,`season_plot_total_list_size_London`,`season_plot_total_list_size_Midlands`, `season_plot_total_list_size_North East and Yorkshire`,
                                                    `season_plot_total_list_size_North West`,`season_plot_total_list_size_South East`,`season_plot_total_list_size_South West`)



ggsave(
  filename = here::here(
    "output",
    "isat",
    "atorvastatin",
    glue("list_size_seasons_sibl.png")),
  seasons_combined_list_size,
  dpi = 600,
  width = 40,
  height = 15,
  units = "cm"
)


for(i in regions){
  
  
  assign(glue("season_plot_total_list_size_{df_regions %>% filter(regional_team == i) %>% select(region)}"),   assign(glue("season_plot_{i}"), ggseasonplot(ts(season_pct %>% filter(regional_team_id == i) %>% select(total_list_size), start = c(2020,01), frequency = 12),main = glue("{df_regions %>% filter(regional_team == i) %>% select(region)} list size seasonal plot"))))
  
}

seasons_combined_list_size <- patchwork::wrap_plots(`season_plot_total_list_size_East of England`,`season_plot_total_list_size_London`,`season_plot_total_list_size_Midlands`, `season_plot_total_list_size_North East and Yorkshire`,
                                                    `season_plot_total_list_size_North West`,`season_plot_total_list_size_South East`,`season_plot_total_list_size_South West`)



ggsave(
  filename = here::here(
    "output",
    "isat",
    "atorvastatin",
    glue("list_size_seasons_pct.png")),
  seasons_combined_list_size,
  dpi = 600,
  width = 40,
  height = 15,
  units = "cm"
)
