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

tinytheme("ipsum")
set.seed(48103)

source("analysis/design.R")

df_atorvastatin <- read_csv(here::here(directory_name,"atorvastatin_list.csv")) %>%
  filter(str_detect(bnf_name, '20')) %>%
  group_by(month,regional_team) %>%
  summarise(items=sum(items),
            total_list_size=mean(total_list_size),
            rate = items / total_list_size *1000)  %>%
  ungroup() %>%
  mutate(guidance = case_when(month < date_atorvastatin_ng~0,
                              month >= date_atorvastatin_ng ~1),
         time = interval(min(month),month) %/% months(1),
         year = year(month),
         month_number = month(month))

split_above <- interval(min(df_atorvastatin$month),date_atorvastatin_ng) %/% months(1)
split_below <-  split_above - 1

df_regions <- read_csv(here::here("data","NHS_England_Names_and_Codes_in_England.csv")) %>%
  rename(regional_team = NHSER24CDH ,region = NHSER24NM) %>%
  select(regional_team,region)

df_atorvastatin <- df_atorvastatin %>%
  left_join(df_regions)

regions <-unique(df_regions$region)
df_slope <- tibble()
df_comparison <-tibble()


for(i in regions){
  
  
  assign(glue("season_plot_{i}"), ggseasonplot(ts(df_atorvastatin %>% filter(region == i) %>% select(rate), start = c(2020,01), frequency = 12),main = glue("{i} seasonal plot")))
  
  
  assign(glue("decomp_plot_{i}"), ts(df_atorvastatin %>% filter(region == i) %>% select(rate), 
     start = c(df_atorvastatin %>% filter(region == i)  %>% filter(month == min(month)) %>% select(year),
               df_atorvastatin %>% filter(region == i)  %>% filter(month == min(month)) %>% select(month_number)), 
     frequency = 12) %>% decompose(type="additive") %>%
    autoplot() + xlab("Year") + ggtitle(glue("Decompostition {i}")))
  
  
  tinyplot(rate ~ month | guidance, type = "p", palette = "okabeito",
           data = transform(df_atorvastatin %>% filter(region == i), guidance = factor(guidance)))
  
  mod <- lm(rate ~ time * guidance, data = df_atorvastatin %>% filter(region == i))
  summary(mod)
  
  # what would be the predicted outcome at time X with or without intervention?
  p0 <- predictions(mod, newdata = datagrid(time = max(df_atorvastatin$time-1), guidance = 0))
  p1 <- predictions(mod, newdata = datagrid(time = max(df_atorvastatin$time), guidance = 1))
  
  p0
  p1
  
  # is the difference between those predictions statistically significant?
  df_comparison_i<-tibble(comparisons(mod, variables = "guidance", newdata = datagrid(time = split_above)))  %>% bind_cols(region = i)
  df_comparison <- df_comparison %>%
    bind_rows(df_comparison_i)
  # What is the average slope of the outcome equation with respect to time, before and after the intervention?
  df_slope_i<-tibble(avg_slopes(mod, variables = "time", by = "guidance"))  %>% bind_cols(region = i)
  df_slope <- df_slope %>%
    bind_rows(df_slope_i)
  
  # Many analysts like to visualize the results of an ITS analysis, by plotting the predicted outcome for the control group, knowing that these predictions are counterfactual after the intervention. This can be done by first make predictions for all combinations of time and treatment, dropping the observations we do not want to plot, and feeding the data to a plotting package like ggplot2.
  intervention <- split_below
  
  p <- predictions(mod, variables = c("time", "guidance"))
  p <- subset(p, time > split_below | guidance == 0)
  assign(glue("naive_itsa_plot_{i}"), ggplot(p, aes(x = time, y = estimate, color = factor(guidance))) +
    geom_line() +
    labs(title = glue("Predicted outcome over time {i}"),
         x = "Time",
         y = "Outcome") +
    geom_point(data=df_atorvastatin %>% filter(region==i),aes(x = time, y = rate)))
 
 # ggsave(
 #   filename = here::here(
 #     "output",
 #     "isat",
 #     "atorvastatin",
 #     glue("atorvastatin_{i}_itsa_plot.png")),
 #   df_plot,
 #   dpi = 600,
 #   width = 40,
 #   height = 15,
 #   units = "cm"
 # )
}


decomp_combined <- patchwork::wrap_plots(`decomp_plot_East of England`,`decomp_plot_London`,`decomp_plot_Midlands`, `decomp_plot_North East and Yorkshire`,
                                          `decomp_plot_North West`,`decomp_plot_South East`,`decomp_plot_South West`)

ggsave(
  filename = here::here(
    "output",
    "isat",
    "atorvastatin",
    glue("atorvastatin_decomp_combined.png")),
  decomp_combined,
  dpi = 600,
  width = 60,
  height = 45,
  units = "cm"
)

seasons_combined <- patchwork::wrap_plots(`season_plot_East of England`,`season_plot_London`,`season_plot_Midlands`, `season_plot_North East and Yorkshire`,
                                          `season_plot_North West`,`season_plot_South East`,`season_plot_South West`)
                                        
ggsave(
  filename = here::here(
    "output",
    "isat",
    "atorvastatin",
    glue("atorvastatin_seasons_combined.png")),
  seasons_combined,
  dpi = 600,
  width = 40,
  height = 15,
  units = "cm"
)

itsa_combined <- patchwork::wrap_plots(`naive_itsa_plot_East of England`,`naive_itsa_plot_London`,`naive_itsa_plot_Midlands`, `naive_itsa_plot_North East and Yorkshire`,
                                       `naive_itsa_plot_North West`,`naive_itsa_plot_South East`,`naive_itsa_plot_South West`)


ggsave(
  filename = here::here(
    "output",
    "isat",
    "atorvastatin",
    glue("atorvastatin_naive_itsa_combined.png")),
  itsa_combined,
  dpi = 600,
  width = 50,
  height = 20,
  units = "cm"
)

write_csv(df_comparison,
  here::here(
  "output",
  "isat",
  "atorvastatin",
  glue("atorvastatin_itsa.csv"))
  )


#### tirzepatide


df_tirzepatide <- read_csv(here::here(directory_name,"tirzepatide_list.csv")) %>%
  group_by(month,regional_team) %>%
  summarise(items=sum(items),
            total_list_size=mean(total_list_size),
            rate = items / total_list_size) %>%
  ungroup() 

split_above <- interval(min(df_tirzepatide$month),date_tirzepatide_ng) %/% months(1)
split_below <-  split_above - 1

df_tirzepatide <- df_tirzepatide %>%
  left_join(df_regions) 


df_tirzepatide_missing_month <- df_tirzepatide %>%
  group_by(region) %>%
  transmute(region, month = map2(min(month), max(month), seq, by = "1 month")) %>%
  unnest(cols= month) %>%
  distinct() %>%
  mutate(rate = 0,
         month = as.Date(month)) %>%
  anti_join(df_tirzepatide,by = join_by(region,month))

df_tirzepatide <- df_tirzepatide %>%
  full_join(df_tirzepatide_missing_month) %>%
  mutate(guidance = case_when(month < date_tirzepatide_ng~0,
                              month >= date_tirzepatide_ng ~1),
         year = year(month),
         month_number = month(month)) %>%
  group_by(region) %>%
  mutate(time = interval(min(month),month) %/% months(1)) %>%
  ungroup() %>%
  arrange(region,month)
   




for(i in regions){
  
  
  assign(glue("season_plot_{i}"), ggseasonplot(ts(df_tirzepatide %>% filter(region == i) %>% select(rate), 
                                                  start = c(df_tirzepatide %>% filter(region == i)  %>% filter(month == min(month)) %>% select(year),
                                                            df_tirzepatide %>% filter(region == i)  %>% filter(month == min(month)) %>% select(month_number)), 
                                                  frequency = 12),
                                               main = glue("{i} seasonal plot")))
  
 
  ts(df_tirzepatide %>% filter(region == i) %>% select(rate), 
     start = c(df_tirzepatide %>% filter(region == i)  %>% filter(month == min(month)) %>% select(year),
               df_tirzepatide %>% filter(region == i)  %>% filter(month == min(month)) %>% select(month_number)), 
     frequency = 12) %>% decompose(type="additive") %>%
    autoplot() + xlab("Year")
  
  
  
   tinyplot(rate ~ month | guidance, type = "p", palette = "okabeito",
           data = transform(df_tirzepatide %>% filter(region == i), guidance = factor(guidance)))
  
  mod <- lm(rate ~ time * guidance, data = df_tirzepatide %>% filter(region == i))
  summary(mod)
  
  # what would be the predicted outcome at time X with or without intervention?
  p0 <- predictions(mod, newdata = datagrid(time = max(df_tirzepatide$time-1), guidance = 0))
  p1 <- predictions(mod, newdata = datagrid(time = max(df_tirzepatide$time), guidance = 1))
  
  p0
  p1
  
  # is the difference between those predictions statistically significant?
  df_comparison_i<-tibble(comparisons(mod, variables = "guidance", newdata = datagrid(time = split_above)))  %>% bind_cols(region = i)
  df_comparison <- df_comparison %>%
    bind_rows(df_comparison_i)
  # What is the average slope of the outcome equation with respect to time, before and after the intervention?
  df_slope_i<-tibble(avg_slopes(mod, variables = "time", by = "guidance"))  %>% bind_cols(region = i)
  df_slope <- df_slope %>%
    bind_rows(df_slope_i)
  
  # Many analysts like to visualize the results of an ITS analysis, by plotting the predicted outcome for the control group, knowing that these predictions are counterfactual after the intervention. This can be done by first make predictions for all combinations of time and treatment, dropping the observations we do not want to plot, and feeding the data to a plotting package like ggplot2.
  intervention <- split_below
  
  p <- predictions(mod, variables = c("time", "guidance"))
  p <- subset(p, time > split_below | guidance == 0)
 
  
   assign(glue("naive_itsa_plot_{i}"), ggplot(p, aes(x = time, y = estimate, color = factor(guidance))) +
           geom_line() +
           labs(title = glue("Predicted outcome over time {i}"),
                x = "Time",
                y = "Outcome") +
           geom_point(data=df_tirzepatide %>% filter(region==i),aes(x = time, y = rate)))
  
  # ggsave(
  #   filename = here::here(
  #     "output",
  #     "isat",
  #     "tirzepatide",
  #     glue("tirzepatide_{i}_itsa_plot.png")),
  #   df_plot,
  #   dpi = 600,
  #   width = 40,
  #   height = 15,
  #   units = "cm"
  # )
}

seasons_combined <- patchwork::wrap_plots(`season_plot_East of England`,`season_plot_London`,`season_plot_Midlands`, `season_plot_North East and Yorkshire`,
                                          `season_plot_North West`,`season_plot_South East`,`season_plot_South West`)

ggsave(
  filename = here::here(
    "output",
    "isat",
    "tirzepatide",
    glue("tirzepatide_seasons_combined.png")),
  seasons_combined,
  dpi = 600,
  width = 40,
  height = 15,
  units = "cm"
)

itsa_combined <- patchwork::wrap_plots(`naive_itsa_plot_East of England`,`naive_itsa_plot_London`,`naive_itsa_plot_Midlands`, `naive_itsa_plot_North East and Yorkshire`,
                                       `naive_itsa_plot_North West`,`naive_itsa_plot_South East`,`naive_itsa_plot_South West`)

ggsave(
  filename = here::here(
    "output",
    "isat",
    "tirzepatide",
    glue("tirzepatide_naive_itsa_combined.png")),
  itsa_combined,
  dpi = 600,
  width = 50,
  height = 20,
  units = "cm"
)



#### inclisiran

#### tirzepatide


df_tirzepatide <- read_csv(here::here(directory_name,"tirzepatide_list.csv")) %>%
  group_by(month,regional_team) %>%
  summarise(items=sum(items),
            total_list_size=mean(total_list_size),
            rate = items / total_list_size)  %>%
  ungroup() 

split_above <- interval(min(df_tirzepatide$month),date_tirzepatide_ng) %/% months(1)
split_below <-  split_above - 1

df_tirzepatide <- df_tirzepatide %>%
  left_join(df_regions) %>%
  group_by(region) %>%
  mutate(guidance = case_when(month < date_tirzepatide_ng~0,
                              month >= date_tirzepatide_ng ~1),
         time = interval(min(month),month) %/% months(1),
         year = year(month),
         month_number = month(month)) %>%




for(i in regions){
  
  
  assign(glue("season_plot_{i}"), ggseasonplot(ts(df_tirzepatide %>% filter(region == i) %>% select(rate), 
                                                  start = c(df_tirzepatide %>% filter(region == i)  %>% filter(month == min(month)) %>% select(year),
                                                            df_tirzepatide %>% filter(region == i)  %>% filter(month == min(month)) %>% select(month_number)), 
                                                  frequency = 12),
                                               main = glue("{i} seasonal plot")))
  
  
  
  tinyplot(rate ~ month | guidance, type = "p", palette = "okabeito",
           data = transform(df_tirzepatide %>% filter(region == i), guidance = factor(guidance)))
  
  mod <- lm(rate ~ time * guidance, data = df_tirzepatide %>% filter(region == i))
  summary(mod)
  
  # what would be the predicted outcome at time X with or without intervention?
  p0 <- predictions(mod, newdata = datagrid(time = max(df_tirzepatide$time-1), guidance = 0))
  p1 <- predictions(mod, newdata = datagrid(time = max(df_tirzepatide$time), guidance = 1))
  
  p0
  p1
  
  # is the difference between those predictions statistically significant?
  df_comparison_i<-tibble(comparisons(mod, variables = "guidance", newdata = datagrid(time = split_above)))  %>% bind_cols(region = i)
  df_comparison <- df_comparison %>%
    bind_rows(df_comparison_i)
  # What is the average slope of the outcome equation with respect to time, before and after the intervention?
  df_slope_i<-tibble(avg_slopes(mod, variables = "time", by = "guidance"))  %>% bind_cols(region = i)
  df_slope <- df_slope %>%
    bind_rows(df_slope_i)
  
  # Many analysts like to visualize the results of an ITS analysis, by plotting the predicted outcome for the control group, knowing that these predictions are counterfactual after the intervention. This can be done by first make predictions for all combinations of time and treatment, dropping the observations we do not want to plot, and feeding the data to a plotting package like ggplot2.
  intervention <- split_below
  
  p <- predictions(mod, variables = c("time", "guidance"))
  p <- subset(p, time > split_below | guidance == 0)
  
  
  assign(glue("naive_itsa_plot_{i}"), ggplot(p, aes(x = time, y = estimate, color = factor(guidance))) +
           geom_line() +
           labs(title = glue("Predicted outcome over time {i}"),
                x = "Time",
                y = "Outcome") +
           geom_point(data=df_tirzepatide %>% filter(region==i),aes(x = time, y = rate)))
  
  # ggsave(
  #   filename = here::here(
  #     "output",
  #     "isat",
  #     "tirzepatide",
  #     glue("tirzepatide_{i}_itsa_plot.png")),
  #   df_plot,
  #   dpi = 600,
  #   width = 40,
  #   height = 15,
  #   units = "cm"
  # )
}

seasons_combined <- patchwork::wrap_plots(`season_plot_East of England`,`season_plot_London`,`season_plot_Midlands`, `season_plot_North East and Yorkshire`,
                                          `season_plot_North West`,`season_plot_South East`,`season_plot_South West`)

ggsave(
  filename = here::here(
    "output",
    "isat",
    "tirzepatide",
    glue("tirzepatide_seasons_combined.png")),
  seasons_combined,
  dpi = 600,
  width = 40,
  height = 15,
  units = "cm"
)

itsa_combined <- patchwork::wrap_plots(`naive_itsa_plot_East of England`,`naive_itsa_plot_London`,`naive_itsa_plot_Midlands`, `naive_itsa_plot_North East and Yorkshire`,
                                       `naive_itsa_plot_North West`,`naive_itsa_plot_South East`,`naive_itsa_plot_South West`)

ggsave(
  filename = here::here(
    "output",
    "isat",
    "tirzepatide",
    glue("tirzepatide_naive_itsa_combined.png")),
  itsa_combined,
  dpi = 600,
  width = 50,
  height = 20,
  units = "cm"
)


###### inclisiran

df_inclisiran <- read_csv(here::here(directory_name,"inclisiran_list.csv")) %>%
  group_by(month,regional_team) %>%
  summarise(items=sum(items),
            total_list_size=mean(total_list_size),
            rate = items / total_list_size)  %>%
  ungroup() %>%
  mutate(guidance = case_when(month < date_inclisiran_ng~0,
                              month >= date_inclisiran_ng ~1),
         time = interval(min(month),month) %/% months(1),
         year = year(month),
         month_number = month(month)) %>%


split_above <- interval(min(df_inclisiran$month),date_inclisiran_ng) %/% months(1)
split_below <-  split_above - 1

df_inclisiran <- df_inclisiran %>%
  left_join(df_regions) %>%
  group_by(region) %>%
  complete(month)



for(i in regions){
  
  
  assign(glue("season_plot_{i}"), ggseasonplot(ts(df_inclisiran %>% filter(region == i) %>% select(rate), 
                                                  start = c(df_inclisiran %>% filter(region == i)  %>% filter(month == min(month)) %>% select(year),
                                                            df_inclisiran %>% filter(region == i)  %>% filter(month == min(month)) %>% select(month_number)), 
                                                  frequency = 12),
                                               main = glue("{i} seasonal plot")))
  
  
  
  tinyplot(rate ~ month | guidance, type = "p", palette = "okabeito",
           data = transform(df_inclisiran %>% filter(region == i), guidance = factor(guidance)))
  
  mod <- lm(rate ~ time * guidance, data = df_inclisiran %>% filter(region == i))
  summary(mod)
  
  # what would be the predicted outcome at time X with or without intervention?
  p0 <- predictions(mod, newdata = datagrid(time = max(df_inclisiran$time-1), guidance = 0))
  p1 <- predictions(mod, newdata = datagrid(time = max(df_inclisiran$time), guidance = 1))
  
  p0
  p1
  
  # is the difference between those predictions statistically significant?
  df_comparison_i<-tibble(comparisons(mod, variables = "guidance", newdata = datagrid(time = split_above)))  %>% bind_cols(region = i)
  df_comparison <- df_comparison %>%
    bind_rows(df_comparison_i)
  # What is the average slope of the outcome equation with respect to time, before and after the intervention?
  df_slope_i<-tibble(avg_slopes(mod, variables = "time", by = "guidance"))  %>% bind_cols(region = i)
  df_slope <- df_slope %>%
    bind_rows(df_slope_i)
  
  # Many analysts like to visualize the results of an ITS analysis, by plotting the predicted outcome for the control group, knowing that these predictions are counterfactual after the intervention. This can be done by first make predictions for all combinations of time and treatment, dropping the observations we do not want to plot, and feeding the data to a plotting package like ggplot2.
  intervention <- split_below
  
  p <- predictions(mod, variables = c("time", "guidance"))
  p <- subset(p, time > split_below | guidance == 0)
  
  
  assign(glue("naive_itsa_plot_{i}"), ggplot(p, aes(x = time, y = estimate, color = factor(guidance))) +
           geom_line() +
           labs(title = glue("Predicted outcome over time {i}"),
                x = "Time",
                y = "Outcome") +
           geom_point(data=df_inclisiran %>% filter(region==i),aes(x = time, y = rate)))
  
  # ggsave(
  #   filename = here::here(
  #     "output",
  #     "isat",
  #     "inclisiran",
  #     glue("inclisiran_{i}_itsa_plot.png")),
  #   df_plot,
  #   dpi = 600,
  #   width = 40,
  #   height = 15,
  #   units = "cm"
  # )
}

seasons_combined <- patchwork::wrap_plots(`season_plot_East of England`,`season_plot_London`,`season_plot_Midlands`, `season_plot_North East and Yorkshire`,
                                          `season_plot_North West`,`season_plot_South East`,`season_plot_South West`)

ggsave(
  filename = here::here(
    "output",
    "isat",
    "inclisiran",
    glue("inclisiran_seasons_combined.png")),
  seasons_combined,
  dpi = 600,
  width = 40,
  height = 15,
  units = "cm"
)

itsa_combined <- patchwork::wrap_plots(`naive_itsa_plot_East of England`,`naive_itsa_plot_London`,`naive_itsa_plot_Midlands`, `naive_itsa_plot_North East and Yorkshire`,
                                       `naive_itsa_plot_North West`,`naive_itsa_plot_South East`,`naive_itsa_plot_South West`)

ggsave(
  filename = here::here(
    "output",
    "isat",
    "inclisiran",
    glue("inclisiran_naive_itsa_combined.png")),
  itsa_combined,
  dpi = 600,
  width = 50,
  height = 20,
  units = "cm"
)
