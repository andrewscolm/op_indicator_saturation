library(readr)
library(here)
library(glue)
library(dplyr)
library(tidyr)
library(gets)
library(caTools)
library(stringr)
library(ggplot2)
library(scales)
library(segmented)
library(mcp)
library(lubridate)


source("analysis/design.R")
# source("analysis/isat/change_detection.R")

start_date_atorvastatin<-as.Date("2023/02/01")
end_date_atorvastatin <- as.Date("2025/06/01")

df_atorvastatin <- read_csv(here::here(directory_name,"atorvastatin_list.csv")) %>%
  filter(str_detect(bnf_name, '20')) %>%
  group_by(month,regional_team) %>%
  summarise(items=sum(items),
            total_list_size=mean(total_list_size),
            rate = items / total_list_size * 1000)  %>%
  ungroup()


# df_atorvastatin_30 <- min(df_atorvastatin$month) + months(30)

df_inclisiran <- read_csv(here::here(directory_name,"inclisiran_list.csv")) %>%
  group_by(month,regional_team) %>%
  summarise(items=sum(items),
            total_list_size=mean(total_list_size * 1000),
            rate = items / total_list_size) %>%
  ungroup()

# df_inclisiran_30 <- min(df_inclisiran$month) + months(30)

df_tirzepatide <- read_csv(here::here(directory_name,"tirzepatide_list.csv")) %>%
  group_by(month,regional_team) %>%
  summarise(items=sum(items),
            total_list_size=mean(total_list_size),
            rate = items / total_list_size * 1000) %>%
  ungroup() 


df<-df_inclisiran %>%
  mutate(time = as.numeric(interval(min(month),month) %/% months(1)+1)) %>%
  filter(regional_team=="Y62") %>%
  reframe(time,rate,regional_team)

fit_lm_inclisiran = lm(rate ~ 1 + time, data = df)
fit_segmented_2 = segmented(fit_lm_inclisiran, seg.Z = ~time, npsi = 2)
fit_segmented_4 = segmented(fit_lm_inclisiran, seg.Z = ~time, npsi = 4)
fit_segmented = segmented(fit_lm_inclisiran, seg.Z = ~time, fix.npsi = F)
fit_segmented_3 = segmented(fit_lm_inclisiran, seg.Z = ~time, npsi = 3)


summary(fit_segmented_2)
summary(fit_segmented_4)
summary(fit_segmented_3)
summary(fit_segmented)

filename <-paste("output/segmented","/","inclisiran", "/","North_West_2", ".png", sep="")



png(filename,width= 30,height=15,units="cm",res = 600)

plot(fit_segmented_2)
lines(df)
lines.segmented(fit_segmented_2)
points.segmented(fit_segmented_2)

dev.off()


filename <-paste("output/segmented","/","inclisiran", "/","North_West_4", ".png", sep="")



png(filename,width= 30,height=15,units="cm",res = 600)

plot(fit_segmented_4)
lines(df)
lines.segmented(fit_segmented_4)
points.segmented(fit_segmented_4)

dev.off()

filename <-paste("output/segmented","/","inclisiran", "/","North_West", ".png", sep="")



png(filename,width= 30,height=15,units="cm",res = 600)

plot(fit_segmented)
lines(df)
lines.segmented(fit_segmented)
points.segmented(fit_segmented)

dev.off()

filename <-paste("output/segmented","/","inclisiran", "/","North_West_3", ".png", sep="")



png(filename,width= 30,height=15,units="cm",res = 600)

plot(fit_segmented_3)
lines(df)
lines.segmented(fit_segmented_3)
points.segmented(fit_segmented_3)

dev.off()

BF = exp((BIC(fit_segmented_4) - BIC(fit_segmented_2))/2)  # From Wagenmakers (2007)
BF

df_mcp <- df %>%
  summarise(time,
  response = rate) %>%
  arrange(time)

###### mcp

model = list(
  response ~ 0 + time + ar(1),
  ~ 0 + time + ar(1),
 # ~ 0 + time,#
  ~ 0 + time + ar(1)
)


ex = mcp_example("demo")
fit = mcp(model,par_x = "time", data = df_mcp)
#pp_check(fit)
plot_a<-plot(fit, geom_data="line",q_fit = 0.5,cp_dens=F) +
  theme_light()
fixef(fit)
ggsave(
  filename = here::here(
    "output",
    "mcp",
    "inclisiran",
    glue("inclisiran_north_west_3.png")),
  plot_a,
  dpi = 600,
  width= 30,
  height=15,
  units = "cm"
)