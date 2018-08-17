
rm(list = ls())  #clear environment
library(tidyverse)
library(lubridate)
library(magrittr)
library(viridis)
library(ggridges)


water_year <- function(date) {
  ifelse(month(date) < 10, year(date), year(date)+1)}
setwd("C:/Projects/HAR/r")
daily  <- read_csv("SOtimeseries.csv") %>% mutate(date = mdy(date), yday = yday(date), wy = water_year(date), year = year(date),
                                                       value = as.numeric(value))
cumdoy <- daily %>% group_by(year) %>%  summarize(cumdoy= max(yday, na.rm=T))  

daily <- inner_join(cumdoy, daily)

daily %<>% mutate(dowy =   ifelse(cumdoy > 365, 
                                      ifelse(yday>274, yday-274, yday+92), 
                                      ifelse(yday>273, yday-273, yday+92)) ) 

cum_annual <- daily %>% group_by(wy) %>%  summarize(cum_ann_taf= sum(value/1000, na.rm=T))
daily <- left_join(cum_annual, daily, by.x = "wy", by.y = "wy")
daily %<>% mutate(wy = as.numeric(wy), dowy = as.numeric(dowy), cum_annual = as.numeric(cum_ann_taf))

ggplot(daily, aes(dowy, wy, height = value , group = wy, fill=cum_ann_taf))  + 
  scale_fill_viridis(name = "taf/water yr") +
  geom_ridgeline(col = "dark gray", stat="identity", scale=0.00015, size=0.0000015)+ theme_gray() + #0.3 size orig
  theme(axis.text.y = element_text(color = 'gray')) +  labs(x = NULL, y=NULL) +
  theme(axis.ticks = element_line(color = "gray")) +  

  scale_x_continuous(expand = c(0.02,0.02),
                   breaks = c(1,32,62,93,124,152,183,213,244,274,305,336) 
                    ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S"),
                    sec.axis = dup_axis()) + 
  
  scale_y_continuous(expand = c(0,0),
                     breaks = c(1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000),
                     labels = c("1930", "1940", "1950", "1960", "1970", "1980", "1990", "2000"), sec.axis = dup_axis())+
  
  geom_vline(aes(xintercept=123), color="red", linetype="dotted") +  #Feb
  geom_vline(aes(xintercept=182), color="red", linetype="dotted") +  #Apr
  geom_vline(aes(xintercept=243), color="red", linetype="dotted") +  #Jun
  annotate("text", x = 182, y = as.factor(1952), label="April 1", color="gray60", size=6)+
  theme(plot.title = element_text(hjust = 0.5))  + theme(plot.title = element_text(vjust=2)) +
  theme(axis.text.x = element_text(color = 'gray'), axis.ticks = element_line(color = "gray")) +
  theme(plot.margin = unit(c(0.05, 0.02, 0.05, 0.02), "cm")) +theme(axis.text.y = element_text(color = 'gray')) + 
  theme(axis.ticks = element_line(color = "gray")) +  theme(axis.text.x = element_text(color = 'gray'), axis.ticks = element_line(color = "gray")) +
  theme(legend.key.width = unit(1, "in"), legend.key.height = unit(2, "in"))
ggsave("test.pdf", width = 17, height = 11, units = "in") + ggtitle("test.pdf", size = 20, color = "gray")


                                          



