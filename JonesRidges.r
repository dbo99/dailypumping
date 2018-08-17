
rm(list = ls())  #clear environment
setwd("C:/Projects/HAR/r")
library(tidyverse)
library(lubridate)
library(magrittr)
library(viridis)
library(ggridges)
library(tidyr)
source("fun_defs.r")
source("plotthemes.r")

water_year <- function(date) {
  ifelse(month(date) < 10, year(date), year(date)+1)}

## Jones ## 

dailyjones  <- read_csv("joneswithstate_daily.csv") %>% transmute(date = mdy(date), jonestotal_af = jonestotal_af, jonesstate_af = jonesstate_af,
                                                          jonesfed_af = jonestotal_af-jonesstate_af) 

dailyjones <- dailyjones %>% gather("exporter", "af", 2:4) %>% mutate(yday = yday(date), wy = water_year(date), year = year(date))


cumdoy <- dailyjones %>% group_by(year) %>%  summarize(cumdoy= max(yday, na.rm=T))  

dailyjones <- inner_join(cumdoy, dailyjones)# by.x = "wy", by.y = "wy")#, all.x=TRUE)

dailyjones %<>% mutate(dowy =   ifelse(cumdoy > 365, 
                                      ifelse(yday>274, yday-274, yday+92), 
                                      ifelse(yday>273, yday-273, yday+92)) ) 

cum_annual_j <- dailyjones %>% group_by(wy, exporter) %>%  summarize(cum_ann_taf= sum(af/1000, na.rm=T)) %>% ungroup()
dailyjones <- left_join(cum_annual_j, dailyjones, by.x = "wy", by.y = "wy")

jonesdaily_both <- daily %>% filter(exporter == "jonestotal_af")
jonesdaily_state <- daily %>% filter(exporter == "jonesstate_af")
jonesdaily_fed <- daily %>% filter(exporter == "jonesfed_af")

## Banks ##

dailybanks  <- read_csv("bankswithfed_daily.csv") %>% mutate(date = mdy(date),  bankstotal_af = bankstotal_af, banksfed_af = banksfed_af, 
                                                        banksstate_af = bankstotal_af - banksfed_af)


dailybanks <- dailybanks %>% gather("exporter", "af", 2:4) %>% mutate(yday = yday(date), wy = water_year(date), year = year(date))


cumdoy <- dailybanks %>% group_by(year) %>%  summarize(cumdoy= max(yday, na.rm=T))  

dailybanks <- inner_join(cumdoy, dailybanks)# by.x = "wy", by.y = "wy")#, all.x=TRUE)

dailybanks %<>% mutate(dowy =   ifelse(cumdoy > 365, 
                                  ifelse(yday>274, yday-274, yday+92), 
                                  ifelse(yday>273, yday-273, yday+92)) ) 

cum_annual_b <- dailybanks %>% group_by(wy, exporter) %>%  summarize(cum_ann_taf= sum(af/1000, na.rm=T)) %>% ungroup()
dailybanks <- left_join(cum_annual_b, dailybanks, by.x = "wy", by.y = "wy")

banksdaily_both <- daily %>% filter(exporter == "bankstotal_af")
banksdaily_state <- daily %>% filter(exporter == "banksstate_af")
banksdaily_fed <- daily %>% filter(exporter == "banksfed_af")



### JONES ### 
## total pumping

ggplot(daily_both, aes(dowy, wy, height = af , group = wy, fill=cum_ann_taf))  + 
  scale_fill_viridis(limits = range(daily$cum_ann_taf), name = "TAF/WY") +
  geom_ridgeline(col = "dark gray", stat="identity", scale=0.00015, size=0.0000015)+ theme_black() + 
  theme(axis.text.y = element_text(color = 'gray')) +  labs(x = NULL, y=NULL) +
  theme(axis.ticks = element_line(color = "gray")) +  

  scale_x_continuous(expand = c(0.02,0.02),
                   breaks = c(1,32,62,93,124,152,183,213,244,274,305,336) 
                    ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S"),
                    sec.axis = dup_axis()) +
  
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(from = 1952, to = 2018, by = 1),
                     labels = seq(from = 1952, to = 2018, by = 1),
                     sec.axis = dup_axis())+
  
  
  
ggtitle("Jones Pumping, total, 6/11/51 - 5/20/18") + 
  geom_vline(aes(xintercept=124), color="red", linetype="dotted") +  #Feb
  geom_vline(aes(xintercept=183), color="red", linetype="dotted") +  #Apr
  geom_vline(aes(xintercept=244), color="red", linetype="dotted") +  #Jun
  annotate("text", x = 183, y = 1952, label="April 1", color="gray60", size=6)+
  theme(plot.title = element_text( size = 20, color = 'gray'))  + theme(plot.title = element_text(vjust=2)) +
  theme(axis.text.x = element_text(color = 'gray'), axis.ticks = element_line(color = "gray")) +
  theme(plot.margin = unit(c(0.15, 0.02, 0.05, 0.02), "cm")) +theme(axis.text.y = element_text(color = 'gray')) + 
  theme(axis.ticks = element_line(color = "gray")) +  theme(axis.text.x = element_text(color = 'gray'), axis.ticks = element_line(color = "gray")) +
  theme(legend.key.width = unit(1, "in"), legend.key.height = unit(2, "in")) 
ggsave("JonesTotalPumping'51_'18.jpg", width = 17, height = 11, units = "in") 
ggsave("JonesTotalPumping'51_'18.pdf", width = 17, height = 11, units = "in") 


## federal pumping

ggplot(daily_fed, aes(dowy, wy, height = af , group = wy, fill=cum_ann_taf))  + 
  scale_fill_viridis(limits = range(daily$cum_ann_taf), name = "TAF/WY") +
  geom_ridgeline(col = "dark gray", stat="identity", scale=0.00015, size=0.0000015)+ theme_black() + 
  theme(axis.text.y = element_text(color = 'gray')) +  labs(x = NULL, y=NULL) +
  theme(axis.ticks = element_line(color = "gray")) +  
  
  scale_x_continuous(expand = c(0.02,0.02),
                     breaks = c(1,32,62,93,124,152,183,213,244,274,305,336) 
                     ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S"),
                     sec.axis = dup_axis()) +
  
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(from = 1952, to = 2018, by = 1),
                     labels = seq(from = 1952, to = 2018, by = 1),
                     sec.axis = dup_axis())+
  
  
  
  ggtitle("Jones Pumping, federal, 6/11/51 - 5/20/18") + 
  geom_vline(aes(xintercept=124), color="red", linetype="dotted") +  #Feb
  geom_vline(aes(xintercept=183), color="red", linetype="dotted") +  #Apr
  geom_vline(aes(xintercept=244), color="red", linetype="dotted") +  #Jun
  annotate("text", x = 183, y = 1952, label="April 1", color="gray60", size=6)+
  theme(plot.title = element_text( size = 20, color = 'gray'))  + theme(plot.title = element_text(vjust=2)) +
  theme(axis.text.x = element_text(color = 'gray'), axis.ticks = element_line(color = "gray")) +
  theme(plot.margin = unit(c(0.15, 0.02, 0.05, 0.02), "cm")) +theme(axis.text.y = element_text(color = 'gray')) + 
  theme(axis.ticks = element_line(color = "gray")) +  theme(axis.text.x = element_text(color = 'gray'), axis.ticks = element_line(color = "gray")) +
  theme(legend.key.width = unit(1, "in"), legend.key.height = unit(2, "in")) 
ggsave("JonesFederalPumping'51_'18.jpg", width = 17, height = 11, units = "in") 
ggsave("JonesFederalPumping'51_'18.pdf", width = 17, height = 11, units = "in")                                           

## jones state pumping

ggplot(daily_state, aes(dowy, wy, height = af , group = wy, fill=cum_ann_taf))  + 
  scale_fill_viridis(limits = range(daily$cum_ann_taf), name = "TAF/WY") +
  geom_ridgeline(col = "dark gray", stat="identity", scale=0.00015, size=0.0000015)+ theme_black() + 
  theme(axis.text.y = element_text(color = 'gray')) +  labs(x = NULL, y=NULL) +
  theme(axis.ticks = element_line(color = "gray")) +  
  
  scale_x_continuous(expand = c(0.02,0.02),
                     breaks = c(1,32,62,93,124,152,183,213,244,274,305,336) 
                     ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S"),
                     sec.axis = dup_axis()) +
  
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(from = 1952, to = 2018, by = 1),
                     labels = seq(from = 1952, to = 2018, by = 1),
                     sec.axis = dup_axis())+
  
  
  
  ggtitle("Jones Pumping, state, 6/11/51 - 5/20/18") + 
  geom_vline(aes(xintercept=124), color="red", linetype="dotted") +  #Feb
  geom_vline(aes(xintercept=183), color="red", linetype="dotted") +  #Apr
  geom_vline(aes(xintercept=244), color="red", linetype="dotted") +  #Jun
  annotate("text", x = 183, y = 1952, label="April 1", color="gray60", size=6)+
  theme(plot.title = element_text( size = 20, color = 'gray'))  + theme(plot.title = element_text(vjust=2)) +
  theme(axis.text.x = element_text(color = 'gray'), axis.ticks = element_line(color = "gray")) +
  theme(plot.margin = unit(c(0.15, 0.02, 0.05, 0.02), "cm")) +theme(axis.text.y = element_text(color = 'gray')) + 
  theme(axis.ticks = element_line(color = "gray")) +  theme(axis.text.x = element_text(color = 'gray'), axis.ticks = element_line(color = "gray")) +
  theme(legend.key.width = unit(1, "in"), legend.key.height = unit(2, "in")) 
ggsave("JonesStatePumping'51_'18.jpg", width = 17, height = 11, units = "in") 
ggsave("JonesStatePumping'51_'18.pdf", width = 17, height = 11, units = "in")   

