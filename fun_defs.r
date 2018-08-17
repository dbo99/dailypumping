#######################
### water years #######
#######################

water_year <- function(date) {
  ifelse(month(date) < 10, year(date), year(date)+1)}

water_month <-function(date) {
  ifelse(month(date) < 10, month(date)+3, month(date)-9)}

fx2_month <-function(date) {
  ifelse(month(date) < 9, month(date)+4, month(date)-8)}

febjanwy <- function(date) {  
  ifelse(month(date) > 1, year(date), year(date)-1)}

marfebwy <- function(date){    #cvp contract year
  ifelse(month(date) > 2, year(date), year(date)-1)}

fx2sepaugwy<- function(date) {
  ifelse(month(date) > 8, year(date), year(date)-1)}

sepaugwy <- function(date) {
  ifelse(month(date) < 9, year(date), year(date)+1)}

cfs_taf <- function(cfs,tstep) {cfs*1.98347*days_in_month(tstep)/1000}
taf_cfs <- function(taf,tstep) {taf*1000/(1.98347*days_in_month(tstep))}

#################################
#################################
###  Data Summarizing Funs # ####
#################################
#################################

## Ann Avgs
mn_ann_perav_taf<- function(df) {
mnanntaf_perav <- df %>% filter(!kind == "storage") %>% group_by(dv, scen) %>%  summarize(mnanntaf_perav =  round(12*mean(taf),0)) %>%
  spread(dv, mnanntaf_perav)
 print(mnanntaf_perav)
}  

mn_ann_eomstor_taf <- function(df) {
mnanntaf_eomstor <- df %>% filter(kind == "storage") %>% group_by(dv, scen) %>% summarize(mnanntaf_eomstor =  round(mean(taf), 0)) %>% 
  spread(dv, mnanntaf_eomstor)
  print(mnanntaf_eomstor)
}

md_ann_perav_taf <- function(df) {
mdannwytaf_perav <-  df %>% filter(!kind == "storage") %>% group_by(dv, scen, wy) %>% summarize(sumperavtaf = sum(taf)) %>%
summarize(mdannwytaf_perav = round(median(sumperavtaf),0)) %>% spread(dv,mdannwytaf_perav )
print(mdannwytaf_perav)
}

mn_ann_perav_native <- function(df) {
  mn_ann_perav_native <- df %>% filter(!kind == "storage") %>% group_by(dv, scen) %>%  summarize(mnannnat_perav =  round(12*mean(rawval),0)) %>%
    spread(dv, mnannnat_perav)
  print(mn_ann_perav_native)
}  

md_ann_perav_native <- function(df) {
  md_ann_perav_native <-  df %>% filter(!kind == "storage") %>% group_by(dv, scen, wy) %>% summarize(sumperavnat = sum(rawval)) %>%
    summarize(mdannwynat_perav = round(median(sumperavnat),0)) %>% spread(dv,mdannwynat_perav )
  print(md_ann_perav_native)
}

## End of May, Sept
eo_may_stor_taf <- function(df) {
  eo_may_stor <- df %>% filter(kind == "storage", m== 5) %>% group_by(dv, scen) %>% summarize(eo_may_stor =  round(mean(taf), 0)) %>%
  spread(dv, eo_may_stor )
  print(eo_may_stor)
}

eo_sep_stor_taf <- function(df) {
  eo_sep_stor <- df %>% filter(kind == "storage", m== 9) %>% group_by(dv, scen) %>% summarize(eo_sep_stor =  round(mean(taf), 0)) %>%
  spread(dv, eo_sep_stor )
  print(eo_sep_stor)
}

## Max, Min monthly
max_mon_native <- function(df) {
  maxmonrawval <- df  %>% group_by(dv, scen) %>% summarize(maxmonval=  round(max(rawval), 0)) %>% spread(dv, maxmonval)
  print(maxmonrawval)
}

min_mon_native <- function(df) {
  maxmonrawval <- df  %>% group_by(dv, scen) %>% summarize(minmonval =  round(min(rawval), 0)) %>% spread(dv, minmonval)
  print(maxmonrawval)
}

## WYT means

mn_ann_scwyt_perav_taf <- function(df) {
  mnanntaf_perav_wyt <- df %>% filter(!kind == "storage", fjwy>1921, fjwy<2003) %>% group_by(dv, scen, scwyt_scwytt) %>%
  summarize(mnanntaf_perav_wyt =  round(12*mean(taf),0)) %>% spread(scwyt_scwytt, mnanntaf_perav_wyt)
  print(mnanntaf_perav_wyt)
} 

mn_ann_perav_sjwyt_taf <- function(df) {
  mnanntaf_perav_wyt <- df %>% filter(!kind == "storage", fjwy>1921, fjwy<2003) %>% group_by(dv, scen, sjwyt_sjwytt) %>%
  summarize(mnanntaf_perav_wyt =  round(12*mean(taf),0)) %>% spread(sjwyt_sjwytt, mnanntaf_perav_wyt)
  print(mnanntaf_perav_wyt)
} 

## ID timesteps under/over a value

showtstepsallscensunder <- function(df, limit) {  #for single DV only!
  findDVtstepsunder <- df %>% group_by(tstep) %>% 
    filter(all(rawval < limit)) %>% ungroup() %>% transmute(tstep, scen, rawval) %>%  spread(tstep, rawval)
  print(findDVtstepsunder)
}

showtstepsallscensover <- function(df, limit) {   #for single DV only!
  findDVtstepsunder <- df %>% group_by(tstep) %>% 
    filter(all(rawval > limit)) %>% ungroup() %>% transmute(tstep, scen, rawval) %>%  spread(tstep, rawval)
  print(findDVtstepsunder)
}

## all timesteps in period of record (984 for CSII)

mn984_taf <- function(df) {
 mn984 <- df %>% group_by(dv,scen) %>% summarize(mean984 = mean(taf)) %>% spread(dv,mean984)
 print(mn984)
}

sum984_taf <- function(df) {
  sum984 <- df %>% group_by(dv,scen) %>% summarize(sum984 = sum(taf)) %>% spread(dv,sum984)
  print(sum984)
}



#################################
#################################
########  Plotting Funs  ########
#################################
#################################


##### annual stats ######

## Ann Avgs

pb_mn_ann_perav_taf <- function(df) {
df %>% filter(!kind == "storage") %>% group_by(dv, scen) %>%  summarize(mnanntaf_perav =  12*mean(taf))  %>%
ggplot(aes(x = scen, y = mnanntaf_perav, fill = scen, label = round(mnanntaf_perav, 0))) + geom_bar(position = "dodge",stat = "identity") + 
     theme_gray()   + guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
     ylab("mean annual per avg taf") + geom_text(color = "dark blue") + facet_grid(~dv) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) +ggtitle("mean annual per. avg. (82 yrs)")
  
} 

pb_mn_ann_eomstor_taf <- function(df) {
df %>% filter(kind == "storage") %>% group_by(dv, scen) %>% summarize(mnanntaf_eomstor =  mean(taf)) %>%
    ggplot(aes(x = scen, y = mnanntaf_eomstor,   fill = scen, label = round(mnanntaf_eomstor, 0))) +
    geom_bar(position = "dodge",stat = "identity") + 
    theme_gray()  + guides(colour = guide_legend(override.aes = list(size=2)))+ theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
    ylab("mean annual eom taf") + geom_text(color = "dark blue") + facet_grid(~dv)  +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) +ggtitle("mean annual storage (82 yrs)")
}

pb_md_ann_perav_taf <- function(df) {
df %>% filter(!kind == "storage") %>% group_by(dv, scen, wy) %>% summarize(sumperavtaf = sum(taf)) %>%
  summarize(mdannwytaf_perav = median(sumperavtaf)) %>% 
  ggplot(aes(x = scen, y = mdannwytaf_perav, fill = scen, label = round(mdannwytaf_perav, 0))) + geom_bar(position = "dodge",stat = "identity") + 
  theme_gray() + guides(colour = guide_legend(override.aes = list(size=2)))+ theme(plot.margin=grid::unit(c(8,8,8,8), "mm"))+
    ylab("median annual per avg taf") + geom_text(color = "dark blue") + facet_grid(~dv) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) +ggtitle("median annual per. avg. (82 yrs)")
}


### WYT Avgs ###

pb_mn_scwyt_perav_taf <- function(df) {
  df %>% filter(!kind == "storage", fjwy>1921, fjwy<2003) %>% group_by(dv, scen, scwyt_scwytt) %>%  summarize(mnanntaf_perav_wyt =  12*mean(taf)) %>% 
  ggplot(aes(x = scwyt_scwytt, y = mnanntaf_perav_wyt, fill = scwyt_scwytt, label = round(mnanntaf_perav_wyt,0))) +
    geom_bar(position = "dodge",stat = "identity") + 
    theme_gray()   + guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
    ylab("mean sac wyt per avg taf") + geom_text(color = "dark blue", angle = 90) +facet_grid(dv~scen) +ggtitle("sac wyt means (82 yrs)") +
    scale_fill_discrete(name = "sac wyt") + theme(axis.title.x=element_blank())
    } 

pb_mn_sjwyt_perav_taf <- function(df) {
  df %>% filter(!kind == "storage", fjwy>1921, fjwy<2003) %>% group_by(dv, scen, sjwyt_sjwytt) %>%  summarize(mnanntaf_perav_wyt =  12*mean(taf)) %>% 
    ggplot(aes(x = sjwyt_sjwytt, y = mnanntaf_perav_wyt, fill = sjwyt_sjwytt, label = round(mnanntaf_perav_wyt,0))) +
    geom_bar(position = "dodge",stat = "identity") + 
    theme_gray()   + guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
    ylab("mean sj wyt per avg taf") + geom_text(color = "dark blue", angle = 90) +facet_grid(dv~scen) +ggtitle("sj wyt means (82 yrs)")+
    scale_fill_discrete(name = "sj wyt") + theme(axis.title.x=element_blank())
} 

#### month specific ####

pb_mn_eomay_stor_taf <- function(df) {
  df %>% filter(kind == "storage", m == 5) %>% group_by(dv, scen) %>% summarize(eo_sep_stor =  round(mean(taf), 0)) %>% 
    ggplot(aes(x = scen, y = eo_sep_stor , fill = scen, label = round(eo_sep_stor, 0))) + geom_bar(position = "dodge",stat = "identity") + 
    theme_gray() + guides(colour = guide_legend(override.aes = list(size=2)))+ theme(plot.margin=grid::unit(c(8,8,8,8), "mm"))+
    ylab("eo may avg taf") + geom_text(color = "dark blue") + facet_grid(~dv) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) +ggtitle("mean eo may storage (82 yrs)")
}

pb_mn_eosep_stor_taf <- function(df) {
  df %>% filter(kind == "storage", m == 9) %>% group_by(dv, scen) %>% summarize(eo_may_stor =  round(mean(taf), 0)) %>% 
    ggplot(aes(x = scen, y = eo_may_stor,  fill = scen, label = round(eo_may_stor, 0))) + geom_bar(position = "dodge",stat = "identity") + 
    theme_gray() + guides(colour = guide_legend(override.aes = list(size=2)))+ theme(plot.margin=grid::unit(c(8,8,8,8), "mm"))+
    ylab("eo sept avg taf") + geom_text(color = "dark blue")  + facet_grid(~dv)+
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) +ggtitle("mean eo sept. storage (82 yrs)")
}


######
### Difference 
######

pb_mn_ann_perav_taf_d <- function(df) {
df %>% filter(!kind == "storage") %>% group_by(dv, scen) %>%  summarize(mnanntaf_perav =  12*mean(taf))  %>%
    ggplot(aes(x = scen, y = mnanntaf_perav, fill = scen, label = round(mnanntaf_perav, 0))) + geom_bar(position = "dodge",stat = "identity") + 
    theme_gray()   + guides(colour = guide_legend(override.aes = list(size=2)))+ theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
    ylab("mean annual per avg taf [study difference]")  + geom_text(color = "dark blue")+ facet_grid(~dv) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) +ggtitle("mean annual per. avg. difference (82 yrs)")
} 


pb_mn_ann_eomstor_taf_d <- function(df) {
df %>% filter(kind == "storage") %>% group_by(dv, scen) %>% summarize(mnanntaf_eomstor =  mean(taf)) %>%
    ggplot(aes(x = scen, y = mnanntaf_eomstor, fill = scen, label = round(mnanntaf_eomstor, 0))) + geom_bar(position = "dodge",stat = "identity") + 
    theme_gray()  + guides(colour = guide_legend(override.aes = list(size=2)))+ theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
    ylab("mean annual eom taf [study difference]") + geom_text(color = "dark blue")+ facet_grid(~dv) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) +ggtitle("mean ann. storage difference (82 yrs)")
}

pb_md_ann_perav_taf_d <- function(df) {
df %>% filter(!kind == "storage") %>% group_by(dv, scen, wy) %>% summarize(sumperavtaf = sum(taf)) %>%
    summarize(mdannwytaf_perav = median(sumperavtaf)) %>% 
    ggplot(aes(x = scen, y = mdannwytaf_perav, fill = scen, label = round(mdannwytaf_perav, 0))) + geom_bar(position = "dodge",stat = "identity") + 
    theme_gray() + guides(colour = guide_legend(override.aes = list(size=2)))+ theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
    ylab("median annual per avg taf [study difference]") + geom_text(color = "dark blue")+ facet_grid(~dv) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) +ggtitle("median annual per. avg. difference (82 yrs)")
}

pb_mn_scwyt_perav_taf_d <- function(df) {
  df %>% filter(!kind == "storage", fjwy>1921, fjwy<2003) %>% group_by(dv, scen, scwyt_scwytt) %>%  summarize(mnanntaf_perav_wyt =  12*mean(taf)) %>% 
    ggplot(aes(x = scwyt_scwytt, y = mnanntaf_perav_wyt, fill = scwyt_scwytt, label = round(mnanntaf_perav_wyt,0))) +
    geom_bar(position = "dodge",stat = "identity") + 
    theme_gray()   + guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
    ylab("mean sac wyt per avg taf [study difference]") + geom_text(color = "dark blue", angle = 90) +facet_grid(dv~scen) +
    ggtitle("mean sac wyt per. avg., difference (82 yrs)") +
    scale_fill_discrete(name = "sac wyt") + theme(axis.title.x=element_blank())
                                                
} 

pb_mn_sjwyt_perav_taf_d <- function(df) {
  df %>% filter(!kind == "storage", fjwy>1921, fjwy<2003) %>% group_by(dv, scen, sjwyt_sjwytt) %>%  summarize(mnanntaf_perav_wyt =  12*mean(taf)) %>% 
    ggplot(aes(x = sjwyt_sjwytt, y = mnanntaf_perav_wyt, fill = sjwyt_sjwytt, label = round(mnanntaf_perav_wyt,0))) +
    geom_bar(position = "dodge",stat = "identity") + 
    theme_gray()   + guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
    ylab("mean sj wyt per avg taf [study difference]") + geom_text(color = "dark blue", angle = 90) +facet_grid(dv~scen) +
    ggtitle("mean sj wyt per. avg., difference (82 yrs)")+
    scale_fill_discrete(name = "sj wyt") + theme(axis.title.x=element_blank())
} 

#### month specific difference ####

pb_eosep_stor_d_taf <- function(df) {
   df %>% filter(kind == "storage", m== 9) %>% group_by(dv, scen) %>% summarize(eo_sep_stor =  round(mean(taf), 0)) %>% 
    ggplot(aes(x = scen, y = eo_sep_stor , fill = scen, label = round(eo_sep_stor, 0))) + geom_bar(position = "dodge",stat = "identity") + 
    theme_gray() + guides(colour = guide_legend(override.aes = list(size=2)))+ theme(plot.margin=grid::unit(c(8,8,8,8), "mm"))+
    ylab("eo sep avg taf [study difference]") + geom_text(color = "dark blue") + facet_grid(~dv) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) +ggtitle("mean eo september storage, difference (82 yrs)")
}

pb_eomay_stor_d_taf <- function(df) {
   df %>% filter(kind == "storage", m== 5) %>% group_by(dv, scen) %>% summarize(eo_may_stor =  round(mean(taf), 0)) %>% 
    ggplot(aes(x = scen, y = eo_may_stor,  fill = scen, label = round(eo_may_stor, 0))) + geom_bar(position = "dodge",stat = "identity") + 
    theme_gray() + guides(colour = guide_legend(override.aes = list(size=2)))+ theme(plot.margin=grid::unit(c(8,8,8,8), "mm"))+
    ylab("eo may avg taf [study difference]") + geom_text(color = "dark blue") + facet_grid(~dv)+
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) +ggtitle("mean eo may storage, difference (82 yrs)")
}


#######################
###### timeseries #####
#######################

p_mon_ts_taf <- function(df, yrmin, yrmax) {
df %>% filter(rawunit != "km") %>% ggplot(aes(x = yearmon, y = taf, color = scen, linetype = dv, label = round(taf, 0)))+geom_line()+
    scale_x_continuous(limits = c(yrmin, yrmax), labels = yearmon, expand = c(0.01, 0.01))+
    labs(x= "month", y = "taf -- monthly") + theme_gray() + guides(colour = guide_legend(override.aes = list(size=2)))+
    theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
    ggtitle("monthly taf")
}

p_mon_ts_may_taf <- function(df, yrmin, yrmax) {
  df %>% filter(rawunit != "km", m == 5) %>% ggplot(aes(x = yearmon, y = taf, color = scen, linetype = dv, label = round(taf, 0)))+geom_line()+
    scale_x_continuous(limits = c(yrmin, yrmax), labels = yearmon, expand = c(0.01, 0.01))+
    labs(x= "month", y = "taf -- just mays") + theme_gray() + guides(colour = guide_legend(override.aes = list(size=2)))+
    theme(plot.margin=grid::unit(c(8,8,8,8), "mm"))+
    ggtitle("mays, taf")
}

p_mon_ts_sep_taf <- function(df, yrmin, yrmax) {
  df %>% filter(rawunit != "km", m == 9) %>% ggplot(aes(x = yearmon, y = taf, color = scen, linetype = dv, label = round(taf, 0)))+geom_line()+
    scale_x_continuous(limits = c(yrmin, yrmax), labels = yearmon, expand = c(0.01, 0.01))+
    labs(x= "month", y = "taf -- just septs") + theme_gray() + guides(colour = guide_legend(override.aes = list(size=2)))+
    theme(plot.margin=grid::unit(c(8,8,8,8), "mm"))+
    ggtitle("septembers, taf")
}

p_mon_ts_taf_maysseps_taf <- function(df, yrmin, yrmax) {
  df %>% filter(rawunit != "km", m == 5 | m==9) %>% ggplot(aes(x = yearmon, y = taf, color = scen, linetype = dv, label = round(taf, 0), shape =dv))+geom_line()+
    scale_x_continuous(limits = c(yrmin, yrmax), labels = yearmon, expand = c(0.01, 0.01))+
    labs(x= "month", y = "taf -- just Mays & Septs") + theme_gray() + guides(colour = guide_legend(override.aes = list(size=2)))+
    theme(plot.margin=grid::unit(c(8,8,8,8), "mm"))+
    ggtitle("mays and septembers, taf")
}


p_mon_ts_cfs    <- function(df, yrmin, yrmax) {
df %>% filter(kind != "storage",rawunit != "km") %>% ggplot(aes(x = yearmon, y = cfs, color = scen, linetype = dv, label = round(cfs, 0)))+geom_line() +
   scale_x_continuous(limits = c(yrmin, yrmax), labels = yearmon, expand = c(0.01, 0.01)) +
   labs(x= "month", y = "monthly average cfs")+ theme_gray()  + guides(colour = guide_legend(override.aes = list(size=2)))+
    theme(plot.margin=grid::unit(c(8,8,8,8), "mm"))+
    ggtitle("monthly cfs")
}
p_mon_ts_native    <- function(df, yrmin, yrmax) {
df  %>% #filter(!(kind %in% c("storage", "flow_delivery", "flow_channel")))
    ggplot(aes(x = yearmon, y = rawval, color = scen, linetype = dv, label = round(rawval, 0)))+
    geom_line() + scale_x_continuous(limits = c(yrmin, yrmax), labels = yearmon, expand = c(0.01, 0.01)) +
    labs(x= "month", y = "monthly (unassigned unit)") + theme_gray() + guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm"))+
    ggtitle("monthly, other unit")
}

p_ann_ts_sum_taf <- function(df, yrmin, yrmax) {
df %>% filter(!kind == "storage") %>% group_by(scen, dv, wy) %>% summarize(wytafsum =  sum(taf)) %>% 
    ggplot(aes(x = wy, y = wytafsum, color = scen, linetype = dv, label = round(wytafsum, 0))) + geom_line() + 
    scale_x_continuous(limits = c(yrmin, yrmax), expand = c(0.01, 0.01))+ labs(x = "water year", y = "taf -- water year sum")+
    theme_gray()  + guides(colour = guide_legend(override.aes = list(size=2)))+ theme(plot.margin=grid::unit(c(8,8,8,8), "mm"))+
    ggtitle("water year sums, taf")
}

p_ann_ts_mn_taf <- function(df, yrmin, yrmax) {
  df %>% filter(rawunit == "cfs"|rawunit == "taf") %>% group_by(scen, dv, wy) %>% summarize(wytafmean =  mean(taf)) %>% 
    ggplot(aes(x = wy, y = wytafmean, color = scen, linetype = dv, label = round(wytafmean, 0))) + geom_line() + 
    scale_x_continuous(limits = c(yrmin, yrmax), expand = c(0.01, 0.01))+
    labs(x = "water year", y = "taf -- water year mean (82 yrs)")+ theme_gray() + guides(colour = guide_legend(override.aes = list(size=2))) +
    theme(plot.margin=grid::unit(c(8,8,8,8), "mm"))+
    ggtitle("water year means, taf")
} 
p_ann_ts_sum_native <- function(df, yrmin, yrmax) {
df  %>% #filter(!(kind %in% c("storage", "flow_delivery", "flow_channel")))  
     group_by(scen, dv, wy) %>%
    summarize(annmean =  mean(rawval)) %>% ggplot(aes(x = wy, y = annmean, color = scen, linetype = dv, label = round(annmean, 0))) + geom_line() + 
    scale_x_continuous(limits = c(yrmin, yrmax), expand = c(0.01, 0.01))+ labs(x = "water year",
    y = "mean water year annual (unassigned unit)") + theme_gray()  + guides(colour = guide_legend(override.aes = list(size=2))) +
    theme(plot.margin=grid::unit(c(8,8,8,8), "mm"))+
    ggtitle("water year [other units], taf")
}


################
## diff ########
################

p_mon_ts_taf_d <- function(df, yrmin, yrmax) {
df %>% filter(rawunit != "km") %>% ggplot(aes(x = yearmon, y = taf, color = scen, linetype = dv, label = round(taf, 0))) + geom_line()+
    scale_x_continuous(limits = c(yrmin, yrmax), labels = yearmon, expand = c(0.01, 0.01))+
    labs(x= "month", y = "taf -- monthly [study difference]")+ theme_gray()  + guides(colour = guide_legend(override.aes = list(size=2))) +
    theme(plot.margin=grid::unit(c(8,8,8,8), "mm"))+
    ggtitle("monthly difference, taf")
} 

p_mon_ts_cfs_d    <- function(df, yrmin, yrmax) {
df %>% filter(kind != "storage",rawunit != "km") %>% ggplot(aes(x = yearmon, y = cfs, color = scen, linetype = dv, label = round(cfs, 0)))+geom_line() +
    scale_x_continuous(limits = c(yrmin, yrmax), labels = yearmon, expand = c(0.01, 0.01)) +
    labs(x= "month", y = "monthly average cfs [study difference]")+ theme_gray()  + guides(colour = guide_legend(override.aes = list(size=2))) +
    theme(plot.margin=grid::unit(c(8,8,8,8), "mm"))+
    ggtitle("monthly difference, cfs")
} 

p_mon_ts_native_d    <- function(df, yrmin, yrmax) {
df  %>% #filter(!(kind %in% c("storage", "flow_delivery", "flow_channel")))  %>% 
    ggplot(aes(x = yearmon, y = rawval,
    color = scen, linetype = dv, label = round(rawval, 0)))+geom_line() + scale_x_continuous(limits = c(yrmin, yrmax), labels = yearmon, expand = c(0.01, 0.01)) +
    labs(x= "month", y = "monthly average (unassigned unit) [study difference]")+ theme_gray()+ guides(colour = guide_legend(override.aes = list(size=2))) +
    theme(plot.margin=grid::unit(c(8,8,8,8), "mm"))+
    ggtitle("monthly difference, [other units]")
} 


p_ann_ts_sum_taf_d <- function(df, yrmin, yrmax) {
df %>% filter(rawunit == "cfs") %>% group_by(scen, dv, wy) %>% summarize(wytafsum =  sum(taf)) %>% 
    ggplot(aes(x = wy, y = wytafsum, color = scen, linetype = dv, label = round(wytafsum, 0))) + geom_line() + 
    scale_x_continuous(limits = c(yrmin, yrmax), expand = c(0.01, 0.01))+
    labs(x = "water year", y = "taf -- water year sum [study difference]") + theme_gray()  + guides(colour = guide_legend(override.aes = list(size=2))) +
    theme(plot.margin=grid::unit(c(8,8,8,8), "mm"))+
    ggtitle("water year difference, taf")
} 

p_ann_ts_mn_taf_d <- function(df, yrmin, yrmax) {
df %>% filter(rawunit == "cfs"|rawunit == "taf") %>% group_by(scen, dv, wy) %>% summarize(wytafmean =  mean(taf)) %>% 
    ggplot(aes(x = wy, y = wytafmean, color = scen, linetype = dv, label = round(wytafmean, 0))) + geom_line() + 
    scale_x_continuous(limits = c(yrmin, yrmax), expand = c(0.01, 0.01))+
    labs(x = "water year", y = "taf -- water year mean [study difference]")+ theme_gray() + guides(colour = guide_legend(override.aes = list(size=2))) +
    theme(plot.margin=grid::unit(c(8,8,8,8), "mm"))+
    ggtitle("water year mean, differences, taf")
} 

################################
#######  exceedance ############
################################

p_mon_excd_taf <- function(df) {
df %>% filter(rawunit != "km")%>% group_by(scen, dv) %>% arrange(dv, desc(taf)) %>% mutate(taf_dv_rank = row_number(),
         excdxaxis = taf_dv_rank/(n()+1)) %>% ggplot(aes(x = excdxaxis, y = taf, color = scen,
          linetype = dv)) + geom_line() + labs(x = "probability of exceedance", y = "taf -- monthly")+theme_gray() +
           guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +ggtitle("984 months (taf)")
}

p_mon_excd_cfs <- function(df) {
df %>% filter(kind != "storage", rawunit != "km") %>% group_by(scen, dv) %>% arrange(dv, desc(cfs)) %>% mutate(cfs_dv_rank = row_number(),
        excdxaxis = cfs_dv_rank/(n()+1)) %>% ggplot(aes(x = excdxaxis, y = cfs, color = scen,
        linetype = dv)) + geom_line() + labs(x = "probability of exceedance", y = "monthly average cfs") + theme_gray() +   
    guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +ggtitle("984 months (cfs)")
}

p_mon_excd_native <- function(df) {
df  %>% #filter(!(kind %in% c("storage", "flow_delivery", "flow_channel"))) %>% 
    group_by(scen, dv) %>% arrange(dv, desc(rawval)) %>% 
    mutate(oth_dv_rank = row_number(), excdxaxis = oth_dv_rank/(n()+1)) %>% ggplot(aes(x = excdxaxis, y = rawval, color = scen,
    linetype = dv)) + geom_line() + labs(x = "probability of exceedance", y = "monthly (unassigned)")  + theme_gray() +  
    guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +ggtitle("984 months (other units)")
}

p_ann_wysum_excd_taf <- function(df) {
df %>% filter(rawunit == "cfs") %>% group_by(scen, dv, wy) %>% summarize(wytafsum =  sum(taf)) %>%
    group_by(scen, dv) %>% arrange(dv, desc(wytafsum)) %>% mutate(taf_dv_rank = row_number(),
    excdxaxis = taf_dv_rank/(n()+1)) %>% ggplot(aes(x = excdxaxis, y = wytafsum, color = scen,
    linetype = dv)) + geom_line() + labs(x = "probability of exceedance", y = "taf -- water year sum (82 yrs)")+
    theme_gray() +  
    guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +ggtitle("82 water year totals")
}

p_ann_fjwysum_excd_taf <- function(df) {
df %>% filter(rawunit == "cfs", fjwy > 1921, fjwy < 2003) %>% group_by(scen, dv, fjwy) %>% summarize(fjwytafsum =  sum(taf)) %>%
    group_by(scen, dv) %>% arrange(dv, desc(fjwytafsum)) %>% mutate(taf_dv_rank = row_number(),
   excdxaxis = taf_dv_rank/(n()+1)) %>% ggplot(aes(x = excdxaxis, y = fjwytafsum, color = scen,
    linetype = dv)) + geom_line() + labs(x = "probability of exceedance", y = "taf -- feb - jan year sum (81 yrs)")+
    theme_gray() +  
    guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +ggtitle("81 feb-jan totals")
}

p_ann_mfwysum_excd_taf <- function(df) {
df %>% filter(rawunit == "cfs", mfwy > 1921, mfwy < 2003) %>% group_by(scen, dv, mfwy) %>% summarize(mfwytafsum =  sum(taf)) %>%
    group_by(scen, dv) %>% arrange(dv, desc(mfwytafsum)) %>% mutate(taf_dv_rank = row_number(),
    excdxaxis = taf_dv_rank/(n()+1)) %>% ggplot(aes(x = excdxaxis, y = mfwytafsum, color = scen,
    linetype = dv)) + geom_line() + labs(x = "probability of exceedance", y = "taf -- mar - feb year sum (81 yrs)")+
    theme_gray() +  
    guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +ggtitle("81 mar-feb totals")
}

p_ann_wymn_excd_taf <- function(df) {
df %>% filter(rawunit == "taf" | rawunit == "cfs" ) %>% group_by(scen, dv, wy) %>% summarize(wytafmean =  mean(taf)) %>%
    group_by(scen, dv) %>% arrange(dv, desc(wytafmean)) %>% mutate(taf_dv_rank = row_number(),
    excdxaxis = taf_dv_rank/(n()+1)) %>% ggplot(aes(x = excdxaxis, y = wytafmean, color = scen,
    linetype = dv)) + geom_line() + labs(x = "probability of exceedance", y = "taf -- water year mean (82 yrs)")+
    theme_gray()+ 
    guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +ggtitle("82 water year means")
}



############
## diff ####
############


p_mon_excd_taf_d <- function(df) {
df %>% filter(rawunit != "km") %>% group_by(scen, dv) %>% arrange(dv, desc(taf)) %>% mutate(taf_dv_rank = row_number(),
      excdxaxis = taf_dv_rank/(n()+1)) %>% ggplot(aes(x = excdxaxis, y = taf, color = scen,
      linetype = dv)) + geom_line() + labs(x = "probability of exceedance", y = "taf -- monthly [study difference]")+
      theme_gray()+ 
    guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +ggtitle("984 months, difference")
}

p_mon_excd_cfs_d <- function(df) {
df %>% filter(kind != "storage", rawunit != "km") %>% group_by(scen, dv) %>% arrange(dv, desc(cfs)) %>% mutate(cfs_dv_rank = row_number(),
     excdxaxis = cfs_dv_rank/(n()+1)) %>% ggplot(aes(x = excdxaxis, y = cfs, color = scen,
    linetype = dv)) + geom_line() + labs(x = "probability of exceedance", y = "monthly average cfs [study difference]")+
    theme_gray()  + guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
    ggtitle("984 months, difference")
}

p_ann_wysum_excd_taf_d <- function(df)  {
df %>% filter(rawunit == "cfs") %>% group_by(scen, dv, wy) %>% summarize(wytafsum =  sum(taf)) %>%
    group_by(scen, dv) %>% arrange(dv, desc(wytafsum)) %>% mutate(taf_dv_rank = row_number(),
    excdxaxis = taf_dv_rank/(n()+1)) %>% ggplot(aes(x = excdxaxis, y = wytafsum, color = scen,
    linetype = dv)) + geom_line() + labs(x = "probability of exceedance", y = "taf -- water year sum (82 yrs) [study difference]")+
    theme_gray()+ guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
    ggtitle("82 oct-sep totals, difference")
}

p_ann_fjwysum_excd_taf_d <- function(df) {
df %>% filter(rawunit == "cfs", fjwy > 1921, fjwy < 2003) %>% group_by(scen, dv, fjwy) %>% summarize(fjwytafsum =  sum(taf)) %>%
    group_by(scen, dv) %>% arrange(dv, desc(fjwytafsum)) %>% mutate(taf_dv_rank = row_number(),
    excdxaxis = taf_dv_rank/(n()+1)) %>% ggplot(aes(x = excdxaxis, y = fjwytafsum, color = scen,
    linetype = dv)) + geom_line() + labs(x = "probability of exceedance", y = "taf -- feb - jan year sum (81 yrs) [study difference]")+
    theme_gray() + guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
    ggtitle("81 feb-jan totals, difference")
}

p_ann_mfwysum_excd_taf_d <- function(df) {
df %>% filter(rawunit == "cfs", mfwy > 1921, mfwy < 2003) %>% group_by(scen, dv, mfwy) %>% summarize(mfwytafsum =  sum(taf)) %>%
    group_by(scen, dv) %>% arrange(dv, desc(mfwytafsum)) %>% mutate(taf_dv_rank = row_number(),
    excdxaxis = taf_dv_rank/(n()+1)) %>% ggplot(aes(x = excdxaxis, y = mfwytafsum, color = scen, 
    linetype = dv)) + geom_line() + labs(x = "probability of exceedance", y = "taf -- mar - feb year sum (81 yrs) [study difference]")+
    theme_gray()  +  guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
    ggtitle("81 mar-feb totals, difference")
}


p_ann_wymn_excd_taf_d <- function(df) {
df %>% filter(rawunit == "cfs"| rawunit == "taf") %>% group_by(scen, dv, wy) %>% summarize(wytafmean =  mean(taf)) %>%
    group_by(scen, dv) %>% arrange(dv, desc(wytafmean)) %>% mutate(taf_dv_rank = row_number(),
    excdxaxis = taf_dv_rank/(n()+1)) %>% ggplot(aes(x = excdxaxis, y = wytafmean, color = scen, 
    linetype = dv)) + geom_line() + labs(x = "probability of exceedance", y = "taf -- water year mean (82 yrs) [study difference]")+
    theme_gray()  + guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
    ggtitle("82 water year means, difference")
}

###################################################
### plot exceedance by wyt, line plot grid ########
###################################################

## Sac WYT taf
p_ann_fjwysum_scwyt_excd_taf <- function(df)  {
df_0 <- df %>% filter(rawunit == "cfs", fjwy>1921, fjwy<2003) %>% group_by(scen, dv, fjwy) %>% summarize(wytafsum =  sum(taf)) %>%
        group_by(scen, dv) %>% arrange(dv, desc(wytafsum)) %>% mutate(taf_dv_rank = row_number(), excdxaxis = taf_dv_rank/(n()+1), 
                                                                 scwyt_scwytt = "all") 

df_1_5 <- df %>% filter(rawunit == "cfs", fjwy>1921, fjwy<2003) %>% group_by(scen, dv, fjwy, scwyt_scwytt) %>% summarize(wytafsum =  sum(taf)) %>%
          group_by(scen, dv, scwyt_scwytt) %>% arrange(dv, desc(wytafsum)) %>% mutate(taf_dv_rank = row_number(), excdxaxis = taf_dv_rank/(n()+1))

df_0_5 <- rbind(df_0, df_1_5) %>%  ggplot(aes(x = excdxaxis, y = wytafsum, color = scen,
                                              linetype = dv)) + geom_line() + geom_point(size =0.5) +labs(x = "probability of exceedance", y = "taf -- feb - jan water year sum") +
  facet_wrap(~scwyt_scwytt, ncol = 3) +scale_x_continuous(sec.axis = dup_axis(name = NULL))+scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
  ggtitle("sac wyt sums")
df_0_5 +  theme_gray() + guides(colour = guide_legend(override.aes = list(size=2)))
}

## SJ WYT taf
p_ann_fjwysum_sjwyt_excd_taf <- function(df) {
df_0 <- df %>% filter(rawunit == "cfs", fjwy>1921, fjwy<2003) %>% group_by(scen, dv, fjwy) %>% summarize(wytafsum =  sum(taf)) %>%
        group_by(scen, dv) %>% arrange(dv, desc(wytafsum)) %>% mutate(taf_dv_rank = row_number(), excdxaxis = taf_dv_rank/(n()+1), sjwyt_sjwytt = "all")

df_1_5 <- df %>% filter(rawunit == "cfs", fjwy>1921, fjwy<2003) %>% group_by(scen, dv, fjwy, sjwyt_sjwytt) %>% summarize(wytafsum =  sum(taf)) %>%
         group_by(scen, dv, sjwyt_sjwytt) %>% arrange(dv, desc(wytafsum)) %>% mutate(taf_dv_rank = row_number(), excdxaxis = taf_dv_rank/(n()+1))

df_0_5 <- rbind(df_0, df_1_5) %>%  ggplot(aes(x = excdxaxis, y = wytafsum, color = scen,
                                              linetype = dv)) + geom_line() + geom_point(size =0.5) +labs(x = "probability of exceedance", y = "taf -- feb - jan water year sum") +
          facet_wrap(~sjwyt_sjwytt, ncol = 3) +scale_x_continuous(sec.axis = dup_axis(name = NULL))+scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
          ggtitle("sjr wyt sums")
df_0_5 + theme_gray() + guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm"))
}

###########
## diff ###
###########

## Sac WYT taf
p_ann_fjwysum_scwyt_excd_taf_d <- function(df) {
df_0 <- df %>% filter(rawunit == "cfs", fjwy>1921, fjwy<2003) %>% group_by(scen, dv, fjwy) %>% summarize(wytafsum =  sum(taf)) %>%
        group_by(scen, dv) %>% arrange(dv, desc(wytafsum)) %>% mutate(taf_dv_rank = row_number(), excdxaxis = taf_dv_rank/(n()+1), 
        scwyt_scwytt = "all")

df_1_5 <- df %>% filter(rawunit == "cfs", fjwy>1921, fjwy<2003) %>% group_by(scen, dv, fjwy, scwyt_scwytt) %>% summarize(wytafsum =  sum(taf)) %>%
          group_by(scen, dv, scwyt_scwytt) %>% arrange(dv, desc(wytafsum)) %>% mutate(taf_dv_rank = row_number(),
                                                                               excdxaxis = taf_dv_rank/(n()+1))

df_0_5 <- rbind(df_0, df_1_5) %>%  ggplot(aes(x = excdxaxis, y = wytafsum, color = scen, linetype = dv)) + geom_line() + geom_point(size =0.5) +
          labs(x = "probability of exceedance", y = "taf -- feb - jan water year sum [study difference]") +
          facet_wrap(~scwyt_scwytt, ncol = 3) +scale_x_continuous(sec.axis = dup_axis(name = NULL))+scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
          ggtitle("sac wyt sums, difference")
df_0_5  + theme_gray() + guides(colour = guide_legend(override.aes = list(size=2)))
}

## SJ WYT taf
p_ann_fjwysum_sjwyt_excd_taf_d <- function(df) {
df_0 <- df %>% filter(rawunit == "cfs", fjwy>1921, fjwy<2003) %>% group_by(scen, dv, fjwy) %>% summarize(wytafsum =  sum(taf)) %>%
        group_by(scen, dv) %>% arrange(dv, desc(wytafsum)) %>% mutate(taf_dv_rank = row_number(), excdxaxis = taf_dv_rank/(n()+1), sjwyt_sjwytt = "all")

df_1_5 <- df %>% filter(rawunit == "cfs", fjwy>1921, fjwy<2003) %>% group_by(scen, dv, fjwy, sjwyt_sjwytt) %>% summarize(wytafsum =  sum(taf)) %>%
          group_by(scen, dv, sjwyt_sjwytt) %>% arrange(dv, desc(wytafsum)) %>% mutate(taf_dv_rank = row_number(), excdxaxis = taf_dv_rank/(n()+1))

df_0_5 <- rbind(df_0, df_1_5) %>%  ggplot(aes(x = excdxaxis, y = wytafsum, color = scen,
          linetype = dv)) + geom_line() + geom_point(size =0.5) +labs(x = "probability of exceedance", y = "taf -- feb - jan water year sum [study difference]") +
          facet_wrap(~sjwyt_sjwytt, ncol = 3) +scale_x_continuous(sec.axis = dup_axis(name = NULL))+scale_y_continuous(sec.axis = dup_axis(name = NULL)) +ggtitle("sj wyt sums, difference")
df_0_5 + theme_gray() + guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm"))
}

###################################################
### stack sequential bars by wyt            #######
###################################################

## bar sums ##

pb_ann_fjwysum_scwyt_taf <- function(df) {
  df_bars <- df %>% filter(rawunit == "cfs", fjwy>1921, fjwy<2003) %>% group_by(scen, dv, fjwy, scwyt_scwytt) %>%
    summarize(fjwytafsum =  sum(taf)) %>%
    group_by(scen, dv, scwyt_scwytt) %>% arrange(dv, desc(fjwytafsum)) %>%
    mutate(taf_dv_rank = row_number()) %>% group_by(scen, dv, scwyt_scwytt) %>% arrange(scwyt_scwytt, desc(fjwytafsum)) %>%
    group_by(scen,dv) %>% mutate(x = row_number())
  df_bars_mns <- df_bars %>% group_by(scen, dv, scwyt_scwytt) %>% summarize(mean = mean(fjwytafsum))
  df_bars_mn <- df_bars %>% group_by(scen, dv) %>% summarize(mean = mean(fjwytafsum))
  
ggplot(df_bars, aes(x, fjwytafsum, fill = scwyt_scwytt, label = fjwy))  +geom_bar(stat = "identity", position = "dodge") +
  facet_grid(dv~scen) + scale_fill_discrete(name = "sac wyt") + labs(x = "CalSim Feb-Jan Yrs") + theme_gray() +
  scale_y_continuous(sec.axis = dup_axis()) + labs(y = "feb - jan wy sum (taf)") + geom_hline(data = df_bars_mns, mapping = aes(yintercept = mean, color = scwyt_scwytt), show.legend = FALSE) +
  geom_hline(data = df_bars_mn, mapping = aes(yintercept = mean, linetype = "Overall Ann. Average"), color = "dark blue") +
  scale_linetype_manual(name = " ", values = c(2, 2))+ggtitle("sac wyt sums and means") #+geom_text(angle = 90) #unselect for year labels
}

pb_ann_fjwysum_sjwyt_taf <- function(df) {
  df_bars <- df %>% filter(rawunit == "cfs", fjwy>1921, fjwy<2003) %>% group_by(scen, dv, fjwy, sjwyt_sjwytt) %>%
    summarize(fjwytafsum =  sum(taf)) %>%
    group_by(scen, dv, sjwyt_sjwytt) %>% arrange(dv, desc(fjwytafsum)) %>%
    mutate(taf_dv_rank = row_number()) %>% group_by(scen, dv, sjwyt_sjwytt) %>% arrange(sjwyt_sjwytt, desc(fjwytafsum)) %>%
    group_by(scen,dv) %>% mutate(x = row_number())
  df_bars_mns <- df_bars %>% group_by(scen, dv, sjwyt_sjwytt) %>% summarize(mean = mean(fjwytafsum))
  df_bars_mn <- df_bars %>% group_by(scen, dv) %>% summarize(mean = mean(fjwytafsum))
  
  ggplot(df_bars, aes(x, fjwytafsum, fill = sjwyt_sjwytt, label = fjwy))  +geom_bar(stat = "identity", position = "dodge") +
    facet_grid(dv~scen) + scale_fill_discrete(name = "sj wyt") + labs(x = "CalSim Feb-Jan Yrs") + theme_gray() +
    scale_y_continuous(sec.axis = dup_axis()) + labs(y = "feb - jan wy sum (taf)") +
    geom_hline(data = df_bars_mns, mapping = aes(yintercept = mean, color = sjwyt_sjwytt), show.legend = FALSE) +
    geom_hline(data = df_bars_mn, mapping = aes(yintercept = mean, linetype = "Overall Ann. Average"), color = "dark blue") +
    scale_linetype_manual(name = " ", values = c(2, 2)) +ggtitle("sj wyt sums and means") #+geom_text(angle = 90) #unselect for year labels
}

## storage ## 

pb_eomay_stor_scwyt_taf <- function(df) {
  df_bars <- df %>% filter(kind == "storage", fjwy>1921, fjwy<2003, wm == 8) %>% group_by(scen, dv, fjwy, scwyt_scwytt) %>%
    summarize(eomay_stor =  taf) %>%
    group_by(scen, dv, scwyt_scwytt) %>% arrange(dv, desc(eomay_stor)) %>%
    mutate(taf_dv_rank = row_number()) %>% group_by(scen, dv, scwyt_scwytt) %>% arrange(scwyt_scwytt, desc(eomay_stor)) %>%
    group_by(scen,dv) %>% mutate(x = row_number())
  df_bars_mns <- df_bars %>% group_by(scen, dv, scwyt_scwytt) %>% summarize(mean = mean(eomay_stor))
  df_bars_mn <- df_bars %>% group_by(scen, dv) %>% summarize(mean = mean(eomay_stor))
  
  ggplot(df_bars, aes(x, eomay_stor, fill = scwyt_scwytt, label = fjwy))  +geom_bar(stat = "identity", position = "dodge") +
    facet_grid(dv~scen) + scale_fill_discrete(name = "sac wyt") + labs(x = "CalSim Feb-Jan Yrs") + theme_gray() +
    scale_y_continuous(sec.axis = dup_axis()) + labs(y = "end of may storage (taf)") +
    geom_hline(data = df_bars_mns, mapping = aes(yintercept = mean, color = scwyt_scwytt), show.legend = FALSE) +
    geom_hline(data = df_bars_mn, mapping = aes(yintercept = mean, linetype = "Overall May Average"), color = "dark blue") +
    scale_linetype_manual(name = " ", values = c(2, 2)) +ggtitle("82 end of may storages") #+geom_text(angle = 90) #unselect for year labels
}

pb_eosep_stor_scwyt_taf <- function(df) {
  df_bars <- df %>% filter(kind == "storage", fjwy>1921, fjwy<2003, wm == 12) %>% group_by(scen, dv, fjwy, scwyt_scwytt) %>%
    summarize(eomay_stor =  taf) %>%
    group_by(scen, dv, scwyt_scwytt) %>% arrange(dv, desc(eomay_stor)) %>%
    mutate(taf_dv_rank = row_number()) %>% group_by(scen, dv, scwyt_scwytt) %>% arrange(scwyt_scwytt, desc(eomay_stor)) %>%
    group_by(scen,dv) %>% mutate(x = row_number())
  df_bars_mns <- df_bars %>% group_by(scen, dv, scwyt_scwytt) %>% summarize(mean = mean(eomay_stor))
  df_bars_mn <- df_bars %>% group_by(scen, dv) %>% summarize(mean = mean(eomay_stor))
  
  ggplot(df_bars, aes(x, eomay_stor, fill = scwyt_scwytt, label = fjwy))  +geom_bar(stat = "identity", position = "dodge") +
    facet_grid(dv~scen) + scale_fill_discrete(name = "sac wyt") + labs(x = "CalSim Feb-Jan Yrs") + theme_gray() +
    scale_y_continuous(sec.axis = dup_axis()) + labs(y = "end of september storage (taf)") +
    geom_hline(data = df_bars_mns, mapping = aes(yintercept = mean, color = scwyt_scwytt), show.legend = FALSE) +
    geom_hline(data = df_bars_mn, mapping = aes(yintercept = mean, linetype = "Overall Sept. Average"), color = "dark blue") +
    scale_linetype_manual(name = " ", values = c(2, 2)) +ggtitle("82 end of september storages")#+geom_text(angle = 90) #unselect for year labels
}



###########
## diff ###
###########

## bars sumss ##

pb_ann_fjwysum_scwyt_taf_d <- function(df) {
  df_bars <- df %>% filter(rawunit == "cfs", fjwy>1921, fjwy<2003) %>% group_by(scen, dv, fjwy, scwyt_scwytt) %>%
    summarize(fjwytafsum =  sum(taf)) %>%
    group_by(scen, dv, scwyt_scwytt) %>% arrange(dv, desc(fjwytafsum)) %>%
    mutate(taf_dv_rank = row_number()) %>% group_by(scen, dv, scwyt_scwytt) %>% arrange(scwyt_scwytt, desc(fjwytafsum)) %>%
    group_by(scen,dv) %>% mutate(x = row_number())
  df_bars_mns <- df_bars %>% group_by(scen, dv, scwyt_scwytt) %>% summarize(mean = mean(fjwytafsum))
  df_bars_mn <- df_bars %>% group_by(scen, dv) %>% summarize(mean = mean(fjwytafsum))
  
  ggplot(df_bars, aes(x, fjwytafsum, fill = scwyt_scwytt, label = fjwy))  +geom_bar(stat = "identity", position = "dodge") +
    facet_grid(dv~scen) + scale_fill_discrete(name = "sac wyt") + labs(x = "CalSim Feb-Jan Yrs") + theme_gray() +
    scale_y_continuous(sec.axis = dup_axis()) + labs(y = "feb - jan wy sum (taf) [study difference]") +
    geom_hline(data = df_bars_mns, mapping = aes(yintercept = mean, color = scwyt_scwytt), show.legend = FALSE) +
    geom_hline(data = df_bars_mn, mapping = aes(yintercept = mean, linetype = "Overall Avg. Ann. Diff"), color = "dark blue") +
    scale_linetype_manual(name = " ", values = c(2, 2)) +ggtitle("sac wyt sums and means, difference")#+geom_text(angle = 90) #unselect for year labels
} 

pb_ann_fjwysum_sjwyt_taf_d <- function(df) {
  df_bars <- df %>% filter(rawunit == "cfs", fjwy>1921, fjwy<2003) %>% group_by(scen, dv, fjwy, sjwyt_sjwytt) %>%
    summarize(fjwytafsum =  sum(taf)) %>%
    group_by(scen, dv, sjwyt_sjwytt) %>% arrange(dv, desc(fjwytafsum)) %>%
    mutate(taf_dv_rank = row_number()) %>% group_by(scen, dv, sjwyt_sjwytt) %>% arrange(sjwyt_sjwytt, desc(fjwytafsum)) %>%
    group_by(scen,dv) %>% mutate(x = row_number())
  df_bars_mns <- df_bars %>% group_by(scen, dv, sjwyt_sjwytt) %>% summarize(mean = mean(fjwytafsum))
  df_bars_mn <- df_bars %>% group_by(scen, dv) %>% summarize(mean = mean(fjwytafsum))
  
  ggplot(df_bars, aes(x, fjwytafsum, fill = sjwyt_sjwytt, label = fjwy))  +geom_bar(stat = "identity", position = "dodge") +
    facet_grid(dv~scen) + scale_fill_discrete(name = "sj wyt") + labs(x = "CalSim Feb-Jan Yrs") + theme_gray() +
    scale_y_continuous(sec.axis = dup_axis()) + labs(y = "feb - jan wy sum (taf) [study difference]") +
    geom_hline(data = df_bars_mns, mapping = aes(yintercept = mean, color = sjwyt_sjwytt), show.legend = FALSE) +
    geom_hline(data = df_bars_mn, mapping = aes(yintercept = mean, linetype = "Overall Avg. Ann. Diff."), color = "dark blue") +
    scale_linetype_manual(name = " ", values = c(2, 2)) +ggtitle("sj wyt sums means, difference")#+geom_text(angle = 90) #unselect for year labels
    
  }

## storage ##

pb_eomay_stor_scwyt_taf_d <- function(df) {
  df_bars <- df %>% filter(kind == "storage", fjwy>1921, fjwy<2003, wm == 8) %>% group_by(scen, dv, fjwy, scwyt_scwytt) %>%
    summarize(eomay_stor =  taf) %>%
    group_by(scen, dv, scwyt_scwytt) %>% arrange(dv, desc(eomay_stor)) %>%
    mutate(taf_dv_rank = row_number()) %>% group_by(scen, dv, scwyt_scwytt) %>% arrange(scwyt_scwytt, desc(eomay_stor)) %>%
    group_by(scen,dv) %>% mutate(x = row_number())
  df_bars_mns <- df_bars %>% group_by(scen, dv, scwyt_scwytt) %>% summarize(mean = mean(eomay_stor))
  df_bars_mn <- df_bars %>% group_by(scen, dv) %>% summarize(mean = mean(eomay_stor))
  
  ggplot(df_bars, aes(x, eomay_stor, fill = scwyt_scwytt, label = fjwy))  +geom_bar(stat = "identity", position = "dodge") +
    facet_grid(dv~scen) + scale_fill_discrete(name = "sac wyt") + labs(x = "CalSim Feb-Jan Yrs") + theme_gray() +
    scale_y_continuous(sec.axis = dup_axis()) + labs(y = "end of may storage (taf) [study difference]") +
    geom_hline(data = df_bars_mns, mapping = aes(yintercept = mean, color = scwyt_scwytt), show.legend = FALSE) +
    geom_hline(data = df_bars_mn, mapping = aes(yintercept = mean, linetype = "Overall May Avg. Diff."), color = "dark blue") +
    scale_linetype_manual(name = " ", values = c(2, 2)) +ggtitle("82 end of may storages, difference")#+geom_text(angle = 90) #unselect for year labels
}

pb_eosep_stor_scwyt_taf_d <- function(df) {
  df_bars <- df %>% filter(kind == "storage", fjwy>1921, fjwy<2003, wm == 12) %>% group_by(scen, dv, fjwy, scwyt_scwytt) %>%
    summarize(eomay_stor =  taf) %>%
    group_by(scen, dv, scwyt_scwytt) %>% arrange(dv, desc(eomay_stor)) %>%
    mutate(taf_dv_rank = row_number()) %>% group_by(scen, dv, scwyt_scwytt) %>% arrange(scwyt_scwytt, desc(eomay_stor)) %>%
    group_by(scen,dv) %>% mutate(x = row_number())
  df_bars_mns <- df_bars %>% group_by(scen, dv, scwyt_scwytt) %>% summarize(mean = mean(eomay_stor))
  df_bars_mn <- df_bars %>% group_by(scen, dv) %>% summarize(mean = mean(eomay_stor))
  
  ggplot(df_bars, aes(x, eomay_stor, fill = scwyt_scwytt, label = fjwy))  +geom_bar(stat = "identity", position = "dodge") +
    facet_grid(dv~scen) + scale_fill_discrete(name = "sac wyt") + labs(x = "CalSim Feb-Jan Yrs") + theme_gray() +
    scale_y_continuous(sec.axis = dup_axis()) + labs(y = "end of september storage (taf) [study difference]") +
    geom_hline(data = df_bars_mns, mapping = aes(yintercept = mean, color = scwyt_scwytt), show.legend = FALSE) +
    geom_hline(data = df_bars_mn, mapping = aes(yintercept = mean, linetype = "Overall Sept. Avg. Diff"), color = "dark blue") +
    scale_linetype_manual(name = " ", values = c(2, 2)) +ggtitle("82 end of sept. storages, difference")#+geom_text(angle = 90) #unselect for year labels
}


################################
#######  non-stor taf ridges  ##  #for one dv only!
################################


pr_taf <- function(df, yrmin, yrmax, scalingfactor) { #plots monthly output on y, colors by annual total
df <-df  %>% group_by(scen, wy) %>% mutate(sumtaf = sum(taf))  
sc <- (max(df$taf) / min(df$taf) * scalingfactor) # usually needs to be very low, say 0.00005 for c9
minh <- min(df$taf)
yrmaxtitle <- yrmax-1
ggplot(df, aes(wm, wy, height = taf, group=as.factor(wy), fill = sumtaf))+
  geom_ridgeline( stat = "identity", show.legend = T, scale = sc, alpha = 0.8,
                  min_height = minh) + 
  facet_grid(~scen) +
  scale_fill_viridis() + theme_gray() +
  scale_x_continuous(expand = c(0.02, 0.02),
                     breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A","S"),
                     sec.axis = dup_axis()) +
  scale_y_continuous(expand = c(0.02, 0.02),
                     limits  =c(yrmin,yrmax), 
                     breaks = seq(from = 1922, to = 2002, by = 2),
                     labels = seq(from = 1922, to = 2002, by = 2),
                     sec.axis = dup_axis())  +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  ggtitle(paste0(df$dv[1],"  ", yrmin, " - ", yrmaxtitle))
}

pr_taf_d <- function(df, yrmin, yrmax, scalingfactor) { #plots monthly output on y, colors by annual total
  df_diff <-df_diff  %>% mutate(abstaf = abs(taf)) %>% group_by(scen, wy) %>% mutate(sumabsdiff = sum(abstaf)) 
  sc <- (max(df_diff$taf) / min(df_diff$taf) * scalingfactor) # usually needs to be very low, say 0.00005 for c9
  
  minh <- max(df_diff$taf)
  yrmaxtitle <- yrmax-1
  yrmintitle <- yrmin+1
  ggplot(df_diff, aes(wm, wy, height = -taf, group=as.factor(wy), fill = sumabsdiff))+
    geom_ridgeline( stat = "identity", show.legend = T, scale = sc, alpha = 0.8, min_height = -minh) + 
    facet_grid(~scen) +
    scale_fill_viridis() + theme_gray() +
    scale_x_continuous(expand = c(0.02, 0.02),
                       breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                       labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A","S"),
                       sec.axis = dup_axis()) +
    scale_y_continuous(expand = c(0.02, 0.02),
                       limits  =c(yrmin,yrmax), 
                       breaks = seq(from = 1922, to = 2002, by = 2),
                       labels = seq(from = 1922, to = 2002, by = 2),
                       sec.axis = dup_axis())  +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    ggtitle(paste0(df_diff$dv[1], " [difference]", yrmintitle, " - ", yrmaxtitle))
}


##################
## eom ridges diff ###
##################

pr_eomstormean_taf <- function(df, yrmin, yrmax, scalingfactor) { #plots monthly output on y, colors by annual total
  df <-df  %>% group_by(scen, wy) %>% mutate(meaneomtaf = mean(taf))  
  sc <- (max(df$taf) / min(df$taf) * scalingfactor) # usually needs to be very low, say 0.00005 for c9
  yrmaxtitle <- yrmax-1
  ggplot(df, aes(wm, wy, height = taf, group=as.factor(wy), fill = meaneomtaf))+
    geom_ridgeline( stat = "identity", show.legend = T, scale = sc, alpha = 0.8 ) + 
    facet_grid(~scen) +
    scale_fill_viridis() + theme_gray() +
    scale_x_continuous(expand = c(0.02, 0.02),
                       breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                       labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A","S"),
                       sec.axis = dup_axis()) +
    scale_y_continuous(expand = c(0.02, 0.02),
                       limits  =c(yrmin,yrmax), 
                       breaks = seq(from = 1922, to = 2002, by = 2),
                       labels = seq(from = 1922, to = 2002, by = 2),
                       sec.axis = dup_axis())  +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
    ggtitle(paste0(df$dv[1],"  ", yrmin, " - ", yrmaxtitle))
}





pr_eomstormean_taf_d <- function(df, yrmin, yrmax, scalingfactor) { #plots monthly output on y, colors by annual total
  df_diff <- df_diff  %>% mutate(abstaf = abs(taf)) %>% group_by(scen, wy) %>% mutate(mnabseomtaf_d = mean(abstaf)) 
  sc <- (max(df_diff$taf) / min(df_diff$taf) * scalingfactor) # usually needs to be very low, say 0.00005 for c9
  mh <- max(df_diff$taf)
  yrmaxtitle <- yrmax-1
  yrmintitle <- yrmin+1
  ggplot(df_diff, aes(wm, wy, height = -taf, group=as.factor(wy), fill = mnabseomtaf_d))+
    geom_ridgeline( stat = "identity", show.legend = T, scale = sc, alpha = 0.8, min_height = -mh) + 
    facet_grid(~scen) +
    scale_fill_viridis() + theme_gray() +
    scale_x_continuous(expand = c(0.05, 0.05),
                       breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                       labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A","S"),
                       sec.axis = dup_axis()) +
    scale_y_continuous(expand = c(0.02, 0.02),
                       limits  =c(yrmin,yrmax), 
                       breaks = seq(from = 1922, to = 2002, by = 2),
                       labels = seq(from = 1922, to = 2002, by = 2),
                       sec.axis = dup_axis())  +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    ggtitle(paste0(df_diff$dv[1], " [difference]", yrmintitle, " - ", yrmaxtitle))
}

#########################
## Plot Box plot
#########################

## Monthly ## use for one dv only, otherwise too cluttered

pbp_mon_taf <- function(df) {
  
df %>% group_by(wm, scen)%>% mutate(scen_wm= paste0(scen, "_", wm)) %>%
    ggplot(aes(x = wm, y = taf, color = scen, group = scen_wm)) + geom_boxplot(outlier.alpha = 0.1)+
    scale_x_continuous(expand = c(0.01, 0.01),limits = c(0.5,12.5), breaks = c(1:12),
    labels = c('O', 'N', 'D', 'J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S'),sec.axis = dup_axis(name = NULL) ) +
    theme_gray() + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +ggtitle("monthly distribution (984 months)")
}


pbp_mon_scwyt_taf <- function(df) {  #use for one dv only, otherwise too cluttered

df %>% group_by(wm, scen)%>% mutate(scen_wm= paste0(scen, "_", wm)) %>%
ggplot(aes(x = wm, y = taf, color = scen, group = scen_wm)) + geom_boxplot(outlier.alpha = 0.1)+
   scale_x_continuous(expand = c(0.01, 0.01),limits = c(0.5,12.5), breaks = c(1:12),
  labels = c('O', 'N', 'D', 'J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S'),sec.axis = dup_axis(name = NULL) )+
    facet_grid(~scwyt)+ theme_gray() + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +ggtitle("monthly distribution by sac wyt")
}
  

#################################
#################################
#####   data.frame create   #####
#################################
#################################
 
create_df <-function(df_csv){
df <- df_csv %>% filter(Variable %in% dvs) %>%  transmute(scen = as.factor(scen), tstep = Date_Time,
      wy = water_year(tstep), fjwy = febjanwy(tstep), mfwy = marfebwy(tstep),
      wm = water_month(tstep), m = month(tstep), tstep,  dv = Variable, kind = Kind, 
      rawunit = Units, rawval = Value,
      taf = ifelse(rawunit == "cfs", cfs_taf(rawval,tstep),
                   ifelse(rawunit == "taf", rawval, NA)),
      cfs = ifelse(rawunit == "taf", taf_cfs(rawval,tstep),
                   ifelse(rawunit == "cfs", rawval, NA)), 
      yearmon = as.yearmon(tstep), tstep = ymd(tstep)) %>%
      filter(wy >= 1922) #ignore any pre-Oct-'21 data

wyt <- read_csv("wyt.csv") %>% mutate(tstep = mdy(tstep)) #makes sure date read in as date
scwyt_txt <- data.frame("scwyt" = c(1,2,3,4,5), "scwytt"=c("wt", "an", "bn", "dr", "cr"))
sjwyt_txt <- data.frame("sjwyt" = c(1,2,3,4,5), "sjwytt"=c("wt", "an", "bn", "dr", "cr"))

df <- df %>% inner_join(wyt) %>% inner_join(scwyt_txt) %>% inner_join(sjwyt_txt) %>%
  mutate(scwyt_scwytt = paste0(scwyt, "_", scwytt), sjwyt_sjwytt = paste0(sjwyt, "_", sjwytt))  %>%
  mutate(scwyt = as.integer(scwyt), sjwyt = as.integer(sjwyt)) 
}

create_df_diff <-function(df_csv){
baseline_df <- df %>% filter(scen == "baseline") %>% mutate(id = row_number())

df_diff <- df %>% select(scen, taf, cfs, rawval) %>%
  group_by(scen) %>% 
  mutate(id = row_number()) %>% 
  left_join(baseline_df, by = "id", suffix = c("_scen", "_bl")) %>% 
  ungroup() %>%
  mutate(taf = taf_scen - taf_bl,
         cfs = cfs_scen - cfs_bl,
         rawval = rawval_scen - rawval_bl,
         scen = paste0(scen_scen," - bl")) %>% 
  select(-scen_scen, -scen_bl) %>%
  filter(scen != "baseline - bl")


wyt <- read_csv("wyt.csv") %>% mutate(tstep = mdy(tstep)) #makes sure date read in as date
scwyt_txt <- data.frame("scwyt" = c(1,2,3,4,5), "scwytt"=c("wt", "an", "bn", "dr", "cr"))
sjwyt_txt <- data.frame("sjwyt" = c(1,2,3,4,5), "sjwytt"=c("wt", "an", "bn", "dr", "cr"))

df_diff  <- df_diff %>% inner_join(wyt) %>% inner_join(scwyt_txt) %>% inner_join(sjwyt_txt) %>%
  mutate(scwyt_scwytt = paste0(scwyt, "_", scwytt), sjwyt_sjwytt = paste0(sjwyt, "_", sjwytt)) %>%
  mutate(scwyt = as.integer(scwyt), sjwyt = as.integer(sjwyt)) 
}

################################
## add spec. scen data frame ###
################################


create_fallx2_df <- function(df) {
  df_fallx2 <- df %>% mutate(fx2spagwy = fx2sepaugwy(tstep), spagwy = sepaugwy(tstep)) %>% filter(spagwy>1922, spagwy<2003) %>%
    mutate(nxtscwyt = lead(scwyt,5)) %>%
    mutate(nxtscwytt = ifelse(nxtscwyt == 1, "wt", ifelse(nxtscwyt == 2, "an", ifelse(nxtscwyt == 3, "bn", ifelse(nxtscwyt == 4, "dr", 
                                                                                                                  ifelse(nxtscwyt ==5, "cr", NA)))))) %>%
    mutate(fx2yt = ifelse(m == 9  & scwyt == 1, "wt", ifelse(m == 9 & scwyt == 2, "an", NA))) %>%
    group_by(grp = cumsum(!is.na(fx2yt))) %>% mutate(fx2yt = replace(fx2yt, 2:pmin(9,n()), fx2yt[1])) %>% ungroup %>% select(-grp) %>%
    mutate(fx2ytn = ifelse(fx2yt == "wt", 1, ifelse(fx2yt == "an", "2", NA))) %>%
    mutate(facet = ifelse(fx2ytn >0, paste0(fx2yt, fx2spagwy, "_", nxtscwytt, spagwy), NA))%>%
    mutate(facet2 = ifelse(fx2ytn >0, paste0(fx2ytn, "_",  nxtscwyt,"_", fx2spagwy, "_", spagwy), NA)) 
}
