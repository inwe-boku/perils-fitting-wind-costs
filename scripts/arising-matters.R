### Script for Arising Matter "The perils of automated fitting of datasets: the case of a wind turbine cost model"
### Authors: Kloeckl, C., Gruber, K., Regner, P., Schmidt, J.
### University of Natural Resources and Life Sciences, Institute for Sustainable Economic Development


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")

### Calculate cost curves for Rinne turbine
### V90 3M, Radius 45 Meter, Hub height 75

#rotor<-45
#hub_height<-75
#age<-3
#power<-3

#zond z40
rotor<-20
hub_height<-40
age<-21
power<-0.55

#vergnet GEV mp-3
#2007 mentioned here http://prdrse4all.spc.int/system/files/r_-_market_review_311007_final.pdf
rotor<-29/2
hub_height<-32
age<-9
power<-0.275

#Vestas V42
#1999 mentioned here https://www.hitwind.com/product/vestas-v42-600kw-wind-turbine/
rotor<-39/2
hub_height<-30
#age<-17
age<-17
power<-0.5


# all 3 turbine types found are in fact older than our approximation indicates. If we use the real age data, they are well within the plausible range.

#alstom eco80
#rotor<-90/2
#hub_height<-80
#age<-0
#power<-3


theme_set(theme_bw(base_size = 16))

plot_feasible_infeasible_curve(rotor,
                               hub_height,
                               age,
                               "figure-1.png",
                               power,
                               max_power=2)



### Check plausible/implausible regions for all turbines in US windturbine database

### download data from US windturbine database
f<-"../data/tempturbines.zip"
download.file("https://eerscmap.usgs.gov/uswtdb/assets/data/uswtdbCSV.zip",f)
unzip(f,exdir="../data")

### read turbines
turbines_download<-read_delim("../data/uswtdb_v4_1_20210721.csv",delim=",") %>% 
  dplyr::select(t_cap,t_hh,t_rd, p_year, t_manu, t_model) %>%
  na.omit() %>%
  unique() %>%
  filter(p_year<2017) %>% 
  mutate(power=t_cap/1000,hub_height=t_hh,radius=t_rd/2,age=2016-p_year) %>%
  mutate(age=ifelse(age<0,0,age)) %>%
  mutate(max_power=rinne_derivative(radius,hub_height,age)) %>%
  mutate(unlikely_region=ifelse(power<max_power,"Plausible","Implausible")) %>%
  mutate(data_rinne = 10) %>%
  mutate(power_density=power*10^6/(radius^2*pi)) %>% 
  filter(!is.na(t_manu)) %>% 
  filter(!is.na(t_model)) %>% 
  group_by(t_manu, t_model) %>% 
  mutate(age=max(age))


### plot plausible and implausible turbines
turbines_download %>%
  filter(power_density<700) %>%
  filter(unlikely_region=="Plausible") %>% 
  mutate(`Hub height (m)`=hub_height) %>%
  arrange(desc(unlikely_region)) %>%
  mutate(Region=unlikely_region) %>%
  ggplot(aes(x=age,y=power_density)) +
  geom_point(aes(col=Region, size=`Hub height (m)`)) +
  xlab("Age (Years before 2016)") +
  ylab(bquote('Specific power (W'~m^-2~')')) +
  scale_color_manual(values=c(colors[1],colors[2]))

ggsave("../figures/figure-2.png")
