### Script for Arising Matter "The perils of automated fitting of datasets: the case of a wind turbine cost model"
### Author: J. Schmidt

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")

colorsERC10<-c("#6e9b9e", "#c72321")

### Calculate cost curves for Rinne turbine
### V90 3M, Radius 45 Meter, Hub height 75

rotor<-45
hub_height<-75
age<-12
power<-3

plot_feasible_infeasible_curve(rotor, 
                               hub_height, 
                               age, 
                               "Rinne_cost_curve.png",
                               power)


### download data from US windturbine database
f<-"../data/tempturbines.zip"
download.file("https://eerscmap.usgs.gov/uswtdb/assets/data/uswtdbCSV.zip",f)
unzip(f,exdir="../data")

### read turbines
turbines_download<-read_delim("../data/uswtdb_v2_0_20190417.csv",delim=",") %>% 
  select(t_cap,t_hh,t_rd, p_year) %>% 
  na.omit() %>% 
  unique() %>% 
  mutate(power=t_cap/1000,hub_height=t_hh,radius=t_rd/2,age=2016-p_year) %>% 
  mutate(age=ifelse(age<0,0,age)) %>% 
  mutate(max_power=rinne_derivative(radius,hub_height,age)) %>% 
  mutate(unlikely_region=ifelse(power<max_power,"Turbine in  plausible region","Turbine in implausible region")) %>% 
  mutate(data_rinne = 10) %>% 
  mutate(power_density=power*10^6/(radius^2*pi))


### plot plausible and implausible turbines

turbines_download %>% 
  arrange(unlikely_region) %>%           
  ggplot(aes(x=age,y=power_density)) + 
  geom_point(aes(col=as.character(unlikely_region))) +
  xlab("Age") + 
  ylab("Power density (W/m^2)") +
  scale_color_manual(values=c(colors[1],colors[2])) + 
  theme(legend.title = element_blank())

ggsave("../figures/max_power_rinne_vs_real_turbines.png")
