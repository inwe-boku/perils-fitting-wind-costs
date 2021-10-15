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

rotor<-63
hub_height<-135
age<-0
power<-7.5


theme_set(theme_bw(base_size = 16))

plot_feasible_infeasible_curve(rotor,
                               hub_height,
                               age,
                               "figure-1.png",
                               power)



### Check plausible/implausible regions for all turbines in US windturbine database

### download data from US windturbine database
f<-"../data/tempturbines.zip"
download.file("https://eerscmap.usgs.gov/uswtdb/assets/data/uswtdbCSV.zip",f)
unzip(f,exdir="../data")

### read turbines
turbines_download<-read_delim("../data/uswtdb_v4_1_20210721.csv",delim=",") %>%
  dplyr::select(t_cap,t_hh,t_rd, p_year) %>%
  na.omit() %>%
  unique() %>%
  mutate(power=t_cap/1000,hub_height=t_hh,radius=t_rd/2,age=2016-p_year) %>%
  mutate(age=ifelse(age<0,0,age)) %>%
  mutate(max_power=rinne_derivative(radius,hub_height,age)) %>%
  mutate(unlikely_region=ifelse(power<max_power,"Plausible","Implausible")) %>%
  mutate(data_rinne = 10) %>%
  mutate(power_density=power*10^6/(radius^2*pi))


### plot plausible and implausible turbines
turbines_download %>%
  filter(power_density<700) %>%
  mutate(`Hub height (m)`=hub_height) %>%
  arrange(desc(unlikely_region)) %>%
  mutate(Region=unlikely_region) %>%
  ggplot(aes(x=age,y=power_density)) +
  geom_point(aes(col=Region, size=`Hub height (m)`)) +
  xlab("Age (Years before 2016)") +
  ylab(bquote('Specific power (W'~m^-2~')')) +
  scale_color_manual(values=c(colors[1],colors[2]))

ggsave("../figures/figure-2.png")
