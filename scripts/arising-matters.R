library(ggplot2)
library(tidyverse)
library(XML)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rinne_derivative<-function(r, hh, age){
  
  max_power <- (r^2*pi)*(620*log(hh) + 182*sqrt(age) - 1005) / (2*1.68) 
  return(max_power/10^6)
  
}

radius_rotor<-seq(1,100,1)
derivatives_rinne <- NULL

for(hub_height in seq(50,150,20)){
  max_power<-rinne_derivative(radius_rotor, hub_height, 0)
  max_power<-tibble(radius_rotor,max_power,hub_height=rep(hub_height,length(radius_rotor)))
  derivatives_rinne<-bind_rows(derivatives_rinne,max_power)
}

### load real data from website
turbine_models<-"http://windni.com/turbines/turbine-database/" %>% 
  readHTMLTable(header=T, which=1,stringsAsFactors=F) %>% 
  as_tibble() %>% 
  mutate(`Power (kW)`=str_replace(`Power (kW)`,",","")) %>% 
  mutate(`Power (kW)`=as.numeric(`Power (kW)`),`Diameter (m)`=as.numeric(`Diameter (m)`)) %>% 
  filter(`Diameter (m)`<200) %>% mutate(power=`Power (kW)`/10^3, radius=`Diameter (m)`/2) %>% 
  mutate(max_power=rinne_derivative(radius,2*radius,0)) %>% 
  mutate(unlikely_region=ifelse(power>max_power,"Turbine cheaper than maximum cost","Turbine more expensive than maximum cost"))

turbine_models %>% filter(Availability == "Under production") %>% 
  ggplot(aes(x=radius,y=power)) + geom_point(aes(col=as.character(unlikely_region)))



derivatives_rinne %>%
  ggplot(aes(x=radius_rotor,y=max_power)) + 
  geom_line(aes(col=as.character(hub_height))) +
  geom_point(data=turbine_models,aes(x=`Diameter (m)`/2,y=`Power (kW)`/1000),col="red")

ggsave("../figures/max_power_rinne_vs_real_turbines.png")
