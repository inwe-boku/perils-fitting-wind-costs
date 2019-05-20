library(ggplot2)
library(tidyverse)
library(XML)
library(plot3D)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

colorsERC10<-c("#c72321","#861719","#fbd7a9","#ba9f7c","#7a6952","#6e9b9e","#0d8085","#19484c","#f0c320","#af8f19")



rinne_derivative<-function(r, hh, age){
  
  max_power <- (r^2*pi)*(620*log(hh) + 182*sqrt(age) - 1005) / (2*1.68) 
  return(max_power/10^6)
  
}

rinne_total_costs<-function(power, r, hh, age){
  return(10^3*power * (610*log(hh)-1.68*power*10^6/(r^2*pi)+182*sqrt(age)-1005))
}

plot_feasible_infeasible_curve<-function(rotor, hub_height, age, filename, power = 0)
{
  
  
  x_max<-rinne_derivative(rotor, hub_height, age)
  y_max<-rinne_total_costs(x_max, rotor, hub_height ,age)
  
  
  power_range_feasible<-seq(0,x_max,0.1)
  feasible_curve<-tibble(power_range=power_range_feasible,
                         total_costs=rinne_total_costs(power_range_feasible,
                                                       rotor,
                                                       hub_height,
                                                       age),
                         Region="Plausible")
  
  power_range_infeasible<-seq(x_max,10,0.1)
  infeasible_curve<-tibble(power_range=power_range_infeasible,
                           total_costs=rinne_total_costs(power_range_infeasible,
                                                         rotor,
                                                         hub_height,
                                                         age),
                           Region="Inplausible"
  )
  
  if(power != 0){
    
    cost_max_power=rinne_total_costs(power,
                                     rotor,
                                     hub_height,
                                     age)
    
    figure<-bind_rows(feasible_curve, infeasible_curve) %>% ggplot(aes(x=power_range,y=total_costs)) + geom_line(aes(col=Region),size=2) +
      xlab("Power (MW)") + ylab("Total costs of Turbine (???)") + geom_point(aes(x=x_max,y=y_max),size=2) +
      scale_color_manual(values=c(colorsERC10[6],colorsERC10[1])) + 
      geom_point(aes(x=power, y=cost_max_power),size=4,shape=4)
    
    
  } else{
  
  
  
  
  figure<-bind_rows(feasible_curve, infeasible_curve) %>% ggplot(aes(x=power_range,y=total_costs)) + geom_line(aes(col=Region),size=2) +
    xlab("Power (MW)") + ylab("Total costs of Turbine (???)") + geom_point(aes(x=x_max,y=y_max),size=2) +
    scale_color_manual(values=c(colorsERC10[6],colorsERC10[1]))
  }
  
  plot(figure)
  ggsave(paste0("../figures/", filename))
  
}
### Calculate example cost-curve
rotor<-50
hub_height<-100
age<-0
plot_feasible_infeasible_curve(rotor, 
                               hub_height, 
                               age, 
                               "example_cost_curve.png")




### Calculate cost curves for Rinne turbines
power<-rep(c(3, 2, 3.45, 3.45),4)
radius<-rep(c(90/2,90/2,117/2,136/2),4)
hub_height<-c(75,75,75,75,100,100,100,100,125,125,125,125,150,150,150,150)
age<-rep(c(12,12,0,0),4)

rinne_data<-tibble(power, radius, hub_height, age) %>% 
  mutate(max_power=rinne_derivative(radius, hub_height, age)) %>% 
  mutate(unlikely_region=ifelse(power>max_power,
                                 "Turbine more expensive than maximum cost",
                                 "Turbine cheaper than maximum cost")) %>% 
  mutate(data_rinne = 12) %>% 
  mutate(total_costs=rinne_total_costs(power,radius,hub_height,age))

names<-paste0("Rinne",1:length(power),".png")

mapply(plot_feasible_infeasible_curve,
       rinne_data$radius,
       rinne_data$hub_height,
       rinne_data$age,
       names,
       rinne_data$power)


age <- 0

### load real data from website
f<-"../data/tempturbines.zip"
download.file("https://eerscmap.usgs.gov/uswtdb/assets/data/uswtdbCSV.zip",f)
unzip(f,exdir="../data")
turbines_download<-read_delim("../data/uswtdb_v2_0_20190417.csv",delim=",") %>% 
  select(t_cap,t_hh,t_rd, p_year) %>% 
  na.omit() %>% 
  unique() %>% 
  mutate(power=t_cap/1000,hub_height=t_hh,radius=t_rd/2,age=2016-p_year) %>% 
  mutate(age=ifelse(age<0,0,age)) %>% 
  mutate(max_power=rinne_derivative(radius,hub_height,age)) %>% 
  mutate(unlikely_region=ifelse(power<max_power,"Turbine in  plausible region","Turbine in implausible region")) %>% 
  mutate(data_rinne = 10) 

### calculate derivate of Rinne for the relation hub_height=2*radius_rotor
radius_rotor<-seq(1,100,1)
max_power<-rinne_derivative(radius_rotor, 2*radius_rotor, age)
derivatives_rinne<-tibble(radius_rotor,max_power) %>% 
  mutate(hub_height=2*radius_rotor, age=age)

x<-turbines_download$radius
y<-turbines_download$power
z<-turbines_download$age

turbines_download<-turbines_download %>% mutate(power_density=power*10^6/(radius^2*pi))

turbines_download %>% 
  arrange(unlikely_region) %>%           
  ggplot(aes(x=age,y=power_density)) + 
  geom_point(aes(col=as.character(unlikely_region))) +
  xlab("Age") + 
  ylab("Power density (W/m^2)") +
  theme(legend.title = element_blank())

ggsave("../figures/max_power_rinne_vs_real_turbines.png")



turbines_download %>% 
  arrange(unlikely_region) %>%           
  ggplot(aes(x=radius,y=power)) + 
  geom_point(aes(size=age,col=as.character(unlikely_region))) +
  xlab("Radius rotor (m)") + 
  ylab("Power (MW)")


#derivatives_rinne %>%
#  ggplot(aes(x=radius_rotor,y=max_power)) + 
#  geom_line() +
#  geom_point(data=turbines_download,
#             aes(x=radius,
#                 y=power,
#                 col=as.character(unlikely_region)
#                 )) + 
#  xlab("Radius rotor (m)") + 
#  ylab("Power (MW)")
  

ggsave("../figures/max_power_rinne_vs_real_turbines.png")



turbines_download %>% ggplot(aes(x=age,y=power_density)) + geom_point()
cor(turbines_download$age,turbines_download$power_density)

### regression_example
n<-24
sd<-30
x<-1:n + runif(n,max=sd)
y<-x + runif(n,max=sd)
z<-x + runif(n,max=sd)

cor(x,y)
cor(x,z)
cor(y,z)

lm(x~y+z) %>% summary()






                             
                             
                             
                             