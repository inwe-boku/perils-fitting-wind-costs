library(ggplot2)
library(tidyverse)

colors<-c("#6e9b9e", "#c72321")


#hh..hub_height
rinne_derivative_max<-function(rotor, hh, age){

  max_power <- (rotor^2*pi)*(620*log(hh) + 182*sqrt(age) - 1005) / (2*1.68)
  return(max_power/10^6)

}

rinne_total_costs<-function(power, r, hh, age){
  return(10^3*power * (610*log(hh)-1.68*power*10^6/(r^2*pi)+182*sqrt(age)-1005))
}

rinne_derivative<-function(power, r, hh, age){
  
  power<-10^3*power
  return(610*log(hh)-1.68*2*power*10^3/(r^2*pi)+182*sqrt(age)-1005)
  
}



get_feasible_curve<-function(data){
  power_range_feasible<-seq(data$min_power,data$x_max,0.01)
  feasible_curve<-tibble(power_range=power_range_feasible,
                          total_costs=rinne_total_costs(power_range_feasible,
                                                        data$rotor,
                                                        data$hub_height,
                                                        data$age)/10^6)
  return(feasible_curve)
  
}


get_infeasible_curve<-function(data){
  
  power_range_infeasible<-seq(data$x_max,data$max_power,0.01)
  infeasible_curve<-tibble(power_range=power_range_infeasible,
                             total_costs=rinne_total_costs(power_range_infeasible,
                                                        data$rotor,
                                                        data$hub_height,
                                                        data$age)/10^6) 
  return(infeasible_curve)
}

plot_feasible_infeasible_curves<-function(turbine_data, filename)
{
  
  
  turbine_data<-turbine_data %>% 
    mutate(x_max=rinne_derivative_max(rotor, hub_height, age)) %>% 
    mutate(y_max=rinne_total_costs(x_max, rotor, hub_height, age)/10^6)
  
  p<-turbine_data %>% 
    group_by(Name) %>% 
    nest() %>% 
    mutate(feasible_range=map(data, get_feasible_curve)) %>%
    unnest(data) %>% 
    unnest(feasible_range) %>% 
    mutate(Region="Feasible range") %>% 
    bind_rows(turbine_data %>% 
                group_by(Name) %>% 
                nest() %>% 
                mutate(infeasible_range=map(data, get_infeasible_curve)) %>%
                unnest(data) %>% 
                unnest(infeasible_range) %>% 
                mutate(Region="Infeasible range")) %>% 
    mutate(Turbine=Name) %>% 
    ggplot(aes(x=power_range,y=total_costs)) + 
    geom_line(aes(col=Region) ,size=2) +
    facet_wrap(.~Turbine,scales="free") +
    scale_color_manual(values=c(colors[1],colors[2])) +
    geom_point(aes(x=power,y=rinne_total_costs(power, rotor, hub_height, age)/10^6,shape=Turbine),size=4) +
    xlab("Turbine power (MW)") +
    ylab("Total turbine cost \n as projected by model (M€)")
    
  plot(p)
  
  ggsave(paste0("../figures/", filename))
  
    

}
  
  