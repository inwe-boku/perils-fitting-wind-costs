### Script for Arising Matter "The perils of automated fitting of datasets: the case of a wind turbine cost model"
### Authors: Kloeckl, C., Gruber, K., Regner, P., Schmidt, J.
### University of Natural Resources and Life Sciences, Institute for Sustainable Economic Development


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
library(readxl)

tab_filtered<-read_csv("../data/turbine-data.csv")

wrong_turbines<-tab_filtered %>% 
  mutate(x_max=rinne_derivative_max(rotor, hub_height, age)) %>% 
  mutate(diff=(x_max-power)/x_max) %>% 
  arrange(diff) %>% 
  filter(diff<0)

all_turbines <- wrong_turbines %>% 
  bind_rows(tab_filtered %>% 
              filter(Name=="E82/3000")) %>% 
  mutate(min_power = c(0.27,0.72,2.2),
    max_power = c(0.8, 1, 3.85)) 


###Figure 1
plot_feasible_infeasible_curves(all_turbines, 
                                "figure1.png")


###Figure 2
tab_filtered %>% 
  mutate(x_max=rinne_derivative_max(rotor, hub_height, age)) %>%
  mutate(derivative_cost=rinne_derivative(power,rotor,hub_height,age)) %>% 
  mutate(diff=(x_max-power)) %>% 
  arrange(derivative_cost) %>%
  ggplot(aes(x=derivative_cost))+
  geom_histogram() +
  xlab("Additional cost (€/kW)")
ggsave("../figures/Figure2.png")





