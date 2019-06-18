library(ggplot2)
library(tidyverse)
library(car)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


cost_data<-read_delim("../data/cost_data_rinne.csv", delim=";")

names(cost_data)<-c("Source",
                    "Year",
                    "Hh",
                    "Rd",
                    "Cost",
                    "Country",
                    "Currency",
                    "Pow_dens",
                    "Rotor_area",
                    "Rated_cap")

#plot some elements

Year <- 2010:2015
Exchange_rate <- c(1.45, 1.30, 1.30, 1.33, 1.37, 1.21) 
Inflation <- c(0, 2.2, 2.8, 2.2, 0.8, 0.01)
Inflation_aggregate <- cumprod(1-Inflation/100)

ex_rate<-tibble(Year, Exchange_rate, Inflation_aggregate)

cost_data<-full_join(ex_rate,cost_data) %>% 
  mutate(Cost_conv = ifelse(Country=="United States", Cost / Exchange_rate, Cost )) %>% 
  mutate(Cost_conv = Cost_conv * Inflation_aggregate) %>% 
  mutate(Cost_tot = Cost * Rated_cap) %>% 
  mutate(Rd_3 = Rd^3) %>% 
  mutate(Rd_2_5 = Rd^2.5) %>% 
  mutate(Rd_4 = Rd^4) %>% 
  mutate(Rd_exp = exp(Rd)) %>% 
  mutate(Rd_log = log(Rd)) %>% 
  na.omit()


cost_data %>% dplyr::select(Cost_tot,Rated_cap,Rd,Hh,Year) %>% 
  cor(method = "pearson")

cost_data %>% dplyr::select(Cost_tot,Rated_cap,Rd,Hh,Year) %>% 
  cor(method = "spearman")

mod1<-lm(Cost_conv~Rated_cap+Rd+Hh+Year, cost_data)
summary(mod1)

mod2<-lm(Cost_conv~Rated_cap+(Rd_3)+Hh+Year, cost_data)
summary(mod2)

mod3<-lm(Cost_conv~Rated_cap+(Rd_2_5)+Hh+Year, cost_data)
summary(mod3)

mod4<-lm(Cost_conv~Rated_cap+(Rd_4)+Hh+Year, cost_data)
summary(mod4)

mod5<-lm(Cost_conv~Rated_cap+(Rd_exp)+Hh+Year, cost_data)
summary(mod5)

mod6<-lm(Cost_conv~Rated_cap+(Rd_log)+Hh+Year, cost_data)
summary(mod6)








