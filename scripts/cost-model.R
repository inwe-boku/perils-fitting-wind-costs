library(ggplot2)
library(tidyverse)

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
  na.omit()



cost_data %>% ggplot(aes(x=Rated_cap, y=Cost_conv)) + geom_point(aes(col=Year))

cost_data %>% ggplot(aes(x=Rd, y=Cost_conv)) + geom_point(aes(col=Year))

cost_data %>% ggplot(aes(x=Hh, y=Cost_conv)) + geom_point(aes(col=Year))

cost_data %>% ggplot(aes(x=Year, y=Cost_conv)) + geom_point(aes(col=Hh))

cost_data %>% dplyr::select(Cost_conv,Rated_cap,Rd,Hh,Year) %>% 
  cor()

mod1<-lm(Cost_conv~Rated_cap+Rd+Hh+Year, cost_data)
summary(mod1)

mod2<-lm(Cost_conv~log(Rated_cap)+log(Rd)+log(Hh)+log(Year), cost_data)
summary(mod2)

mod3<-lm(Cost_conv~Rated_cap + sqrt(Rated_cap)+Rd + Rd^2 + Hh + sqrt(Hh)+sqrt(Year), cost_data)
summary(mod3)

mod4<-lm(Cost_conv~log(Rated_cap)+Rd^3+Hh^2+log(Year), cost_data)
summary(mod4)

mod1.1<-lm(Cost_conv~Rated_cap+Hh+Year, cost_data)
summary(mod1.1)

mod1.2<-lm(Cost_conv~Rd+Hh+Year, cost_data)
summary(mod1.2)


### Power density is significant...
mod1.3<-lm(Cost_conv~Pow_dens+Hh+Year, cost_data)
summary(mod1.3)

###But rated capacity alone also is...
mod1.1<-lm(Cost_conv~Rated_cap+Hh+Year, cost_data)
summary(mod1.1)




cost_data_mod<-cost_data %>% 
  dplyr::select(Cost_conv,Rated_cap,Rd,Hh,Year) %>%
  mutate(predict=predict(mod1.1))

cost_data_mod %>% 
  gather(variable, val, -Year, -Rated_cap, -Rd, -Hh) %>% 
  ggplot(aes(x=Year, y=val)) + geom_point(aes(col=variable))

