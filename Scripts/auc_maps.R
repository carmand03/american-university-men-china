## Mapping geographies of education 

# load packages 

library(tidyverse)

# load data

# compute distribution by state

state_count <- aucv2 %>% filter(Country_edu == "USA") %>% group_by(State) %>% count(sort = TRUE)

# total 668 degrees in the US, 43 states, 

132/668
aucv2 %>% group_by(State) %>% 