# For right now we are doing some nonsense stuff. fuck I relaized that I do need to do the smoothing! 
# and stuff in python can't immeidately jump to R but idk might do some stuff casue 
library(dplyr)
library(ggplot2)

data <- read.csv("data/temp-anomaly.csv")



split(data, interaction(data$experiment, data$model, data$ensemble), drop = TRUE) %>% 
  lapply(function(d){
    
    d %>% 
      select(experiment, ensemble, model) %>% 
      distinct() -> 
      info
    
    if(length(unique(diff(d$year))) > 1){
      return(info)
    }
    
    
  }) %>% 
  do.call(what ="rbind") -> 
  to_keep

to_keep %>% 
  mutate(name = paste0(experiment, "_", ensemble, "_", model)) %>% 
  pull(name) -> 
  drop

data %>% 
  mutate(name = paste0(experiment, "_", ensemble, "_", model)) %>% 
  filter(!name %in% drop) %>%  
  select(-name) -> 
  dd
  
#data <- read.csv("data/temp-anomaly.csv")
write.csv(dd, file = "data/temp-anomaly.csv", row.names = FALSE)

data %>% 
  dplyr::filter(experiment == "ssp534-over") %>% 
ggplot() + 
  geom_point(aes(year, value)) + 
  facet_wrap("model")



data %>% 
  dplyr::filter(experiment == "1pctCO2") %>% 
  ggplot() + 
  geom_point(aes(year, value)) + 
  facet_wrap("model", scales = "free")


data %>% 
  dplyr::filter(experiment == "abrupt-2xCO2") %>% 
  ggplot() + 
  geom_point(aes(year, value)) + 
  facet_wrap("model", scales = "free")


data %>% 
  dplyr::filter(experiment == "abrupt-4xCO2") %>% 
  ggplot() + 
  geom_point(aes(year, value)) + 
  facet_wrap("model", scales = "free")

data %>% 
  dplyr::filter(experiment == "abrupt-0p5xCO2") %>% 
  ggplot() + 
  geom_point(aes(year, value)) + 
  facet_wrap("model", scales = "free")
