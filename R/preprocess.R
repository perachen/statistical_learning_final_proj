{
  library(tidyverse)
}

social_mobility <- read_csv("data/H-2018-Data.csv")
social_mobility %>% select(SemelEretzEm_C) %>% View()


social_mobility %>% 
  mutate_all(~na_if(., 999999)) %>%
  select(-c(shana,
            contains("Kodem"),
            SibaMaavarMegurim,
            contains("Zara"),
            ))
  
  View()
  
  
  
