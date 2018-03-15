library(dpylr)
library(zoo)

#need to read data if it didn't already exist in global enviroment

yrmo_huskerMax <- mutate(huskerMax, ym = as.yearmon(time)) %>% 
  filter(time > "2012-01-01")


yrmo_huskerMax <- yrmo_huskerMax  %>%
  group_by(ym) %>% 
  summarise(count = n(),
  #mean = mean(afinn),
  #mean2 = mean(syuzhet)
  )

yrmo_footballMax <- mutate(footballMax, ym = as.yearmon(time)) %>% 
  filter(time > "2012-01-01")


yrmo_footballMax <- yrmo_footballMax  %>%
  group_by(ym) %>% 
  summarise(count = n(),
            mean = mean(afinn),
            mean2 = mean(syuzhet))
