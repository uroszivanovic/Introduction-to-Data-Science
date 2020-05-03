#1. US Murders:

url <- paste0("https://en.wikipedia.org/w/index.php?title=",
              "Gun_violence_in_the_United_States_by_state",
              "&direction=prev&oldid=810166167")

library(tidyverse)
library(rvest)
h <- read_html(url)

tab <- h %>% html_nodes("table")

tab <- tab[[1]] %>% html_table
class(tab)

tab <- tab %>% setNames(c("state", "population", "total", "murder_rate")) 
head(tab)

#2. JSON:

library(jsonlite)
citi_bike <- fromJSON("http://citibikenyc.com/stations/json")

names(citi_bike)

citi_bike$stationBeanList %>% as_tibble()
