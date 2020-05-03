library(tidyverse)
library(dslabs)
data(polls_us_election_2016)

polls <- polls_us_election_2016 %>% 
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+","A","A-","B+") | is.na(grade)))

polls <- polls %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

#estimated spread:
d_hat <- polls %>% 
  summarize(d_hat = sum(spread * samplesize) / sum(samplesize)) %>% 
  pull(d_hat)

#standard error:
p_hat <- (d_hat+1)/2 
moe <- 1.96 * 2 * sqrt(p_hat * (1 - p_hat) / sum(polls$samplesize))
moe

#the data does not appear to be normally distributed:
polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = .01)

ggsave("statistical-models/figs/polls_spread.png")
