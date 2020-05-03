library(tidyverse)
library(dslabs)
data(polls_us_election_2016)

polls <- polls_us_election_2016 %>% 
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+","A","A-","B+") | is.na(grade)))

polls <- polls %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

#last reported results:
one_poll_per_pollster <- polls %>% group_by(pollster) %>% 
  filter(enddate == max(enddate)) %>%
  ungroup()

qplot(spread, data = one_poll_per_pollster, binwidth = 0.01)

ggsave("statistical-models/figs/last_poll(s)_spread.png")

#confidence interval based on data-driven model:
results <- one_poll_per_pollster %>% 
  summarize(avg = mean(spread), 
            se = sd(spread) / sqrt(length(spread))) %>% 
  mutate(start = avg - 1.96 * se, 
         end = avg + 1.96 * se) 

round(results * 100, 1)



