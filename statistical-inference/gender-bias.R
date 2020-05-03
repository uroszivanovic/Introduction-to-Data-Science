library(tidyverse)
library(dslabs)
data("research_funding_rates")

totals <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(sum) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women) 

totals %>% summarize(percent_men = yes_men/(yes_men+no_men),
                     percent_women = yes_women/(yes_women+no_women))

rate <- totals %>%
  summarize(percent_total = 
              (yes_men + yes_women)/
              (yes_men + no_men +yes_women + no_women)) %>%
  pull(percent_total)
rate

two_by_two <- data.frame(awarded = c("no", "yes"), 
                         men = c(totals$no_men, totals$yes_men),
                         women = c(totals$no_women, totals$yes_women))
two_by_two

#chi-square test:
data.frame(awarded = c("no", "yes"), 
           men = round((totals$no_men + totals$yes_men) * c(1 - rate, rate)),
           women = round((totals$no_women + totals$yes_women) * c(1 - rate, rate)))

#how likely it is to see a deviation this large:
chisq_test <- two_by_two %>% select(-awarded) %>% chisq.test()
chisq_test$p.value

#odds ratio:
odds_men <- with(two_by_two, (men[2]/sum(men)) / (men[1]/sum(men)))
odds_men

odds_women <- with(two_by_two, (women[2]/sum(women)) / (women[1]/sum(women)))
odds_women

odds_men / odds_women

#confidence interval for odds ratio:
log_or <- log(odds_men / odds_women)
se <- two_by_two %>% select(-awarded) %>%
  summarize(se = sqrt(sum(1/men) + sum(1/women))) %>%
  pull(se)
ci <- log_or + c(-1,1) * qnorm(0.975) * se
ci

