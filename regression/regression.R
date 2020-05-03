library(tidyverse)
library(HistData)
data("GaltonFamilies")

set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

galton_heights %>% 
  summarize(mean(father), sd(father), mean(son), sd(son))

galton_heights %>% ggplot(aes(father, son)) + 
  geom_point(alpha = 0.5)

ggsave("regression/figs/father_son_height.png")

galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)

#Monte Carlo simulation - a random sample of 25 pairs:
B <- 1000
N <- 25
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    summarize(r=cor(father, son)) %>% 
    pull(r)
})
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))

ggsave("regression/figs/cor_distribution.png")

#the expected value of R is the population correlation:
mean(R)
sd(R)

#CLT for a sapmle size of 25:
ggplot(aes(sample=R), data = data.frame(R)) + 
  stat_qq() + 
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))
#note: by increasing N the distribution converges to normal.

ggsave("regression/figs/cor_distribution.png")

#improving estimates of the conditional expectations 
#by defining strata of similar values of x:
conditional_avg <- galton_heights %>% 
  filter(round(father) == 72) %>%
  summarize(avg = mean(son)) %>% 
  pull(avg)
conditional_avg

#the prediction of any heights:
galton_heights %>% mutate(father_strata = factor(round(father))) %>% 
  ggplot(aes(father_strata, son)) + 
  geom_boxplot() + 
  geom_point()

ggsave("regression/figs/son_father_strata.png")

#the regression line:
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)

galton_heights %>% 
  ggplot(aes(father, son)) + 
  geom_point(alpha = 0.5) +
  geom_abline(slope = r * s_y/s_x, intercept = mu_y - r * s_y/s_x * mu_x) 

ggsave("regression/figs/son_father_regression.png")

#regression improves precision:
B <- 1000
N <- 50

set.seed(1983)
conditional_avg <- replicate(B, {
  dat <- sample_n(galton_heights, N)
  dat %>% filter(round(father) == 72) %>% 
    summarize(avg = mean(son)) %>% 
    pull(avg)
})

regression_prediction <- replicate(B, {
  dat <- sample_n(galton_heights, N)
  mu_x <- mean(dat$father)
  mu_y <- mean(dat$son)
  s_x <- sd(dat$father)
  s_y <- sd(dat$son)
  r <- cor(dat$father, dat$son)
  mu_y + r*(72 - mu_x)/s_x*s_y
})

mean(conditional_avg, na.rm = TRUE)
mean(regression_prediction)

sd(conditional_avg, na.rm = TRUE)
sd(regression_prediction)

#bivariate normal distribution:
galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample = son)) +
  facet_wrap( ~ z_father) 

ggsave("regression/figs/bivariate_normal_distribution.png")
