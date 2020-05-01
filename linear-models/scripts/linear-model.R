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

rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss))

ggsave("linear_models/figs/rss.png")

#lm function:
fit <- lm(son ~ father, data = galton_heights)
fit$coef
summary(fit)

#lse - radnom variables:
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% 
    .$coef 
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

qplot(lse$beta_0, geom = "histogram", binwidth = 5, color = I("black"))
ggsave("linear_models/figs/beta_0.png")

qplot(lse$beta_1, geom = "histogram", binwidth = 0.10, color = I("black"))
ggsave("linear_models/figs/beta_1.png")

sample_n(galton_heights, N, replace = TRUE) %>% 
  lm(son ~ father, data = .) %>% 
  summary %>% .$coef

lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

galton_heights %>% ggplot(aes(son, father)) +
  geom_point() +
  geom_smooth(method = "lm")

ggsave("linear_models/figs/confidence_intervals.png")

fit <- galton_heights %>% lm(son ~ father, data = .) 
y_hat <- predict(fit, se.fit = TRUE)

#from broom package:
broom::tidy(fit)
broom::tidy(fit, conf.int = TRUE)
broom::glance(fit)
broom::augment(fit) %>% 
  ggplot() +
  geom_point(aes(father, son)) + 
  geom_line(aes(father, .fitted), col = "blue")

ggsave("linear_models/figs/broom_augment.png")
