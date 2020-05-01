library(tidyverse)
library(dslabs)
data(heights)

index <- heights$sex == "Male"
x <- heights$height[index]

m <- mean(x)
s <- sd(x)
c(average = m, sd = s)

z <- scale(x)

mean(abs(z) < 2)

q <- qnorm(0.975, m, s)
mean(x<=q)

#QQ-plot:
p <- seq(0.05, 0.95, 0.05)
sample_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()

ggsave("data-visualization/figs/plot_1.png")

#the code that is produce using standard units:
sample_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p) 
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()

ggsave("data-visualization/figs/plot_2.png")

#exp. with ggplot2:
heights %>% filter(sex == "Male") %>%
  ggplot(aes(sample = scale(height))) + 
  geom_qq() +
  geom_abline()

ggsave("data-visualization/figs/plot_3.png")
