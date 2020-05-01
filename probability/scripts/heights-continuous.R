library(tidyverse)
library(dslabs)
data(heights)

x <- heights %>% filter(sex=="Male") %>% pull(height)
n <- length(x)
m <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, m, s)

B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(n, m, s)
  max(simulated_data)
})

mean(tallest >= 7*12)

qplot(tallest, bins = 10)

ggsave("probability/figs/heights_plot.png")
