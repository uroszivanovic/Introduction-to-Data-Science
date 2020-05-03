library(tidyverse)
library(caret)
library(dslabs)
data("mnist_27")

mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

fit <- mnist_27$train %>%
  mutate(y = ifelse(y==7, 1, 0)) %>%
  lm(y ~ x_1 + x_2, data = .)


