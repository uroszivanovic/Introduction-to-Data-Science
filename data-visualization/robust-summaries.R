library(tidyverse)
library(dslabs)
data(outlier_example)
str(outlier_example)

#outlier:
mean(outlier_example)

sd(outlier_example)

boxplot(outlier_example)

#The non-parametric estimate of the standard deviation:
IQR(outlier_example)/1.349

q3 <- qnorm(0.75)
q1 <- qnorm(0.25)
iqr <- q3 - q1
r <- c(q1 - 1.5*iqr, q3 + 1.5*iqr)
r

#far out outliers:
max_height <- quantile(outlier_example, 0.75) + 3*IQR(outlier_example)
max_height

x <- outlier_example[outlier_example < max_height]
qqnorm(x)
qqline(x)

#MAD:
mad(outlier_example)

#case study:
data("reported_heights")

reported_heights <- reported_heights %>%
  mutate(original_heights = height, height = as.numeric(height))

reported_heights %>% filter(is.na(height)) %>%  head()

reported_heights <- filter(reported_heights, !is.na(height))

reported_heights %>% 
  group_by(sex) %>%
  summarize(average = mean(height), sd = sd(height),
            median = median(height), MAD = mad(height))

boxplot(reported_heights$height~reported_heights$sex)

reported_heights %>% arrange(desc(height)) %>% top_n(10, height)

whisker <- 3*IQR(reported_heights$height)
max_height <- quantile(reported_heights$height, .75) + whisker
min_height <- quantile(reported_heights$height, .25) - whisker
reported_heights %>% 
  filter(!between(height, min_height, max_height)) %>% 
  select(original_heights) %>%
  head(n=10) %>% pull(original_heights)
