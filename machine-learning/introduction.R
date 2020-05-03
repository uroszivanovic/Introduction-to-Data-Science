library(tidyverse)
library(caret)
library(dslabs)
data(heights)
y <- heights$sex
x <- heights$height

#splitting data into the training and test sets:
set.seed(2007)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

#guessing algorithm:
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>%
  factor(levels = levels(test_set$sex))

mean(y_hat == test_set$sex)

#EDA to construct better algorithm:
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

y_hat <- ifelse(x > 62, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))

mean(y == y_hat)

#optimizing the cutoff using only the training set:
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})

max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

#test the cutoff on the test set:
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

#the confusion matrix:
table(predicted = y_hat, actual = test_set$sex)

#the accuracy by sex:
test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))

#prevalence:
prev <- mean(y == "Male")
prev

#sensitivity and specificity:
cm <- confusionMatrix(data = y_hat, reference = test_set$sex)
cm$overall[["Accuracy"]]
cm$byClass[c("Sensitivity", "Specificity", "Prevalence")]

#balanced accuracy and f-1 score:
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

max(F_1)
best_cutoff <- cutoff[which.max(F_1)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)

#ROC and precision-recall curves:
p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)
#note: due to the bias in the sample, 
#guessing Male with higher probability would give us higher accuracy,
#but his would come at the cost of lower sensitivity!

probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

guessing %>% ggplot() + 
  geom_point(aes(x = FPR, y = TPR)) + 
  geom_line(aes(x = FPR, y = TPR))

ggsave("machine-learning/figs/ROC.png")
