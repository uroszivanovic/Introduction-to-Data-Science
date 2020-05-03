library(tidyverse)
library(dslabs)
data("mnist_27")
mnist_27$test%>% ggplot(aes(x_1, x_2, color = y)) +  geom_point()

ks <- seq(3, 251, 2)

accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(y_hat, mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(y_hat, mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(k = k, train = train_error, test = test_error)
})

accuracy %>% 
  pivot_longer(c("train", "test"), 
               names_to = "dataset", values_to = "accuracy") %>% 
  ggplot(mapping = aes(k, accuracy, colour = dataset)) + 
  geom_line() +
  geom_point()
  
ggsave("machine-learning/figs/knn.png")

ks[which.max(accuracy$test)]
max(accuracy$test)
