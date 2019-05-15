library(tidyverse)

iris_url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"

iris <- read.csv(iris_url, 
                 strip.white = TRUE, 
                 header = FALSE) %>%
  rename("sepal_length" = V1,
         "sepal_width" = V2,
         "petal_length" = V3,
         "petal_width" = V4,
         "species" = V5) %>%
  mutate(
    setosa = ifelse(species == "Iris-setosa", 1, 0),
    versicolor = ifelse(species == "Iris-versicolor", 1, 0),
    virginica = ifelse(species == "Iris-virginica", 1, 0)
    )

# train/test split
p_train <- 0.8
idx <- 1:nrow(iris)
train_idx <- sample(idx, 
                    size= p_train*nrow(iris),
                    replace = FALSE)
test_idx <- idx[!(idx %in% train_idx)]

iris_train <- iris[train_idx,]
iris_test <- iris[test_idx,]
