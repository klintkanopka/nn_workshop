library(neuralnet)

nn_1 <- neuralnet(versicolor ~ sepal_length + sepal_width + petal_length + petal_width,
                hidden=1,
                lifesign="full",
                rep=1,
                threshold = 0.001,
                learningrate = 0.001,
                linear.output = FALSE,
                data=iris_train)

plot(nn_1)

nn_predictions <- predict(nn_1, 
        newdata = iris_train)
nn_preds <- ifelse(nn_predictions >= 0.5, 1, 0)

# accuracy and confusion matrix on training data
sum(iris_train$versicolor == nn_preds)/nrow(iris_train)
table(iris_train$versicolor, nn_preds)


# out of sample evaluation
nn_predictions <- predict(nn_1, 
                          newdata = iris_test)
nn_preds <- ifelse(nn_predictions >= 0.5, 1, 0)

# accuracy and confusion matrix on out of sample data
sum(iris_test$versicolor == nn_preds)/nrow(iris_test)
table(iris_test$versicolor, nn_preds)



# let's try a deeper network!

nn_2 <- neuralnet(versicolor ~ sepal_length + sepal_width + petal_length + petal_width,
                  hidden=c(3,3,3),
                  lifesign="full",
                  rep=1,
                  threshold = 1e-10,
                  learningrate = 0.001,
                  linear.output = FALSE,
                  data=iris_train)

plot(nn_2)

nn_predictions <- predict(nn_2, 
                          newdata = iris_train)
nn_preds <- ifelse(nn_predictions >= 0.5, 1, 0)

# accuracy and confusion matrix on training data
sum(iris_train$versicolor == nn_preds)/nrow(iris_train)
table(iris_train$versicolor, nn_preds)


# out of sample evaluation
nn_predictions <- predict(nn_2, 
                          newdata = iris_test)
nn_preds <- ifelse(nn_predictions >= 0.5, 1, 0)

# accuracy and confusion matrix on out of sample data
sum(iris_test$versicolor == nn_preds)/nrow(iris_test)
table(iris_test$versicolor, nn_preds)

# let's try an even deeper network!

nn_3 <- neuralnet(versicolor ~ sepal_length + sepal_width + petal_length + petal_width,
                  hidden=c(10, 10, 10, 10, 10),
                  lifesign="full",
                  rep=1,
                  threshold = 1e-10,
                  learningrate = 0.001,
                  linear.output = FALSE,
                  data=iris_train)

plot(nn_3)

nn_predictions <- predict(nn_3, 
                          newdata = iris_train)
nn_preds <- ifelse(nn_predictions >= 0.5, 1, 0)

# accuracy and confusion matrix on training data
sum(iris_train$versicolor == nn_preds)/nrow(iris_train)
table(iris_train$versicolor, nn_preds)
# cool!

# out of sample evaluation
nn_predictions <- predict(nn_3, 
                          newdata = iris_test)
nn_preds <- ifelse(nn_predictions >= 0.5, 1, 0)

# accuracy and confusion matrix on out of sample data
sum(iris_test$versicolor == nn_preds)/nrow(iris_test)
table(iris_test$versicolor, nn_preds)

