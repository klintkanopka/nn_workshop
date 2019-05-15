# first we fit a logistic regression
logit <- glm(versicolor ~ sepal_length + sepal_width + petal_length + petal_width, 
             data=iris_train, 
             family=binomial(link="logit"))

summary(logit)

# training data evaluation
logit_predictions <- predict(logit, 
                             newdata = iris_train, 
                             type="response")
logit_preds <- ifelse(logit_predictions >= 0.5, 1, 0)

# accuracy and confusion matrix on out of sample data
sum(iris_train$versicolor == logit_preds)/nrow(iris_train)
table(iris_train$versicolor, logit_preds)
# kind of crappy?

# test data evaluation
logit_predictions <- predict(logit, 
                             newdata = iris_test, 
                             type="response")
logit_preds <- ifelse(logit_predictions >= 0.5, 1, 0)

# accuracy and confusion matrix on out of sample data
sum(iris_test$versicolor == logit_preds)/nrow(iris_test)
table(iris_test$versicolor, logit_preds)
# also not great?