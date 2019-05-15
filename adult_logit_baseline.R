# first we fit a logistic regression
logit <- glm(income_bin ~ age + education_num + female + hours_per_week + white, 
    data=adult_train, 
    family=binomial(link="logit"))

summary(logit)

# training data evaluation
logit_predictions <- predict(logit, 
                             newdata = adult_train, 
                             type="response")
logit_preds <- ifelse(logit_predictions >= 0.5, 1, 0)

# accuracy and confusion matrix on out of sample data
sum(adult_train$income_bin == logit_preds)/nrow(adult_train)
table(adult_train$income_bin, logit_preds)


# test data evaluation
logit_predictions <- predict(logit, 
                             newdata = adult_test, 
                             type="response")
logit_preds <- ifelse(logit_predictions >= 0.5, 1, 0)

# accuracy and confusion matrix on out of sample data
sum(adult_test$income_bin == logit_preds)/nrow(adult_test)
table(adult_test$income_bin, logit_preds)
