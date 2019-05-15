mc_nn <- neuralnet(species ~ sepal_length + sepal_width + petal_length + petal_width,
                  hidden=c(5, 5, 5),
                  lifesign="full",
                  rep=1,
                  threshold = 1e-10,
                  learningrate = 0.001,
                  linear.output = FALSE,
                  data=iris_train)

nn_pred <- predict(mc_nn, newdata = iris_test, type="response")

class_pred <- c()
for (i in 1:nrow(nn_pred)){
  row <- nn_pred[i,]
  class_pred[i] <- which(row == max(row))
}

class_act <- case_when(
  iris_test$setosa == 1 ~ 1,
  iris_test$versicolor == 1 ~ 2,
  iris_test$virginica == 1 ~ 3
)

# accuracy and confusion matrix:
sum(class_act == class_pred)/length(class_act)
table(class_act, class_pred)
