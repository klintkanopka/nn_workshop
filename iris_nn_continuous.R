l_mod <- lm(sepal_length ~ sepal_width + petal_length + petal_width, 
            data=iris_train)

cont_nn <- neuralnet(sepal_length ~ sepal_width + petal_length + petal_width,
                   hidden=c(25, 25, 25, 25, 25),
                   lifesign="full",
                   rep=1,
                   stepmax = 1e6,
                   threshold = 1e-5,
                   learningrate = 0.01,
                   linear.output = TRUE,
                   data=iris_train)

ggplot() +
  geom_point(aes(x=jitter(iris_train$sepal_length), 
                 y=jitter(predict(cont_nn,
                           newdata = iris_train,
                           type="response"))),
             color="darkorchid", alpha = 0.3) +
  ggtitle("Neural Predicted Sepal Lengths - Training Set (Jittered)") +
  xlab("Actual Sepal Length") +
  ylab("Neural Predicted Sepal Length") +
  theme_bw()

linear_pred <- predict(l_mod,
                       newdata = iris_test,
                       type="response")
neural_pred <- predict(cont_nn,
                       newdata = iris_test,
                       type="response")

ggplot() +
  geom_point(aes(x=iris_test$sepal_length, y=linear_pred, color="linear"), 
             alpha = 0.6) +
  geom_point(aes(x=iris_test$sepal_length, y=neural_pred, color="neural"), 
             alpha = 0.6) +
  geom_abline(intercept=0, slope=1, lty=2, alpha=0.4) +
  scale_color_manual(name="", 
                     values=c("linear" = "orange", "neural" = "blue")) +
  ggtitle("Comparison of Neural vs. Linear Prediction - Test Set") +
  xlab("Actual Sepal Length") +
  ylab("Predicted Sepal Length") +
  theme_bw()

# WTF? I THOUGHT THIS WAS SUPPOSED TO WORK REALLY WELL!
# Well, this is what's called "overfitting!" It's (one of) the dangers
# of using a huge NN with very little training data. Let's try something 
# a little smaller.

cont_nn <- neuralnet(sepal_length ~ sepal_width + petal_length + petal_width,
                     hidden=c(3, 3, 3),
                     lifesign="full",
                     rep=1,
                     stepmax = 1e6,
                     threshold = 0.02,
                     learningrate = 0.01,
                     linear.output = TRUE,
                     data=iris_train)

ggplot() +
  geom_point(aes(x=jitter(iris_train$sepal_length), 
                 y=jitter(predict(cont_nn,
                                  newdata = iris_train,
                                  type="response"))),
             color="darkorchid", alpha = 0.3) +
  ggtitle("Neural Predicted Sepal Lengths - Training Set (Jittered)") +
  xlab("Actual Sepal Length") +
  ylab("Neural Predicted Sepal Length") +
  theme_bw()

# looks worse...

neural_pred <- predict(cont_nn,
                       newdata = iris_test,
                       type="response")

ggplot() +
  geom_point(aes(x=iris_test$sepal_length, y=linear_pred, color="linear"), 
             alpha = 0.6) +
  geom_point(aes(x=iris_test$sepal_length, y=neural_pred, color="neural"), 
             alpha = 0.6) +
  geom_abline(intercept=0, slope=1, lty=2, alpha=0.4) +
  scale_color_manual(name="", 
                     values=c("linear" = "orange", "neural" = "blue")) +
  ggtitle("Comparison of Neural vs. Linear Prediction - Test Set") +
  xlab("Actual Sepal Length") +
  ylab("Predicted Sepal Length") +
  theme_bw()

# better? With wiggling and luck, you might do better. In reality, you should
# just find more data.
