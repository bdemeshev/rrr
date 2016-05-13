library("caret")
library("kernlab")
library("e1071")
library("dplyr")

mtcars
help(mtcars)

glimpse(mtcars)

mtcars2 <- mutate(mtcars,
        am = factor(am,
          labels = c("automatic", "manual")))
glimpse(mtcars2)

# SVM с гауссовским ядром и заданными C и sigma

svm <- ksvm(data = mtcars2,
            am ~ hp + mpg + disp + gear,
            kernel = "rbfdot",
            kpar = list(sigma = 1), C = 2)

new_car <- data_frame(
  mpg = 20, disp = 180, gear = 5, hp = 120)
predict(svm, newdata = new_car)

svm_options <- trainControl(classProbs = TRUE)
set.seed(530)
svm_cv <- train(data = mtcars2,
                am ~ hp + mpg + disp + gear,
                method = "svmRadial",
                trControl = svm_options)
svm_cv
predict(svm_cv, newdata = new_car)
predict(svm_cv, newdata = new_car, type = "prob")
