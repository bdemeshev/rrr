library("ggplot2")
library("dplyr")
library("glmnet")


cars
qplot(data = cars, x = speed, y = dist)

ols_lin <- lm(data = cars, dist ~ speed)
summary(ols_lin)

cars2 <- mutate(cars,
  speed2 = speed^2,
  speed3 = speed^3,
  speed4 = speed^4)
glimpse(cars2)
ols <- lm(data = cars2,
      dist ~ speed + speed2 + speed3 + speed4)
summary(ols)

# для glmnet требуется отдельно задать
# X и y
X <- model.matrix(data = cars2,
    ~ 0 + speed + speed2 + speed3 + speed4)
head(X)
y <- cars2$dist

# корреляции очень высокие!
cor(cars2)

# LASSO
# компьютер сам переберёт много разных лямбд
lasso <- glmnet(X, y)
lasso
coef(lasso, s = 1) # LASSO для lambda = 1

coef(lasso, s = 10) # LASSO для lambda = 10

plot(lasso, xvar = "dev", label = TRUE)
plot(lasso, xvar = "lambda", label = TRUE)
plot(lasso, xvar = "norm", label = TRUE)

# интуитивно: lambda = 0.2
cars2

newdata <- data_frame(speed = c(30, 60),
                      speed2 = speed^2,
                      speed3 = speed^3,
                      speed4 = speed^4)
newdata
predict(ols, newdata)

predict(lasso, as.matrix(newdata), s = 0.2)

# подбор оптимального лямбда с помощью кросс-валидации
cv_lasso <- cv.glmnet(X, y)
cv_lasso$lambda.min
coef(cv_lasso, s = "lambda.min")

plot(cv_lasso)
cv_lasso$lambda.1se
coef(cv_lasso, s = "lambda.1se")

predict(cv_lasso,
        as.matrix(newdata),
        s = "lambda.1se")
predict(cv_lasso,
        as.matrix(newdata),
        s = "lambda.min")
predict(cv_lasso,
        as.matrix(newdata),
        s = 0.2)

# гребневая регрессия
ridge <- glmnet(X, y, alpha = 0)
ridge

?cv.glmnet
