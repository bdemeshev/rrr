library("ggplot2")
library("dplyr")
library("GGally")
library("vcd")
library("psych")
library("Ecdat")
library("broom")
library("lattice")

# философия грамматики графиков
# пакет ggplot2

glimpse(cars)
help(cars)

base_plot <- ggplot() +
  geom_point(data = cars,
    aes(x = speed, y = dist))

base_plot +
  xlab("Скорость машины, миль в час")

plot2 <- base_plot +
  xlab("Скорость машины, миль в час") +
  ylab("Длина тормозного пути, футов") +
  ggtitle("Данные 1920х годов")

base_plot
plot2

model <- lm(data = cars,
            dist ~ speed)

summary(model)
# hat dist = -17.57 + 3.93 * speed

tidy(model)
glance(model)

speed_min <- min(cars$speed)
speed_max <- max(cars$speed)
speed_min
speed_max

cars_pred <- data_frame(
  speed = seq(from = speed_min,
              to = speed_max,
              by = 1)
)
cars_pred
cars_pred <- augment(model,
        newdata = cars_pred)
cars_pred

# лирическое отступление про
# списки, чуланы и мешки
meshok <- list(a = 5,
      v = c(1, 1, 1),
      paket = list())



meshok$krokodil <- diag(9)
meshok$v
meshok[[4]]
meshok[4]
class(meshok[[4]])
# конец лирического отступления

plot2
plot3 <- plot2 +
  geom_line(data = cars_pred,
          color = "blue",
      aes(x = speed, y = .fitted))
plot2
plot3


cars_pred <- mutate(cars_pred,
  left = .fitted - 2 * .se.fit,
  right = .fitted + 2 * .se.fit)
glimpse(cars_pred)

plot4 <- plot3 + geom_ribbon(
  data = cars_pred,
  alpha = 0.3,
  aes(x = speed,
      ymin = left, ymax = right)                  )
plot3
plot4


qplot(data = cars,
      x = speed, y = dist) +
  stat_smooth(method = "lm")

help(swiss)
# матрица диаграмм рассеяния
ggpairs(swiss)
splom(swiss) # другой пакет

library("titanic")
tit <- titanic_train
help("titanic_train")
mosaic(data = tit,
  ~ Pclass + Sex + Survived,
  shade = TRUE)




