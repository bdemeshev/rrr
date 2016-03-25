# goo.gl/zxwS7u

library("dplyr")
library("quantreg")
library("ggplot2")
library("broom")

# как загрузить данные из текстового файла для чайников
# tools --- import data set --- from file
flats <- read.delim("~/Downloads/flats_moscow.txt")

# by hands:
flats <- read.table("~/Downloads/flats_moscow.txt",
  header = TRUE, sep = "\t", dec = ".")

# смотрим всё ли загрузилось
glimpse(flats)

# введем новую переменную
flats2 <- mutate(flats, othersp = totsp - livesp - kitsp)

# обычная парная регрессия:
ols <- lm(data = flats2, price ~ totsp)
summary(ols)
tidy(ols)

# медианная регрессия
med <- rq(data = flats2, price ~ totsp, tau = 0.5)
summary(med)

# квантильная регрессия
q_reg <- rq(data = flats2, price ~ totsp,
            tau = c(0.1, 0.9))
summary(q_reg)

plot0 <- qplot(data = flats2, x = totsp, y = price)
plot0
plot1 <- plot0 +
  ggtitle("Стоимость квартир в Москве в 2001 (?) г.") +
  xlab("Общая площадь (м^2)") +
  ylab("Цена (тысяч у.е.)")
plot1


# так разорвать строку можно:
5 *
  7

# так нельзя:
5
 * 7

plot2 <- plot1 + stat_smooth(method = "lm", se = FALSE)
plot2
plot3 <- plot2 + stat_smooth(method = "rq", se = FALSE,
          colour = "red", size = 0.7, linetype = "dotted",
          method.args = list(tau = 0.1) ) +
          stat_smooth(method = "rq", se = FALSE,
              colour = "red", size = 0.7, linetype = "dotted",
              method.args = list(tau = 0.9) )
plot3

####

