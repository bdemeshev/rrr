# заголовки столбцов - нет
# sep (разделитель данных) ;
# dec (разделитель целых) .
filename <- file.choose()
filename <- "/Users/boris/Downloads/outlier_search.csv"
data <- read.table(filename,
      header = FALSE,
      sep = ";",
      dec = ".")

library("dplyr") # агрегирование/консолидирование
library("reshape2") # длинные/широкие крокодилы
library("ggplot2")

nrow(data)
ncol(data)

# посчитать меру разброса (стандартная ошибка или IQR)
# [mean - 3 * sd; mean + 3 * sd]
# [median - 1.5 * IQR; median + 1.5 * IQR]


z <- c(5, 6, -3, 2, 8)
mean(z)
median(z)
sd(z)
IQR(z)

v <- rnorm(1000)
v
tail(v)
qplot(v)

# install.packages("ggplot2")

set.seed(13)
v <- rnorm(1000)
tail(v)

?rnorm

# способ 1: через цикл по рабоче-крестьянски
n_row <- nrow(data)
out_border <- data_frame(left = rep(NA, n_row),
                         right = rep(NA, n_row))
head(out_border)

for (i in 1:10) {
  cat("Маша нашла ", i, " сушку\n")
}


stroka <- data[42, ]
sd(stroka)
sd(stroka, na.rm = TRUE)
left <- mean(unlist(stroka), na.rm = TRUE) -
  3 * sd(stroka, na.rm = TRUE)
left
mean(unlist(stroka), na.rm = TRUE)
str(stroka)

for (i in 1:n_row) {
  stroka <- data[i, ]
  out_border$left[i] <- mean(unlist(stroka), na.rm = TRUE) -
    3 * sd(stroka, na.rm = TRUE)
  out_border$right[i] <- mean(unlist(stroka), na.rm = TRUE) +
    3 * sd(stroka, na.rm = TRUE)
}

# способ 2: через длинных и широких крокодилов

data2 <- t(data)
data2 <- data %>% t()

data2
glimpse(data2)
?glimpse
str(data2)
data3 <- as.data.frame(data2)

data4 <- select(data3, -V1)
nrow(data2)
nrow(data)

data_long <- melt(data4)
glimpse(data_long)

out_border2 <- data_long %>% group_by(variable) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE)) %>%
  mutate(left = mean - 3 * sd, right = mean + 3 * sd )

out_border2

# способ 3: с применением sd/mean по столбикам

M <- matrix(rnorm(100), nrow = 20)
max(M) # для всей матрицы
mean(M)

?apply
apply(M, 1, max) # max по строкам
apply(M, 2, max) # max по столбцам

data2 <- t(data) # data2 - это матрица
data3 <- as.data.frame(data2) # превратили матрицу в data.frame
data4 <- select(data3, -V1) # удалили V1

all_sd <- apply(data4, 2, function(x) sd(x, na.rm = TRUE))
all_mean <- apply(data4, 2, function(x) mean(x, na.rm = TRUE))

out_border_3 <- data_frame(sd = all_sd, mean = all_mean,
                           left = mean - 3 * sd,
                           right = mean + 3 *sd)
out_border_3
