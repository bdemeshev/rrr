# форматы хранения данных
# * csv
# * excel
# * eviews/stata/spss/gretl/...
# * базы данных

# csv - comma separated values
# rost, ves, pol
# 150, 50.5, F
# 180, 80.5, M
# ...

# три отличия от стандарта
# * есть ли заголовок в первой строке
# * как отделяется дробная часть от целой
# * как отделяются наблюдения

# flats_moscow
# заголовок есть
# десятичный разделитель = .
# разделитель наблюдений = табуляция

getwd()
setwd("~/Downloads")
# dir.create("subfolder") # создать папку

flats <- read.table("flats_moscow.txt",
    header = TRUE, dec = ".", sep = "\t")

library("dplyr") # манипуляции с данными

library("readr") # быстрые команды для чтения
library("readxl") # excel
library("haven") # spss/sas/stata
library("hexView") # eviews
library("foreign") # stata/spss/... (более старый пакет, чем haven)

library("reshape2") # длинная таблица <-> широкая таблица

library("broom") # стандартизация вывода результатов разных моделей

test <- read_excel("test_excel.xlsx")

# отберём переменные и сменим имя с bb на privet
subtest <- select(test, aaa, privet = bb)
?read_csv

# 0. бросить взгляд на набор данных
# glimpse
# 1. отбор переменных
# select
# 2. создание переменных
# mutate
# 3. отбор наблюдений
# filter
# 4. группировка
# group_by
# 5. описание групп (???)
# summarise
# 6. сортировка
# arrange
# 7. слияние таблиц
# left_join
# 8. перевод длинной таблицы в широкую
# dcast
# 9. перевод широкой таблицы в длинную
# melt

# бросим взгляд на табличку с данными
glimpse(flats)

# создание переменных
flats2 <- mutate(flats,
  othersp = totsp - livesp - kitsp,
  log_totsp = log(totsp))

# отбор переменных
flats3 <- select(flats2, -n)

# отбор наблюдений
flats4 <- filter(flats3,
     totsp > 60, dist < 10, brick == 1)

# ! --- НЕ
# | --- ИЛИ
# & --- И

flats5 <- filter(flats,
                 totsp > 70 | totsp < 50)

# стратегия разделяй, властвуй и соединяй!
# по каждому району, по каждому типу (кирпичные/некирпичные)
# посчитаем среднюю стоимость, среднюю площадь
# выборочное стандартное отклонение стоимости и площади

# выборочное среднее арифметическое
mean(flats3$price)
# выборочное стандартное отклонение
sd(flats3$price)

desc_table <- flats3 %>% group_by(code, brick) %>%
   summarise(av_price = mean(price),
             av_totsp = mean(totsp),
             sd_price = sd(price),
             sd_totsp = sd(totsp))

# хочу такую таблицу со средней ценой:
# район кирп/на 1ом  кирп/не на 1ом некирп/на 1ом некирп/не на 1ом
#   1      24234      234234234234     32423423    423423423432

# шаг 1. сначала делаем длинную таблицу
# район кирпичность на1ом средняя_цена
long_table <- flats3 %>%
  group_by(code, brick, floor) %>%
  summarise(av_price = mean(price))

# шаг 2. превратим длинную в широкую
wide_table <- dcast(long_table,
        code ~ brick + floor,
        value.var = "av_price")

# метод наименьших квадратов
# константа добавляется автоматически
model <- lm(data = flats3,
  price ~ livesp + kitsp + othersp + brick)
summary(model)


# если нужно исключить константу
model_noconst <- lm(data = flats3,
  price ~ 0 + livesp + kitsp + othersp + brick)

# пакет broom
# три уровня информации о модели
# glance - модель с высоты птичьего полёта
glance(model)

# tidy - параметры модели
tidy(model)

# augment - дополнение исходных данных
flats6 <- augment(model, flats3)

# отсортируем по убыванию ошибки прогноза
flats7 <- arrange(flats6, -abs(.resid))







