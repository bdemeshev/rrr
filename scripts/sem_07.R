library("titanic") # набор данных
library("psych") # описательные статистики/график
library("ggplot2") # графики
library("vcd") #  мозаичный график
library("broom") # стандартизация всего в таблицы
library("dplyr") # обработка данных
library("GGally") # ggpairs
library("car")

help(swiss)
glimpse(swiss)
pairs.panels(swiss)

tit <- titanic_train
help("titanic_train")
glimpse(tit)

tit2 <- select(tit,
  -PassengerId, -Name,
  -Ticket, -Cabin,
  -Embarked)
glimpse(tit2)

table(tit2$SibSp)
table(tit2$Parch)

tit3 <- mutate(tit2,
        Survived = factor(Survived),
        Pclass = factor(Pclass),
        Sex = factor(Sex))
tit3 <- mutate_each(tit2,
  "factor", Survived, Pclass, Sex)
ggpairs(tit3)
?ggpairs

# немного про факторные переменные

x <- c(1, 5, 7)
y <- c("male", "female", "male")
str(x)
x2 <- as.integer(c(1, 5, 7))
str(x2)

mix <- c(5, "male", sqrt(5))
x + x
z <- factor(c("male", "female", "male"))
str(z)
z + z
levels(z)

w <- c("middle", "bachelor", "master",
              "bachelor")
w
str(w)
w2 <- factor(w, ordered = TRUE)
str(w2)
w2 <- factor(w,
  levels = c("middle", "bachelor", "master"),
  ordered = TRUE)
str(w2)

# recoding factor variables:
# without car:

table(tit3$Parch)
tit4 <- mutate(tit3,
    Parch2 = ifelse(Parch %in% 3:8, "3+", Parch),
    SibSp2 = ifelse(SibSp %in% 3:8, "3+", SibSp))
tit5 <- mutate_each(tit4, "factor",
          Parch2, SibSp2)
tit6 <- select(tit5, -SibSp, -Parch)
ggpairs(tit6)

# recoding levels with car package
tit4 <- mutate(tit3,
     Parch2 = recode(Parch, "2:8='2+'"),
     SibSp2 = recode(SibSp, "2:8='2+'"))
tit5 <- mutate_each(tit4, "factor",
                    Parch2, SibSp2)
tit6 <- select(tit5, -SibSp, -Parch)
ggpairs(tit6)




