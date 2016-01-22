# встреча 2

# вектор
x <- c(5, 6, -3, 14.5)
x
y <- c(-2, 7, 9, -9)
x + y
z <- c(1, 2)
x + z
x + 10
w <- c(1, 2, 3)
w + x
r <- 20:50
r
privet <- rep("Мартышка", 10)
privet

privet <- rep('Мартышка', 10)
privet

text <- "Тортик 'Наполеон'"
text
text2 <- 'Тортик "Наполеон"'
text2

head(r)
tail(r)
tail(r, 10)
r[5]

r[c(3, 5, 7)]

r[5:10]

x
x[2]
x[-2]
x[-c(2, 3)]

tail(x, 1)
head(x, -1)
x[-1]

length(x)
NA
y <- c(5, 6, NA, 11, 5)
u <- 0/0
w <- 7/0
u
w
w + 9
Inf - Inf
1/Inf



atan(Inf)
pi/2

index <- seq(from = 5, to = 20, by = 3)
seq(5, 20, 3)
r[index]

mean(x)
mean(y)
mean(y, na.rm = TRUE)

sum(x)
prod(x)
sd(x)
x

x > 5.5
x[x > 5.5]
which(x > 5.5)
x[(x > 5.5) & (x < 10)]
x[!(x > 5.5)]
x[ (x > 5) & (x < 0)]
6 == 7
4 == 3 + 1
0.4 == 0.3 + 0.1
0.4 - 0.1 == 0.3

library("readr")
library("readxl")
library("dplyr")

x
q <- 1:4
q
df <- data_frame(id = q,
                 temp = x,
                 temp_sq = temp ^ 2)
df

df
head(df, 3)
df[3, 2]
df[3, 2] <- -16
df$temp
df$temp[3]


df2 <- arrange(df, abs(temp - 3))

df3 <- filter(df, id < 3, temp > 5)
df3

cos(sin(5))

5 %>% sin() %>% cos()

df4 <- mutate(df, temp3 = temp ^ 3)
df4
df5 <- df %>% arrange(temp) %>%
    filter(temp > 0) %>%
    mutate(temp3 = temp ^ 3)
df5
df5_bis <- mutate(filter(arrange(df, temp),
                         temp > 0),
                  temp3 = temp ^ 3)

step_1 <- arrange(df, temp)
step_2 <- filter(step_1, temp > 0)
df5_2bis <- mutate(step_2, temp3 = temp ^ 3)
