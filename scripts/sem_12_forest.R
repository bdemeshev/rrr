library("randomForest")
library("dplyr")

# goo.gl/zxwS7u
file <- file.choose()

file <- "/Users/boris/Documents/openedu_metrics/week_05/lab_03/flats_moscow.txt"

flats <- read.table(file, header = TRUE,
                    sep = "\t", dec = ".")
glimpse(flats)


bitz_les <- randomForest(data = flats,
                price ~ totsp + livesp + dist)
new <- data_frame(totsp = 60,
                livesp = 40, dist = 10)
predict(bitz_les, newdata = new)
?randomForest

bitz_les <- randomForest(data = flats,
                         importance = TRUE,
              price ~ totsp + livesp + dist)
importance(bitz_les)



ols <- lm(data = flats,
                         price ~ totsp + livesp + dist)
summary(ols)

