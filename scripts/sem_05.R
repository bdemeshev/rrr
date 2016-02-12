library("dplyr")


filename <- "~/Downloads/ege/russian.txt"


create_df_from_ege_file <- function(filename) {
  ege_str <- readLines(filename)
  # head(ege_str, 10)
  # tail(ege_str)
  # Encoding(ege_str) <- "UTF-8"
  empty <- ege_str[2]
  # empty
  ege_non_empty <- ege_str[!ege_str == empty]
  # ege_non_empty

  # head(ege_non_empty)

  ege_matrix <- matrix(ege_non_empty,
                       nrow = length(ege_non_empty)/3,
                       byrow = TRUE)
  # head(ege_matrix)
  ege_df <- data.frame(ege_matrix,
                       stringsAsFactors = FALSE)
  # head(ege_df)
  # str(ege_df)

  ege_df[1, 1] <- "1"

  # str(ege_df)
  # colnames(ege_df)

  colnames(ege_df) <-
    c("rating", "school", "grade")
  # str(ege_df)

  # с помощью базового R
  # ege_df$rating <- as.numeric(ege_df$rating)
  # ege_df$grade <- as.numeric(ege_df$grade)

  # с помощью пакета dplyr
  ege_df <- mutate(ege_df,
                   rating = as.numeric(rating),
                   grade = as.numeric(grade))
  return(ege_df)
}

ege_rus <- create_df_from_ege_file(
  "~/Downloads/ege/russian.txt")
head(ege_rus)

ege_math <- create_df_from_ege_file(
  "~/Downloads/ege/math.txt")
head(ege_math)

ege_phys <- create_df_from_ege_file(
  "~/Downloads/ege/physics.txt")
head(ege_phys)
tail(ege_phys)

# head(ege_df)
# str(ege_df)


x <- 5
plus_six <- function(a) {
  x <- 6
  return(a + x)
}
x

plus_six(10)

glimpse(ege_rus)
ege_rus <- select(ege_rus,
                  school,
        russian = grade)
glimpse(ege_rus)
ege_math <- select(ege_math,
                  school,
        math = grade)
ege_phys <- select(ege_phys,
                   school,
          physics = grade)
all_data <-
  full_join(ege_rus, ege_math,
            by = "school")
all_data <- full_join(all_data,
          ege_phys, by = "school")
?full_join


library("psych")
desc_stats <- describe(all_data)
desc_stats[2, c("mean", "sd", "max")]
?describe
# summary(all_data) # old school way

cor(all_data[, 2:4],
    use = "pairwise")

library("broom")
model <- lm(data = all_data,
      russian ~ math + physics)
glance(model)
tidy(model)
summary(model)

new_ege <- data_frame(
  name = c("Vovochka", "Mashenka"),
  math = c(34, 58),
  physics = c(15, 90)
)
new_ege
new_ege_pred <- augment(model,
            newdata = new_ege)
new_ege_pred

all_data_augmented <- augment(model,
          all_data)
glimpse(all_data_augmented)



# факторная переменная :)
z <- c("Male", "Female", "Male")
str(z)
z[2] <- "ET"
z

w <- factor(c("Male",
              "Female", "Male"))
str(w)
w == 2
w == "Male"
w[2] <- "ET"
w
