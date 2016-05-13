

mtcars
View(mtcars)

library("dplyr")
library("ggplot2")

set.seed(42)
mtcars_km <- kmeans(mtcars, centers = 3)
mtcars_km$cluster

mtcars_aug <- data.frame(mtcars,
                  cluster = mtcars_km$cluster)
glimpse(mtcars_aug)

mtcars_dist <- dist(mtcars)
nrow(mtcars)

mtcars_hc <- hclust(mtcars_dist)
plot(mtcars_hc)

mtcars_hc_clusters <- cutree(mtcars_hc, k = 3)
mtcars_aug <- data.frame(mtcars,
            cluster_km = mtcars_km$cluster,
            cluster_hc = mtcars_hc_clusters)
glimpse(mtcars_aug)

table(mtcars_aug$cluster_km, mtcars_aug$cluster_hc)

