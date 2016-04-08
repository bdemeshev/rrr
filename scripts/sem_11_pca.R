library("HSAUR")
devtools::install_github("vqv/ggbiplot") #, ref = "experimental")

head(heptathlon)
help("heptathlon")

h <- heptathlon[, -8]
h_pca <- prcomp(h, scale. = TRUE)
pc <- h_pca$x
head(pc)
summary(h_pca)
lambda <- diag(crossprod(pc))
lambda
plot(h_pca)

alpha <- h_pca$rotation
alpha

pc1 <- pc[, 1]
score <- heptathlon[, 8]
cor(pc1, score)
biplot(h_pca)

solve(alpha)
Z <- pc
Z[, 2:7] <- 0
Z
X_hat <- Z %*% solve(alpha)
X_hat[, 1]
scale(h)[, 1]

compare <-
 cbind(X_hat[, 1], scale(h)[, 1])
compare

library("ggbiplot")
ggbiplot(h_pca, circle = TRUE)
?ggbiplot
