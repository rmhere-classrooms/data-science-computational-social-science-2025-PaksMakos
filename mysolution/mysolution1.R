library(igraph)

n <- 100
p <- 0.05

g <- sample_gnp(n = n, p = p)

plot(g)
summary(g)
