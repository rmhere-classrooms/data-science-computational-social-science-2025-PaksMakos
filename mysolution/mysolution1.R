library(igraph)

n <- 100
p <- 0.05

g <- sample_gnp(n = n, p = p)

summary(g)
# graf nie jest ważony

E(g)
V(g)

E(g)$weight <- runif(length(E(g)), min = 0.01, max = 1)

summary(g)
# po dodaniu wag do krawędzi graf jest ważony

degree(g)

hist(degree(g))

# connected components (klastry)
comp <- components(g)
cat("Number of connected components:", comp$no, "\n")

pr <- page.rank(g)$vector

plot(g, vertex.size=pr*250, vertex.label=NA, edge.arrow.size=.2)