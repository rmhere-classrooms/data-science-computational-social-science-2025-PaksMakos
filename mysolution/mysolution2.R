library(igraph)

set.seed(42)
n <- 1000
m <- 1

g <- sample_pa(n, m=m, directed = FALSE)

layout_fr <- layout_with_fr(g)

plot(g,
     layout = layout_fr,
     vertex.size = 1.5,
     vertex.label = NA,
     edge.arrow.size = 0.1
)

b_centrality <- betweenness(g, directed = FALSE)
most_central_node_id <- which.max(b_centrality)

print(paste("Najbardziej centralny węzeł:", most_central_node_id))
print(paste("Wartość centralności:", b_centrality[most_central_node_id]))

d <- diameter(g, directed = FALSE)

print(paste("Średnica grafu:", d))

# Główna różnica pomiędzy grafem Barabási-Albert, a grafem Erdős-Rényi jest sposób ich tworzenia
# W grafie Erdős-Rényi każda para węzłów ma jednakowe prawdopodobieństwo bycia połączonymi krawędzią. Algorytm tworzący bierze każdą możliwą krawędź i decyduje, czy ją dodać na podstawie ustalonego prawdopodobieństwa p.
# W grafie Barabási-Albert nowe węzły są dodawane jeden po drugim i łączone z istniejącymi węzłami. Prawdopodobieństwo połączenia określane jest przez stopień istniejącego węzła. "Bogatsi stają się jeszcze bogatsi"