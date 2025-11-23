library(igraph)
library(ggplot2)
library(dplyr)
library(tidyr)

data <- read.table("D:/Dokumenty/Studia/2_Stosowana/7_SEM/Danologia/sieci/out.radoslaw_email_email", skip = 2)
edges_df <- data[, 1:2]
g <- graph_from_data_frame(d = edges_df, directed = TRUE)

cnt_i <- degree(g, mode = "out")
E(g)$weight <- 1

g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = list(weight = "sum"))

sources <- ends(g, E(g))[,1]
E(g)$weight <- E(g)$weight / cnt_i[sources]

print(is_simple(g))
print(g)

run_simulation <- function(graph, seeds) {
  g_copy <- graph
  V(g_copy)$activated <- FALSE
  V(g_copy)[seeds]$activated <- TRUE
  last_activated <- seeds
  iteration <- 1
  history <- c(sum(V(g_copy)$activated))

  repeat {
    if (length(last_activated) == 0) {
      break
    }
  
    edges_to_check <- E(g_copy)[.from(last_activated)]
    targets <- head_of(g_copy, edges_to_check)
    already_activated <- V(g_copy)[targets]$activated
    edges_to_check <- edges_to_check[!already_activated]
  
    q <- edges_to_check$weight
    rolls <- runif(length(edges_to_check))
  
    successful_activations <- which(rolls < q)
  
    if(length(successful_activations) > 0) {
      successful_edges <- edges_to_check[successful_activations]
      activated_nodes <- unique(head_of(g_copy, successful_edges))
      V(g_copy)[activated_nodes]$activated <- TRUE
      
      last_activated <- activated_nodes
    } else {
      last_activated <- integer(0)
    }
  
    history <- c(history, sum(V(g_copy)$activated))
    iteration <- iteration + 1
  }
  
  return(history)
}

k <- ceiling(0.05 * vcount(g))
cat("Liczba węzłów początkowych (k):", k, "\n\n")
global_results <- list()

for (i in 1:100) {
  # (i) węzłów o największym outdegree
  top_degree <- order(degree(g, mode = "out"), decreasing = TRUE)[1:k]
  results[["Degree (Out)"]] <- run_simulation(g, top_degree)

  # (ii) najbardziej centralnych węzłów wedle metody betweenness
  top_betweenness <- order(betweenness(g, directed = TRUE), decreasing = TRUE)[1:k]
  results[["Betweenness"]] <- run_simulation(g, top_betweenness)

  # (iii) węzłów największym closeness,
  top_closeness <- order(closeness(g, mode = "out"), decreasing = TRUE)[1:k]
  results[["Closeness"]] <- run_simulation(g, top_closeness)

  # (iv) dowolnych losowych węzłów
  random_nodes <- sample(V(g), k)
  results[["Random"]] <- run_simulation(g, random_nodes)

  # (v) Miara własna: PageRank
  top_pagerank <- order(page_rank(g, directed = TRUE)$vector, decreasing = TRUE)[1:k]
  results[["PageRank (Custom)"]] <- run_simulation(g, top_pagerank)

  global_results[[i]] <- results
}

strategies <- names(global_results[[1]])
plot_data <- data.frame()

for (strategy in strategies) {
  all_runs <- lapply(global_results, function(iter) iter[[strategy]])
  
  max_len <- max(sapply(all_runs, length))
  avg_len <- mean(sapply(all_runs, length))
  max_activated <- max(sapply(all_runs, tail, 1))
  avg_activated <- mean(sapply(all_runs, tail, 1))
  
  cat(strategy, "\n")
  cat("Maksymalna ilość iteracji: ", max_len, "\n")
  cat("Maksymalna ilość aktywacji: ", max_activated, "\n")
  cat("Średnia ilość iteracji: ", avg_len, "\n")
  cat("Średnia ilość aktywacji: ", avg_activated, "\n\n")
  
  padded_runs <- sapply(all_runs, function(x) {
    if (length(x) < max_len) {
      return(c(x, rep(tail(x, 1), max_len - length(x))))
    } else {
      return(x)
    }
  })
  
  avg_trajectory <- rowMeans(padded_runs)
  temp_df <- data.frame(
    Iteration = 1:max_len,
    Activated_Nodes = avg_trajectory,
    Strategy = strategy
  )
  
  plot_data <- rbind(plot_data, temp_df)
}

p <- ggplot(plot_data, aes(x = Iteration, y = Activated_Nodes, color = Strategy)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Dynamika dyfuzji informacji w sieci",
    subtitle = paste0("Średnia liczba aktywowanych węzłów (uśrednione ze 100 symulacji, k=", k, ")"),
    x = "Numer iteracji",
    y = "Liczba aktywowanych węzłów",
    color = "Strategia wyboru węzłów"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 10)
  )

print(p)