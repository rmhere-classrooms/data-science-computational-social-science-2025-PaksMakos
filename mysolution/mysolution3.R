library(shiny)
library(bslib)
library(igraph)
library(ggplot2)
library(dplyr)
library(tidyr)

run_simulation <- function(graph, seeds, max_iterations) {
  g_copy <- graph
  V(g_copy)$activated <- FALSE
  V(g_copy)[seeds]$activated <- TRUE
  last_activated <- seeds
  iteration <- 1
  history <- c(sum(V(g_copy)$activated))

  repeat {
    if (length(last_activated) == 0 | max_iterations < iteration) {
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

url_data <- "https://bergplace.org/share/out.radoslaw_email_email"
data_raw <- read.table(url(url_data), skip = 2)
edges_df <- data_raw[, 1:2]
g_base <- graph_from_data_frame(d = edges_df, directed = TRUE)

E(g_base)$weight <- 1

g_base <- simplify(g_base, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = list(weight = "sum"))
cnt_i <- strength(g_base, mode = "out")

sources <- ends(g_base, E(g_base))[,1]
E(g_base)$base_weight <- E(g_base)$weight / cnt_i[sources]
E(g_base)$weight <- E(g_base)$weight / cnt_i[sources]

print(g_base)

k <- ceiling(0.05 * vcount(g_base))

seeds_list <- list()
seeds_list[["Degree (Out)"]] <- order(degree(g_base, mode = "out"), decreasing = TRUE)[1:k]
seeds_list[["Betweenness"]] <- order(betweenness(g_base, directed = TRUE), decreasing = TRUE)[1:k]
seeds_list[["Closeness"]] <- order(closeness(g_base, mode = "out"), decreasing = TRUE)[1:k]
seeds_list[["PageRank"]] <- order(page_rank(g_base, directed = TRUE)$vector, decreasing = TRUE)[1:k]


ui <- page_sidebar(
  title = "Symulacja Dyfuzji (Independent Cascade)",
  
  sidebar = sidebar(
    h5("Parametry symulacji"),
    
    sliderInput(
      inputId = "prob_factor",
      label = "Modyfikator prawdopodobieństwa (%):",
      min = 10,
      max = 200,
      value = 100,
      step = 10
    ),
    div(class = "text-muted", style="font-size: 0.8em; margin-bottom: 15px;",
        "100% = oryginalne wagi. >100% zwiększa szansę zarażenia."),
    
    sliderInput(
      inputId = "iterations",
      label = "Liczba iteracji:",
      min = 1,
      max = 50,
      value = 10
    ),
    div(class = "text-muted", style="font-size: 0.8em;",
        "Większa liczba daje możliwość dotarcia do większej ilości węzłów.")
  ),
  
  card(
    card_header("Dynamika dyfuzji informacji w czasie"),
    plotOutput(outputId = "diffusionPlot", height = "500px"),
    card_footer(
      paste0("Analizowana sieć: radoslaw_email_email | Liczba węzłów początkowych (k): ", k)
    )
  )
)

server <- function(input, output) {
  output$diffusionPlot <- renderPlot({
    
    g_current <- g_base
    multiplier <- input$prob_factor / 100
    
    new_weights <- pmin(E(g_current)$base_weight * multiplier, 1)
    E(g_current)$weight <- new_weights
    
    current_seeds_list <- seeds_list
    current_seeds_list[["Random"]] <- sample(V(g_current), k)
    
    strategies <- names(current_seeds_list)
    global_results <- list()
    
    withProgress(message = 'Trwa symulacja...', value = 0, {
      
      n_sims <- 100
      
      for (i in 1:n_sims) {
        results <- list()
        for (strat in strategies) {
          results[[strat]] <- run_simulation(g_current, current_seeds_list[[strat]], input$iterations)
        }
        global_results[[i]] <- results
        
        incProgress(1/n_sims, detail = paste("Przebieg", i, "z", n_sims))
      }
    })
    
    plot_data <- data.frame()
    
    for (strategy in strategies) {
      all_runs <- lapply(global_results, function(iter) iter[[strategy]])
      max_len <- max(sapply(all_runs, length))
      
      padded_runs <- sapply(all_runs, function(x) {
        if (length(x) < max_len) {
          return(c(x, rep(tail(x, 1), max_len - length(x))))
        } else {
          return(x)
        }
      })
      
      if (is.vector(padded_runs)) {
        padded_runs <- matrix(padded_runs, ncol = 1)
      }
      
      avg_trajectory <- rowMeans(padded_runs)
      
      temp_df <- data.frame(
        Iteration = 1:max_len,
        Activated_Nodes = avg_trajectory,
        Strategy = strategy
      )
      plot_data <- rbind(plot_data, temp_df)
    }
    
    ggplot(plot_data, aes(x = Iteration, y = Activated_Nodes, color = Strategy)) +
      geom_line(linewidth = 1.2, alpha = 0.8) +
      labs(
        title = paste0("Wpływ modyfikatora wag: ", input$prob_factor, "%"),
        subtitle = paste0("Średnia z ", input$iterations, " symulacji"),
        x = "Krok czasowy",
        y = "Liczba aktywowanych węzłów",
        color = "Strategia"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      ) +
      scale_color_brewer(palette = "Set1")
    
  })
}

shinyApp(ui = ui, server = server)