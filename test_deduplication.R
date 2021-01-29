library(revtools)

id_graph_selected <- 24954

create_duplicate_edges <- function(df_nodes, id_graph_selected) {
  
  df_pre_dedup <- df_nodes %>% 
    filter(id_graph == id_graph_selected) %>% 
    filter(is_country == 0) %>% 
    filter(name_node != "THE BEARER") %>% 
    mutate(label = name_node) %>% 
    rename(id = name)
  
  # Find indexes that indicate the duplicates
  idx_duplicates <- find_duplicates(df_pre_dedup, match_variable = "name_node")
  
  # Make link between duplicates
  df_duplicates <- cbind(
    df_pre_dedup[idx_duplicates, c("name_node", "id")] %>%
      select(to = id, name_node_unique = name_node), 
    df_pre_dedup[, c("name_node", "id")] %>% 
      select(from = id, name_node_duplicate = name_node)
  ) %>% 
    filter(from != to) %>% 
    select(from, to, everything())
  
  
  # Rerouting duplicates to the ultimate node
  graph_duplicates <- graph_from_data_frame(d = df_duplicates, directed = TRUE)
  
  lst_graph_duplicates <- decompose(graph_duplicates)
  
  for(i in 1:length(lst_graph_duplicates)){
    
    idx_root <- which(sapply(sapply(V(lst_graph_duplicates[[i]]),
                                    function(x) neighbors(lst_graph_duplicates[[i]], x, mode="out")),
                             length) == 0)
    
    E(lst_graph_duplicates[[i]])$root <- V(lst_graph_duplicates[[i]])[idx_root]$name
    
    df_test <- as_data_frame(lst_graph_duplicates[[i]], what = "edges") %>% 
      mutate(to = root) %>% 
      select(-root)
    
    lst_graph_duplicates[[i]] <- graph_from_data_frame(d = df_test, directed = TRUE)
  }
  
  df_edges_rerouted <- do.call(rbind, lapply(lst_graph_duplicates,
                                             as_data_frame,
                                             what = "edges"))
  # End rerouting
  
  # Adding detected duplicates as edges ---
  df_edges_duplicate <- df_edges_rerouted %>% 
    select(from, to) %>% 
    mutate(type_edge = "is_duplicate",
           id_source = as.character(NA),
           valid_until = as.character(NA),
           date_start = as.character(NA),
           date_end = as.character(NA),
           id_graph = id_graph_selected) %>% 
    select(from, to, everything())
  
  return(df_edges_duplicate)
}

df_edges_duplicate <- create_duplicate_edges(df_nodes, id_graph_selected)
df_edges_graph <- df_edges %>% filter(id_graph == id_graph_selected)
graph_with_dupes <- igraph::graph_from_data_frame(d = rbind(df_edges_graph, df_edges_duplicate),
                                                  vertices = df_nodes %>% filter(id_graph == id_graph_selected),
                                                  directed = TRUE) 

source("plot_graph_functions.R")
plot_leaks_graph(graph_with_dupes)
