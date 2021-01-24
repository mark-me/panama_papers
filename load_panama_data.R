library(yaml)
library(DBI)
library(RSQLite)
config <- read_yaml("config.yml")

load_panama_papers <- function(read_raw_data = TRUE, only_graph_data = TRUE){

  if(read_raw_data){

    df_nodes <- load_nodes()
    df_edges <- load_edges(df_nodes)
    df_nodes <- add_country_nodes(df_nodes)           # Add countries as separate nodes
    df_edges <- add_country_edges(df_edges, df_nodes) # Connect country nodes to their originating nodes
  
    # Create list of graphs and enriched nodes and edges data frames and a summary data frame
    lst_output <- create_graphs(df_nodes, df_edges)
    lst_graphs <- lst_output$lst_graphs
    df_nodes <- lst_output$df_nodes
    df_edges <- lst_output$df_edges
    df_graph_summaries <- lst_output$df_graph_summaries
    
    # Writing results to a SQLite database ----
    con <- dbConnect(RSQLite::SQLite(), paste0(config$dir_data, "panama_papers.sqlite"))
    dbWriteTable(con, "nodes", df_nodes, overwrite = TRUE)
    dbWriteTable(con, "edges", df_edges, overwrite = TRUE)
    dbWriteTable(con, "graph_summaries", df_graph_summaries, overwrite = TRUE)
    dbDisconnect(con)  

    # Write graphs to 
    write_rds(lst_graphs, paste0(config$dir_data, "list_graphs.Rds")  )

  } else {

    # Read nodes, edges and summary from SQLite database
    con <- dbConnect(RSQLite::SQLite(), paste0(config$dir_data, "panama_papers.sqlite"))
    df_nodes <- dbReadTable(con, "nodes")
    df_edges <- dbReadTable(con, "edges")
    df_graph_summaries <- dbReadTable(con, "graph_summaries")
    dbDisconnect(con)  
    
    # Read list of graph objects from RDS
    lst_graphs <- read_rds(paste0(config$dir_data, "list_graphs.Rds"))
  }
  
  # Create a list with all data frames and return it
  if(only_graph_data){
    
    list_df <- list(lst_graphs = lst_graphs,
                    df_graph_summaries = df_graph_summaries)
  } else {
    
    list_df <- list(df_nodes = df_nodes,
                    df_edges = df_edges,
                    lst_graphs = lst_graphs,
                    df_graph_summaries = df_graph_summaries)
  }
  
  
  return(list_df)
}


# Loading node data ----
load_nodes <- function(){
  
  # Addresses ---
  vec_files <- c("bahamas_leaks.nodes.address.csv",
                 "offshore_leaks.nodes.address.csv",
                 "panama_papers.nodes.address.csv",
                 "paradise_papers.nodes.address.csv")
  vec_files <- paste0(config$dir_data, vec_files)
  df_address <- merge_files(vec_files)
  df_address %<>% mutate(type_node = "address")
  
  # Entities ---
  vec_files <- c("bahamas_leaks.nodes.entity.csv",
                 "offshore_leaks.nodes.entity.csv", 
                 "panama_papers.nodes.entity.csv", 
                 "paradise_papers.nodes.entity.csv")
  vec_files <- paste0(config$dir_data, vec_files)
  df_entity <- merge_files(vec_files)
  df_entity %<>% mutate(type_node = "company")
  
  # Intermediaries ---
  vec_files <- c("bahamas_leaks.nodes.intermediary.csv",
                 "offshore_leaks.nodes.intermediary.csv", 
                 "panama_papers.nodes.intermediary.csv", 
                 "paradise_papers.nodes.intermediary.csv")
  vec_files <- paste0(config$dir_data, vec_files)
  df_intermediaries <- merge_files(vec_files)
  df_intermediaries %<>% mutate(type_node = "intermediaries")
  
  # Officer ---
  vec_files <- c("bahamas_leaks.nodes.officer.csv",
                 "offshore_leaks.nodes.officer.csv", 
                 "panama_papers.nodes.officer.csv",
                 "paradise_papers.nodes.officer.csv")
  vec_files <- paste0(config$dir_data, vec_files)
  df_officers <- merge_files(vec_files)
  df_officers %<>% mutate(type_node = "officers")
  
  # Merge nodes ---
  df_nodes <- plyr::rbind.fill(df_address, df_entity, df_intermediaries, df_officers)
  
  rm(df_address, df_entity, df_intermediaries, df_officers)
  
  # Transformations on nodes
  df_nodes %<>%
    rename(id_node = node_id,
           id_source = sourceID,
           name_node = name) %>% 
    mutate(id_source = as.factor(id_source),
           valid_until = as.factor(valid_until),
           note = ifelse(note == "", NA, note),
           is_company = type_node == "company",
           is_officer = type_node == "officers",
           is_intermediary = type_node == "intermediaries",
           is_address = type_node == "address") %>% 
    select(-type_node) %>% 
    group_by_at(vars(!starts_with("is_"))) %>% 
    summarise_at(vars(starts_with("is")), max) %>% 
    ungroup() %>% 
    mutate(is_dutch = str_detect(country_codes, 'NLD'),
           is_belgian = str_detect(country_codes, 'BEL'),
           is_UK = str_detect(country_codes, 'GBR')) %>% 
    mutate(is_dutch = ifelse(is.na(is_dutch), FALSE, is_dutch),
           is_belgian = ifelse(is.na(is_belgian), FALSE, is_belgian),
           is_UK = ifelse(is.na(is_UK), FALSE, is_UK)) %>% 
    select(id_node, everything())
  
  return(df_nodes)
}

# Loading connection data ----
load_edges <- function(df_nodes){
  
  # Edges ---
  vec_files <- c("bahamas_leaks.edges.csv",
                 "offshore_leaks.edges.csv",
                 "panama_papers.edges.csv",
                 "paradise_papers.edges.csv")
  vec_files <- paste0(config$dir_data, vec_files)
  df_edges <- merge_files(vec_files)
  
  rm(vec_files)
  
  df_edges %<>%
    rename(from = START_ID,
           to = END_ID,
           type_edge = TYPE,
           id_source = sourceID,
           date_start = start_date,
           date_end = end_date) %>% 
    mutate(id_source = as.factor(id_source),
           type_edge = as.factor(type_edge),
           date_start = ifelse(date_start == "", NA, date_start),
           date_end = ifelse(date_end == "", NA, date_end)) %>% 
    select(from,
           to,
           everything())
  
  # Detecting edges without non-existing start or end points
  df_invalid_edges <- df_edges %>% 
    filter(!from %in% df_nodes$id_node | !to %in% df_nodes$id_node)
  
  write_feather(df_invalid_edges, paste0(config$dir_data, "edges_invalid.feather"))
  
  df_edges %<>% 
    filter(from %in% df_nodes$id_node & to %in% df_nodes$id_node)
  
  return(df_edges)
}

# Adding country information as seperate nodes ----
add_country_nodes <- function(df_nodes){
  
  df_countries <- read.csv("https://raw.githubusercontent.com/mark-me/mark-me.github.io/master/_pages/tutorials/clustering-mds/COW_country_codes.csv",
                           stringsAsFactors = FALSE) %>% 
    select(code_country = iso_alpha_3,
           country = country_area,
           region,
           sub_region)
  
  df_node_countries <- df_nodes %>% 
    mutate(country_codes = ifelse(country_codes == "", NA, country_codes)) %>% 
    filter(!is.na(country_codes)) %>% 
    select(id_node, id_source, country_codes)
  
  df_country_codes <- cbind(
    select(df_node_countries, id_node, id_source),
    as.data.frame(str_split(df_node_countries$country_codes, pattern=";", simplify = TRUE))
  )
  
  df_country_codes %<>% 
    pivot_longer(cols = starts_with("V"), values_to = "code_country") %>%
    select(-name) %>% 
    filter(code_country != "") %>% 
    mutate(url_flag = paste0("https://raw.githubusercontent.com/linssen/country-flag-icons/master/images/png/", tolower(code_country), ".png")) %>% 
    rename(from = id_node) %>% 
    mutate(id_node = paste0(code_country, "_", from)) %>%
    left_join(df_countries, by = "code_country")
  
  df_country_codes %<>%
    select(id_node,
           from_id_node = from,
           name_node = code_country,
           image = url_flag,
           country,
           region,
           sub_region)%>% 
    mutate(is_country = TRUE,
           is_company = 0,
           is_officer = 0,
           is_intermediary = 0,
           is_address = 0,
           is_dutch = FALSE,
           is_belgian = FALSE,
           is_UK = FALSE)
  
  df_nodes <- plyr::rbind.fill(df_nodes, df_country_codes) %>% 
    mutate(is_country = ifelse(is.na(is_country), FALSE, is_country))
  
  return(df_nodes)
}

# Adding edges between countries and relevant entity nodes ----
add_country_edges <- function(df_edges, df_nodes){
  
  df_located_in <- df_nodes %>%
    filter(is_country) %>% 
    mutate(type_edge = "located_in") %>% 
    select(from = from_id_node,
           to = id_node,
           type_edge, 
           id_source,
           valid_until ) %>% 
    mutate(date_start = NA,
           date_end = NA)
  
  df_edges <- rbind(df_edges, df_located_in)
  
  return(df_edges)
}


# Merging similarly shaped files ----
merge_files <- function(file_names){
  
  df_bahama <- read.csv(file_names[1], stringsAsFactors = FALSE) 
  df_offshore <- read.csv(file_names[2], stringsAsFactors = FALSE)
  df_panama <- read.csv(file_names[3], stringsAsFactors = FALSE)
  df_paradise <- read.csv(file_names[4], stringsAsFactors = FALSE) 
  
  cols_intersecting <-  intersect(names(df_bahama), names(df_offshore))
  cols_intersecting2 <- intersect(names(df_panama), names(df_paradise))
  cols_intersecting <- intersect(cols_intersecting, cols_intersecting2)
  
  df_bahama %<>% select(cols_intersecting)
  df_offshore %<>% select(cols_intersecting)
  df_panama %<>% select(cols_intersecting)
  df_paradise %<>% select(cols_intersecting)
  
  df_merged <- rbind(df_bahama, df_offshore, df_panama, df_paradise)
  rm(df_bahama, df_offshore, df_panama, df_paradise)
  
  return(df_merged)
}

# Create graphs and their summaries ----
create_graphs <- function(df_nodes, df_edges){
  
  graph_panama <- graph_from_data_frame(d = df_edges, vertices = df_nodes, directed = TRUE)
  
  lst_graphs_decomposed <- decompose.graph(graph_panama)
  
  lst_graphs <- list(0)
  lst_df_nodes <- list(0)
  lst_df_edges <- list(0)
  lst_df_summaries <- list(0)
  
  for (graph_concern in lst_graphs_decomposed){
    
    if(class(graph_concern) == "igraph") {
      
      id_graph <- length(lst_graphs) + 1
      lst_graphs[[id_graph]] <- graph_concern
      
      lst_df_nodes[[id_graph]] <- as_data_frame(graph_concern, what = "vertices") %>% mutate(id_graph = id_graph)
      lst_df_edges[[id_graph]] <- as_data_frame(graph_concern, what = "edges") %>% mutate(id_graph = id_graph)
      lst_df_summaries[[id_graph]] <- data.frame(
        id_graph = id_graph,
        name_source = as.character(V(graph_concern)$id_source[1]),
        qty_companies =  sum(V(graph_concern)$is_company),
        qty_officers = sum(V(graph_concern)$is_officer),
        qty_intermediaries = sum(V(graph_concern)$is_intermediary),
        qty_address = sum(V(graph_concern)$is_address),
        qty_dutch = sum(V(graph_concern)$is_dutch),
        qty_belgian = sum(V(graph_concern)$is_belgian),
        qty_uk = sum(V(graph_concern)$is_UK)
      )
    }
    
  }
  rm(graph_concern)
  
  df_nodes_new <- do.call(rbind,lst_df_nodes) %>% filter(id_graph != 0)
  df_edges_new <- do.call(rbind,lst_df_edges) %>% filter(id_graph != 0)
  df_graph_summaries <- do.call(rbind,lst_df_summaries) %>% filter(id_graph != 0) %>% 
    mutate(qty_nodes = qty_companies + qty_officers + qty_intermediaries + qty_address)
  
  # Create a list with all data frames and return it
  list_objects <- list(lst_graphs = lst_graphs,
                       df_nodes = df_nodes_new,
                       df_edges = df_edges_new,
                       df_graph_summaries = df_graph_summaries)
  
  return(list_objects)  
}
