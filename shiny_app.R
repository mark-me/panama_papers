require(shiny)
require(visNetwork)
library(tidyverse)
library(magrittr)
library(feather)
library(igraph)
library(DT)
library(DBI)
source("load_panama_data.R")
source("plot_graph_functions.R")


server <- function(input, output) {
  
  con <- dbConnect(RSQLite::SQLite(), paste0(config$dir_data, "panama_papers.sqlite"))
  df_graph_summaries <- dbReadTable(con, "graph_summaries")
  dbDisconnect(con)  
  
  output$df_summaries <- renderDT(df_graph_summaries)
  
  # Showing a network of entities
  output$network <- renderVisNetwork({
    plot_network(20339)
  })
  
}

ui <- fluidPage(
  
  titlePanel("Panama Papers"),
  
  sidebarLayout(
    
    sidebarPanel("sidebar panel"),
    
    mainPanel(
      
      dataTableOutput("df_summaries"),
      
      visNetworkOutput("network")
    )
  )
  
)

get_graph_data <- function(id_graph){
  con <- dbConnect(RSQLite::SQLite(), paste0(config$dir_data, "panama_papers.sqlite"))
  
  res <- dbSendQuery(con, paste0("SELECT * FROM nodes WHERE id_graph = ", id_graph))
  df_nodes_graph <- dbFetch(res)
  dbClearResult(res)
  
  res <- dbSendQuery(con, paste0("SELECT * FROM edges WHERE id_graph = ", id_graph))
  df_edges_graph <- dbFetch(res)
  dbClearResult(res)
  
  dbDisconnect(con)
  
  return(list(df_nodes = df_nodes_graph,
              df_edges = df_edges_graph))
}

prettify_nodes <- function(df_nodes){
  
  lst_image_urls <- read_yaml("config_graph.yml")
  col_graydon <- c("red", "orange", "blue",  "darkgreen", "eggplant" )
  
  df_nodes_vis <- df_nodes %>% 
    rename(id = name) %>% 
    mutate(label = ifelse(is_address == 1, "", name_node),
           label = ifelse(is_country, country, name_node),
           title = ifelse(is_address == 1, address, NA),
           title = ifelse(is_company == 1, paste(paste0("Type: ", company_type),
                                                 paste0("Status:", status),
                                                 sep = "<br>"), title),
           shadow = TRUE,
           shape = "circularImage",
           shape = ifelse(is_country == 1 | is_address, "image", shape),
           color = col_graydon[5],
           color = ifelse(is_company == 1, col_graydon[2], color),
           color = ifelse(is_officer == 1, col_graydon[4], color),
           color = ifelse(is_intermediary == 1, col_graydon[2], color),
           color = ifelse(name_node == "THE BEARER", "white", color),
           font.color = col_graydon[1],
           image = ifelse(is_officer == 1, lst_image_urls$url_person, image),
           image = ifelse(is_intermediary == 1, lst_image_urls$url_intermediary, image),
           image = ifelse(name_node == "THE BEARER", lst_image_urls$url_bearer, image),
           image = ifelse(is_company == 1, lst_image_urls$url_company, image),
           image = ifelse(is_country & name_node == "XXX", lst_image_urls$url_country_unknown, image),
           image = ifelse(is_address == 1, lst_image_urls$url_address, image),
           group = ifelse(is_officer == 1, "Officers", NA),
           group = ifelse(is_company == 1, "Companies", group),
           group = ifelse(is_intermediary == 1, "Intermediaries", group))
  
  return(df_nodes_vis)
}

prettify_edges <- function(df_edges){
  
  df_edges_vis <- df_edges %>% 
    mutate(arrows = ifelse(type_edge %in% c("located_in", "same_as", "same_company_as", "similar_company_as"), "", "to"),
           title = ifelse(type_edge == "same_address_as", "Same address", ""),
           title = ifelse(type_edge == "same_company_as", "Same company", "")
    ) %>% 
    mutate(color = "black") %>% 
    mutate(dashes = type_edge %in% c("same_company_as", "same_address_as", "similar_company_as", "same_name_as", "same_id_as",
                                     "same_intermediary_as", "same_as") )
  return(df_edges_vis)
}

plot_network <- function(id_graph){
  
  lst_df <- get_graph_data(id_graph)
  
  df_nodes_vis <- prettify_nodes(lst_df$df_nodes)
  df_edges_vis <- prettify_edges(lst_df$df_edges)
  
  # Remove country nodes of entities which have an associated address (the country will show up at the adress anyway)
  id_country_superfluous <- (df_nodes_vis %>% 
                               filter(is_country == 1) %>% 
                               inner_join(df_edges_vis, by = c("id"="to")) %>% 
                               select(id_node_country = id, 
                                      id_node_entity = from) %>% 
                               #mutate(id_node_entity = as.integer(id_node_entity)) %>% 
                               inner_join(df_edges_vis, by = c("id_node_entity"="from")) %>% 
                               filter(type_edge == "registered_address"))$id_node_country
  
  df_nodes_vis %<>% filter(!id %in% id_country_superfluous)
  df_edges_vis %<>% filter(!to %in% id_country_superfluous)
  
  visNetwork(df_nodes_vis, df_edges_vis,
             height = "800px", width = "100%") %>% 
    visNodes(color = list(background = "lightblue", 
                          border = "darkblue",
                          highlight = "yellow"),
             shadow = list(enabled = TRUE, size = 10))  %>%
    visOptions(highlightNearest = TRUE, selectedBy = "group") %>%
    visLayout(randomSeed = 12) 
}

shinyApp(ui = ui, server = server)