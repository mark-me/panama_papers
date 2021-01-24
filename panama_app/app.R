require(shiny)
require(visNetwork)
library(tidyverse)
library(magrittr)
library(feather)
library(igraph)
library(DT)
library(DBI)
library(yaml)

config <- read_yaml("config.yml")

server <- function(input, output) {
  
  con <- dbConnect(RSQLite::SQLite(), paste0(config$dir_data, "panama_papers.sqlite"))
  df_graph_summaries <- dbReadTable(con, "graph_summaries") %>% 
    filter(qty_nodes < 120)
  dbDisconnect(con)  
  
  vis_df_graph_summaries <- df_graph_summaries %>% 
    select(id = id_graph,
           Source = name_source,
           `# Companies` = qty_companies,
           `# Dutch` = qty_dutch,
           `# Belgian` = qty_belgian,
           `# UK` = qty_uk)
  
  output$df_summaries <- renderDT(vis_df_graph_summaries, selection = 'single', rownames = FALSE)
  
  output$selected_network <- reactive({
    paste0(ifelse(is.null(input$df_summaries_rows_selected),
                  "None selected",
                  paste0(df_graph_summaries[input$df_summaries_rows_selected, 1], ") ",
                         df_graph_summaries[input$df_summaries_rows_selected, 2]))
                                        )
  })

  # Showing a network of entities
  output$network <- renderVisNetwork({
    id_graph <- df_graph_summaries[input$df_summaries_rows_selected, 1]
    plot_network(id_graph)
  })

  # Showing data frame with selected network nodes
  output$df_network_nodes <- renderDT({
    id_graph <- df_graph_summaries[input$df_summaries_rows_selected, 1]
    con <- dbConnect(RSQLite::SQLite(), paste0(config$dir_data, "panama_papers.sqlite"))

    res <- dbSendQuery(con, paste0("SELECT * FROM nodes WHERE id_graph = ", id_graph))
    df_nodes_graph <- dbFetch(res)
    dbClearResult(res)
    dbDisconnect(con)
    df_nodes_graph
  })

  # In server.R:
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      id_graph <- df_graph_summaries[input$df_summaries_rows_selected, 1]
      con <- dbConnect(RSQLite::SQLite(), paste0(config$dir_data, "panama_papers.sqlite"))

      res <- dbSendQuery(con, paste0("SELECT * FROM nodes WHERE id_graph = ", id_graph))
      df_nodes_graph <- dbFetch(res)

      readr::write_csv2(df_nodes_graph, con)

      dbClearResult(res)
      dbDisconnect(con)
      }
  )
}

ui <- fluidPage(
  
  titlePanel(
    fluidRow(
      column(1, img(height = 64, src = "https://www.icij.org/app/themes/icij/dist/images/logo.svg")),
      column(8, h1("Panama papers", align = "center")), 
      column(3, img(height = 64, width = 64, src = "https://cadran-analytics.nl/wp-content/uploads/2018/10/shiny.png"))
    )
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      h3("Available structures"),
      DTOutput("df_summaries")
    ),
    
    mainPanel(
      h3(textOutput("selected_network")),
      tabsetPanel(type="tabs",
                  tabPanel("Plot", visNetworkOutput("network", height = "800px", width = "100%")),
                  tabPanel("Table",
                           downloadButton("downloadData", "Download..."),
                           br(),
                           DTOutput("df_network_nodes")
                           )
                  )
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
  col_graydon <- c("black", "orange", "blue",  "darkgreen", "eggplant" )
  
  df_nodes_vis <- df_nodes %>% 
    rename(id = name) %>% 
    mutate(label = ifelse(is_country == 1, country, NA),
           label = ifelse(is_address == 1, "", label),
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
           title = type_edge) %>% 
    mutate(color = "black") %>% 
    mutate(dashes = type_edge %in% c("same_company_as", "same_address_as", "similar_company_as", "same_name_as", "same_id_as",
                                     "same_intermediary_as", "same_as") )
  return(df_edges_vis)
}

plot_network <- function(id_graph){
  
  lst_df <- get_graph_data(id_graph)
  
  df_nodes_vis <- prettify_nodes(lst_df$df_nodes)
  df_edges_vis <- prettify_edges(lst_df$df_edges)
  
  # Remove country nodes of entities which have an associated address (the country will show up at the address anyway)
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
             height = "1080px") %>% 
    visNodes(color = list(background = "lightblue", 
                          border = "darkblue",
                          highlight = "yellow"),
             shadow = list(enabled = TRUE, size = 10))  %>%
    visOptions(highlightNearest = TRUE, selectedBy = "group") %>%
    visLayout(randomSeed = 12) 
}

shinyApp(ui = ui, server = server)