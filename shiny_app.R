require(shiny)
require(visNetwork)
library(tidyverse)
library(magrittr)
library(feather)
library(graydon.package)
library(igraph)
library(DT)
source("load_panama_data.R")
source("plot_graph_functions.R")

lst_data_objects  <- load_panama_papers(read_raw_data = FALSE)
lst_graphs <- lst_data_objects$lst_graphs
df_graph_summaries <- lst_data_objects$df_graph_summaries


server <- function(input, output) {
  
  # Showing a network of entities
  output$network <- renderVisNetwork({
    plot_leaks_graph(lst_graphs[[20970]])
  })
  
  output$df_summaries <- renderDT(df_graph_summaries)
  
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

shinyApp(ui = ui, server = server)