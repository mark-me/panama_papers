library(yaml)
library(DBI)
library(RSQLite)
library(revtools)

config <- read_yaml("config.yml")

df_nodes_graph <- df_nodes %>% 
  filter(id_graph == 24954) %>% 
  filter(is_country == 0) %>% 
  filter(name_node != "THE BEARER")

screen_duplicates(df_nodes_graph)

matches <- find_duplicates(df_nodes_graph, match_variable = "name_node")

con <- dbConnect(RSQLite::SQLite(), paste0(config$dir_data, "panama_papers.sqlite"))
dbWriteTable(con, "matches", matches, overwrite = TRUE)
dbDisconnect(con)  