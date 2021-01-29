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

idx_matches <- find_duplicates(df_nodes_graph, match_variable = "name_node")
df_matches <- cbind(df_nodes_graph[idx_matches, c("name_node", "name")] %>% 
                      select(name_unique = name, name_node_unique = name_node), 
                    df_nodes_graph[, c("name_node", "name")] %>% 
                      select(name_original = name, name_node_original = name_node))

con <- dbConnect(RSQLite::SQLite(), paste0(config$dir_data, "panama_papers.sqlite"))
dbWriteTable(con, "matches", matches, overwrite = TRUE)
dbDisconnect(con)  