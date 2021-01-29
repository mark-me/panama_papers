plot_leaks_graph <- function(graph){

  col_graydon <- c("red", "orange", "blue",  "darkgreen", "eggplant" )
  
  lst_image_urls <- read_yaml("config_graph.yml")
  
  df_nodes_vis <- as_data_frame(graph, what = "vertices") %>% 
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
  
  
  df_edges_vis <- as_data_frame(graph, what = "edges") %>% 
    mutate(arrows = ifelse(type_edge %in% c("located_in", "same_as", "same_company_as", "similar_company_as"), "", "to"),
           title = ifelse(type_edge == "same_address_as", "Same address", ""),
           title = ifelse(type_edge == "same_company_as", "Same company", "")
    ) %>% 
    mutate(color = "black") %>% 
    mutate(dashes = type_edge %in% c("same_company_as", "same_address_as", "similar_company_as", "same_name_as", "same_id_as",
                                     "same_intermediary_as", "same_as") )
  
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


plot_graph_legend <- function(){
  
  lst_image_urls <- read_yaml("config_graph.yml")
  
  vec_from <- c(1, 2, 3, 4, 5)
  vec_to <- c(NA, NA, NA, NA, NA)
  vec_description <- c("Company", "Company (BE, NL, UK)", "Person", "Person  (BE, NL, UK)", "Bearer share")
  vec_color <- c(col_graydon[2], col_graydon[2], col_graydon[3], col_graydon[3], "white")
  vec_shape <- c("square", "image", "dot", "circularImage", "circularImage")
  vec_url_image <- c(NA, lst_image_urls$url_flag_nl, NA, lst_image_urls$url_flag_be, lst_image_urls$url_bearer)
  
  df_nodes_legend <- data.frame(
    from = vec_from,
    to = vec_to,
    label = vec_description,
    color = vec_color,
    shape = vec_shape,
    image = vec_url_image
  )
  
  rm(vec_from, vec_to, vec_description, vec_color, vec_shape, vec_url_image)
  
  visNetwork(df_nodes_legend, df_nodes_legend,
             height = "100px", width = "100%") %>% 
    visNodes(color = list(background = "lightblue", 
                          border = "darkblue",
                          highlight = "yellow"),
             shadow = list(enabled = TRUE, size = 10))  %>%
    visLayout(randomSeed = 12) %>% 
    visHierarchicalLayout()%>% 
    visLegend()
}
