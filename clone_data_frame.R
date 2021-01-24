vapply(names(df_nodes), paste0,  FUN.VALUE = character(1))
test <- vapply(names(df_nodes), function(x) class(df_nodes[[x]]),  FUN.VALUE = character(1))
paste0(names(df_nodes), " = as.", test, "(),")