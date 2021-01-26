library(DBI)
library(RSQLite)
library(tidyverse)

# Load raw data
df_countries <- read.csv("https://raw.githubusercontent.com/mark-me/mark-me.github.io/master/_pages/tutorials/clustering-mds/COW_country_codes.csv")

# Transforming data
df_countries_transformed <- df_countries %>%
  select(-iso_UN_M49, -CCode, -StateAbb) %>% 
  mutate(is_europe = region == "Europe")

# Open a SQLite database file, or create one if it doesn't exist
con <- dbConnect(RSQLite::SQLite(), "data/example.sqlite")

# Create a table, overwrite one if it already exists (append = TRUE is also an option)
dbWriteTable(con, "countries", df_countries_transformed, overwrite = TRUE)

# See if the table is there
dbListTables(con)

# See the fields of the table
dbListFields(con, "countries")

# Load whole table in data frame
df_countries_db <- dbReadTable(con, "countries")

# See a loss of data-types. Doesn't happen with RDS or feather files. Sad.... But surmountable, make a separate reading function
str(df_countries_transformed)
str(df_countries_db)

# Read a section of that data (countries from Europe)
region <- "Europe"
res <- dbSendQuery(con, paste0("SELECT * FROM countries WHERE region = \"", region,"\""))
df_countries_db <- dbFetch(res)
dbClearResult(res)

# Good practice: disconnect from the database
dbDisconnect(con)
