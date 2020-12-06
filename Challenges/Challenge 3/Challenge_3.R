# list of the top 10 companies (worldwide) with the most patents. The top 5 USPTO tech main classes
library(vroom)
library(data.table)
library(tidyverse)
library(lubridate)


col_types <- list(
  id = col_character(),
  type = col_character(),
  name_first = col_character(),
  name_last = col_character(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "C:/Users/anilm/OneDrive/Documents/GitHub/datascience/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

col_types <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "C:/Users/anilm/OneDrive/Documents/GitHub/datascience/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

col_types <- list(
  uuid = col_character(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_character(),
  sequence = col_character()
  
)

uspc_tbl <- vroom(
  file       = "C:/Users/anilm/OneDrive/Documents/GitHub/datascience/uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

Join_tbl <- merge(patent_assignee_tbl,assignee_tbl, by.x = "assignee_id", by.y = "id") 
Join_tbl_3 <- merge(Join_tbl,uspc_tbl, by = "patent_id")



final_tbl <- Join_tbl_3 %>%
  select(patent_id,organization,mainclass_id,sequence)%>%
  group_by(organization)%>%
  sort(n, decreasing = TRUE)%>%
  filter(sequence == 4)

