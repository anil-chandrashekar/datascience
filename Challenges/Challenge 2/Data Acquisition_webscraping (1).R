
library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing

url_home          <- "https://www.radon-bikes.de/"
xopen(url_home)
html_home         <- read_html(url_home)
bike_family_tbl <- html_home %>%
html_nodes(css = ".megamenu__item > a") %>%  
html_attr('href') %>%  
discard(.p = ~stringr::str_detect(.x,"wear")) %>%  
enframe(name = "position", value = "cat_subcat_url") %>%  
  
mutate(family_id = str_glue("https://www.radon-bikes.de{cat_subcat_url}bikegrid"))
bike_family_tbl


bike_category_url <- bike_family_tbl$family_id[1]
xopen(bike_category_url)
html_bike_category  <- read_html(bike_category_url)

bike_name_tbl        <- html_bike_category %>%
html_nodes(css = ".m-bikegrid__info .a-heading--small") %>%
html_text() %>%


enframe(name = "position", value = "name")
bike_name_tbl 

bike_price_tbl <- html_bike_category %>%
html_nodes(css = ".m-bikegrid__price.currency_eur .m-bikegrid__price--active") %>%  
html_text() %>% 
enframe(name = "position", value = "price")
bike_price_tbl

model_price_tbl <- left_join(bike_name_tbl, bike_price_tbl)%>% 
select(name, price)
model_price_tbl
