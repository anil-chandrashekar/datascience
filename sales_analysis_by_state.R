#import libraries
library(tidyverse)
library(readxl)
library(lubridate)
#load files
bikes_tbl      <- read_excel ("D:/pADHAI/SEM_3/DataScience/DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel ("D:/pADHAI/SEM_3/DataScience/DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx" )
bikeshops_tbl  <- read_excel("D:/pADHAI/SEM_3/DataScience/DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

#examine files

#join files using the keys product id, bikeshop id and customer id
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

#Examine the joined files 

# Select State as feature and examine the data 

# wrangle data 
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  separate(col    = location,
           into   = c("city","state"),
           sep    = ",") %>%
  mutate(total.price = price * quantity) %>%
  select(-...1, -gender) %>%
  select(-ends_with(".id")) %>%
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  
  select(order.id, contains("order"), contains("model"), contains("city"),state,
         price, quantity, total.price,
         everything()) %>%
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

#extract required column
sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>%
  select(order_date, total_price) %>%
  mutate(year = year(order_date)) %>%
  group_by(year) %>% 
  summarize(sales = sum(total_price)) %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = "???"))



#plot bar chart
sales_by_year_tbl %>%
  
  # Setup canvas with the columns year (x-axis) and sales (y-axis)
  ggplot(aes(x = year, y = sales)) +
  
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = sales_text)) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = "???")) +
  labs(
    title    = "Revenue by year",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )


sales_by_year_state_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # Select columns and add a year
  select(order_date, total_price, state) %>%
  mutate(year = year(order_date)) %>%
  
  # Group by and summarize year and main catgegory
  group_by(year, state) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # Format $ Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = "???"))

#plot by year and locATION 



sales_by_year_state_tbl %>%

  ggplot(aes(x = year, y = sales, fill = state)) +
  geom_col() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

  facet_wrap(~ state, scales = "free_y") +
  
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = "???")) +
  labs(
    title = "Revenue by year and state",
    subtitle = "Each product category has an upward trend",
    fill = "State" # Changes the legend name
  )


library("writexl")
bike_orderlines_wrangled_tbl %>%
  write_xlsx("D:/pADHAI/SEM_3/DataScience/DS_101/00_data/01_bike_sales/bike_orderlines.xlsx")
# 7.2 CSV ----
bike_orderlines_wrangled_tbl %>% 
  write_csv("D:/pADHAI/SEM_3/DataScience/DS_101/00_data/01_bike_sales/bike_orderlines.csv")


# 7.3 RDS ----

bike_orderlines_wrangled_tbl %>% 
  write_rds("D:/pADHAI/SEM_3/DataScience/DS_101/00_data/01_bike_sales/bike_orderlines.rds")












